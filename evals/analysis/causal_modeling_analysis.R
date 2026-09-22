# Packages
library(tidyverse)
library(broom)
library(broom.helpers)
library(glue)
library(fixest)       # fast GLMs with clustering
library(janitor)
library(cobalt)     # for balance metrics & love plots
library(tableone)   # for standardized mean differences
library(MatchIt)
library(gt)

# ---- helpers ----

# Build hour-of-day bin and day-of-week from first_message_time
.enrich_time <- function(df) {
  df %>%
    mutate(
      first_message_time = as_datetime(first_message_time),
      hour = lubridate::hour(first_message_time),
      dow  = lubridate::wday(first_message_time, label = TRUE, week_start = 1),
      tod_bin = cut(
        hour, breaks = c(-Inf,3,6,9,12,15,18,21,24),
        labels = c("12-3a","3-6a","6-9a","9-12p","12-3p","3-6p","6-9p","9-12a"),
        right = TRUE
      )
    )
}

add_period_convo <- function(df) {
  stopifnot("first_message_time" %in% names(df))
  dt <- df$first_message_time
  if (inherits(dt, "POSIXt")) dt <- as.Date(dt, tz = "UTC") else dt <- as.Date(dt)
  
  pre_cutoff   <- as.Date("2025-01-09")
  pilot_end    <- as.Date("2025-03-07")
  deploy_start <- as.Date("2025-03-08")
  
  period <- dplyr::case_when(
    dt <  pre_cutoff                 ~ "Pre-deployment",
    dt >= pre_cutoff & dt <= pilot_end ~ "Pilot deployment",
    dt >= deploy_start               ~ "Deployment",
    TRUE                             ~ NA_character_
  )
  df$period <- factor(period, levels = c("Pre-deployment","Pilot deployment","Deployment"))
  df
}

# Parse case_categorization like "Anxiety; Depression; Other: xyz"
# Returns top_K one-hot indicators (excluding "Other*") + "Other" catchall if present
.build_presenting_concerns <- function(df, col = "case_categorization", top_K = 12) {
  if (!col %in% names(df)) return(df)
  raw <- df[[col]]
  raw[is.na(raw)] <- ""
  # split on ; or , and trim
  split_list <- strsplit(raw, "[;,]", perl = TRUE)
  split_list <- lapply(split_list, function(v) {
    v <- str_squish(v)
    v <- v[nzchar(v)]
    v
  })
  # frequency table
  all_cats <- unlist(split_list, use.names = FALSE)
  if (length(all_cats) == 0L) return(df)
  tab <- sort(table(all_cats), decreasing = TRUE)
  cats <- names(tab)
  top <- cats[!grepl("^Other", cats)]
  top <- head(top, top_K)
  
  out <- df
  for (cat in top) {
    nm <- janitor::make_clean_names(paste0("cc_", cat))
    out[[nm]] <- vapply(split_list, function(v) cat %in% v, logical(1))
  }
  # add "cc_other" if any non-top/Other category exists or any "Other:*"
  has_other <- vapply(split_list, function(v) any(grepl("^Other", v)), logical(1)) |
    vapply(split_list, function(v) any(!(v %in% top) & !grepl("^Other", v)), logical(1))
  out[["cc_other"]] <- has_other
  out
}

# Convert 0/1 or TRUE/FALSE to numeric 0/1 for GLM
.bin01 <- function(x) as.integer(as.logical(x))

# Pretty labels for output
.pretty_var <- function(x) {
  x <- gsub("^cc_", "PC: ", x)
  x <- gsub("_", " ", x, fixed = TRUE)
  tools::toTitleCase(x)
}

# ---- main runner ----
# Fits:
#   - logistic (binomial) for binary outcomes (P*, M*, S*, X*)
#   - linear for fractional_score
# Adjusts for specified covariates; clusters SEs by message_sender_id.
# Optionally adds counselor fixed effects (FE) and/or calendar-week FE.
run_ai_vs_noai_models <- function(
    df,
    outcomes = c("P1","P2","P3","P4","P5","P6","P8","M1","M2","M3","M4","M5",
                 "S1","S2","S3","S4","S5","S6","S7","X0","X1","X2","X3","X4",
                 "fractional_score"),
    pc_top_k = 12,
    add_counselor_fe = FALSE,     # if TRUE: include counselor fixed effects
    add_week_fe = TRUE,           # if TRUE: include calendar week FE
    exposure = c("binary","count","binned","both","both_binned"),
    standardize_count = TRUE      # used only when exposure includes "count"
) {
  exposure <- match.arg(exposure)
  
  # ---- prep exposure(s) ----
  stopifnot("ai_used" %in% names(df))
  df <- df %>%
    dplyr::mutate(
      ai_used = factor(ifelse(ai_used %in% c(TRUE,1,"1","Yes","yes"), "Yes","No"),
                       levels = c("No","Yes"))
    )
  
  # ai_message_count handling
  if (!"ai_message_count" %in% names(df)) df$ai_message_count <- NA_real_
  df$ai_message_count <- as.numeric(df$ai_message_count)
  df$ai_message_count[is.na(df$ai_message_count)] <- 0
  
  # Continuous (std) version (only used for "count" / "both")
  if (isTRUE(standardize_count)) {
    s <- stats::sd(df$ai_message_count, na.rm = TRUE)
    m <- mean(df$ai_message_count, na.rm = TRUE)
    if (isTRUE(is.finite(s)) && s > 0) {
      df$ai_message_count_std <- (df$ai_message_count - m) / s
    } else {
      df$ai_message_count_std <- df$ai_message_count
    }
  } else {
    df$ai_message_count_std <- df$ai_message_count
  }
  
  # Binned version: 0, 1–5, 6–10, >10 (>10 means >= 11)
  df <- df %>%
    dplyr::mutate(
      ai_count_bin = dplyr::case_when(
        ai_message_count == 0                        ~ "0",
        ai_message_count >= 1  & ai_message_count <= 5  ~ "1-5",
        ai_message_count >= 6  & ai_message_count <= 10 ~ "6-10",
        ai_message_count >= 11                          ~ ">10",
        TRUE ~ NA_character_
      ),
      ai_count_bin = factor(ai_count_bin, levels = c("0","1-5","6-10",">10"))
    )
  
  # ---- time + presenting concerns ----
  df <- df %>%
    .enrich_time() %>%
    .build_presenting_concerns(top_K = pc_top_k)
  
  # calendar week FE (optional)
  if (add_week_fe) {
    df <- df %>% dplyr::mutate(week = lubridate::floor_date(first_message_time, "week", week_start = 1))
  }
  
  # ---- covariates ----
  base_covars <- c(
    "counselor_total_msgs_sent_so_far",
    "counselor_copilot_msgs_sent_so_far",
    "time_with_foundation_so_far",
    # "n_messages","n_counselor_messages","n_client_messages","convo_duration_mins",
    "employment_status",
    "education_level",
    "num_simultaneous_convos_cat",
    # "is_multitasking",
    "tod_bin",
    "dow"
  )
  pc_covars <- grep("^cc_", names(df), value = TRUE)
  covars <- c(base_covars[base_covars %in% names(df)], pc_covars)
  
  count_NAs <- map_dbl(covars, 
                   ~sum(is.na(df[[.x]]))) %>% 
    setNames(covars)
  
  print(count_NAs)
  
  # ---- RHS assembly ----
  exposure_terms <- switch(
    exposure,
    "binary"       = "ai_used",
    "count"        = "ai_message_count_std",
    "binned"       = "ai_count_bin",
    "both"         = "ai_used + ai_message_count_std",
    "both_binned"  = "ai_used + ai_count_bin"
  )
  
  rhs_main <- paste(
    c(exposure_terms,
      if (length(covars)) paste(covars, collapse = " + ") else NULL),
    collapse = " + "
  )
  
  fe_part <- paste(
    if (add_counselor_fe) "message_sender_id" else NULL,
    if (add_week_fe) "week" else NULL,
    sep = " + "
  )
  fe_part <- gsub("^ \\+ | \\+ $", "", fe_part)
  fe_clause <- if (nzchar(gsub("\\s|\\+", "", fe_part))) paste0(" | ", fe_part) else ""
  
  results <- list()
  
  for (y in outcomes) {
    if (!y %in% names(df)) next
    dat <- df
    
    # Build formula
    fml_str <- glue::glue("{y} ~ {rhs_main}{fe_clause}")

    # Outcome family
    is_binary <- all(stats::na.omit(unique(dat[[y]])) %in% c(0,1,TRUE,FALSE,"0","1"))
    
    if (is_binary) {
      dat[[y]] <- .bin01(dat[[y]])
      fit <- fixest::feglm(
        stats::as.formula(fml_str),
        data = dat, family = "binomial",
        cluster = ~ message_sender_id
      )
      tid <- broom::tidy(fit, conf.int = TRUE)
      
      # Which exposure terms to pull out?
      target_terms <- character(0)
      # binary
      if (exposure %in% c("binary","both","both_binned")) target_terms <- c(target_terms, "ai_usedYes")
      # continuous count
      if (exposure %in% c("count","both")) target_terms <- c(target_terms, "ai_message_count_std")
      # binned count (all non-reference levels)
      if (exposure %in% c("binned","both_binned")) {
        target_terms <- c(target_terms, grep("^ai_count_bin", tid$term, value = TRUE))
      }
      
      tid_exp <- tid %>%
        dplyr::filter(term %in% target_terms) %>%
        dplyr::mutate(
          exposure = dplyr::case_when(
            term == "ai_usedYes" ~ "binary",
            term == "ai_message_count_std" ~ if (standardize_count) "count (per 1 SD)" else "count (per 1 unit)",
            grepl("^ai_count_bin", term) ~ "binned",
            TRUE ~ "other"
          ),
          contrast = dplyr::case_when(
            term == "ai_usedYes" ~ "AI used: Yes vs No",
            term == "ai_message_count_std" ~ "Per-unit increase",
            grepl("^ai_count_bin", term) ~ paste0(
              gsub("^ai_count_bin", "", term),
              " vs 0"
            ),
            TRUE ~ term
          )
        ) %>%
        dplyr::transmute(
          outcome = y,
          term,
          exposure,
          contrast,
          # manual exponentiation to get ORs & CI
          OR        = exp(estimate),
          conf.low  = exp(conf.low),
          conf.high = exp(conf.high),
          p.value
        )
      
      results[[y]] <- list(model = fit, tidy = tid_exp, full = tid)
      
    } else {
      fit <- fixest::feols(
        stats::as.formula(fml_str),
        data = dat,
        cluster = ~ message_sender_id
      )
      tid <- broom::tidy(fit, conf.int = TRUE)
      
      target_terms <- character(0)
      if (exposure %in% c("binary","both","both_binned")) target_terms <- c(target_terms, "ai_usedYes")
      if (exposure %in% c("count","both")) target_terms <- c(target_terms, "ai_message_count_std")
      if (exposure %in% c("binned","both_binned")) {
        target_terms <- c(target_terms, grep("^ai_count_bin", tid$term, value = TRUE))
      }
      
      tid_exp <- tid %>%
        dplyr::filter(term %in% target_terms) %>%
        dplyr::mutate(
          exposure = dplyr::case_when(
            term == "ai_usedYes" ~ "binary",
            term == "ai_message_count_std" ~ if (standardize_count) "count (per 1 SD)" else "count (per 1 unit)",
            grepl("^ai_count_bin", term) ~ "binned",
            TRUE ~ "other"
          ),
          contrast = dplyr::case_when(
            term == "ai_usedYes" ~ "AI used: Yes vs No",
            term == "ai_message_count_std" ~ "Per-unit increase",
            grepl("^ai_count_bin", term) ~ paste0(
              gsub("^ai_count_bin", "", term),
              " vs 0"
            ),
            TRUE ~ term
          )
        ) %>%
        dplyr::transmute(
          outcome = y,
          term,
          exposure,
          contrast,
          beta = estimate, conf.low, conf.high, p.value
        )
      
      results[[y]] <- list(model = fit, tidy = tid_exp, full = tid)
    }
  }
  
  # Combine exposure rows
  primary <- dplyr::bind_rows(lapply(results, `[[`, "tidy"))
  
  # Order exposure types in output (binary → binned → count)
  if ("exposure" %in% names(primary)) {
    primary$exposure <- factor(
      primary$exposure,
      levels = c("binary", "binned", "count (per 1 SD)", "count (per 1 unit)")
    )
  }
  
  list(primary = primary, models = results)
}



plot_forest <- function(tbl,
                        estimate_col = "OR",
                        conf_low = "conf.low",
                        conf_high = "conf.high",
                        p_col = "p.value",
                        outcome_col = "outcome",
                        title = "Effect of AI Use",
                        color_var = NULL) {
  use_log <- estimate_col %in% c("OR", "odds_ratio", "exp(estimate)")
  
  df <- tbl %>%
    filter(!is.na(.data[[estimate_col]]),
           !is.na(.data[[conf_low]]),
           !is.na(.data[[conf_high]]),
           is.finite(.data[[estimate_col]]),
           is.finite(.data[[conf_low]]),
           is.finite(.data[[conf_high]])) %>%
    mutate(
      outcome_chr = as.character(.data[[outcome_col]]),
      sig = case_when(
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.001 ~ "***",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.01  ~ "**",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  # order outcomes by estimate (ascending)
  ord <- df %>% arrange(.data[[estimate_col]]) %>% pull(outcome_chr)
  df <- df %>% mutate(outcome_f = factor(outcome_chr, levels = unique(ord)))
  
  if (is.null(color_var)) {
    gg_base <- ggplot(df, aes(x = .data[[estimate_col]], y = outcome_f)) 
  } else {
    gg_base <- ggplot(df, aes(x = .data[[estimate_col]], y = outcome_f, color = .data[[color_var]])) 
  }
  
  
  
  gg <- gg_base +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = .data[[conf_low]], xmax = .data[[conf_high]]),
                   height = 0.2) +
    geom_vline(xintercept = if (use_log) 1 else 0, linetype = "dashed") +
    labs(
      x = if (use_log) "Odds Ratio (95% CI)" else "Coefficient (95% CI)",
      y = NULL,
      title = title,
      subtitle = "Adjusted association between AI use and outcomes"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
  
  if (use_log) gg <- gg + scale_x_log10()
  
  gg + geom_text(aes(label = sig),
                 nudge_x = if (use_log) 0 else 0.0,
                 hjust = -0.2, size = 4)
}


plot_forest_binned <- function(tbl,
                               estimate_col = "OR",
                               conf_low = "conf.low",
                               conf_high = "conf.high",
                               p_col = "p.value",
                               outcome_col = "outcome",
                               contrast_col = "contrast",
                               title = "Effect of copilot message count on\ncounseling quality",
                               bin_order = c("1-5 vs 0", "6-10 vs 0", ">10 vs 0"),
                               filter_exposure = TRUE,
                               show_category_labels = TRUE) {
  
  library(showtext)
  
  # Mapping of outcome codes to descriptive names
  outcome_labels <- c(
    "fractional_score" = "Overall Quality Score",
    "P1" = "Greet & share protocol",
    "P2" = "Open-ended exploration",
    "P3" = "Collaborative agenda",
    "P4" = "Ask what support wanted",
    "P5" = "No justify third party",
    "P6" = "Check social support",
    "P8" = "Provide resources",
    "M1" = "Active listening",
    "M2" = "Check consent",
    "M3" = "Positive strokes",
    "M4" = "Never challenge",
    "M5" = "No medical advice",
    "S1" = "Active listening phrase",
    "S2" = "Mirror keywords",
    "S3" = "De-escalate language",
    "S4" = "Professional tone",
    "S5" = "Avoid multiple questions",
    "S6" = "No self-disclosure",
    "S7" = "No platitudes",
    "X1" = "Protective factors",
    "X2" = "Safety check",
    "X3" = "Risk assessment",
    "X4" = "Progressive ladder-up"
  )
  
  use_log <- estimate_col %in% c("OR", "odds_ratio", "exp(estimate)")
  
  df <- tbl %>%
    { if ("exposure" %in% names(.)) {
      if (isTRUE(filter_exposure)) filter(., exposure == "binned") else .
    } else . } %>%
    filter(!is.na(.data[[estimate_col]]),
           !is.na(.data[[conf_low]]),
           !is.na(.data[[conf_high]]),
           is.finite(.data[[estimate_col]]),
           is.finite(.data[[conf_low]]),
           is.finite(.data[[conf_high]])) %>%
    mutate(
      outcome_chr = as.character(.data[[outcome_col]]),
      # Clean outcome names
      outcome_clean = case_when(
        outcome_chr %in% names(outcome_labels) ~ outcome_labels[outcome_chr],
        TRUE ~ outcome_chr
      ),
      # Category labels
      category = case_when(
        str_detect(outcome_chr, "fractional|quality") ~ "Overall",
        str_detect(outcome_chr, "^P") ~ "Productivity",
        str_detect(outcome_chr, "^M") ~ "Microskills",
        str_detect(outcome_chr, "^S") ~ "Style/Tone",
        str_detect(outcome_chr, "^X") ~ "Safety/Risk",
        TRUE ~ "Other"
      ),
      category = factor(category, levels = c("Overall", "Productivity", 
                                             "Microskills", "Style/Tone", "Safety/Risk", "Other")),
      contrast_chr = as.character(.data[[contrast_col]]),
      # significance stars
      sig = case_when(
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.001 ~ "***",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.01  ~ "**",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.05  ~ "*",
        TRUE ~ ""
      ),
      # clean contrast labels
      contrast_chr = recode(contrast_chr,
                            "1-5 vs 0" = "1–5",
                            "6-10 vs 0" = "6–10",
                            ">10 vs 0" = ">10",
                            .default = contrast_chr),
      contrast_f = factor(contrast_chr,
                          levels = c("1–5", "6–10", ">10"))
    )
  
  # Order outcomes within each category by median effect
  df <- df %>%
    group_by(outcome_chr, category) %>%
    mutate(order_key = median(.data[[estimate_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(category, desc(order_key)) %>%
    mutate(outcome_f = factor(outcome_clean, levels = unique(outcome_clean)))
  
  # Colors matching your other plots (Okabe-Ito palette)
  bin_colors <- c(
    "1–5" = "#0072B2",    # blue
    "6–10" = "#009E73",   # green
    ">10" = "#D55E00"     # vermillion
  )
  
  # Build plot
  posd <- position_dodge(width = 0.6)
  
  g <- ggplot(df, aes(x = .data[[estimate_col]],
                      y = outcome_f,
                      color = contrast_f,
                      shape = contrast_f)) +
    geom_vline(xintercept = if (use_log) 1 else 0,
               linetype = "solid", color = "gray40", linewidth = 0.4) +
    geom_point(size = 2.5, position = posd) +
    geom_errorbarh(aes(xmin = .data[[conf_low]], xmax = .data[[conf_high]]),
                   height = 0, position = posd, linewidth = 0.6) +
    
    # Significance stars
    # geom_text(
    #   aes(label = sig,
    #       x = .data[[conf_high]]),
    #   position = posd, 
    #   hjust = -0.3, 
    #   size = 3.5,
    #   show.legend = FALSE,
    #   family = "Avenir Medium"
    # ) +
    
    scale_color_manual(
      values = bin_colors,
      name = "Copilot messages"
    ) +
    scale_shape_manual(
      values = c("1–5" = 16, "6–10" = 17, ">10" = 15),
      name = "Copilot messages"
    ) +
    
    labs(
      x = if (use_log) "Odds Ratio (95% CI)" else "Coefficient (95% CI)",
      y = NULL,
      title = title
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
      legend.position = "bottom", #c(0.85, 0.15),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(r = 10, l = 10),
      plot.title = element_text(face = "bold", family = "Avenir Medium"),
      text = element_text(family = "Avenir Medium"),
      axis.title.x = element_text(margin = margin(t = 10)),
      strip.text = element_text(face = "bold", size = 8),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  
  if (use_log) {
    g <- g + scale_x_log10(
      breaks = c(0.5, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
      labels = c(
        "0.5",
        "1",
        "10",
        expression(10^2),
        expression(10^3),
        expression(10^4),
        expression(10^5),
        expression(10^6),
        expression(10^7),
        expression(10^8)
      )
    )
    
  }
  
  # Facet by category
  g <- g + facet_grid(category ~ ., scales = "free_y", space = "free_y")
  
  g
}

plot_forest_binned_combined <- function(tbl,
                                        estimate_col = "OR",
                                        beta_col = "beta",
                                        conf_low = "conf.low",
                                        conf_high = "conf.high",
                                        p_col = "p.value",
                                        outcome_col = "outcome",
                                        contrast_col = "contrast",
                                        title_or = "",
                                        title_beta = "",
                                        filter_exposure = TRUE,
                                        return_plots = FALSE,
                                        return_table = FALSE,
                                        table_title = "Association of copilot message count with counseling outcomes") {
  
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(gt)
  
  outcome_labels <- c(
    "fractional_score" = "Overall Quality Score",
    "P1" = "Greet & share protocol",
    "P2" = "Open-ended exploration",
    "P3" = "Collaborative agenda",
    "P4" = "Ask what support wanted",
    "P5" = "No justify third party",
    "P6" = "Check social support",
    "P8" = "Provide resources",
    "M1" = "Active listening",
    "M2" = "Check consent",
    "M3" = "Positive strokes",
    "M4" = "Never challenge",
    "M5" = "No medical advice",
    "S1" = "Active listening phrase",
    "S2" = "Mirror keywords",
    "S3" = "De-escalate language",
    "S4" = "Professional tone",
    "S5" = "Avoid multiple questions",
    "S6" = "No self-disclosure",
    "S7" = "No platitudes",
    "X1" = "Protective factors",
    "X2" = "Safety check",
    "X3" = "Risk assessment",
    "X4" = "Progressive ladder-up"
  )
  
  bin_colors <- c(
    "1–5" = "#0072B2",
    "6–10" = "#009E73",
    ">10" = "#D55E00"
  )
  
  base_df <- tbl %>%
    { if ("exposure" %in% names(.)) {
      if (isTRUE(filter_exposure)) filter(., exposure == "binned") else .
    } else . } %>%
    mutate(
      outcome_chr = as.character(.data[[outcome_col]]),
      outcome_clean = case_when(
        outcome_chr %in% names(outcome_labels) ~ outcome_labels[outcome_chr],
        TRUE ~ outcome_chr
      ),
      category = case_when(
        str_detect(outcome_chr, "fractional|quality") ~ "Overall",
        str_detect(outcome_chr, "^P") ~ "Productivity",
        str_detect(outcome_chr, "^M") ~ "Microskills",
        str_detect(outcome_chr, "^S") ~ "Style/Tone",
        str_detect(outcome_chr, "^X") ~ "Safety/Risk",
        TRUE ~ "Other"
      ),
      category = factor(category, levels = c("Overall", "Productivity",
                                             "Microskills", "Style/Tone", "Safety/Risk", "Other")),
      contrast_chr = as.character(.data[[contrast_col]]),
      contrast_chr = recode(
        contrast_chr,
        "1-5 vs 0" = "1–5",
        "6-10 vs 0" = "6–10",
        ">10 vs 0" = ">10",
        .default = contrast_chr
      ),
      contrast_f = factor(contrast_chr, levels = c("1–5", "6–10", ">10")),
      sig = case_when(
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.001 ~ "***",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.01  ~ "**",
        !is.na(.data[[p_col]]) & .data[[p_col]] < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  common_theme <- theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(r = 10, l = 10),
      plot.title = element_text(face = "bold", family = "Avenir Medium"),
      text = element_text(family = "Avenir Medium"),
      axis.title.x = element_text(margin = margin(t = 10)),
      strip.text = element_text(face = "bold", size = 8),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  
  posd <- position_dodge(width = 0.6)
  
  # ----------------------------
  # 1) Odds ratio plot
  # ----------------------------
  df_or <- base_df %>%
    filter(outcome_chr != "fractional_score") %>%
    filter(!is.na(.data[[estimate_col]]),
           !is.na(.data[[conf_low]]),
           !is.na(.data[[conf_high]]),
           is.finite(.data[[estimate_col]]),
           is.finite(.data[[conf_low]]),
           is.finite(.data[[conf_high]])) %>%
    group_by(outcome_chr, category) %>%
    mutate(order_key = median(.data[[estimate_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(category, desc(order_key)) %>%
    mutate(outcome_f = factor(outcome_clean, levels = unique(outcome_clean)))
  
  g_or <- ggplot(df_or, aes(x = .data[[estimate_col]],
                            y = outcome_f,
                            color = contrast_f,
                            shape = contrast_f)) +
    geom_vline(xintercept = 1, color = "gray40", linewidth = 0.4) +
    geom_point(size = 2.5, position = posd) +
    geom_errorbarh(aes(xmin = .data[[conf_low]], xmax = .data[[conf_high]]),
                   height = 0, position = posd, linewidth = 0.6) +
    scale_color_manual(values = bin_colors, name = "Copilot messages") +
    scale_shape_manual(values = c("1–5" = 16, "6–10" = 17, ">10" = 15),
                       name = "Copilot messages") +
    scale_x_log10(
      breaks = c(0.5, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
      labels = c("0.5", "1", "10", expression(10^2), expression(10^3),
                 expression(10^4), expression(10^5), expression(10^6),
                 expression(10^7), expression(10^8))
    ) +
    labs(
      x = "Odds Ratio (95% CI)",
      y = NULL,
      title = title_or
    ) +
    facet_wrap(~ category, ncol = 1, scales = "free_y", strip.position = "top") +
    common_theme 
  
  # ----------------------------
  # 2) Beta plot for overall quality score only
  # ----------------------------
  df_beta <- base_df %>%
    filter(outcome_chr == "fractional_score") %>%
    filter(!is.na(.data[[beta_col]]),
           !is.na(.data[[conf_low]]),
           !is.na(.data[[conf_high]]),
           is.finite(.data[[beta_col]]),
           is.finite(.data[[conf_low]]),
           is.finite(.data[[conf_high]])) %>%
    mutate(outcome_f = factor("Overall Quality Score", levels = "Overall Quality Score")) %>% 
    mutate(facet_label = "Overall")
  
  g_beta <- ggplot(df_beta, aes(x = .data[[beta_col]],
                                y = outcome_f,
                                color = contrast_f,
                                shape = contrast_f)) +
    geom_vline(xintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_point(size = 2.8, position = posd) +
    geom_errorbarh(aes(xmin = .data[[conf_low]], xmax = .data[[conf_high]]),
                   height = 0, position = posd, linewidth = 0.7) +
    scale_color_manual(values = bin_colors, name = "Copilot messages") +
    scale_shape_manual(values = c("1–5" = 16, "6–10" = 17, ">10" = 15),
                       name = "Copilot messages") +
    facet_grid(~facet_label) +
    labs(
      x = "Beta (95% CI)",
      y = NULL,
      title = title_beta
    ) +
    guides(color = F,
           shape = F) +
    common_theme
  
  # ----------------------------
  # 3) Stack plots
  # ----------------------------
  combined_plot <- g_beta / g_or + 
    plot_layout(heights = c(1, 15), guides = "collect") &
    theme(legend.position = "bottom")
  
  
  # ----------------------------
  # Simple supplement table
  # ----------------------------
  results_table <- base_df %>%
    mutate(
      Outcome = outcome_clean,
      Estimate = case_when(
        outcome_chr == "fractional_score" ~ beta,
        TRUE ~ .data[[estimate_col]]
      ),
      Estimate_type = case_when(
        outcome_chr == "fractional_score" ~ "Beta",
        TRUE ~ "OR"
      )
    ) %>%
    select(
      category,
      Outcome,
      contrast_chr,
      Estimate_type,
      Estimate,
      all_of(conf_low),
      all_of(conf_high),
      all_of(p_col)
    ) %>%
    rename(
      Contrast = contrast_chr,
      CI_low = all_of(conf_low),
      CI_high = all_of(conf_high),
      p_value = all_of(p_col)
    ) %>%
    arrange(category, Outcome, Contrast)
  
  if (return_plots || return_table) {
    return(list(
      beta_plot = g_beta,
      or_plot = g_or,
      combined_plot = combined_plot,
      results_table = results_table
    ))
  } else {
    return(combined_plot)
  }
}


if (F) {
  
  load("/Users/akshayswaminathan/Downloads/vf_obs_causal_dfs.RData")
  
  model_df <- all_dfs$full_dataset %>% 
    add_period_convo() %>% 
    filter(period != "Pilot deployment") %>% 
    mutate_at(vars(education_level, employment_status), ~coalesce(.x, "Missing")) %>% 
    mutate(time_with_foundation_so_far = coalesce(time_with_foundation_so_far, 0))
  
  # Check covariate balance
  
  model_df_bal <- model_df %>%
    .enrich_time() %>%
    .build_presenting_concerns(top_K = 12) %>%
    mutate(
      ai_used = ifelse(ai_used %in% c(TRUE, 1, "1", "Yes", "yes"), "Yes", "No"),
      ai_used = factor(ai_used, levels = c("No", "Yes"))
    )
  
  base_covars <- c(
    "counselor_total_msgs_sent_so_far",
    "counselor_copilot_msgs_sent_so_far",
    "time_with_foundation_so_far",
    "employment_status",
    "education_level",
    "num_simultaneous_convos_cat",
    "tod_bin",
    "dow"
  )
  
  pc_covars <- grep("^cc_", names(model_df_bal), value = TRUE)
  
  covars <- c(base_covars[base_covars %in% names(model_df_bal)], pc_covars)
  
  # -----------------------
  # 1) Balance with cobalt
  # -----------------------
  
  bal_res <- bal.tab(model_df_bal %>% 
                       select(all_of(covars)), 
          treat = model_df_bal$ai_used,
          binary = "std", continuous = "std")
  
  love.plot(x = model_df_bal %>% 
              select(all_of(covars)), 
            treat = model_df_bal$ai_used,
            thresholds = c(m = .1), binary = "std",
            continuous = "std", abs = FALSE)
  
  bal_res  # print summary
  
  
  # --- 1. Prepare data & covariates exactly as in your model code ---
  
  model_df_cem <- model_df %>%
    add_period_convo() %>%                       # if you want to drop pilot
    dplyr::filter(period != "Pilot deployment") %>%
    .enrich_time() %>%
    .build_presenting_concerns(top_K = 12) %>%
    mutate(
      ai_used = ifelse(ai_used %in% c(TRUE, 1, "1", "Yes", "yes"), "Yes", "No"),
      ai_used = factor(ai_used, levels = c("No", "Yes"))
    )
  
  base_covars <- c(
    "counselor_total_msgs_sent_so_far",
    "counselor_copilot_msgs_sent_so_far",
    "time_with_foundation_so_far",
    "employment_status",
    "education_level",
    # "num_simultaneous_convos_cat",
    "tod_bin",
    "dow"
  )
  
  pc_covars <- grep("^cc_", names(model_df_cem), value = TRUE)
  
  covars <- c(base_covars[base_covars %in% names(model_df_cem)], pc_covars)
  
  # --- 2. Define coarsening for continuous covariates (quantile-based) ---
  
  # Helper to get unique quantile cutpoints
  qcuts <- function(x, probs = c(0, .25, .5, .75, 1)) {
    qs <- quantile(x, probs = probs, na.rm = TRUE)
    sort(unique(qs))
  }
  
  cut_total_msgs <- qcuts(model_df_cem$counselor_total_msgs_sent_so_far)
  cut_copilot   <- qcuts(model_df_cem$counselor_copilot_msgs_sent_so_far)
  cut_tenure    <- qcuts(model_df_cem$time_with_foundation_so_far)
  
  # Build formula for MatchIt
  cem_fml <- as.formula(
    paste("ai_used ~", paste(covars, collapse = " + "))
  )
  
  # --- 3. Run CEM via MatchIt ---
  
  m_cem <- matchit(
    formula   = cem_fml,
    data      = model_df_cem,
    method    = "cem",
    cutpoints = list(
      counselor_total_msgs_sent_so_far = cut_total_msgs,
      counselor_copilot_msgs_sent_so_far = cut_copilot,
      time_with_foundation_so_far = cut_tenure
      # other continuous vars can go here if you add them later
    ),
    estimand  = "ATT"   # effect of AI use among AI users
  )
  
  summary(m_cem)
  
  # --- 4. Check covariate balance after CEM ---
  
  # cobalt balance table (pre/post)
  bal.tab(m_cem, un = TRUE)
  
  # Love plot
  balance_plot <- love.plot(
    m_cem,
    stats = "mean.diffs",
    abs = TRUE,
    thresholds = c(m = 0.1),  # |SMD|<0.1 often used as "good" balance
    var.order = "unadjusted"
  )
  
  png("/tmp/testplot.png", width = 2000, height = 1500, res = 300)
  print(balance_plot)
  dev.off()
  
  # --- 5. Get matched data for downstream models ---
  
  matched_df <- match.data(m_cem)
  matched_df <- left_join(matched_df,
                          model_df %>% 
                            select(conversation_uid, setdiff(names(model_df), names(matched_df))))
    
    
  
  
  ########
  # Do the modeling
  ########
  
  # The two dfs are model_df and matched_df
  
  outcomes <- c(
    "P1","P2","P3","P4","P5","P6","P8",
    "M1","M2","M3","M4","M5",
    "S1","S2","S3","S4","S5","S6","S7",
    "X1","X2","X3","X4",
    "fractional_score", "num_simultaneous_convos",
    "avg_counselor_response_mins"
  )
  
  dfs <- list(unmatched = model_df, 
              matched = matched_df)
  
  results <- map(dfs,
                 function(df) {
                   
                   out_binary <- run_ai_vs_noai_models(
                     df = df,
                     outcomes = outcomes,
                     pc_top_k = 12,
                     add_counselor_fe = FALSE,
                     add_week_fe = TRUE,
                     exposure = c("binary")    # 🔹 AI used (Yes vs No)
                   )
                   
                   out_count <- run_ai_vs_noai_models(
                     df = df,
                     outcomes = outcomes,
                     pc_top_k = 12,
                     add_counselor_fe = FALSE,
                     add_week_fe = TRUE,
                     exposure = c("binned"),
                     standardize_count = F
                   )
                   
                   out <- list(binary = out_binary,
                               count = out_count)
                   
                 })
  
  # View results
  out_binary$primary
  
  out_count$primary
  
  out_count
  
  out_binary$primary %>% View()
  
  out$models
  
  all_binary_results <- results$matched$binary$primary %>% 
    mutate(matching = "unmatched") %>% 
    bind_rows(results$matched$binary$primary %>% 
                mutate(matching = "matched"))
  
  all_binned_results <- results$matched$count$primary %>% 
    mutate(matching = "unmatched") %>% 
    bind_rows(results$matched$count$primary %>% 
                mutate(matching = "matched"))
  
  binned_forest <- plot_forest_binned(all_binned_results %>% 
                       filter(matching == "matched"),
                       title = "")
  
  binned_forest <- plot_forest_binned_combined(all_binned_results %>% 
                                        filter(matching == "matched"),
                                      return_plots = T,
                                      return_table = T)
  
  png("/tmp/testplot.png", width = 3000, height = 2000, res = 300)
  print(binned_forest)
  dev.off()
  
  
}

# plot the base rates
if (F) {
  
  # outcome labels to match the forest plot
  outcome_labels <- c(
    "fractional_score" = "Overall Quality Score",
    "P1" = "Greet & share protocol",
    "P2" = "Open-ended exploration",
    "P3" = "Collaborative agenda",
    "P4" = "Ask what support wanted",
    "P5" = "No justify third party",
    "P6" = "Check social support",
    "P8" = "Provide resources",
    "M1" = "Active listening",
    "M2" = "Check consent",
    "M3" = "Positive strokes",
    "M4" = "Never challenge",
    "M5" = "No medical advice",
    "S1" = "Active listening phrase",
    "S2" = "Mirror keywords",
    "S3" = "De-escalate language",
    "S4" = "Professional tone",
    "S5" = "Avoid multiple questions",
    "S6" = "No self-disclosure",
    "S7" = "No platitudes",
    "X1" = "Protective factors",
    "X2" = "Safety check",
    "X3" = "Risk assessment",
    "X4" = "Progressive ladder-up"
  )
  
  outcome_vars <- c(
    "fractional_score",
    "P1","P2","P3","P4","P5","P6","P8",
    "M1","M2","M3","M4","M5",
    "S1","S2","S3","S4","S5","S6","S7",
    "X1","X2","X3","X4"
  )
  
  base_rate_table <- dfs$matched %>%
    mutate(
      ai_message_count_bin = case_when(
        ai_message_count == 0 ~ "0",
        ai_message_count <= 5 ~ "1-5",
        ai_message_count <= 10 ~ "6-10",
        ai_message_count > 10 ~ ">10",
        TRUE ~ NA_character_
      ),
      ai_message_count_bin = factor(
        ai_message_count_bin,
        levels = c("0", "1-5", "6-10", ">10")
      )
    ) %>%
    select(ai_message_count_bin, all_of(outcome_vars)) %>%
    pivot_longer(
      cols = all_of(outcome_vars),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    mutate(
      outcome_label = recode(outcome, !!!outcome_labels),
      category = case_when(
        outcome == "fractional_score" ~ "Overall",
        str_detect(outcome, "^P") ~ "Productivity",
        str_detect(outcome, "^M") ~ "Microskills",
        str_detect(outcome, "^S") ~ "Style/Tone",
        str_detect(outcome, "^X") ~ "Safety/Risk",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(category, outcome, outcome_label, ai_message_count_bin) %>%
    summarise(
      n_nonmissing = sum(!is.na(value)),
      n_positive = if_else(
        first(outcome) == "fractional_score",
        NA_integer_,
        sum(value == 1, na.rm = TRUE)
      ),
      base_rate = if_else(
        first(outcome) == "fractional_score",
        mean(value, na.rm = TRUE),
        mean(value == 1, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      base_rate = round(100 * base_rate, 1)
    ) %>%
    arrange(category, outcome, ai_message_count_bin)
  
  base_rate_table
  
  base_rate_table_wide <- base_rate_table %>%
    mutate(
      display = case_when(
        outcome == "fractional_score" ~ paste0(
          "mean=", round(base_rate / 100, 3),
          " (n=", n_nonmissing, ")"
        ),
        TRUE ~ paste0(
          n_positive, "/", n_nonmissing,
          " (", base_rate, "%)"
        )
      )
    ) %>%
    select(category, outcome_label, ai_message_count_bin, display) %>%
    pivot_wider(
      names_from = ai_message_count_bin,
      values_from = display
    ) %>%
    arrange(category, outcome_label)
  
  base_rate_table_wide
  
}

if (F) {
  
  # Table plotting
  
  format_use_results_gt <- function(tbl,
                                    or_col = "OR",
                                    beta_col = "beta",
                                    conf_low = "conf.low",
                                    conf_high = "conf.high",
                                    p_col = "p.value",
                                    outcome_col = "outcome",
                                    matching_col = "matching",
                                    show_matching = "matched",  # "matched", "unmatched", or "both"
                                    title = "Effect of AI Copilot Use on Counseling Quality Metrics",
                                    subtitle = "Coarsened exact matching estimates") {
    
    # Mapping of outcome codes to descriptive names
    outcome_labels <- c(
      "fractional_score" = "Overall Quality Score",
      # "num_simultaneous_convos" = "Number of Simultaneous Conversations",
      # "avg_counselor_response_mins" = "Average Response Time (minutes)",
      # Protocol Adherence
      "P1" = "Greet & share session protocol",
      "P2" = "Open-ended exploration before solutions",
      "P3" = "Collaborative agenda (non-directive)",
      "P4" = "Ask what support is wanted",
      "P5" = "Do not justify third party actions",
      "P6" = "Check social support",
      "P7" = "Reference earlier conversation details",
      "P8" = "Provide resources/next steps",
      # Manner/Approach
      "M1" = "Active listening & validation",
      "M2" = "Check consent before suggestions",
      "M3" = "Use positive strokes",
      "M4" = "Never challenge or command client",
      "M5" = "No medical advice/diagnosis",
      # Style/Communication
      "S1" = "Use explicit active listening phrase",
      "S2" = "Mirror client's keywords",
      "S3" = "No triggering jargon; de-escalate",
      "S4" = "Maintain professional tone",
      "S5" = "Avoid multiple questions at once",
      "S6" = "No comparisons or self-disclosure",
      "S7" = "No platitudes",
      # Crisis Management
      "X0" = "Any self-harm/suicide risk present",
      "X1" = "Acknowledge protective factors (if risk)",
      "X2" = "Check safety of current situation (if risk)",
      "X3" = "Initiate risk assessment (if risk)",
      "X4" = "Complete progressive ladder-up (if risk)"
    )
    
    df <- tbl %>%
      # Filter by matching status if specified
      filter(
        if (show_matching == "both") TRUE 
        else .data[[matching_col]] == show_matching
      ) %>%
      mutate(
        category = case_when(
          str_detect(.data[[outcome_col]], "fractional|overall|quality") ~ "Overall Quality",
          # str_detect(.data[[outcome_col]], "num_simultaneous|avg_counselor") ~ "Operational Outcomes",
          str_detect(.data[[outcome_col]], "^P[0-9]") ~ "Productivity",
          str_detect(.data[[outcome_col]], "^M[0-9]") ~ "Microskills",
          str_detect(.data[[outcome_col]], "^S[0-9]") ~ "Style and Tone",
          str_detect(.data[[outcome_col]], "^X[0-9]") ~ "Safety and Risk",
          TRUE ~ "Other"
        ),
        # Use the mapping for clean names
        outcome_clean = case_when(
          .data[[outcome_col]] %in% names(outcome_labels) ~ outcome_labels[.data[[outcome_col]]],
          TRUE ~ as.character(.data[[outcome_col]])
        ),
        is_significant = .data[[p_col]] < 0.05,
        # Determine if outcome is binary (has OR) or continuous (has beta)
        is_binary = !is.na(.data[[or_col]]),
        # Create category order with Overall Quality first
        category = factor(category, levels = c("Overall Quality", "Operational Outcomes",
                                               "Productivity", "Microskills", 
                                               "Style and Tone", "Safety and Risk", "Other"))
      ) %>%
      # Sort by category, then by effect size within each category
      # For binary outcomes, sort by log(OR); for continuous, sort by beta
      arrange(category, desc(if_else(is_binary, log(.data[[or_col]]), .data[[beta_col]]))) %>%
      # Create outcome factor to preserve this ordering in gt
      mutate(outcome_clean = factor(outcome_clean, levels = unique(outcome_clean))) %>%
      select(category, outcome_clean, is_binary,
             or = !!sym(or_col),
             beta = !!sym(beta_col),
             conf_low = !!sym(conf_low),
             conf_high = !!sym(conf_high),
             p_value = !!sym(p_col),
             is_significant,
             matching = !!sym(matching_col))
    
    gt_tbl <- df %>%
      gt(groupname_col = "category") %>%
      # Format OR column (only for binary outcomes)
      gt::fmt_number(
        columns = or,
        decimals = 2,
        rows = is_binary
      ) %>%
      # Format beta column (only for continuous outcomes)
      gt::fmt_number(
        columns = beta,
        decimals = 2,
        rows = !is_binary
      ) %>%
      # Format CI columns
      gt::fmt_number(
        columns = c(conf_low, conf_high),
        decimals = 2
      ) %>%
      # Format p-value
      gt::fmt_number(
        columns = p_value,
        decimals = 3
      ) %>%
      # Merge CI columns
      cols_merge(
        columns = c(conf_low, conf_high),
        pattern = "[{1}, {2}]"
      ) %>%
      # Create combined effect size column showing OR or beta as appropriate
      cols_merge(
        columns = c(or, beta),
        pattern = "<<x>>"  # We'll handle this with text_transform
      ) %>%
      # Format the merged effect size column
      # Format the merged effect size column (no stars)
      text_transform(
        locations = cells_body(columns = or),
        fn = function(x) {
          effect_size <- ifelse(df$is_binary, 
                                sprintf("%.2f", df$or), 
                                sprintf("%.2f", df$beta))
          effect_size
        }
      ) %>%
      # Add significance stars to p-value column
      text_transform(
        locations = cells_body(columns = p_value),
        fn = function(x) {
          sig_stars <- ifelse(df$p_value < 0.001, "***",
                              ifelse(df$p_value < 0.01, "**",
                                     ifelse(df$p_value < 0.05, "*", "")))
          paste0(x, sig_stars)
        }
      ) %>%
      # Column labels
      cols_label(
        outcome_clean = "Metric",
        or = "Effect",
        conf_low = "95% CI",
        p_value = "p-value"
      ) %>%
      # Add spanner for effect type
      tab_spanner(
        label = "Binary outcomes: OR | Continuous outcomes: β",
        columns = c(or, conf_low, p_value)
      ) %>%
      # Align columns
      cols_align(
        align = "left",
        columns = outcome_clean
      ) %>%
      cols_align(
        align = "right",
        columns = c(or, conf_low, p_value)
      ) %>%
      # Add title and subtitle
      tab_header(
        title = title,
        subtitle = subtitle
      ) %>%
      # Add source note
      tab_source_note(
        source_note = paste0(
          "Note: Estimates from generalized linear models comparing conversations with versus without copilot use. ",
          if (show_matching == "matched") {
            "Sample constructed using coarsened exact matching on counselor experience, tenure, employment status, education, and temporal factors. "
          } else if (show_matching == "unmatched") {
            "Unmatched sample with covariate adjustment. "
          } else {
            ""
          },
          "Binary outcomes reported as odds ratios (OR), continuous outcomes as mean differences (β). ",
          "Standard errors clustered at counselor level. N=66,234 conversations."
        )
      ) %>%
      tab_source_note(
        source_note = md("Significance: ***p<0.001, **p<0.01, *p<0.05")
      ) %>%
      # Style the table
      tab_options(
        table.font.size = px(12),
        heading.title.font.size = px(14),
        heading.subtitle.font.size = px(12),
        row_group.font.weight = "bold",
        row_group.background.color = "#f3f4f6",
        table.border.top.style = "solid",
        table.border.bottom.style = "solid",
        heading.border.bottom.style = "solid",
        column_labels.border.top.style = "solid",
        column_labels.border.bottom.style = "solid",
        row_group.border.top.style = "solid",
        row_group.border.bottom.style = "solid"
      ) %>%
      # Hide helper columns
      cols_hide(columns = c(is_significant, is_binary, beta, matching))
    
    return(gt_tbl)
  }
  
  # Usage examples:
  
  results_tbl <- all_binary_results
  
  # Show matched results only (primary analysis)
  results_tbl %>%
    format_use_results_gt(show_matching = "matched")
  
  # Show unmatched results only (sensitivity analysis)
  results_tbl %>%
    format_use_results_gt(
      show_matching = "unmatched",
      subtitle = "Unmatched sample with covariate adjustment"
    )
  
  # Side-by-side comparison table
  format_use_comparison_gt <- function(tbl,
                                       or_col = "OR",
                                       beta_col = "beta",
                                       conf_low = "conf.low",
                                       conf_high = "conf.high",
                                       p_col = "p.value",
                                       outcome_col = "outcome",
                                       matching_col = "matching") {
    
    outcome_labels <- c(
      "fractional_score" = "Overall Quality Score",
      "num_simultaneous_convos" = "Simultaneous Conversations",
      "avg_counselor_response_mins" = "Avg Response Time (min)",
      "P1" = "Greet & share protocol",
      "P2" = "Open-ended exploration",
      "P3" = "Collaborative agenda",
      "P4" = "Ask what support wanted",
      "P5" = "No justify third party",
      "P6" = "Check social support",
      "P8" = "Provide resources",
      "M1" = "Active listening",
      "M2" = "Check consent",
      "M3" = "Positive strokes",
      "M4" = "Never challenge",
      "M5" = "No medical advice",
      "S1" = "Active listening phrase",
      "S2" = "Mirror keywords",
      "S3" = "De-escalate language",
      "S4" = "Professional tone",
      "S5" = "Avoid multiple questions",
      "S6" = "No self-disclosure",
      "S7" = "No platitudes",
      "X1" = "Protective factors",
      "X2" = "Safety check",
      "X3" = "Risk assessment",
      "X4" = "Progressive ladder-up"
    )
    
    # Prepare matched and unmatched data
    matched_df <- tbl %>%
      filter(.data[[matching_col]] == "matched") %>%
      mutate(
        outcome_clean = case_when(
          .data[[outcome_col]] %in% names(outcome_labels) ~ outcome_labels[.data[[outcome_col]]],
          TRUE ~ as.character(.data[[outcome_col]])
        ),
        is_binary = !is.na(.data[[or_col]]),
        matched_effect = if_else(is_binary, .data[[or_col]], .data[[beta_col]]),
        matched_ci = sprintf("[%.2f, %.2f]", .data[[conf_low]], .data[[conf_high]]),
        matched_sig = case_when(
          .data[[p_col]] < 0.001 ~ "***",
          .data[[p_col]] < 0.01 ~ "**",
          .data[[p_col]] < 0.05 ~ "*",
          TRUE ~ ""
        )
      ) %>%
      select(outcome = !!sym(outcome_col), outcome_clean, is_binary, matched_effect, matched_ci, matched_sig)
    
    unmatched_df <- tbl %>%
      filter(.data[[matching_col]] == "unmatched") %>%
      mutate(
        is_binary = !is.na(.data[[or_col]]),
        unmatched_effect = if_else(is_binary, .data[[or_col]], .data[[beta_col]]),
        unmatched_ci = sprintf("[%.2f, %.2f]", .data[[conf_low]], .data[[conf_high]]),
        unmatched_sig = case_when(
          .data[[p_col]] < 0.001 ~ "***",
          .data[[p_col]] < 0.01 ~ "**",
          .data[[p_col]] < 0.05 ~ "*",
          TRUE ~ ""
        )
      ) %>%
      select(outcome = !!sym(outcome_col), unmatched_effect, unmatched_ci, unmatched_sig)
    
    # Merge
    df <- matched_df %>%
      left_join(unmatched_df, by = "outcome") %>%
      mutate(
        category = case_when(
          str_detect(outcome, "fractional|quality") ~ "Overall",
          str_detect(outcome, "num_simultaneous|avg_counselor") ~ "Operational",
          str_detect(outcome, "^P") ~ "Productivity",
          str_detect(outcome, "^M") ~ "Microskills",
          str_detect(outcome, "^S") ~ "Style and Tone",
          str_detect(outcome, "^X") ~ "Safety and Risk",
          TRUE ~ "Other"
        )
      )
    
    gt(df, groupname_col = "category") %>%
      cols_label(
        outcome_clean = "Metric",
        matched_effect = "Effect",
        matched_ci = "95% CI",
        matched_sig = "",
        unmatched_effect = "Effect",
        unmatched_ci = "95% CI",
        unmatched_sig = ""
      ) %>%
      tab_spanner(
        label = "Matched Sample",
        columns = c(matched_effect, matched_ci, matched_sig)
      ) %>%
      tab_spanner(
        label = "Unmatched Sample",
        columns = c(unmatched_effect, unmatched_ci, unmatched_sig)
      ) %>%
      fmt_number(
        columns = c(matched_effect, unmatched_effect),
        decimals = 2
      ) %>%
      cols_hide(columns = c(outcome, is_binary)) %>%
      tab_header(
        title = "Effect of AI Copilot Use: Matched vs Unmatched Estimates",
        subtitle = "Binary outcomes: OR | Continuous outcomes: β"
      ) %>%
      tab_source_note(
        source_note = "Note: Matched sample constructed using coarsened exact matching. ***p<0.001, **p<0.01, *p<0.05"
      )
  }
  
  # Show side-by-side comparison
  results_tbl %>%
    format_use_comparison_gt()
  
}

