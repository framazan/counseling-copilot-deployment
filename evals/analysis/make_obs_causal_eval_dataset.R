# =========================
# Master assembly script
# =========================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(readr)
  library(jsonlite)
  library(digest)
  library(ggplot2)
  library(data.table)
  library(tibble)
  library(arrow)
  library(glue)
})

# ---------- HELPERS ----------
hash_rows_fast <- function(conversation_id, msg_sequence_overall, 
                           prefix = NULL, algo = "xxhash64") {
  # Normalize to character and replace NA with ""
  conv_chr  <- ifelse(is.na(conversation_id),  "", as.character(conversation_id))
  seq_chr   <- ifelse(is.na(msg_sequence_overall), "", as.character(msg_sequence_overall))
  
  # Build a single key string per row (vectorized)
  key <- paste(conv_chr, seq_chr, sep = "|")
  
  # Hash each row key – vapply is faster and type-safe
  ids <- vapply(
    key,
    digest::digest,
    FUN.VALUE = character(1),
    algo = algo,
    serialize = FALSE
  )
  
  if (!is.null(prefix)) {
    ids <- paste0(prefix, "_", ids)
  }
  ids
}

make_attrition_table <- function(data, ie_list, first_row_name = "Before filtering") {
  
  datasets <- list()
  cohort_selection <- data.frame(step = c(first_row_name, names(ie_list)),
                                 count = 1:(length(ie_list) + 1))
  
  datasets[[1]] <- data
  cohort_selection$count[1] <- nrow(data)
  
  for (i in 1:length(ie_list)) {
    data <- data %>% 
      filter(!!ie_list[[i]])
    
    datasets[[i + 1]] <- data
    cohort_selection$count[i + 1] <- nrow(data)
  }
  
  out <- list("filtered_datasets" = datasets %>% 
                setNames(cohort_selection$step),
              "cohort_selection" = cohort_selection)
  
  return(out)
  
}

# ---------- IO & BASIC CLEANUP ----------

# Fast reader with soft type checks
read_messages <- function(path) {
  df <- read_parquet(
    file = path,
    as_data_frame = TRUE
  )
  print("Loaded messages")
  
  # Handle DateInserted / dateinserted case-insensitively
  if ("DateInserted" %in% names(df)) {
    date_col <- "DateInserted"
  } else if ("dateinserted" %in% names(df)) {
    df <- dplyr::rename(df, DateInserted = dateinserted)
    date_col <- "DateInserted"
  } else {
    stop("No DateInserted/dateinserted column found in parquet.")
  }
  
  required <- c(
    "conversation_id",
    "msg_sequence_overall",
    "direction",
    "message",
    "ai_suggestion",
    "DateInserted",
    "counsellor",
    "message_sender_id"
    # uid is optional; can be added here if you want to require it
  )
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }
  
  print("Cleaning messages data")
  print("Creating unique ID")
  start <- Sys.time()
  
  df <- df %>%
    mutate(
      DateInserted      = lubridate::as_datetime(.data[[date_col]], tz = "UTC"),
      counsellor        = na_if(counsellor, ""),
      ai_suggestion     = na_if(ai_suggestion, ""),
      direction         = as.character(direction),
      message           = dplyr::coalesce(as.character(message), ""),
      message_sender_id = as.character(message_sender_id),
      message_uid       = paste0("m", conversation_id, "_", msg_sequence_overall)
    )
  
  end <- Sys.time()
  print(glue("Unique ID created in {round(as.numeric(end-start))} seconds"))
  
  df
}


# Diagnostics (message-level)
describe_messages <- function(messages) {
  list(
    n_rows          = nrow(messages),
    n_conversations = dplyr::n_distinct(messages$conversation_id),
    n_counsellors   = dplyr::n_distinct(messages$counsellor),
    n_sender_ids    = dplyr::n_distinct(messages$message_sender_id),  # <— changed
    time_range      = tibble::tibble(
      min_time = suppressWarnings(min(messages$DateInserted, na.rm = TRUE)),
      max_time = suppressWarnings(max(messages$DateInserted, na.rm = TRUE))
    ),
    sender_missing_rates = messages %>%                                # <— changed block
      dplyr::filter(direction == "Outgoing") %>%
      dplyr::summarise(
        counsellor_missing      = mean(is.na(counsellor)),
        message_sender_id_missing = mean(is.na(message_sender_id)),
        .groups = "drop"
      )
  )
}

# ---------- CONVERSATION-LEVEL FROM MESSAGES ----------

label_row <- function(direction, ai_suggestion) {
  case_when(
    direction == "Incoming" ~ "Patient:",
    direction == "Outgoing" & !is.na(ai_suggestion) ~ "Counselor (AI):",
    direction == "Outgoing" ~ "Counselor:",
    TRUE ~ "Unknown:"
  )
}


assemble_conversations <- function(messages,
                                      exclude_counsellors = c("Sharang Phadke")) {
  dt <- as.data.table(messages)
  
  # Order once by conversation + time for all time-based ops
  setorder(dt, conversation_id, DateInserted)
  
  # --- per-message features ---------------------------------------------------
  # last_client_time_num: last incoming message time carried forward
  dt[, last_client_time_num := fifelse(
    direction == "Incoming",
    as.numeric(DateInserted),
    NA_real_
  )]
  
  dt[, last_client_time_num := nafill(last_client_time_num, type = "locf"),
     by = conversation_id]
  
  # counselor_response_mins since preceding client message
  dt[, counselor_response_mins := fifelse(
    direction == "Outgoing" & !is.na(last_client_time_num),
    (as.numeric(DateInserted) - last_client_time_num) / 60,
    NA_real_
  )]
  
  # labels + labeled_message (assumes label_row is vectorized)
  dt[, label := label_row(direction, ai_suggestion)]
  dt[, labeled_message := paste0(label, " ", message)]
  
  # --- conversation-level aggregation ----------------------------------------
  convo_dt <- dt[, {
    n <- .N
    
    # First / last message times ...
    first_time <- if (n > 5L) sort(DateInserted)[5L] else suppressWarnings(min(DateInserted, na.rm = TRUE))
    last_idx   <- if (n > 5L) max(1L, n - 4L) else n
    last_time  <- DateInserted[order(DateInserted)][last_idx]
    
    # counsellor: first non-NA
    non_na_idx <- which(!is.na(counsellor))
    counsellor_val <- if (length(non_na_idx)) counsellor[non_na_idx[1L]] else NA_character_
    n_counsellors <- n_distinct(counsellor)
    
    # message_sender_id: first non-NA among outgoing
    out_idx <- which(direction == "Outgoing" & !is.na(message_sender_id))
    msid_val <- if (length(out_idx)) as.character(message_sender_id[out_idx[1L]]) else NA_character_
    
    # conversation uid from Python (assumes constant per conversation_id)
    if ("uid" %in% names(.SD)) {
      uid_vals <- unique(uid)
      # Safety check if something is off
      if (length(uid_vals) > 1L) {
        warning("Multiple uid values found within a single conversation_id; using first.")
      }
      conv_uid <- as.character(uid_vals[1L])
    } else {
      conv_uid <- NA_character_
    }
    
    # convo text in msg_sequence_overall order
    convo_text <- paste(labeled_message[order(msg_sequence_overall)], collapse = "\n")
    
    .(
      convo                      = convo_text,
      ai_used                    = any(!is.na(ai_suggestion)),
      ai_message_count           = sum(direction == "Outgoing" & !is.na(ai_suggestion)),
      n_messages                 = n,
      n_counselor_messages       = sum(direction == "Outgoing"),
      n_client_messages          = sum(direction == "Incoming"),
      first_message_time         = first_time,
      last_message_time          = last_time,
      counsellor                 = counsellor_val,
      n_counsellors              = n_counsellors,
      message_sender_id          = msid_val,
      conversation_uid           = conv_uid,
      avg_counselor_response_mins = if (all(is.na(counselor_response_mins)))
        NA_real_
      else
        mean(counselor_response_mins, na.rm = TRUE)
    )
  }, by = conversation_id]
  
  
  
  # --- filter counsellors -----------------------------------------------------
  convo_dt <- convo_dt[!(is.na(counsellor) | counsellor %chin% exclude_counsellors)]
  
  # --- stable conversation UID (hash on few key fields) ----------------------

  convo_df <- as_tibble(convo_dt)
  
  convo_df %>%
    dplyr::select(-counsellor)
  
}
# ---------- WINDOW FILTER (INCLUSIVE) ----------

describe_conversations_quick <- function(convo_df) {
  tibble(
    n_conversations = nrow(convo_df),
    n_counsellors   = n_distinct(convo_df$counsellor),
    ai_used_FALSE   = sum(!convo_df$ai_used, na.rm = TRUE),
    ai_used_TRUE    = sum(convo_df$ai_used,  na.rm = TRUE)
  )
}

# ---------- COUNSELLOR-LEVEL CONFOUNDERS ----------

build_counsellor_df <- function(messages,
                                counselor_df_path = "/Users/akshayswaminathan/Downloads/Stanford_Emp_Data (5).csv") {
  emp <- readr::read_csv(counselor_df_path,
                         col_types = cols(
                           "SalesIQ ID" = col_character()
                         )) %>%
    janitor::clean_names() %>%
    # expect columns like: sales_iq_id, type_education, designation_name, employee_name, etc.
    dplyr::rename(typeeducation = type_education,
                  designation_name = designation_name
    ) %>%
    mutate(
      education_level = case_when(
        typeeducation %in% c("Psychology Under Graduate", "Psychology Under Graduate Pursuing") ~ "Psychology UG",
        typeeducation %in% c("Psychology Post Graduate", "Psychology Post Graduate Pursuing", "Phd/MPhil in Psychology") ~ "Psychology PG",
        typeeducation %in% c("Non Psychology") ~ "Non-Psychology",
        is.na(typeeducation) | typeeducation %in% c("-No Value-", "Psychology") ~ "Missing",
        TRUE ~ "Missing"
      ),
      education_level = factor(education_level,
                               levels = c("Psychology UG", "Psychology PG", "Non-Psychology", "Missing")
      ),
      employment_status = case_when(
        designation_name %in% c("Intern-UG", "Intern-PG") ~ "Intern",
        designation_name == "Volunteer India"             ~ "Volunteer",
        designation_name %in% c("Mentor", "Trainer")      ~ "Senior/Trainer",
        designation_name == "Counsellor"                  ~ "Counsellor",
        is.na(designation_name)                           ~ "Other",
        TRUE                                              ~ "Other"
      ),
      employment_status = factor(employment_status,
                                 levels = c("Intern", "Volunteer", "Counsellor", "Senior/Trainer", "Other")
      )
    )
  
  # map each counsellor to their sender id from messages
  counsellor_key <- messages %>%
    dplyr::filter(!is.na(counsellor) | !is.na(message_sender_id)) %>%
    dplyr::arrange(counsellor, DateInserted) %>%
    dplyr::group_by(counsellor) %>%
    dplyr::summarise(
      message_sender_id = suppressWarnings(first(na.omit(message_sender_id))),
      earliest_counsellor_message = suppressWarnings(min(DateInserted, na.rm = TRUE)),
      latest_counsellor_message   = suppressWarnings(max(DateInserted, na.rm = TRUE)),
      total_counselor_messages = dplyr::n(),
      total_ai_counselor_messages = sum(!is.na(ai_suggestion), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(message_sender_id = as.character(message_sender_id))
  
  # LEFT JOIN: left_on=message_sender_id, right_on=sales_iq_id
  counsellor_df <- counsellor_key %>%
    dplyr::left_join(emp, by = c("message_sender_id" = "sales_iq_id"))
  
  counsellor_df
}


enrich_with_counsellor_confounds <- function(convo_df_filtered, counsellor_df) {
  counsellor_df_filtered <- counsellor_df %>%
    semi_join(convo_df_filtered, by = "message_sender_id")
  
  convo_df_filtered %>%
    left_join(counsellor_df_filtered, by = "message_sender_id")
}

# ---------- EXPERIENCE COUNTS (TOTAL & COPILOT) ----------

#' Compute per-conversation counselor experience metrics
#'
#' @description
#' This function computes two experience-based confounders at the conversation level:
#' 1. `total_msgs_sent` — total number of outgoing messages a counselor has sent
#'    *before* the start of each conversation.
#' 2. `copilot_msgs_sent` — total number of those outgoing messages that were AI-assisted
#'    (`ai_suggestion` not missing or empty).
#'
#' These variables represent counselor experience and familiarity with the copilot
#' up to the time of each conversation, which may confound treatment assignment
#' (use vs. non-use of the copilot) and outcomes.
#'
#' @param convo_df_filtered A dataframe of conversation-level observations,
#'   containing at least `conversation_id`, `counsellor`, and `first_message_time`.
#' @param all_message_level_data A dataframe of all messages, containing
#'   `counsellor`, `DateInserted`, `direction`, and `ai_suggestion`.
#'
#' @return A version of `convo_df_filtered` augmented with:
#'   - `total_msgs_sent`: number of outgoing messages sent by the same counselor
#'     before this conversation started.
#'   - `copilot_msgs_sent`: number of those outgoing messages that included an
#'     AI suggestion before the conversation started.
#'
#' @details
#' Implementation steps:
#' 1. Ensure timestamp columns are standardized as UTC datetimes.
#' 2. Filter all outgoing counselor messages and assign an order within counselor.
#' 3. Use a **non-equi join** (`DateInserted < first_message_time`) to count
#'    how many messages occurred before each conversation.
#' 4. Aggregate the counts and join back to the conversation dataframe.
#' 5. Replace missing values with zero for counselors with no prior messages.
#'
#' @examples
#' convo_df_enriched <- compute_message_experience(convo_df_filtered, all_message_level_data)
#'
compute_message_experience <- function(convo_df_filtered, all_message_level_data) {
  # --- 1. Standardize datetime columns to UTC ---
  convo_df_filtered <- convo_df_filtered %>%
    mutate(first_message_time = lubridate::as_datetime(first_message_time, tz = "UTC"))
  
  msgs <- all_message_level_data %>%
    mutate(DateInserted = lubridate::as_datetime(DateInserted, tz = "UTC"))
  
  # --- 2. Extract all outgoing counselor messages and order them chronologically ---
  counsellor_msgs <- msgs %>%
    filter(direction == "Outgoing") %>%
    arrange(counsellor, DateInserted) %>%
    group_by(counsellor) %>%
    mutate(message_sent_order = row_number()) %>%
    ungroup()
  
  # --- 3. Extract conversation start times ---
  conversation_start_times <- convo_df_filtered %>%
    distinct(conversation_id, counsellor, first_message_time)
  
  # --- 4. Use data.table for efficient non-equi join (messages before convo start) ---
  data.table::setDT(conversation_start_times)
  data.table::setDT(counsellor_msgs)
  data.table::setkey(counsellor_msgs, counsellor, DateInserted)
  
  convo_with_experience <- counsellor_msgs[
    !is.na(counsellor) & !is.na(DateInserted)
  ][
    conversation_start_times,
    on = .(counsellor, DateInserted < first_message_time),
    .(
      conversation_id   = i.conversation_id,
      total_msgs_sent   = .N,  # count all prior outgoing messages
      copilot_msgs_sent = sum(!is.na(ai_suggestion) & ai_suggestion != "", na.rm = TRUE)
    ),
    by = .EACHI
  ][]
  
  # --- 5. Handle missing columns / values ---
  if (!"total_msgs_sent" %in% names(convo_with_experience)) {
    convo_with_experience[, `:=`(total_msgs_sent = 0L, copilot_msgs_sent = 0L)]
  } else {
    convo_with_experience[is.na(total_msgs_sent),  `:=`(total_msgs_sent = 0L)]
    convo_with_experience[is.na(copilot_msgs_sent),`:=`(copilot_msgs_sent = 0L)]
  }
  
  # --- 6. Keep relevant columns only and join back ---
  convo_with_experience <- convo_with_experience[, .(conversation_id, total_msgs_sent, copilot_msgs_sent)]
  
  convo_df_filtered %>%
    left_join(convo_with_experience, by = "conversation_id") %>%
    mutate(
      counselor_total_msgs_sent_so_far   = coalesce(total_msgs_sent, 0L),
      counselor_copilot_msgs_sent_so_far = coalesce(copilot_msgs_sent, 0L)
    ) %>% 
    select(-total_msgs_sent, copilot_msgs_sent)
}

# ---------- TENURE (MONTHS WITH FOUNDATION) ----------

compute_time_with_foundation <- function(convo_df_with_confounds) {
  convo_df_with_confounds %>%
    mutate(
      time_with_foundation_so_far = pmax(
        0,
        lubridate::time_length(
          lubridate::interval(earliest_counsellor_message, first_message_time),
          unit = "months"
        )
      )
    )
}

# ---------- MULTITASKING (OVERLAPS) ----------

build_conversation_times <- function(messages) {
  # Work in data.table
  dt <- as.data.table(messages)
  
  # Sort once by conversation + time (so each group is already ordered)
  setorder(dt, conversation_id, DateInserted)
  
  out <- dt[, {
    n <- .N
    
    # DateInserted is already sorted within each conversation_id
    first_time <- if (n > 5L) DateInserted[5L] else DateInserted[1L]
    
    last_idx   <- if (n > 5L) max(1L, n - 4L) else n
    last_time  <- DateInserted[last_idx]
    
    # First non-NA counsellor (like suppressWarnings(first(na.omit(...))))
    non_na_idx <- which(!is.na(counsellor))
    counsellor_val <- if (length(non_na_idx)) counsellor[non_na_idx[1L]] else NA_character_
    
    convo_start <- first_time
    convo_end   <- last_time
    
    # Ensure end > start
    if (!is.na(convo_end) && !is.na(convo_start) && convo_end <= convo_start) {
      convo_end <- convo_start + seconds(1)
    }
    
    .(counsellor = counsellor_val,
      convo_start = convo_start,
      convo_end   = convo_end)
  }, by = conversation_id]
  
  as_tibble(out)
}

compute_multitasking_features <- function(convo_df_filtered, all_message_level_data,
                                          overlap_mins = 5L) {
  # 1) Build per-conversation intervals
  convo_times <- build_conversation_times(all_message_level_data) %>%
    dplyr::filter(!is.na(counsellor))
  
  # 2) Prep data.tables
  dt <- data.table::as.data.table(convo_times)
  data.table::setkey(dt, counsellor, convo_start, convo_end)
  
  # 3) Create an "i" copy with adjusted bounds that enforce the min-overlap
  overlap <- as.difftime(overlap_mins, units = "mins")
  dt_i <- data.table::copy(dt)
  dt_i[, `:=`(
    start_plus = convo_start + overlap,  # i.convo_start + Δ
    end_minus  = convo_end   - overlap   # i.convo_end   - Δ
  )]
  
  # 4) Non-equi self join using adjusted bounds
  # Require: x.convo_start <= i.end_minus AND x.convo_end >= i.start_plus
  ov <- dt[dt_i,
           on = .(counsellor,
                  convo_start <= end_minus,
                  convo_end   >= start_plus),
           nomatch = 0L,
           allow.cartesian = TRUE]
  
  # 5) Drop self-pairs
  ov <- ov[conversation_id != i.conversation_id]
  
  # 6) Count qualifying overlaps per conversation (i = focal)
  counts <- ov[, .(num_simultaneous_convos = .N), by = .(i.conversation_id)]
  data.table::setnames(counts, "i.conversation_id", "conversation_id")
  
  # 7) Merge back + flags
  convo_df_filtered %>%
    dplyr::left_join(counts, by = "conversation_id") %>%
    dplyr::mutate(
      num_simultaneous_convos = dplyr::coalesce(num_simultaneous_convos, 0L),
      is_multitasking = as.integer(num_simultaneous_convos > 0L)
    )
}

# ---------- MAIN PIPELINE ----------

#' Build full analytic dataset
#' @param path path to message-level CSV
#' @param start_date window start (inclusive) as Date
#' @param end_date window end (inclusive) as Date
#' @param exclude_counsellors vector of counsellor names to drop
#' @param output_path optional CSV path; if provided, write dataset
#' @return list(messages, convo_df_raw, convo_df_filtered, counsellor_df, full_dataset)
main <- function(path = NULL,
                 messages = NULL,                 # <— NEW: optionally pass preloaded messages
                 # IE thresholds
                 min_total_msgs        = 20,
                 min_counselor_msgs    = 5,
                 min_client_msgs       = 5,
                 ie_last_message_cutoff = as.Date("2025-10-08"),
                 exclude_counsellors   = c("Sharang Phadke"),
                 # outcomes (optional)
                 outcome_file_paths = NULL,
                 # IO
                 save_path = NULL,     # path to .RData file
                 verbose = TRUE) {
  
  # --- Acquire messages ---
  if (is.null(messages)) {
    print("Loading messages (might take a while)")
    if (is.null(path)) stop("Provide either `messages` or `path`.")
    messages <- read_messages(path)
  } else {
    # Light normalization if user passed a raw-ish messages table
    if (!inherits(messages$DateInserted, "POSIXt")) {
      messages <- messages %>% dplyr::mutate(DateInserted = lubridate::as_datetime(DateInserted, tz = "UTC"))
    }
    if (!"message_sender_id" %in% names(messages)) {
      stop("`messages` must include `message_sender_id`.")
    }
    messages <- messages %>%
      dplyr::mutate(message_sender_id = as.character(message_sender_id))
  }
  
  print("Names in messages:")
  print(names(messages))
  
  # --- Build conversation-level raw table ---
  print("Building conversation-level table")
  convo_df_raw <- assemble_conversations(messages, exclude_counsellors = exclude_counsellors)
  
  # --- IE criteria as quosures ---
  print("Applying IE criteria")
  ie_list <- rlang::exprs(
    !!glue::glue("≥ {min_total_msgs} total messages")       := n_messages >= !!min_total_msgs,
    !!glue::glue("≥ {min_counselor_msgs} counselor messages") := n_counselor_messages >= !!min_counselor_msgs,
    !!glue::glue("≥ {min_client_msgs} client messages")       := n_client_messages >= !!min_client_msgs,
    !!glue::glue("Last message ≤ {ie_last_message_cutoff}")  := as.Date(last_message_time) <= !!ie_last_message_cutoff
  )
  
  
  # --- Attrition counts only ---
  attrition_full <- make_attrition_table(
    data    = convo_df_raw,
    ie_list = ie_list,
    first_row_name = "Initial conversations"
  )
  attrition <- attrition_full$cohort_selection
  if (verbose) print(attrition)
  
  # --- Final filtered cohort (apply all IE filters) ---
  final_core <- attrition_full$filtered_datasets[[nrow(attrition)]]
  
  # --- Counsellor-level data from messages ---
  print("Building counselor-level table")
  counsellor_df <- build_counsellor_df(messages)
  
  # --- Enrich final cohort with confounders/experience/multitasking ---
  print("Computing confounders")
  print(glue("Rows before adding confounders: {nrow(final_core)}"))
  
  full_dataset <- final_core %>%
    enrich_with_counsellor_confounds(counsellor_df) %>%
    compute_message_experience(messages) %>%
    compute_time_with_foundation() %>%
    compute_multitasking_features(messages)
  
  print(glue("Rows after adding confounders: {nrow(full_dataset)}"))
  
  # --- Final filtered cohort + duration; join counsellor info by sender_id (kept for convenience) ---
  final_df <- full_dataset %>%
    dplyr::mutate(convo_duration_mins = difftime(last_message_time, first_message_time, units = "mins")) %>%
    dplyr::select(conversation_uid, message_sender_id, dplyr::everything(), -conversation_id, -counsellor)
  
  # --- Merge outcomes (optional) ---
  print("Loading conversation-level outcomes")
  all_outcomes  <- NULL
  missing_evals <- NULL
  final_df_with_outcomes <- final_df
  
  if (!is.null(outcome_file_paths)) {
    long_convos_for_join <- final_df %>%
      dplyr::select(convo, conversation_uid, message_sender_id) %>%
      dplyr::distinct()
    
    outcome_res <- combine_outcomes(outcome_file_paths, long_convos_for_join)
    all_outcomes  <- outcome_res$all_outcomes
    missing_evals <- outcome_res$missing_evals
    
    final_df_with_outcomes <- final_df %>%
      dplyr::left_join(all_outcomes, by = "convo") %>%
      dplyr::filter(!is.na(raw_response)) %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::coalesce(.x, "missing"))) %>% 
      mutate(num_simultaneous_convos_cat = case_when(num_simultaneous_convos >= 3 ~ "≥3",
                                                     T ~ as.character(num_simultaneous_convos)))
    
    if (verbose) {
      cat("Rows in merged outcomes:", nrow(all_outcomes), "\n")
      cat("Conversations missing evals:", nrow(missing_evals), "\n")
      cat("Rows in final_df_with_outcomes:", nrow(final_df_with_outcomes), "\n")
    }
  }
  
  # --- Build a bundle for return ---
  all_dfs <- list(
    messages      = messages,
    convo_df_raw  = convo_df_raw,
    all_outcomes  = all_outcomes,
    counsellor_df = counsellor_df,
    attrition     = attrition,              # counts only
    missing_evals = missing_evals,
    full_dataset  = final_df_with_outcomes
  )
  
  # --- Save as .RData if requested ---
  print("Saving to path")
  if (!is.null(save_path)) {
    save(all_dfs, file = save_path)
    if (verbose) cat("Saved to:", save_path, "\n")
  }
  
  return(all_dfs)
  
}



combine_outcomes <- function(outcome_file_paths, long_convos) {
  stopifnot(length(outcome_file_paths) >= 3)
  
  # --- Read CSVs
  test1 <- readr::read_csv(outcome_file_paths[[2]])
  test2 <- readr::read_csv(outcome_file_paths[[1]]) %>%
    dplyr::rename(convo = deid_convo)
  test3 <- readr::read_csv(outcome_file_paths[[3]])
  
  if (length(outcome_file_paths) > 3) {
    all_other_outcome_dfs <- map_dfr(outcome_file_paths[-(1:3)], read_csv)
    dfs_to_merge <- list(test1, test2, test3, all_other_outcome_dfs)
  } else{
    dfs_to_merge <- list(test1, test2, test3)
  }
  
  # --- Merge + standardize columns
  df_outcomes <- dplyr::bind_rows(dfs_to_merge) %>%
    dplyr::select(
      convo, raw_response, parsed_json,
      tidyselect::matches("^P|^M|^S|^X", ignore.case = FALSE),
      case_categorization, task_index
    )
  
  # --- Rubric weights (Rubric 2)
  rubric_weights <- c(
    P1 = 2, P2 = 1, P3 = 1, P4 = 1, P5 = 1, P6 = 1,   # P7 removed
    P8 = 2,
    M1 = 2, M2 = 1, M3 = 1, M4 = 1, M5 = 1,
    S1 = 1, S2 = 1, S3 = 2, S4 = 1, S5 = 1, S6 = 1, S7 = 1,
    X0 = 0, X1 = 4, X2 = 4, X3 = 2, X4 = 2
  )
  rubric_vars <- names(rubric_weights)
  
  # --- Helper: coerce a single value to 0/1/NA
  coerce01_single <- function(x) {
    if (is.null(x)) return(NA_real_)
    if (is.logical(x)) return(as.numeric(x))
    if (is.numeric(x)) return(as.numeric(x))
    if (is.character(x)) {
      z <- trimws(tolower(x))
      if (z %in% c("1","true","t","yes","y"))  return(1)
      if (z %in% c("0","false","f","no","n")) return(0)
      suppressWarnings({
        nn <- as.numeric(z)
        if (!is.na(nn)) return(nn)
      })
      return(NA_real_)
    }
    suppressWarnings(as.numeric(x))
  }
  
  # --- Row scorer
  score_rubric_row <- function(row, rubric_weights = rubric_weights) {
    # Pull rubric fields from the row (non-existent -> NA)
    vals <- setNames(
      vapply(rubric_vars, function(k) coerce01_single(row[[k]]), numeric(1)),
      rubric_vars
    )
    
    # Eligible = not NA
    elig <- names(vals)[!is.na(vals)]
    
    # Gate risk-dependent items when X0 == 0
    if (!is.na(vals[["X0"]]) && vals[["X0"]] == 0) {
      elig <- setdiff(elig, c("X1","X2","X3","X4"))
    }
    
    possible_score   <- if (length(elig)) sum(rubric_weights[elig]) else 0
    total_score      <- if (length(elig)) sum(rubric_weights[elig] * vals[elig]) else 0
    fractional_score <- if (possible_score > 0) total_score / possible_score else NA_real_
    
    list(
      possible_score   = possible_score,
      total_score      = total_score,
      fractional_score = fractional_score
    )
  }
  
  # --- Ensure rubric columns exist in df_outcomes (missing -> NA)
  missing_cols <- setdiff(rubric_vars, names(df_outcomes))
  if (length(missing_cols) > 0) {
    df_outcomes[missing_cols] <- NA
  }
  
  row1 <- df_outcomes[1,]
  
  score_rubric_row(as.list(row1), rubric_weights)
  
  # --- Apply row-wise scorer
  scores_list <- purrr::pmap(df_outcomes, ~ score_rubric_row(list(...), rubric_weights = rubric_weights))
  # Bind back as columns
  scores_df <- map_dfr(scores_list, as_tibble)
  df_outcomes_scored <- dplyr::bind_cols(df_outcomes, scores_df)
  
  # --- Missing evals
  missing_evals <- long_convos %>%
    dplyr::anti_join(df_outcomes_scored, by = "convo")
  
  list(
    all_outcomes  = df_outcomes_scored,
    missing_evals = missing_evals
  )
}



# ---------- Example CLI-ish usage (comment/uncomment as needed) ----------
if (F) {
  
  all_dfs <- main(
    # messages = all_dfs$messages,
    path = "/Users/akshayswaminathan/Downloads/all_message_level_data_20251205.parquet",
    outcome_file_paths = c(
      "/Users/akshayswaminathan/Downloads/merged_batch_results.csv",
      "/Users/akshayswaminathan/Downloads/merged_batch_results_3.csv",
      "/Users/akshayswaminathan/Downloads/merged_batch_results_2.csv",
      "/Users/akshayswaminathan/Downloads/merged_batch_results_4.csv"
    ),
    ie_last_message_cutoff = as.Date("2025-12-04"),
    save_path = "/Users/akshayswaminathan/Downloads/vf_obs_causal_dfs.RData"
  )
  
  all_dfs$full_dataset$ai_used %>% table
  all_dfs$attrition
  
  df <- read_parquet(
    file = "/Users/akshayswaminathan/Downloads/all_message_level_data_20251205.parquet",
    as_data_frame = TRUE  # returns a tibble if tibble is installed
  )
  
  # Write convos that need evals to get evaluated
  all_dfs$missing_evals %>% 
    write_parquet("/tmp/leftover_evals.parquet")
  
}


if (F) { # exploration
  
  # Step 1: Identify long conversations
  long_convos <- res$convo_df_raw %>% 
    filter(n_messages >= 20,
           n_counselor_messages >= 5,
           n_client_messages >= 5,
           as.Date(last_message_time) <= as.Date("2025-10-08")) %>% 
    mutate(convo_duration = difftime(last_message_time, first_message_time, units = "mins"))
  
  long_convos %>% 
    count(last_message_time >= as.Date("2025-03-08"))
  
  long_convos %>% 
    group_by(counsellor) %>% 
    summarize(used_ai = any(ai_used),
              never_used_ai = !any(ai_used),
              both = any(ai_used) & any(!ai_used),
              has_pre_ai_convo = any(last_message_time < as.Date("2025-03-08")),
              has_pre_ai_convo_jan = any(last_message_time < as.Date("2025-01-01")),
              has_pre_and_post_ai_convo = any(last_message_time < as.Date("2025-03-08")) & any(last_message_time >= as.Date("2025-03-08")),
              has_pre_and_post_ai_convo_jan = any(last_message_time < as.Date("2025-01-01")) & any(last_message_time >= as.Date("2025-03-08"))) %>% 
    summarise_if(is.logical, sum)
  
  long_convos %>% 
    ggplot(aes(x = first_message_time, y = ai_used)) +
    geom_point(alpha = 0.3, shape = "|")
  
  ggplot(long_convos, aes(x = convo_duration)) +
    geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
    labs(
      title = "Distribution of Conversation Duration in Mins",
      x = "Conversation Duration (Min)",
      y = "Count"
    ) +
    theme_minimal()
  
  long_convos %>% 
    filter(convo_duration > 500)
  
  long_convos %>% 
    write_csv("~/Downloads/convos_for_eval.csv")
  
  
  
  # Step 2: Join messages and compute inter-message gaps
  longest_gaps <- res$messages %>%
    inner_join(long_convos, by = "conversation_id") %>%
    arrange(conversation_id, msg_sequence_overall) %>%
    group_by(conversation_id) %>%
    filter(msg_sequence_overall > 5, 
           msg_sequence_overall <= (n() - 5)) %>% 
    mutate(
      prev_time = lag(DateInserted),
      gap_mins = as.numeric(difftime(DateInserted, prev_time, units = "mins"))
    ) %>%
    summarise(
      longest_gap_mins = max(gap_mins, na.rm = TRUE),
      n_messages = n()
    ) %>%
    arrange(desc(longest_gap_mins))
  
  longest_gaps %>% 
    filter(n_messages >= 20) %>% pull(longest_gap_mins) %>% summary()
  tail()
  
  # Step 3: View specific conversation (optional)
  res$messages %>%
    filter(conversation_id == 57218) %>%
    arrange(msg_sequence_overall) %>%
    View()
  
  
  # Join in outcomes data
  
  outcomes_df <- read_csv('/tmp/merged_batch_results.csv')
  
  convos_and_outcomes_df <- long_convos %>% 
    left_join(outcomes_df %>% 
                rename(convo = deid_convo), by = "convo")
  
  yet_to_grade <- long_convos %>% 
    anti_join(outcomes_df %>% 
                rename(convo = deid_convo), by = "convo") %>% 
    select(conversation_uid, convo)
  
  yet_to_grade %>% 
    write_csv("/tmp/convos_to_grade_pt2.csv")
  
  sample_row <- convos_and_outcomes_df %>% 
    filter(!is.na(raw_response)) %>% 
    select(conversation_uid, convo, raw_response) %>% 
    sample_n(1)
  
  print(sample_row$convo)
  print(sample_row$raw_response)
  
  
  mean(is.na(test$conversation_uid))
  
  res$convo_df_raw %>% 
    filter(n_messages >= 20) %>% 
    group_by(convo) %>% 
    filter(n() > 1) %>% 
    arrange(convo)
  
  # Get all outcome files
  outcome_file_paths <- list("/Users/akshayswaminathan/Downloads/merged_batch_results.csv",
                             "/Users/akshayswaminathan/Downloads/merged_batch_results_3.csv",
                             "/Users/akshayswaminathan/Downloads/merged_batch_results_2.csv"
  )
  
  test1 <- read_csv(outcome_file_paths[[2]])
  test2 <- read_csv(outcome_file_paths[[1]]) %>% 
    rename(convo = deid_convo)
  test3 <- read_csv(outcome_file_paths[[3]])
  
  same_names <- intersect(names(test1), names(test2))
  
  all_outcomes <- bind_rows(test1, test2, test3) %>% 
    select(all_of(same_names))
  
  missing_evals <- long_convos %>% 
    anti_join(all_outcomes, "convo")
  
  long_convos %>% 
    semi_join(missing_evals, "convo") %>% 
    write_csv("/tmp/leftover_evals.csv")
  
  test1$convo
  
  
  
  
}
