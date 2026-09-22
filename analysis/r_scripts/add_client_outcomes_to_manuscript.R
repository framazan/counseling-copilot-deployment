# =============================================================================
# add_client_outcomes_to_manuscript.R
# =============================================================================
# PURPOSE: Extend the existing ITT DiD analysis (itt_analysis_2.R) to include
#          client outcome metrics — CR1 (retention), CS1 (satisfaction), and
#          CS2_high (experience score ≥4) — and regenerate Table 1 and Figure 3
#          for the LLM deployment manuscript.
#
# DEPENDENCIES: Run AFTER make_obs_causal_eval_dataset.R and itt_analysis_2.R
#               Both scripts must be sourced or their outputs available.
#
# INPUTS:
#   - all_dfs (list): output of make_obs_causal_eval_dataset.R; must contain
#     all_dfs$full_dataset with columns CR1, CS1, CS2 (from the retention/
#     satisfaction scoring pipeline on deployment_full_dataset_20260921.parquet)
#   - df_prepped: output of add_itt_vars() from itt_analysis_2.R
#
# OUTPUTS:
#   - did_summary_client: DiD results for CR1_num, CS1_num, CS2_high
#   - did_summary_combined: existing + new outcomes (for updated Table 1 / Fig 3)
#   - Regenerated Figure 3 (combined forest plot)
#   - Regenerated Table 1 (combined gt table)
#
# USAGE (interactive):
#   source("evals/analysis/make_obs_causal_eval_dataset.R")   # builds all_dfs
#   source("evals/analysis/itt_analysis_2.R")                 # defines helpers
#   source("evals/analysis/add_client_outcomes_to_manuscript.R")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(ggplot2)
  library(glue)
  library(gt)
  library(stringr)
  library(arrow)     # for read_parquet if loading directly
})

# ── STEP 0: Verify prerequisites ──────────────────────────────────────────────

if (!exists("all_dfs") || is.null(all_dfs$full_dataset)) {
  stop(
    "all_dfs not found. Please run make_obs_causal_eval_dataset.R first:\n",
    "  source('evals/analysis/make_obs_causal_eval_dataset.R')"
  )
}

if (!exists("df_prepped")) {
  # Try to reconstruct from all_dfs if not available
  message("df_prepped not found — rebuilding from all_dfs$full_dataset...")
  if (!exists("add_itt_vars")) {
    stop(
      "add_itt_vars() not defined. Please source itt_analysis_2.R first:\n",
      "  source('evals/analysis/itt_analysis_2.R')"
    )
  }
  df_prepped <- add_itt_vars(all_dfs$full_dataset, pc_top_k = 12) %>%
    mutate_at(vars(education_level, employment_status), ~coalesce(.x, "Missing"))
}

# ── STEP 1: Add client outcome columns to df_prepped ─────────────────────────
#
# The retention/satisfaction pipeline produces three outcome columns in the
# full_dataset parquet:
#   CR1       — "True"/"False"/"NA" (string) — was client retained?
#   CS1       — "True"/"False"/"NA" (string) — did client express satisfaction?
#   CS2       — numeric 1–5              — LLM-scored experience rating
#
# We convert these to integer 0/1 so they are compatible with
# run_did_for_outcomes(), which expects outcomes in c(1, TRUE, "1", "true", "TRUE").
#
# NOTE on CS2:
#   CS2 is a 1–5 ordinal scale. We dichotomize at ≥4 (high satisfaction) to
#   maintain comparability with the binary outcomes in the rest of the analysis.
#   See Section D (notebook) for justification; a continuous OLS robustness check
#   is recommended before final publication.

df_prepped <- df_prepped %>%
  mutate(
    # CR1: Python boolean string → integer 0/1
    CR1_num = dplyr::case_when(
      CR1 %in% c("True", "TRUE", TRUE, 1, "1") ~ 1L,
      CR1 %in% c("False", "FALSE", FALSE, 0, "0") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # CS1: same conversion
    CS1_num = dplyr::case_when(
      CS1 %in% c("True", "TRUE", TRUE, 1, "1") ~ 1L,
      CS1 %in% c("False", "FALSE", FALSE, 0, "0") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # CS2_high: dichotomized experience score (≥4 vs <4)
    CS2_high = dplyr::case_when(
      as.numeric(CS2) >= 4 ~ 1L,
      !is.na(as.numeric(CS2)) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Spot-check
message(glue("CR1_num non-null: {sum(!is.na(df_prepped$CR1_num)):,}  ",
             "(base rate: {round(mean(df_prepped$CR1_num, na.rm=TRUE)*100,1)}%)"))
message(glue("CS1_num non-null: {sum(!is.na(df_prepped$CS1_num)):,}  ",
             "(base rate: {round(mean(df_prepped$CS1_num, na.rm=TRUE)*100,1)}%)"))
message(glue("CS2_high non-null: {sum(!is.na(df_prepped$CS2_high)):,}  ",
             "(base rate: {round(mean(df_prepped$CS2_high, na.rm=TRUE)*100,1)}%)"))

# ── STEP 2: Run staggered DiD for client outcomes ─────────────────────────────
#
# run_did_for_outcomes() is defined in itt_analysis_2.R and uses att_gt()
# (Callaway & Sant'Anna 2021) with:
#   - control_group = "notyettreated"
#   - panel = FALSE (repeated cross-section at conversation level)
#   - clustervars = "message_sender_id"
#
# This is the same estimator used for P1–X4 and fractional_score, ensuring
# methodological consistency across all outcomes in Table 1 and Figure 3.

message("\nRunning Callaway-Sant'Anna DiD for client outcomes...")

did_summary_client <- run_did_for_outcomes(
  df_prepped,
  outcomes = c("CR1_num", "CS1_num", "CS2_high"),
  covars   = c("education_level", "employment_status"),
  min_rows = 50
)

# Rename for manuscript-friendly labels
did_summary_client <- did_summary_client %>%
  mutate(
    outcome_label = dplyr::case_when(
      outcome == "CR1_num"  ~ "Client retained through session (CR1)",
      outcome == "CS1_num"  ~ "Client expressed satisfaction (CS1)",
      outcome == "CS2_high" ~ "Experience score ≥4 out of 5 (CS2)",
      TRUE ~ outcome
    ),
    category = "Client Outcomes"
  )

message("\n=== DiD Results: Client Outcomes ===")
print(did_summary_client %>%
        select(outcome, estimate, se, conf.low, conf.high, p.value, sig))

# ── STEP 3: Combine with existing DiD results ─────────────────────────────────
#
# did_summary must already exist from running itt_analysis_2.R.
# If not, re-run the existing analysis first.

if (!exists("did_summary")) {
  message("did_summary not found — running full DiD for existing outcomes...")
  df_raw <- all_dfs$full_dataset
  df_prepped_existing <- add_itt_vars(df_raw, pc_top_k = 12) %>%
    mutate_at(vars(education_level, employment_status), ~coalesce(.x, "Missing"))
  did_summary <- run_did_for_outcomes(df_prepped_existing)
}

did_summary_combined <- bind_rows(
  did_summary %>% mutate(category = "Counseling Quality"),
  did_summary_client
)

# ── STEP 4: Regenerate Table 1 ────────────────────────────────────────────────
#
# Adds a "Client Outcomes" group below the existing counseling quality metrics.
# Uses format_results_gt() from itt_analysis_2.R, extended with client labels.

client_outcome_labels <- c(
  "CR1_num"  = "Client retained through session",
  "CS1_num"  = "Client expressed satisfaction",
  "CS2_high" = "Experience score ≥4 (high)"
)

# Add to the outcome_labels used by format_results_gt
# (extend the mapping at the top of that function if editing permanently)
message("\nGenerating updated Table 1...")

table1_updated <- did_summary_combined %>%
  mutate(
    category = factor(category,
                      levels = c("Counseling Quality", "Client Outcomes")),
    outcome_clean = dplyr::case_when(
      outcome %in% names(client_outcome_labels) ~ client_outcome_labels[outcome],
      TRUE ~ outcome  # fall back to format_results_gt label mapping for P/M/S/X
    )
  ) %>%
  filter(outcome != "X0") %>%   # X0 excluded per original analysis
  format_results_gt(
    title    = "Table 1. Effect of AI Copilot Access on Counseling Quality and Client Outcomes",
    subtitle = "Staggered difference-in-differences; Callaway-Sant'Anna (2021) estimator"
  )

# Save as HTML for easy viewing/copy-paste into manuscript
gt::gtsave(table1_updated,
           filename = "table1_updated.html",
           path     = here::here("evals/analysis/manuscript_outputs"))

message("Table 1 saved to evals/analysis/manuscript_outputs/table1_updated.html")

# ── STEP 5: Regenerate Figure 3 ───────────────────────────────────────────────

message("\nGenerating updated Figure 3...")

fig3_updated <- did_summary_combined %>%
  filter(outcome != "X0") %>%
  mutate(
    category = factor(
      dplyr::case_when(
        category == "Client Outcomes"                                  ~ "Client Outcomes",
        str_detect(outcome, "fractional|overall|quality")             ~ "Overall Quality",
        str_detect(outcome, "^P[0-9]")                                ~ "Protocol",
        str_detect(outcome, "^M[0-9]")                                ~ "Manner",
        str_detect(outcome, "^S[0-9]")                                ~ "Style and Tone",
        str_detect(outcome, "^X[0-9]")                                ~ "Crisis Management",
        TRUE                                                           ~ "Other"
      ),
      levels = c("Overall Quality", "Protocol", "Manner",
                 "Style and Tone", "Crisis Management", "Client Outcomes")
    ),
    is_client_outcome = category == "Client Outcomes",
    is_significant    = !is.na(p.value) & p.value < 0.05
  ) %>%
  plot_did_summary_faceted(title = "Figure 3. Effect of AI Copilot Access on Counseling Quality and Client Outcomes")

# Add client-outcomes panel highlight
fig3_final <- fig3_updated +
  theme(
    strip.text = element_text(face = "bold", size = 9)
  ) +
  labs(caption = paste(
    "Counseling quality metrics (P–X, Overall): Staggered DiD, Callaway-Sant'Anna (2021).",
    "Client outcomes (CR1, CS1, CS2): same estimator applied to LLM-scored retention/satisfaction.",
    "All models: repeated cross-section; SEs clustered by counsellor.",
    "***p<0.001  **p<0.01  *p<0.05",
    sep = "\n"
  ))

dir.create(here::here("evals/analysis/manuscript_outputs"), showWarnings = FALSE, recursive = TRUE)
ggsave(
  here::here("evals/analysis/manuscript_outputs/figure3_updated.png"),
  plot   = fig3_final,
  width  = 10,
  height = 14,
  dpi    = 300
)

message("Figure 3 saved to evals/analysis/manuscript_outputs/figure3_updated.png")
message("\n✓ All manuscript outputs regenerated. See evals/analysis/manuscript_outputs/")
