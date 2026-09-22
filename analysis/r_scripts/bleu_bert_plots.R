library(tidyverse)
library(lubridate)
library(showtext)
library(ggrepel)

# Optional font
# font_add("Avenir Medium", "/System/Library/Fonts/Avenir.ttc")
# showtext_auto()

weekly_df <- read_csv("/Users/akshayswaminathan/Downloads/stats_msg_W.csv")

# -----------------------------
# Prepare data for long-format plotting
# -----------------------------
plot_df <- weekly_df %>%
  transmute(
    week          = time_period,
    bleu          = mean_bleu_scores,
    bert          = mean_bert_f1,
    prop_no_edits = prop_bleu_1,                # sent with no edits
    prop_edits    = prop_sent_with_edits,       # sent with edits
    prop_not_sent = prop_generated_but_not_sent # generated but not sent
  ) %>%
  pivot_longer(
    cols = c(bleu, bert, prop_no_edits, prop_edits, prop_not_sent),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      "bleu"          = "BLEU score",
      "bert"          = "BERT F1 score",
      "prop_no_edits" = "Prop. sent without edits",
      "prop_edits"    = "Prop. sent with edits",
      "prop_not_sent" = "Prop. not sent"
    ),
    metric = factor(
      metric,
      levels = c(
        "BLEU score",
        "BERT F1 score",
        "Prop. sent without edits",
        "Prop. sent with edits",
        "Prop. not sent"
      )
    )
  )

# -----------------------------
# Colors (Okabe–Ito safe palette)
# -----------------------------
cols <- c(
  "BLEU score"               = "#0072B2",  # blue
  "BERT F1 score"            = "#009E73",  # green
  "Prop. sent without edits" = "#D55E00",  # vermillion
  "Prop. sent with edits"    = "#CC79A7",  # pink
  "Prop. not sent"           = "#999999"   # gray
)

# -----------------------------
# Identify final points for labeling
# -----------------------------
label_df <- plot_df %>%
  group_by(metric) %>%
  filter(week == "2025-10-06") %>%
  ungroup() %>%
  mutate(label = metric %>% 
           gsub("Prop.", "%", .))


# -----------------------------
# Plot with IQR ribbons for BLEU + BERT
# -----------------------------
plot_bert_bleu <- ggplot() +
  # IQR ribbon for BLEU
  geom_ribbon(
    data = weekly_df,
    aes(
      x    = time_period,
      ymin = bleu_25,
      ymax = bleu_75
    ),
    fill = cols["BLEU score"],
    alpha = 0.12
  ) +
  # IQR ribbon for BERT
  geom_ribbon(
    data = weekly_df,
    aes(
      x    = time_period,
      ymin = bert_25,
      ymax = bert_75
    ),
    fill = cols["BERT F1 score"],
    alpha = 0.12
  ) +
  # All five mean lines
  geom_line(
    data = plot_df,
    aes(x = week, y = value, color = metric),
    linewidth = 1.1
  ) +
  
  # geom_text(
  #   data = label_df,
  #   aes(x = week + 2, y = value, label = label, color = metric),
  #   hjust = 0,
  #   fontface = "bold",
  #   size = 4
  # ) +
  # 
  geom_text_repel(
    data = label_df,
    aes(x = week, y = value, label = label, color = metric),
    nudge_x = 5,
    direction = "y",
    segment.color = NA,
    size = 4,
    fontface = "bold",
    family = "Avenir Medium"
  ) +
  
  
  scale_color_manual(values = cols, name = "") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  
  labs(
    title = "Weekly BLEU, BERT, and copilot sending behavior over time",
    x = "",
    y = ""
  ) +
  
  guides(color = "none",
         label = "none") +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid       = element_blank(),
    axis.title.x     = element_text(margin = margin(t = 20)),
    axis.title.y     = element_text(margin = margin(r = 20)),
    text             = element_text(family = "Avenir Medium")
  )


save(file = "/tmp/bleu_bert_plot.RData", plot_bert_bleu)

