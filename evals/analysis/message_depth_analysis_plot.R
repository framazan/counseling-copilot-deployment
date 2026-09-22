library(tidyverse)
library(arrow)

# -----------------------------
# Load + prepare data
# -----------------------------
ratings_df <- arrow::read_parquet(
  "/Users/akshayswaminathan/Downloads/counselor_messages_with_ratings.parquet"
) %>% 
  transmute(
    id, uid, message, message_length, message_rating,
    suggestion_length,
    ai_used = !is.na(ai_suggestion)
  ) %>% 
  mutate(
    message_rating = factor(
      message_rating,
      levels = c(1, 2, 3),
      labels = c("Low depth", "Moderate depth", "High depth")
    )
  )

# -----------------------------
# Summary table for stacked bar
# -----------------------------
summary_df <- ratings_df %>% 
  filter(!is.na(message_rating)) %>% 
  group_by(message_rating) %>% 
  summarize(
    n_messages         = n(),
    prop_ai_suggestion = mean(ai_used),
    .groups = "drop"
  ) %>% 
  arrange(message_rating)

plot_df <- summary_df %>% 
  mutate(
    prop_non_ai = 1 - prop_ai_suggestion
  ) %>% 
  select(message_rating, n_messages, prop_ai_suggestion, prop_non_ai) %>% 
  pivot_longer(
    cols      = c(prop_ai_suggestion, prop_non_ai),
    names_to  = "ai_used",
    values_to = "prop"
  ) %>%
  mutate(
    ai_used = recode(
      ai_used,
      "prop_ai_suggestion" = "With copilot",
      "prop_non_ai"        = "Without copilot"
    ),
    ai_used = factor(ai_used, levels = c("Without copilot", "With copilot"))
  )

# -----------------------------
# Example messages per depth
# -----------------------------
set.seed(123)

# helper to truncate after N words
truncate_words <- function(text, n_words = 15) {
  words <- str_split(as.character(text), "\\s+")[[1]]
  if (length(words) <= n_words) {
    paste(words, collapse = " ")
  } else {
    paste(c(words[1:n_words], "..."), collapse = " ")
  }
}

examples_df <- ratings_df %>% 
  filter(!is.na(message_rating)) %>% 
  group_by(message_rating) %>% 
  summarise(
    # prefer shorter messages as examples
    example_message = {
      pool <- message[nchar(message) <= 160]
      if (length(pool) == 0) sample(message, 1) else sample(pool, 1)
    },
    .groups = "drop"
  ) %>% 
  mutate(
    example_short = map_chr(example_message, truncate_words),
    example_wrapped = stringr::str_wrap(example_short, width = 45)
  )

# merge counts + examples for annotation
annot_df <- summary_df %>% 
  left_join(examples_df, by = "message_rating")

# -----------------------------
# Axis labels with n in x-axis
# -----------------------------
axis_labels <- annot_df %>% 
  arrange(message_rating) %>% 
  mutate(
    label = paste0(
      as.character(message_rating),
      "\n(n = ", scales::comma(n_messages), ")"
    )
  ) %>% 
  pull(label)

# -----------------------------
# Colors
# -----------------------------
col_nonai <- "#0072B2"  # blue
col_ai    <- "#D55E00"  # vermillion

# -----------------------------
# Stacked bar plot
# -----------------------------
p_stacked <- plot_df %>% 
  ggplot(aes(x = message_rating, y = prop, fill = ai_used)) +
  geom_col(width = 0.7) +
  
  scale_fill_manual(
    values = c("Without copilot" = col_nonai,
               "With copilot"    = col_ai),
    name = ""
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_x_discrete(labels = axis_labels) +
  labs(
    title = "Copilot use by\nmessage depth",
    x     = "",
    y     = "Proportion of messages"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "left",
    legend.direction  = "vertical",
    axis.text.y       = element_text(lineheight = 1.1)  # axis text now vertical
  ) +
  coord_flip()   # ⭐ MAKE BAR CHART HORIZONTAL


p_stacked
save(file = "/tmp/depth_analysis_plot.RData", p_stacked)

