library(tidyverse)

load("/Users/akshayswaminathan/Downloads/vf_obs_causal_dfs.RData")

simul_convos_df <- all_dfs$full_dataset %>% 
  mutate(pre_access = counselor_copilot_msgs_sent_so_far == 0) %>% 
  select(conversation_uid, message_sender_id, ai_used, 
         counselor_copilot_msgs_sent_so_far, pre_access,
         is_multitasking, num_simultaneous_convos)

# ------------------------------------------------------------
# 1) Build counselor-level + aggregate summaries (pre vs post)
# ------------------------------------------------------------
# --- Counselor-level: mean of means (each counselor equally weighted) ---
counselor_level <- simul_convos_df %>%
  group_by(message_sender_id, pre_access) %>%
  summarize(
    mean_workload     = mean(num_simultaneous_convos, na.rm = TRUE),
    prop_multitasking = mean(is_multitasking == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(pre_access) %>%
  summarize(
    mean_workload     = mean(mean_workload, na.rm = TRUE),
    prop_multitasking = mean(prop_multitasking, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(level = "Counselor-level")

# --- Aggregate: conversation-weighted (each conversation equally weighted) ---
aggregate_level <- simul_convos_df %>%
  group_by(pre_access) %>%
  summarize(
    mean_workload     = mean(num_simultaneous_convos, na.rm = TRUE),
    prop_multitasking = mean(is_multitasking == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(level = "Aggregate")

# Combine
summary_df <- bind_rows(counselor_level, aggregate_level) %>%
  mutate(
    period = if_else(pre_access, "Pre-copilot", "Post-copilot"),
    period = factor(period, levels = c("Pre-copilot", "Post-copilot")),
    level  = factor(level,  levels = c("Counselor-level", "Aggregate"))
  ) %>%
  select(level, period, mean_workload, prop_multitasking)

# ------------------------------------------------------------
# 2) Long format for faceted bar plot
# ------------------------------------------------------------
bar_df <- summary_df %>%
  pivot_longer(
    cols = c(mean_workload, prop_multitasking),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    
    metric = recode(
      metric,
      "mean_workload"     = "Mean simultaneous conversations",
      "prop_multitasking" = "% Multitasking"
    ),
    metric = factor(metric, levels = c("Mean simultaneous conversations",
                                       "% Multitasking")),
    value_plot = if_else(
      metric == "% Multitasking",
      value * 100,
      value
    ),
  ) 

delta_df <- bar_df %>%
  select(level, metric, period, value_plot) %>%
  pivot_wider(names_from = period, values_from = value_plot) %>%
  mutate(
    pct_change = (`Post-copilot` - `Pre-copilot`) / `Pre-copilot`,
    pct_label  = scales::percent(pct_change, accuracy = 1),
    y_pos      = pmax(`Post-copilot`, `Pre-copilot`, na.rm = TRUE)
  ) %>%
  group_by(metric) %>%
  mutate(
    pad   = if_else(str_detect(metric, "%"), 2, 0.15),
    y_lab = y_pos + pad
  ) %>%
  ungroup()



# ------------------------------------------------------------
# 4) Colors (match your style)
# ------------------------------------------------------------
col_pre  <- "#0072B2"  # blue
col_post <- "#D55E00"  # vermillion/orange

# ------------------------------------------------------------
# 5) Bar plot with percent-change annotations + 2 facets
# ------------------------------------------------------------
p_bar_faceted <- bar_df %>%
  ggplot(aes(x = level, y = value_plot, fill = period)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  
  geom_text(
    data = delta_df,
    aes(
      x = level,
      y = y_lab+1,
      label = paste0(ifelse(pct_change > 0, "+", ""), pct_label)
    ),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    family = "Avenir Medium"
  ) +
  
  facet_wrap(~ metric, scales = "free_y") +
  
  scale_fill_manual(
    values = c("Pre-copilot" = col_pre,
               "Post-copilot" = col_post),
    name = ""
  ) +
  
  labs(
    title = "Conversation multitasking before vs after copilot access",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.key.size = unit(0.6, "lines"),
    legend.direction = "vertical",
    legend.text     = element_text(margin = margin(r = 50)),
    text            = element_text(family = "Avenir Medium"),
    axis.title.x    = element_text(margin = margin(t = 10)),
    axis.title.y    = element_text(margin = margin(r = 10))
  )

p_bar_faceted


save(file = "/tmp/multitasking_plot.RData", p_bar_faceted)
