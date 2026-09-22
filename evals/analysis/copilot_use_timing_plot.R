library(tidyverse)
library(patchwork)
library(showtext)

# showtext_auto(enable=FALSE)
font_add_google("Source Sans Pro", "source_sans")
font_add_google("Montserrat", "montserrat")

load("/Users/akshayswaminathan/Downloads/vf_obs_causal_dfs.RData")

# Colorblind-safe palette (Okabe–Ito)
col_abs <- "#0072B2"     # blue
col_rel <- "#D55E00"     # vermillion
col_cdf <- "#009E73"     # bluish green
col_total <- "#999999"

alpha_setting <- 0.8

# -------------------------------------------------------------------
# Filter messages
# -------------------------------------------------------------------
filtered_messages <- all_dfs$messages %>% 
  filter(direction == "Outgoing") %>% 
  inner_join(
    all_dfs$full_dataset %>% filter(ai_used), 
    by = c("uid" = "conversation_uid")
  )

min_n <- 30

# -------------------------------------------------------------------
# (1) Absolute message position plot
# -------------------------------------------------------------------
abs_df <- filtered_messages %>% 
  group_by(msg_sequence) %>% 
  summarize(
    n      = n(),
    n_ai   = sum(!is.na(ai_suggestion)),
    p_ai   = n_ai / n,
    .groups = "drop"
  )

p_abs <- abs_df %>% 
  filter(n >= min_n) %>% 
  ggplot(aes(x = msg_sequence, y = p_ai)) +
  geom_line(color = col_abs, linewidth = 1) +
  geom_point(aes(size = n, color = "Absolute"), alpha = alpha_setting) +
  scale_color_manual(values = c("Absolute" = col_abs), name = "", guide = "none") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_size_continuous(name = "", range = c(1,5)) +
  labs(
    title = "Copilot use by absolute position",
    x = "Message sequence",
    y = "Prevalence of copilot use"
  ) +
  theme_minimal()



# -------------------------------------------------------------------
# Relative bins
# -------------------------------------------------------------------
rel_bin_width <- 0.02
breaks <- seq(0, 1, by = rel_bin_width)

rel_df <- filtered_messages %>% 
  mutate(
    rel_pos   = pmin(pmax(msg_sequence / n_messages, 0), 1),
    rel_bin_id = cut(rel_pos, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  ) %>% 
  filter(!is.na(rel_bin_id)) %>% 
  group_by(rel_bin_id) %>% 
  summarize(
    n      = n(),
    n_ai   = sum(!is.na(ai_suggestion)),
    p_ai   = n_ai / n,
    rel_mid = (breaks[unique(rel_bin_id)] + breaks[unique(rel_bin_id) + 1]) / 2,
    .groups = "drop"
  )

# -------------------------------------------------------------------
# (2) Relative message position plot (points sized by n)
# -------------------------------------------------------------------
p_rel <- rel_df %>% 
  filter(n >= min_n) %>% 
  ggplot(aes(x = rel_mid, y = p_ai)) +
  geom_line(color = col_rel, linewidth = 1) +
  geom_point(aes(size = n, color = "Relative"), alpha = alpha_setting) +
  scale_color_manual(values = c("Relative" = col_rel), name = "", guide = "none") +
  scale_size_continuous(guide = "none") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Copilot use by relative position",
    x = "Relative conversation position",
    y = "Prevalence of copilot use"
  ) +
  theme(text = element_text(family = "source_sans")) +
  theme_minimal() 

# -------------------------------------------------------------------
# (2b) Relative plot with histogram + line, dual y axes
# -------------------------------------------------------------------
# Want prevalence (p_ai) on LEFT axis, counts (n) on RIGHT axis
max_n <- max(rel_df$n, na.rm = TRUE)
max_p <- max(rel_df$p_ai, na.rm = TRUE)

p_rel_hist <- rel_df %>% 
  filter(n >= min_n) %>% 
  ggplot(aes(x = rel_mid)) +
  
  # Histogram on RIGHT axis scale → rescale counts to prevalence scale
  geom_col(
    aes(y = n * (max_p / max_n)),
    fill = col_rel, alpha = 0.25, width = rel_bin_width
  ) +
  
  # Prevalence line on LEFT axis
  geom_line(
    aes(y = p_ai),
    color = col_rel, linewidth = 1
  ) +
  
  scale_x_continuous(labels = scales::percent_format()) +
  
  # LEFT: prevalence (0–1)
  scale_y_continuous(
    name = "Prevalence of copilot use",
    labels = scales::percent_format(),
    
    # RIGHT: counts (n)
    sec.axis = sec_axis(
      trans = ~ . * (max_n / max_p),
      name = "N messages"
    )
  ) +
  
  labs(
    title = "PDF of copilot use by position",
    x = "Relative message position within conversation"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Avenir Medium"),
        axis.title.x = element_text(margin = margin(t = 10)),  # move title *away* from x-axis text
        axis.title.y = element_text(margin = margin(r = 10))   # move title *away* from y-axis text
  )
  


# -------------------------------------------------------------------
# (3) CDF of copilot use timing
# -------------------------------------------------------------------
ai_messages <- filtered_messages %>% 
  filter(!is.na(ai_suggestion)) %>% 
  mutate(rel_pos = pmin(pmax(msg_sequence / n_messages, 0), 1))

cdf_df <- ai_messages %>%
  mutate(rel_bin_id = cut(rel_pos, breaks = breaks, include.lowest = TRUE, labels = FALSE)) %>% 
  count(rel_bin_id) %>% 
  arrange(rel_bin_id) %>% 
  mutate(
    cum_ai      = cumsum(n),
    cum_ai_prop = cum_ai / sum(n),
    rel_mid     = (breaks[rel_bin_id] + breaks[rel_bin_id + 1]) / 2
  )

p_cdf <- cdf_df %>% 
  ggplot(aes(x = rel_mid, y = cum_ai_prop)) +
  geom_line(color = col_cdf, linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(
    title = "CDF of copilot use by position",
    x = "Relative position",
    y = "Cumulative fraction of\ncopilot uses"
  ) +
  theme_minimal()

# -------------------------------------------------------------------
# Combine with patchwork
# -------------------------------------------------------------------
combined_plot <- (
  p_abs + p_rel) /
  (p_rel_hist + p_cdf)

p_rel_hist / p_cdf

ggsave("/tmp/testplot.png", p_rel_hist)

combined_plot


#----------------------

library(tidyverse)

# --- existing abs_df ---
abs_df <- filtered_messages %>% 
  group_by(msg_sequence) %>% 
  summarize(
    n      = n(),
    n_ai   = sum(!is.na(ai_suggestion)),
    p_ai   = n_ai / n,
    .groups = "drop"
  )

min_n <- 30

# scale N to [0, 1] so it can share the same axis as prevalence
abs_df_plot <- abs_df %>%
  filter(n >= min_n) %>%
  mutate(
    n_rel = n / max(n, na.rm = TRUE)   # relative sample size
  )

p_abs <- abs_df_plot %>% 
  ggplot(aes(x = msg_sequence)) +
  
  # relative N as semi-transparent bars
  geom_line(aes(y = n_rel, color = "Relative denominator"), linewidth = 1) +
  
  # prevalence as line
  geom_line(aes(y = p_ai, color = "Copilot use prevalence"), linewidth = 1) +
  
  # dashed mean lines
  geom_hline(yintercept = 1, color = col_total,
    linetype = "dashed",
    linewidth = 0.6,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  
  geom_text(
    x = 30, y = 1, label = paste0(scales::comma(abs_df_plot$n[1]), " messages"), color = col_total,
    hjust = 0, vjust = -0.5,
    size = 4,
    show.legend = FALSE,
    family = "Avenir Medium"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  
  # scale_fill_manual(
  #   values = c("Relative N" = "grey70"),
  #   name   = ""
  # ) +
  scale_color_manual(
    values = c("Copilot use prevalence" = col_abs,
               "Relative denominator" = col_total),
    name   = ""
  ) +
  
  labs(
    title = "Copilot use by message order",
    x     = "Message order",
    y     = ""
  ) +
  theme_minimal(base_family = "Avenir Medium") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.75),    # <— inside the plot
    # legend.background = element_rect(fill = alpha("white", 0.7), 
    #                                  color = NA),
    legend.key.size = unit(0.6, "lines"),
    legend.text = element_text(margin = margin(t = 10, b = 10)),
    legend.direction = "vertical",
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
  )


save(file = "/tmp/copilot_use_timing_plot.RData", p_abs)

