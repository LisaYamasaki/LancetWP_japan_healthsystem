# =============================================================================
# Decomposition of Healthcare and Long-term Care Expenditure Growth
# Comparison of Two Scenarios (Past Projection vs Growth Transition)
# =============================================================================

library(tidyverse)

# Load data
df <- read.csv("medexpend_projection.csv")

# Create labels
df <- df %>%
  mutate(
    year_label = case_when(
      year == 2025 ~ "2025-30",
      year == 2031 ~ "2031-40",
      year == 2041 ~ "2041-50",
      year == 2051 ~ "2051-60"
    ),
    year_label = factor(year_label, levels = c("2025-30", "2031-40", "2041-50", "2051-60")),
    scenario_label = case_when(
      scenario == "past_projection" ~ "Past Projection",
      scenario == "growth_transition" ~ "Growth Transition"
    ),
    scenario_label = factor(scenario_label, levels = c("Past Projection", "Growth Transition")),
    category_label = case_when(
      category == "medicine" ~ "Healthcare",
      category == "nursing" ~ "Long-term Care"
    ),
    category_label = factor(category_label, levels = c("Healthcare", "Long-term Care"))
  )

# Filter to two scenarios only
df <- df %>% filter(scenario %in% c("past_projection", "growth_transition"))

# Reshape to long format for stacked bar chart
df_long <- df %>%
  pivot_longer(
    cols = c(ageing, unit_price, other_factors),
    names_to = "factor",
    values_to = "value"
  ) %>%
  mutate(
    factor_label = case_when(
      factor == "ageing" ~ "Population Aging",
      factor == "unit_price" ~ "Unit Price",
      factor == "other_factors" ~ "Technology/Other"
    ),
    factor_label = factor(factor_label, levels = c("Technology/Other", "Unit Price", "Population Aging"))
  )

# Create plot
p <- ggplot() +
  # Stacked bar chart
  geom_col(
    data = df_long,
    aes(x = year_label, y = value, fill = factor_label),
    width = 0.6,
    color = "white",
    linewidth = 0.3
  ) +
  # Nominal GDP growth rate (dashed line)
  geom_line(
    data = df,
    aes(x = year_label, y = economic_growth, group = 1),
    color = "#27ae60",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    data = df,
    aes(x = year_label, y = economic_growth),
    color = "#27ae60",
    size = 2
  ) +
  # Expenditure growth rate (solid line)
  geom_line(
    data = df,
    aes(x = year_label, y = expenditure_growth, group = 1),
    color = "#2c3e50",
    linewidth = 1
  ) +
  geom_point(
    data = df,
    aes(x = year_label, y = expenditure_growth),
    color = "#2c3e50",
    size = 3
  ) +
  # Facet by category and scenario
  facet_grid(category_label ~ scenario_label) +
  # Colors
  scale_fill_manual(
    values = c(
      "Population Aging" = "#1a5276",
      "Unit Price" = "#aab7b8",
      "Technology/Other" = "#d4ac0d"
    ),
    name = NULL
  ) +
  # Axis settings
  scale_y_continuous(limits = c(-1, 6), breaks = seq(-1, 6, 1)) +
  labs(
    x = NULL,
    y = "Growth Rate (%)",
    title = "Decomposition of Healthcare and Long-term Care Expenditure Growth",
    subtitle = "Healthcare: driven by technology | Long-term Care: driven by aging"
  ) +
  # Theme
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Save outputs
ggsave("healthcare_drivers.png", p, width = 10, height = 8, dpi = 300)
ggsave("healthcare_drivers.pdf", p, width = 10, height = 8)