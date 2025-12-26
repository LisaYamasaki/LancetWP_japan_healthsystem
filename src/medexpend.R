# =============================================================================
# Decomposition of Healthcare and Long-term Care Expenditure Growth
# Comparison of Two Scenarios (Baseline Scenario vs Growth Transition)
# =============================================================================

library(tidyverse)
library(here)
library(patchwork)

# Load data
df <- read.csv(here("data", "medexpend_projection.csv"))

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
      scenario == "past_projection" ~ "Baseline Scenario",
      scenario == "growth_transition" ~ "Growth Scenario"
    ),
    scenario_label = factor(scenario_label, levels = c("Baseline Scenario", "Growth Scenario"))
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
      factor == "unit_price" ~ "Price/Wage Effect",
      factor == "other_factors" ~ "Technology/Other"
    ),
    factor_label = factor(factor_label, levels = c("Technology/Other", "Price/Wage Effect", "Population Aging"))
  )

# Reshape for line chart legend
df_lines <- df %>%
  pivot_longer(
    cols = c(expenditure_growth, economic_growth),
    names_to = "line_type",
    values_to = "line_value"
  ) %>%
  mutate(
    line_label = case_when(
      line_type == "expenditure_growth" ~ "Expenditure Growth",
      line_type == "economic_growth" ~ "Nominal GDP Growth"
    ),
    line_label = factor(line_label, levels = c("Expenditure Growth", "Nominal GDP Growth"))
  )

# Function to create plot for each category
create_plot <- function(cat, y_max, show_legend = FALSE) {
  
  df_cat <- df %>% filter(category == cat)
  df_long_cat <- df_long %>% filter(category == cat)
  df_lines_cat <- df_lines %>% filter(category == cat)
  
  p <- ggplot() +
    # Stacked bar chart
    geom_col(
      data = df_long_cat,
      aes(x = year_label, y = value, fill = factor_label),
      width = 0.6,
      color = "white",
      linewidth = 0.3
    ) +
    # Zero line
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    # Line charts with legend
    geom_line(
      data = df_lines_cat,
      aes(x = year_label, y = line_value, group = line_type, 
          color = line_label, linetype = line_label),
      linewidth = 0.8
    ) +
    geom_point(
      data = df_lines_cat,
      aes(x = year_label, y = line_value, color = line_label, shape = line_label),
      size = 2
    ) +
    # Facet by scenario
    facet_wrap(~ scenario_label, nrow = 1) +
    # Bar colors
    scale_fill_manual(
      values = c(
        "Population Aging" = "darkgreen",
        "Price/Wage Effect" = "gray80",
        "Technology/Other" = "#e67e22"
      ),
      name = NULL
    ) +
    # Line colors and styles
    scale_color_manual(
      values = c(
        "Expenditure Growth" = "#2c3e50",
        "Nominal GDP Growth" = "#c0392b"
      ),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c(
        "Expenditure Growth" = "solid",
        "Nominal GDP Growth" = "dashed"
      ),
      name = NULL
    ) +
    scale_shape_manual(
      values = c(
        "Expenditure Growth" = 16,
        "Nominal GDP Growth" = 15
      ),
      name = NULL
    ) +
    # Axis settings
    scale_y_continuous(limits = c(-1, y_max), breaks = seq(-1, floor(y_max), 1)) +
    labs(
      x = NULL,
      y = "Growth Rate (%)"
    ) +
    # Theme
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12, hjust = 0)
    )
  
  if (show_legend) {
    p <- p + theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
      guides(
        fill = guide_legend(order = 1, nrow = 1),
        color = guide_legend(order = 2, nrow = 1),
        linetype = guide_legend(order = 2, nrow = 1),
        shape = guide_legend(order = 2, nrow = 1)
      )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

# Create individual plots
p_healthcare <- create_plot("medicine", 4, show_legend = FALSE) +
  ggtitle("(A) Healthcare Expenditure")

p_longterm <- create_plot("nursing", 5.5, show_legend = TRUE) +
  ggtitle("(B) Long-term Care Expenditure")

# Combine plots
p_combined <- p_healthcare / p_longterm +
  plot_layout(heights = c(1, 1.15))

# Save outputs
ggsave(here("output", "healthcare_drivers.png"), p_combined, width = 10, height = 10, dpi = 300)
ggsave(here("output", "healthcare_drivers.pdf"), p_combined, width = 10, height = 10)