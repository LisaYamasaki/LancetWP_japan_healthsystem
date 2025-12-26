# =============================================================================
# Foreign Caregivers in Japan
# Figure 1: Trends by Visa Type (2017-2024)
# Figure 2: Nationality Breakdown (2024)
# =============================================================================

library(tidyverse)
library(scales)
library(here)

# =============================================================================
# Figure 1: Trends by Visa Type (2017-2024)
# =============================================================================

# --- Data ---
df_visa <- read_csv(here("data", "foreincaregiver_visa.csv"), show_col_types = FALSE)

# Convert to long format
df_visa_long <- df_visa %>%
  pivot_longer(cols = c(EPA, TITP, SSW), 
               names_to = "visa_type", 
               values_to = "count") %>%
  mutate(
    visa_type = factor(visa_type, 
                       levels = c("EPA", "TITP", "SSW"),
                       labels = c("EPA", "Technical Intern", "Specified Skilled"))
  )

# --- Colors ---
visa_colors <- c(
  "EPA" = "darkgreen",
  "Technical Intern" = "darkcyan", 
  "Specified Skilled" = "darkorange3"
)

# --- Plot ---
fig1 <- ggplot(df_visa_long, aes(x = factor(year), y = count, fill = visa_type)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = visa_colors) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 70000, 10000)
  ) +
  labs(
    x = NULL,
    y = "Number of Workers"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 10, 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE))


# =============================================================================
# Figure 2: Nationality Breakdown (2024)
# =============================================================================

# --- Data ---
df_nationality <- read_csv(here("data", "foreincaregiver_nationality_2024.csv"), show_col_types = FALSE) %>%
  mutate(total = EPA + TITP + SSW) %>%
  arrange(desc(total)) %>%
  mutate(country = factor(country, levels = country))

# Long format
df_nat_long <- df_nationality %>%
  select(country, EPA, TITP, SSW) %>%
  pivot_longer(cols = c(EPA, TITP, SSW),
               names_to = "visa_type",
               values_to = "count") %>%
  mutate(
    visa_type = factor(visa_type,
                       levels = c("EPA", "TITP", "SSW"),
                       labels = c("EPA", "Technical Intern", "Specified Skilled"))
  )

# --- Plot (horizontal bar chart) ---
fig2 <- ggplot(df_nat_long, aes(x = country, y = count, fill = visa_type)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = visa_colors) +
  scale_x_discrete(limits = rev(levels(df_nationality$country))) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 25000, 5000)
  ) +
  labs(
    x = NULL,
    y = "Number of Workers"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 10, 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE))


# =============================================================================
# Save
# =============================================================================

ggsave(
  here("output", "foreign_caregivers_visa.png"),
  plot = fig1, width = 10, height = 6, dpi = 300
)

ggsave(
  here("output", "foreign_caregivers_nationality_2024.png"),
  plot = fig2, width = 9, height = 6, dpi = 300
)

