library(readr)
library(dplyr)
library(ggplot2)

# Create a folder for graphs if it doesn't exist
dir.create("figures", showWarnings = FALSE)

# Read the cleaned data
kenya <- read_csv("kenya_maize_climate_clean.csv",
                  show_col_types = FALSE)

# Check data and summary statistics
View(kenya)
summary(kenya)

# Figure 1: maize yield over time
p1 <- ggplot(kenya, aes(year, maize_yield_t_ha)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Maize Yield in Kenya, 2010–2024",
    x = "Year",
    y = "Maize yield (tonnes per hectare)"
  ) +
  theme_minimal()

ggsave("figures/fig1_maize_yield.png", p1,
       width = 8, height = 5, dpi = 300)

# Figure 2: annual rainfall over time
p2 <- ggplot(kenya, aes(year, annual_rainfall_mm)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(
    title = "Annual Rainfall in Kenya, 2010–2024",
    x = "Year",
    y = "Rainfall (mm)"
  ) +
  theme_minimal()

ggsave("figures/fig2_rainfall.png", p2,
       width = 8, height = 5, dpi = 300)

# Figure 3: annual mean temperature over time
p3 <- ggplot(kenya, aes(year, annual_mean_temp_c)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "firebrick") +
  labs(
    title = "Annual Mean Temperature in Kenya, 2010–2024",
    x = "Year",
    y = "Mean temperature (°C)"
  ) +
  theme_minimal()

ggsave("figures/fig3_temperature.png", p3,
       width = 8, height = 5, dpi = 300)

# Figure 4: rainfall and maize yield
p4 <- ggplot(kenya, aes(annual_rainfall_mm, maize_yield_t_ha)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Rainfall and Maize Yield in Kenya",
    x = "Annual rainfall (mm)",
    y = "Maize yield (tonnes per hectare)"
  ) +
  theme_minimal()

ggsave("figures/fig4_rainfall_vs_yield.png", p4,
       width = 8, height = 5, dpi = 300)

# Figure 5: temperature and maize yield
p5 <- ggplot(kenya, aes(annual_mean_temp_c, maize_yield_t_ha)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Temperature and Maize Yield in Kenya",
    x = "Annual mean temperature (°C)",
    y = "Maize yield (tonnes per hectare)"
  ) +
  theme_minimal()

ggsave("figures/fig5_temperature_vs_yield.png", p5,
       width = 8, height = 5, dpi = 300)

# Correlations
cor.test(kenya$annual_rainfall_mm, kenya$maize_yield_t_ha)
cor.test(kenya$annual_mean_temp_c, kenya$maize_yield_t_ha)

# Multiple linear regression
model <- lm(
  maize_yield_t_ha ~ annual_rainfall_mm + annual_mean_temp_c,
  data = kenya
)

summary(model)