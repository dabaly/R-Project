# Load packages
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Read maize data
maize_raw <- read_csv(
  "maize-area-yield.csv",
  show_col_types = FALSE
)

# Standardise column names and data types
maize <- maize_raw |>
  transmute(
    year = as.integer(Year),
    maize_area_million_ha =
      as.numeric(`Area harvested (million ha) (million ha / t per ha)`),
    maize_yield_t_ha =
      as.numeric(`Yield (t/ha) (million ha / t per ha)`)
  )

# Convert nested CCKP JSON into year/value rows
read_cckp_series <- function(file, value_name) {
  x <- fromJSON(file, simplifyVector = FALSE)
  
  series <- x$data$KEN
  
  tibble(
    date = names(series),
    value = as.numeric(unlist(series))
  ) |>
    mutate(year = as.integer(str_sub(date, 1, 4))) |>
    group_by(year) |>
    summarise(value = mean(value), .groups = "drop") |>
    rename(!!value_name := value)
}

# Read rainfall and temperature
rainfall <- read_cckp_series(
  "kenya_annual_rainfall.json",
  "annual_rainfall_mm"
)

temperature <- read_cckp_series(
  "kenya_annual_temperature.json",
  "annual_mean_temp_c"
)

# Join all three datasets by year
kenya_clean <- maize |>
  inner_join(rainfall, by = "year") |>
  inner_join(temperature, by = "year") |>
  arrange(year)

# Check the data
cat("Rows:", nrow(kenya_clean), "\n")
cat("Year range:", min(kenya_clean$year), "to",
    max(kenya_clean$year), "\n\n")

cat("Missing values by column:\n")
print(colSums(is.na(kenya_clean)))

cat("\nDuplicate years:\n")
print(kenya_clean |>
        count(year) |>
        filter(n > 1))

cat("\nMerged data:\n")
print(kenya_clean)

# Basic range checks—review any rows printed
cat("\nNon-positive or unusually high yield values:\n")
print(kenya_clean |>
        filter(maize_yield_t_ha <= 0 | maize_yield_t_ha > 10))

cat("\nNegative rainfall values:\n")
print(kenya_clean |>
        filter(annual_rainfall_mm < 0))

cat("\nTemperature outside a broad review range (10–35°C):\n")
print(kenya_clean |>
        filter(annual_mean_temp_c < 10 | annual_mean_temp_c > 35))

# Save the cleaned dataset in the main project folder
write_csv(kenya_clean, "kenya_maize_climate_clean.csv")