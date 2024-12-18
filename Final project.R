# Load required packages
library(googlesheets4)

# Read the Google Sheet
tidy_water <- read_sheet("https://docs.google.com/spreadsheets/d/1uWDulSn-N-Hc1CEERQ90AKQ3gJRhe1ac3OslTfgPwKk/edit?usp=sharing")

# Select and rename columns
tidy_water_quality <- tidy_water %>%
  select(
    month = `月份`,
    id = `序號`,
    river_name = `河川名稱`,
    station = `監測站`,
    water_temp = `水溫數值（℃）`,
    air_temp = `氣溫數值（℃）`,
    pH_value = `pH數值`,
    dissolved_oxygen = `溶氧量數值（mg/l）`,
    turbidity = `濁度數值（NTU）`
  )
glimpse(tidy_water_quality)

tidy_water_quality <- tidy_water_quality |>
  mutate(
    river_name = factor(river_name),
    station = factor(station),
    month = factor(month, ordered = TRUE),
    id = factor(id, ordered = TRUE)
  )
summary(tidy_water_quality)

tidy_water_quality <- tidy_water_quality |>
  mutate(
    month = case_when(
      month %in% c(3, 4, 5) ~ "3-5",
      month %in% c(6, 7, 8) ~ "6-8",
      month %in% c(9, 10, 11) ~ "9-11",
      month %in% c(12, 1, 2) ~ "12-1"
    ),
    month = factor(month, levels = c("3-5", "6-8", "9-11", "12-1"), ordered = TRUE)
  )

tidy_water_quality <- tidy_water_quality |>
  mutate(
    Season = case_when(
      month == "3-5" ~ "Spring",
      month == "6-8" ~ "Summer",
      month == "9-11" ~ "Autumn",
      month == "12-1" ~ "Winter"
    ),
    Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
  ) |>
  relocate(Season, .after = month)

summary(tidy_water_quality)

tidy_water_quality_summary <- tidy_water_quality |>
  group_by(river_name, station, Season, month) |>
  summarize(
    avg_water_temp = round(mean(water_temp, na.rm = TRUE), 1),
    avg_air_temp = round(mean(air_temp, na.rm = TRUE), 1),
    avg_pH = round(mean(pH_value, na.rm = TRUE), 1),
    avg_dissolved_oxygen = round(mean(dissolved_oxygen, na.rm = TRUE), 1),
    avg_turbidity = round(mean(turbidity, na.rm = TRUE), 1),
    .groups = "drop"  # Ensures the result is ungrouped after summarizing
  )

