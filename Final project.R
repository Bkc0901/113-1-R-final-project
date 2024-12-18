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

tidy_water_quality_summary1 <- tidy_water_quality |>
  group_by(river_name, station, Season, month) |>
  summarize(
    avg_water_temp = round(mean(water_temp, na.rm = TRUE), 1),
    avg_air_temp = round(mean(air_temp, na.rm = TRUE), 1),
    avg_pH = round(mean(pH_value, na.rm = TRUE), 1),
    avg_dissolved_oxygen = round(mean(dissolved_oxygen, na.rm = TRUE), 1),
    avg_turbidity = round(mean(turbidity, na.rm = TRUE), 1),
    .groups = "drop"  # Ensures the result is ungrouped after summarizing
  )

# Step 1: Summarize by river_name and Season, combining data across stations
river_season_summary <- tidy_water_quality |>
  group_by(river_name, Season) |>
  summarize(
    avg_water_temp = round(mean(water_temp, na.rm = TRUE), 1),
    avg_air_temp = round(mean(air_temp, na.rm = TRUE), 1),
    avg_dissolved_oxygen = round(mean(dissolved_oxygen, na.rm = TRUE), 1),
    avg_turbidity = round(mean(turbidity, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Step 2: Filter for Summer and Winter
summer_winter_summary <- river_season_summary |>
  dplyr::filter(Season %in% c("Summer", "Winter"))

# Step 3: Reshape data for comparison
comparison <- summer_winter_summary |>
  pivot_wider(
    names_from = Season,
    values_from = c(avg_water_temp, avg_air_temp, avg_dissolved_oxygen, avg_turbidity),
    names_sep = "_"
  )

# View the comparison
print(comparison)

###############

library(ggplot2)

# Step 1: Summarize by river_name and Season, combining data across stations
river_season_summary <- tidy_water_quality |>
  group_by(river_name, Season) |>
  summarize(
    avg_water_temp = round(mean(water_temp, na.rm = TRUE), 1),
    avg_air_temp = round(mean(air_temp, na.rm = TRUE), 1),
    avg_pH = round(mean(pH_value, na.rm = TRUE), 1),
    avg_dissolved_oxygen = round(mean(dissolved_oxygen, na.rm = TRUE), 1),
    avg_turbidity = round(mean(turbidity, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Step 2: Filter for Summer and Winter
summer_winter_summary <- river_season_summary |>
  dplyr::filter(Season %in% c("Summer", "Winter"))

# Step 3: Create Plots to Compare Summer and Winter

# Water Temperature Plot
ggplot(summer_winter_summary, aes(x = river_name, y = avg_water_temp, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Water Temperature: Summer vs Winter",
       x = "River Name", y = "Average Water Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Air Temperature Plot
ggplot(summer_winter_summary, aes(x = river_name, y = avg_air_temp, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Air Temperature: Summer vs Winter",
       x = "River Name", y = "Average Air Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# pH Value Plot
ggplot(summer_winter_summary, aes(x = river_name, y = avg_pH, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average pH Value: Summer vs Winter",
       x = "River Name", y = "Average pH Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dissolved Oxygen Plot
ggplot(summer_winter_summary, aes(x = river_name, y = avg_dissolved_oxygen, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Dissolved Oxygen: Summer vs Winter",
       x = "River Name", y = "Average Dissolved Oxygen (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Turbidity Plot
ggplot(summer_winter_summary, aes(x = river_name, y = avg_turbidity, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Turbidity: Summer vs Winter",
       x = "River Name", y = "Average Turbidity (NTU)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




