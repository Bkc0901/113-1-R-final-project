water_quality <- readr::read_csv("113河川水質檢測結果(Big5編碼).csv", 
                                 locale = readr::locale(encoding = "BIG5"))
 glimpse(water_quality)

 #清理資料，翻譯成英文
 tidy_water_quality <- water_quality |>
   dplyr::select(
     `月份`,
     `序號`,
     `河川名稱`,
     `監測站`,
     `水溫數值（℃）`,
     `氣溫數值（℃）`,
     `pH數值`,
     `溶氧量數值（mg/l）`,
     `生化需氧量數值（mg/l）`,
     `氨氮數值（mg/l）`,
     `懸浮固體數值（mg/l）`,
     `濁度數值（NTU）`
   ) |>
   dplyr::rename(
     month = `月份`,
     serial_number = `序號`,
     river_name = `河川名稱`,
     monitoring_station = `監測站`,
     water_temperature = `水溫數值（℃）`,
     air_temperature = `氣溫數值（℃）`,
     pH = `pH數值`,
     dissolved_oxygen = `溶氧量數值（mg/l）`,
     biochemical_oxygen_demand = `生化需氧量數值（mg/l）`,
     ammonia_nitrogen = `氨氮數值（mg/l）`,
     suspended_solids = `懸浮固體數值（mg/l）`,
     turbidity = `濁度數值（NTU）`
   )

 glimpse(tidy_water_quality) 

 tidy_water_quality <- tidy_water_quality |>
   # Parse river_name and monitoring_station as factors
   dplyr::mutate(
     river_name = as.factor(river_name),
     monitoring_station = as.factor(monitoring_station),
     
     # Parse month and serial_number as ordered factors
     month = factor(month, levels = unique(month), ordered = TRUE),
     serial_number = factor(serial_number, levels = unique(serial_number), ordered = TRUE)
   ) |>
   
   # Clean ammonia_nitrogen column
   dplyr::mutate(
     ammonia_nitrogen = stringr::str_replace(ammonia_nitrogen, "ND<0.08", "0.04"), # Replace ND<0.08 with 0.04
     ammonia_nitrogen = dplyr::if_else(
       stringr::str_detect(ammonia_nitrogen, "^\\d+(\\.\\d+)?$"),
       as.numeric(ammonia_nitrogen), # Convert numeric strings to numeric
       NA_real_ # Replace non-numeric values with NA
     )
   )
 

#整理月別資料至季別
 tidy_water_quality <- tidy_water_quality |>
   dplyr::mutate(
     season = dplyr::case_when(
       month %in% 3:5 ~ "Spring",
       month %in% 6:8 ~ "Summer",
       month %in% 9:11 ~ "Autumn",
       month %in% c(12, 1, 2) ~ "Winter",
       TRUE ~ NA_character_
     ),
     season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))
   ) |>
   dplyr::relocate(season, .after = month)
 
 #計算RPI指數，辨別河川污染級別
 tidy_water_quality <- tidy_water_quality |>
   # Assign points based on conditions
   dplyr::mutate(
     do_points = dplyr::case_when(
       dissolved_oxygen >= 6.5 ~ 1,
       dissolved_oxygen < 6.5 & dissolved_oxygen >= 4.6 ~ 3,
       dissolved_oxygen < 4.6 & dissolved_oxygen >= 2.0 ~ 6,
       dissolved_oxygen < 2.0 ~ 10,
       TRUE ~ NA_real_
     ),
     bod_points = dplyr::case_when(
       biochemical_oxygen_demand <= 3.0 ~ 1,
       biochemical_oxygen_demand > 3.0 & biochemical_oxygen_demand <= 4.9 ~ 3,
       biochemical_oxygen_demand > 4.9 & biochemical_oxygen_demand <= 15.0 ~ 6,
       biochemical_oxygen_demand > 15.0 ~ 10,
       TRUE ~ NA_real_
     ),
     ss_points = dplyr::case_when(
       suspended_solids <= 20.0 ~ 1,
       suspended_solids > 20.0 & suspended_solids <= 49.9 ~ 3,
       suspended_solids > 49.9 & suspended_solids <= 100.0 ~ 6,
       suspended_solids > 100.0 ~ 10,
       TRUE ~ NA_real_
     ),
     nh3_points = dplyr::case_when(
       ammonia_nitrogen <= 0.50 ~ 1,
       ammonia_nitrogen > 0.50 & ammonia_nitrogen <= 0.99 ~ 3,
       ammonia_nitrogen > 0.99 & ammonia_nitrogen <= 3.00 ~ 6,
       ammonia_nitrogen > 3.00 ~ 10,
       TRUE ~ NA_real_
     ),
     # Calculate average RPI
     rpi = round((do_points + bod_points + ss_points + nh3_points) / 4, 2),
     # Determine pollution level based on RPI
     pollution_level = dplyr::case_when(
       rpi <= 2.0 ~ "No Pollution or Slight Pollution",
       rpi > 2.0 & rpi <= 3.0 ~ "Mild Pollution",
       rpi > 3.0 & rpi <= 6.0 ~ "Moderate Pollution",
       rpi > 6.0 ~ "Severe Pollution",
       TRUE ~ NA_character_
     )
   )
 
glimpse(tidy_water_quality)

#彙整各河川和觀測站季別資料，計算PRI
# Summarize data
summary_data <- tidy_water_quality |>
  dplyr::group_by(river_name, monitoring_station, season) |>
  dplyr::summarise(
    water_temperature = round(mean(water_temperature, na.rm = TRUE), 2),
    air_temperature = round(mean(air_temperature, na.rm = TRUE), 2),
    pH = round(mean(pH, na.rm = TRUE), 2),
    dissolved_oxygen = round(mean(dissolved_oxygen, na.rm = TRUE), 2),
    biochemical_oxygen_demand = round(mean(biochemical_oxygen_demand, na.rm = TRUE), 2),
    suspended_solids = round(mean(suspended_solids, na.rm = TRUE), 2),
    ammonia_nitrogen = round(mean(ammonia_nitrogen, na.rm = TRUE), 2),
    turbidity = round(mean(turbidity, na.rm = TRUE), 2)
  ) |>
  dplyr::ungroup()

# Calculate RPI and categorize pollution level
summary_data <- summary_data |>
  dplyr::mutate(
    do_points = dplyr::case_when(
      dissolved_oxygen >= 6.5 ~ 1,
      dissolved_oxygen < 6.5 & dissolved_oxygen >= 4.6 ~ 3,
      dissolved_oxygen < 4.6 & dissolved_oxygen >= 2.0 ~ 6,
      dissolved_oxygen < 2.0 ~ 10,
      TRUE ~ NA_real_
    ),
    bod_points = dplyr::case_when(
      biochemical_oxygen_demand <= 3.0 ~ 1,
      biochemical_oxygen_demand > 3.0 & biochemical_oxygen_demand <= 4.9 ~ 3,
      biochemical_oxygen_demand > 4.9 & biochemical_oxygen_demand <= 15.0 ~ 6,
      biochemical_oxygen_demand > 15.0 ~ 10,
      TRUE ~ NA_real_
    ),
    ss_points = dplyr::case_when(
      suspended_solids <= 20.0 ~ 1,
      suspended_solids > 20.0 & suspended_solids <= 49.9 ~ 3,
      suspended_solids > 49.9 & suspended_solids <= 100.0 ~ 6,
      suspended_solids > 100.0 ~ 10,
      TRUE ~ NA_real_
    ),
    nh3_points = dplyr::case_when(
      ammonia_nitrogen <= 0.50 ~ 1,
      ammonia_nitrogen > 0.50 & ammonia_nitrogen <= 0.99 ~ 3,
      ammonia_nitrogen > 0.99 & ammonia_nitrogen <= 3.00 ~ 6,
      ammonia_nitrogen > 3.00 ~ 10,
      TRUE ~ NA_real_
    ),
    # Calculate RPI
    rpi = round((do_points + bod_points + ss_points + nh3_points) / 4, 2),
    
    # Categorize pollution level
    pollution_level = dplyr::case_when(
      rpi <= 2.0 ~ "No Pollution or Slight Pollution",
      rpi > 2.0 & rpi <= 3.0 ~ "Mild Pollution",
      rpi > 3.0 & rpi <= 6.0 ~ "Moderate Pollution",
      rpi > 6.0 ~ "Severe Pollution",
      TRUE ~ NA_character_
    )
  ) |>
  # Parse pollution_level to ordered factor
  dplyr::mutate(
    pollution_level = factor(
      pollution_level,
      levels = c(
        "No Pollution or Slight Pollution",
        "Mild Pollution",
        "Moderate Pollution",
        "Severe Pollution"
      ),
      ordered = TRUE
    )
  )

# Group and compare results across monitoring stations within the same river and season
comparison_results <- summary_data |>
  dplyr::group_by(river_name, season) |>
  dplyr::summarise(
    monitoring_stations = paste(unique(monitoring_station), collapse = ", "),
    rpi_range = paste0(
      "Min: ", round(min(rpi, na.rm = TRUE), 2),
      ", Max: ", round(max(rpi, na.rm = TRUE), 2)
    ),
    pollution_levels = paste(unique(as.character(pollution_level)), collapse = ", ")
  ) |>
  dplyr::ungroup()

# Count the number of samples in each pollution level category
pollution_level_distribution <- summary_data |>
  dplyr::count(pollution_level)

# Stacked bar chart
library(ggplot2)

stacked_bar_chart <- ggplot(pollution_level_distribution, aes(x = "", y = n, fill = pollution_level)) +
  geom_bar(stat = "identity", width = 1) +
  labs(
    title = "Distribution of Pollution Levels (Stacked Bar Chart)",
    x = "Pollution Levels",
    y = "Number of Samples",
    fill = "Pollution Level"
  ) +
  theme_minimal()

# Pie chart
pie_chart <- ggplot(pollution_level_distribution, aes(x = "", y = n, fill = pollution_level)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Pollution Levels (Pie Chart)",
    fill = "Pollution Level"
  ) +
  theme_void()

# Display the charts
print(stacked_bar_chart)
print(pie_chart)

library(ggplot2)

#根據不同季節下同條河川不同監測站做RPI的趨勢圖
# Create trend charts for seasonal values of each river
trend_chart <- ggplot(summary_data, aes(x = season, y = rpi, color = monitoring_station, group = monitoring_station)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend of RPI for Each River",
    x = "Season",
    y = "River Pollution Index (RPI)",
    color = "Monitoring Station"
  ) +
  facet_wrap(~ river_name, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Display the chart
print(trend_chart)

library(ggplot2)

# Trend chart for water temperature
water_temperature_trend <- ggplot(summary_data, aes(x = season, y = water_temperature, color = monitoring_station, group = monitoring_station)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend of Water Temperature for Each River",
    x = "Season",
    y = "Water Temperature (°C)",
    color = "Monitoring Station"
  ) +
  facet_wrap(~ river_name, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Trend chart for turbidity
turbidity_trend <- ggplot(summary_data, aes(x = season, y = turbidity, color = monitoring_station, group = monitoring_station)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend of Turbidity for Each River",
    x = "Season",
    y = "Turbidity (NTU)",
    color = "Monitoring Station"
  ) +
  facet_wrap(~ river_name, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Display the charts
print(water_temperature_trend)
print(turbidity_trend)

library(ggplot2)

# Trend chart for RPI across seasons
rpi_trend_chart <- ggplot(summary_data, aes(x = season, y = rpi, color = interaction(river_name, monitoring_station), group = interaction(river_name, monitoring_station))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend of RPI Across All Rivers and Monitoring Stations",
    x = "Season",
    y = "River Pollution Index (RPI)",
    color = "River & Station"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Display the chart
print(rpi_trend_chart)

#水溫和含氧量的簡單線性回歸
# Perform simple linear regression: DO ~ water temperature
do_temperature_model <- lm(dissolved_oxygen ~ water_temperature, data = tidy_water_quality)

# Summary of the regression model
summary(do_temperature_model)

# Visualize the relationship with a scatterplot and regression line
library(ggplot2)

do_temperature_plot <- ggplot(tidy_water_quality, aes(x = water_temperature, y = dissolved_oxygen)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Relationship Between Water Temperature and Dissolved Oxygen",
    x = "Water Temperature (°C)",
    y = "Dissolved Oxygen (mg/L)"
  ) +
  theme_minimal()

# Display the plot
print(do_temperature_plot)
summary(do_temperature_model)

library(ggplot2)
library(lmtest)

#檢驗殘差
# Model residuals
residuals <- resid(do_temperature_model)
fitted_values <- fitted(do_temperature_model)

#檢測殘差的常態性
# Check normality of residuals
# Histogram 殘差直方圖
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.2, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

#檢驗常態性
# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")

#檢測常態性
# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

#檢測殘差和
# Check zero mean of residuals
mean_residual <- mean(residuals)
cat("Mean of residuals:", mean_residual, "\n")

install.packages("lmtest")

# Check homoscedasticity檢測異質變異
# Residuals vs. Fitted Values
ggplot(data.frame(fitted_values, residuals), aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Breusch-Pagan test for homoscedasticity
bptest_result <- bptest(do_temperature_model)
print(bptest_result)


#利用檢測標準化殘差、高槓桿、庫克距離離群值
# Calculate diagnostic measures
influence_measures <- data.frame(
  standardized_residuals = rstandard(do_temperature_model),
  leverage = hatvalues(do_temperature_model),
  cooks_distance = cooks.distance(do_temperature_model)
)

# Add observation numbers for reference
influence_measures <- influence_measures |>
  dplyr::mutate(observation = row_number())

# Identify outliers (standardized residuals > |3|)
outliers <- influence_measures |>
  dplyr::filter(abs(standardized_residuals) > 3)

# Identify high-leverage points (leverage > 2 * (p/n))
p <- length(coef(do_temperature_model))  # Number of predictors + 1
n <- nrow(tidy_water_quality)           # Number of observations
leverage_threshold <- 2 * (p / n)

high_leverage_points <- influence_measures |>
  dplyr::filter(leverage > leverage_threshold)

# Identify influential points (Cook's Distance > 1)
influential_points <- influence_measures |>
  dplyr::filter(cooks_distance > 1)

# Display results
list(
  outliers = outliers,
  high_leverage_points = high_leverage_points,
  influential_points = influential_points
)

# Residuals vs. Fitted
plot(do_temperature_model, which = 1)

# Cook's Distance
plot(do_temperature_model, which = 4)

# Leverage vs. Residuals
plot(do_temperature_model, which = 5)

#變數變異
# Apply log transformation to DO
do_temperature_model_log <- lm(log(dissolved_oxygen) ~ water_temperature, data = tidy_water_quality)

# Check model summary
summary(do_temperature_model_log)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(do_temperature_model_log)

# Apply log transformation to water temperature
do_temperature_model_temp_log <- lm(dissolved_oxygen ~ log(water_temperature), data = tidy_water_quality)

# Check model summary
summary(do_temperature_model_temp_log)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(do_temperature_model_temp_log)


