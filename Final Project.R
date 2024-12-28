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
    ammonia_nitrogen = round(mean(ammonia_nitrogen, na.rm = TRUE), 2)
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





