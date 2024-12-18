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
