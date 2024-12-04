# Importing the CSV file
tidy_energy_data <- readr::read_csv("經濟部能源署_電力消費年資料(2018年後).csv")

# View the first few rows of the data
print(tidy_energy_data)
head(tidy_energy_data)

# Select, retain the `日期(年)` column, and rename the others
`Electricity_Consumption_by_Sector` <- tidy_energy_data %>%
  select(
    `日期(年)`,
    `電力消費_能源部門自用` = `電力消費_能源部門自用`,
    `電力消費_工業部門_合計` = `電力消費_工業部門_合計`,
    `電力消費_運輸部門_合計` = `電力消費_運輸部門_合計`,
    `電力消費_服務業部門_合計` = `電力消費_服務業部門_合計`,
    `電力消費_住宅部門` = `電力消費_住宅部門`
  ) %>%
  rename(
    Year = `日期(年)`,
    Energy_Self_Use = `電力消費_能源部門自用`,
    Industrial_Total = `電力消費_工業部門_合計`,
    Transport_Total = `電力消費_運輸部門_合計`,
    Services_Total = `電力消費_服務業部門_合計`,
    Residential = `電力消費_住宅部門`
  ) %>%
  as.data.frame()

# View the result
print(head(`Electricity_Consumption_by_Sector`))

