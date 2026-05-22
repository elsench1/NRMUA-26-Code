library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

source("functions.R")

## Solar Production Data

production_data <- read_excel(
  path = "PV_MGH_Winterthur_15min-Werte_2025.xlsx",
  col_names = FALSE,
  skip = 1
)

names(production_data) <- c("Time", "Production [kWh]")

production_data$Time <- as.POSIXct(
  production_data$Time,
  format = "%Y-%m-%dT%H:%M:%S",
  tz = "Europe/Zurich"
)


solar_daily <- aggregate_values(
  data = production_data,
  time_col = "Time",
  value_col = "Production [kWh]",
  period = "daily",
  source_name = "Solar production"
)

solar_weekly <- aggregate_values(
  data = production_data,
  time_col = "Time",
  value_col = "Production [kWh]",
  period = "weekly",
  source_name = "Solar production"
)

solar_monthly <- aggregate_values(
  data = production_data,
  time_col = "Time",
  value_col = "Production [kWh]",
  period = "monthly",
  source_name = "Solar production"
)

solar_yearly <- aggregate_values(
  data = production_data,
  time_col = "Time",
  value_col = "Production [kWh]",
  period = "yearly",
  source_name = "Solar production"
)

solar_total <- total_value(
  data = production_data,
  value_col = "Production [kWh]"
)

plot_values(
  solar_daily,
  title = "Daily Production",
  y_label = "Production [kWh]",
  plot_type = "bar"
)

plot_values(
  solar_weekly,
  title = "Weekly Production",
  y_label = "Production [kWh]",
  plot_type = "bar"
)

plot_values(
  solar_monthly,
  title = "Monthly Production",
  y_label = "Production [kWh]",
  plot_type = "bar"
)

## Total Power Usage

total_power_usage <- read_csv("Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv")

total_power_usage$Timestamp <- as.POSIXct(total_power_usage$Timestamp,
                                   format = "%Y-%m-%dT%H:%M:%S",
                                   tz = "Europe/Zurich"
)






