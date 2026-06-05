library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)

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

total_power_usage <- total_power_usage |> 
  select(
    -StartDateTime,
    -EndDateTime,
    -Resolution,
    -Unit,
    -MeterID,
    -SourceFile,
    -Date
    )


total_power_usage_daily <- aggregate_values(
  data = total_power_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "daily",
)

total_power_usage_weekly <- aggregate_values(
  data = total_power_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "weekly",
)

total_power_usage_monthly <- aggregate_values(
  data = total_power_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "monthly",
)

total_power_usage_yearly <- aggregate_values(
  data = total_power_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "yearly",
  # source_name = "Solar production"
)

total_power_usage_total <- total_value(
  data = total_power_usage,
  value_col = "Volume"
)

plot_values(
  total_power_usage_daily,
  title = "Daily Usage",
  y_label = "Volume",
  plot_type = "bar"
)

plot_values(
  total_power_usage_weekly,
  title = "Weekly Usage",
  y_label = "Volume",
  plot_type = "bar"
)

plot_values(
  total_power_usage_monthly,
  title = "Monthly Usage",
  y_label = "Volume",
  plot_type = "bar"
)


## Total E Mobility Usage

total_E_mob_usage <- read_csv("Verbrauch/Verbrauch_E Mobility_2025.csv")

total_E_mob_usage <- total_E_mob_usage |> 
  select(
    -StartDateTime,
    -EndDateTime,
    -Resolution,
    -Unit,
    -MeterID,
    -SourceFile,
    -Date
  )


total_E_mob_usage_daily <- aggregate_values(
  data = total_E_mob_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "daily",
)

total_E_mob_usage_weekly <- aggregate_values(
  data = total_E_mob_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "weekly",
)

total_E_mob_usage_monthly <- aggregate_values(
  data = total_E_mob_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "monthly",
)

total_E_mob_usage_yearly <- aggregate_values(
  data = total_E_mob_usage,
  time_col = "Timestamp",
  value_col = "Volume",
  period = "yearly",
  # source_name = "Solar production"
)

total_E_mob_usage_total <- total_value(
  data = total_E_mob_usage,
  value_col = "Volume"
)

plot_values(
  total_E_mob_usage_daily,
  title = "Daily E mob Usage",
  y_label = "Volume",
  plot_type = "bar"
)

plot_values(
  total_E_mob_usage_weekly,
  title = "Weekly E mob Usage",
  y_label = "Volume",
  plot_type = "bar"
)

plot_values(
  total_E_mob_usage_monthly,
  title = "Monthly E mob Usage",
  y_label = "Volume",
  plot_type = "bar"
)

## Alternative production

alt_solar_total <- read_excel(
  "PV_Production_alternative.xlsx",
  sheet = "Total"
  ) |> 
  slice(-n()) |> 
  mutate(Production = Production * 0.75) |> # 3/4 use of the roof surface
  select(-Value) |> 
  mutate(Month = factor(Month, levels = month.name)) |> 
  rename(
    Time = Month,
    Value = Production
  )

alt_solar_power <- total_value(
  data = alt_solar_total, 
  value_col = "Value"
  ) %>% 
  {. / 940}

plot_values(
  alt_solar_total,
  title = "Theoretical possible PV production",
  x_label = "Month",
  y_label = "Production [kWh]",
  plot_type = "bar"
)

## Gemeinsamer Plot

## Gemeinsamer Monatsplot

month_levels <- month.name

solar_monthly_plot <- solar_monthly |>
  mutate(
    Month = month(Time, label = FALSE),
    Month = month.name[Month],
    Type = "Production"
  ) |>
  select(Month, Value, Type)

usage_monthly_plot <- total_power_usage_monthly |>
  mutate(
    Month = month(Time, label = FALSE),
    Month = month.name[Month],
    Type = "Consumption"
  ) |>
  select(Month, Value, Type)

alt_solar_plot <- alt_solar_total |>
  mutate(
    Month = as.character(Time),
    Type = "Theoretical possible Production"
  ) |>
  select(Month, Value, Type)

monthly_comparison <- bind_rows(
  solar_monthly_plot,
  usage_monthly_plot,
  alt_solar_plot
) |>
  mutate(
    Month = factor(Month, levels = month_levels),
    Type = factor(
      Type,
      levels = c("Production", "Consumption", "Theoretical possible Production")
    )
  )

ggplot(monthly_comparison, aes(x = Month, y = Value, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Monthly Comparision",
    x = "Month",
    y = "Energy [kWh]",
    fill = "Value"
  ) +
  theme_minimal()



ggplot(monthly_comparison, aes(x = Month, y = Value, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 1000),
    breaks = c(2500,5000, 10000, 15000, 20000, 25000, 50000, 60000),
    labels = label_number(big.mark = "'")
  ) +
  coord_cartesian(ylim = c(2500, 60000)) +
  labs(
    title = "Monthly Comparison",
    x = "Month",
    y = "Energy [kWh]",
    fill = "Value"
  ) +
  theme_minimal()