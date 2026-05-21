library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)


## Solar Production Data

prdouction_data <- read_excel("PV_MGH_Winterthur_15min-Werte_2025.xlsx",
                   col_names = FALSE,
                   skip = 1)

names(prdouction_data) <- c("Time", "Production [kWh]")

prdouction_data$Time <- as.POSIXct(prdouction_data$Time,
                        format = "%Y-%m-%dT%H:%M:%S",
                        tz = "Europe/Zurich"
                        )

# prdouction_data$`Production [kWh]`[is.na(prdouction_data$`Production [kWh]`)] <- 0

total_production <- sum(prdouction_data$`Production [kWh]`, na.rm = TRUE)

monthly_production <- prdouction_data |> 
  mutate(Month = format(Time, "%Y-%m")) |> 
  group_by(Month) |> 
  summarise(`Production [kWh]` = sum(`Production [kWh]`, na.rm = TRUE))

monthly_production <- monthly_production |> 
  mutate(Month_date = as.Date(paste0(Month, "-01")))

weekly_production <- prdouction_data |> 
  mutate(
    Week_start = floor_date(as.Date(Time), unit = "week", week_start = 1),
    Week = format(Week_start, "%G-W%V")
  ) |> 
  group_by(Week_start, Week) |> 
  summarise(
    `Production [kWh]` = sum(`Production [kWh]`, na.rm = TRUE),
    .groups = "drop"
  )

daily_production <- prdouction_data |> 
  mutate(Date = as.Date(Time)) |> 
  group_by(Date) |> 
  summarise(
    `Production [kWh]` = sum(`Production [kWh]`, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(daily_production, aes(x = Date, y = `Production [kWh]`)) +
  geom_col() +
  labs(
    title = "Daly Production",
    x = "Date",
    y = "Production [kWh]"
  ) +
  theme_minimal()

ggplot(monthly_production, aes(x = Month_date, y = `Production [kWh]`)) +
  geom_col() +
  labs(
    title = "Monthly Production",
    x = "Month",
    y = "Production [kWh]"
  ) +
  theme_minimal()

ggplot(weekly_production, aes(x = Week_start, y = `Production [kWh]`)) +
  geom_col() +
  labs(
    title = "Weekly Production",
    x = "Week",
    y = "Production [kWh]"
  ) +
  theme_minimal()


## Total Power Usage

total_power_usage <- read_csv("Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv")