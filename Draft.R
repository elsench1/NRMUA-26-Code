library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)

source("functions.R")



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

producedPower <- total_value(production_data, "Production [kWh]")



