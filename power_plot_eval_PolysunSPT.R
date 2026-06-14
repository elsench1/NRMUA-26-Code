# power_plot_eval_PolysunSPT.R

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)

source("functions.R")

# Erstellt u.a.:
# - target_year
# - tz_local
# - production_15min          = neue theoretisch mögliche PV-Produktion
# - consumption_total_15min   = Verbrauch im 15-Minuten-Raster
#
# Achtung: Diese Datei schreibt aktuell auch Kontrolltabellen in output/.
source("prepareTableForSimulation_PolysunSPT_data.R")


# ------------------------------------------------------------
# 1) Effektiv gemessene PV-Produktion laden
# ------------------------------------------------------------

production_data <- read_excel(
  path = "PV_MGH_Winterthur_15min-Werte_2025.xlsx",
  col_names = FALSE,
  skip = 1
)

names(production_data) <- c("Time", "Production_kWh")

production_data <- production_data |>
  mutate(
    Time = as.POSIXct(
      Time,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = tz_local
    ),
    Production_kWh = as.numeric(Production_kWh)
  ) |>
  filter(!is.na(Time))


# Nur Zieljahr verwenden, damit alle drei Reihen dasselbe Jahr vergleichen
production_data_year <- production_data |>
  filter(year(Time) == target_year)

consumption_total_year <- consumption_total_15min |>
  filter(year(Time) == target_year)

theoretical_production_year <- production_15min |>
  filter(year(Time) == target_year)


# ------------------------------------------------------------
# 2) Monatsdaten für den Vergleichsplot erstellen
# ------------------------------------------------------------

month_levels <- month.name

type_levels <- c(
  "Production",
  "Consumption",
  "Theoretical possible Production"
)

solar_monthly_plot <- production_data_year |>
  mutate(
    Month = month.name[month(Time)],
    Type = "Production"
  ) |>
  group_by(Month, Type) |>
  summarise(
    Value = sum(Production_kWh, na.rm = TRUE),
    .groups = "drop"
  )

usage_monthly_plot <- consumption_total_year |>
  mutate(
    Month = month.name[month(Time)],
    Type = "Consumption"
  ) |>
  group_by(Month, Type) |>
  summarise(
    Value = sum(ConsumptionTotal_kWh, na.rm = TRUE),
    .groups = "drop"
  )

theoretical_monthly_plot <- theoretical_production_year |>
  mutate(
    Month = month.name[month(Time)],
    Type = "Theoretical possible Production"
  ) |>
  group_by(Month, Type) |>
  summarise(
    Value = sum(Production_kWh, na.rm = TRUE),
    .groups = "drop"
  )

monthly_comparison <- bind_rows(
  solar_monthly_plot,
  usage_monthly_plot,
  theoretical_monthly_plot
) |>
  mutate(
    Month = factor(Month, levels = month_levels),
    Type = factor(Type, levels = type_levels)
  ) |>
  complete(
    Month,
    Type,
    fill = list(Value = 0)
  )


# ------------------------------------------------------------
# 3) Jahressummen ausgeben
# ------------------------------------------------------------

annual_sums <- tibble(
  Metric = c(
    "Production",
    "Theoretical possible Production",
    "Consumption"
  ),
  Value_kWh = c(
    sum(production_data_year$Production_kWh, na.rm = TRUE),
    sum(theoretical_production_year$Production_kWh, na.rm = TRUE),
    sum(consumption_total_year$ConsumptionTotal_kWh, na.rm = TRUE)
  )
) |>
  mutate(
    Value_MWh = Value_kWh / 1000
  )

print(annual_sums)

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  annual_sums,
  "output/annual_sums_production_theoretical_consumption.csv"
)


# ------------------------------------------------------------
# 4) Letzten Plot aus power_plot_eval.R mit neuen Daten erstellen
# ------------------------------------------------------------

y_max <- max(monthly_comparison$Value, na.rm = TRUE) * 1.05

p_monthly_comparison <- ggplot(
  monthly_comparison,
  aes(x = Month, y = Value, fill = Type)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 1000),
    breaks = c(2500,5000, 10000, 15000, 20000, 25000, 50000, 60000),
    labels = label_number(big.mark = "'")
  ) +
  coord_cartesian(
    # ylim = c(0, y_max)
    ylim = c(2500, 60000)
  ) +
  labs(
    title = paste("Monthly Comparison", target_year),
    x = "Month",
    y = "Energy [kWh]",
    fill = "Value"
  ) +
  theme_minimal()

print(p_monthly_comparison)

ggsave(
  filename = "output/monthly_comparison_polysun_spt.png",
  plot = p_monthly_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

write_csv(
  monthly_comparison,
  "output/monthly_comparison_polysun_spt.csv"
)