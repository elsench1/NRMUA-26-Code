# PowerSimulation_Autarky_Evaluation.R

library(readr)
library(dplyr)
library(lubridate)

source("PowerSimulationFunction.R")


# ------------------------------------------------------------
# 0) Einstellungen
# ------------------------------------------------------------

input_file <- "output/energy_15min_prepared.csv"

output_summary_file <- "output/autarky_with_without_battery_summary.csv"
output_detail_file <- "output/simulation_with_battery_detail.csv"

charge_efficiency <- 0.95
discharge_efficiency <- 0.95
c_rate_max <- 0.5

battery_capacity_kWh <- 400
zero_grid_import_tolerance_kWh <- 1e-6

consumption_col <- "ConsumptionTotal_kWh"

simulation_year <- 2025

warmup_source_start <- ymd_hms(
  "2025-10-01 00:00:00",
  tz = "Europe/Zurich"
)

warmup_source_end <- ymd_hms(
  "2026-01-01 00:00:00",
  tz = "Europe/Zurich"
)


# ------------------------------------------------------------
# 1) Daten laden und vorbereiten
# ------------------------------------------------------------

df <- load_energy_data(
  input_file = input_file,
  consumption_col = consumption_col
)

df_work_base <- prepare_simulation_data(
  df = df,
  simulation_year = simulation_year,
  warmup_source_start = warmup_source_start,
  warmup_source_end = warmup_source_end
)

df_without_warmup <- df_work_base |>
  filter(!IsWarmup) |>
  arrange(Time_CH)


# ------------------------------------------------------------
# 2) Kennzahlen ohne Batterie berechnen
# ------------------------------------------------------------

df_without_battery_metrics <- df_without_warmup |>
  mutate(
    DirectSelfUsed_kWh = pmin(
      Production_kWh,
      .data[[consumption_col]]
    ),
    
    GridImportWithoutBattery_kWh = pmax(
      .data[[consumption_col]] - Production_kWh,
      0
    ),
    
    FeedInWithoutBattery_kWh = pmax(
      Production_kWh - .data[[consumption_col]],
      0
    )
  )

metrics_without_battery <- df_without_battery_metrics |>
  summarise(
    Scenario = "Without battery",
    BatteryCapacity_kWh = 0,
    
    Production_kWh = sum(Production_kWh, na.rm = TRUE),
    Consumption_kWh = sum(.data[[consumption_col]], na.rm = TRUE),
    
    SelfUsedElectricity_kWh = sum(DirectSelfUsed_kWh, na.rm = TRUE),
    GridImport_kWh = sum(GridImportWithoutBattery_kWh, na.rm = TRUE),
    FeedIn_kWh = sum(FeedInWithoutBattery_kWh, na.rm = TRUE),
    
    BatteryChargeFromPV_kWh = 0,
    BatteryDischargeToLoad_kWh = 0
  ) |>
  mutate(
    Autarky_percent = if_else(
      Consumption_kWh > 0,
      SelfUsedElectricity_kWh / Consumption_kWh * 100,
      NA_real_
    ),
    
    SelfConsumptionRate_percent = if_else(
      Production_kWh > 0,
      SelfUsedElectricity_kWh / Production_kWh * 100,
      NA_real_
    ),
    
    PVUsedOnSiteIncludingBatteryCharging_kWh = SelfUsedElectricity_kWh
  )
# ------------------------------------------------------------
# 3) Simulation mit Batterie ausführen
# ------------------------------------------------------------

simulation_with_battery <- simulate_capacity(
  df_work_base = df_work_base,
  capacity_kWh = battery_capacity_kWh,
  charge_efficiency = charge_efficiency,
  discharge_efficiency = discharge_efficiency,
  c_rate_max = c_rate_max,
  consumption_col = consumption_col,
  zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh
)

df_with_battery <- simulation_with_battery$df_result


# ------------------------------------------------------------
# 4) Kennzahlen mit Batterie berechnen
# ------------------------------------------------------------

metrics_with_battery <- df_with_battery |>
  summarise(
    Scenario = "With battery",
    BatteryCapacity_kWh = battery_capacity_kWh,
    
    Production_kWh = sum(Production_kWh, na.rm = TRUE),
    Consumption_kWh = sum(.data[[consumption_col]], na.rm = TRUE),
    
    GridImport_kWh = sum(GridImportAfterBattery_kWh, na.rm = TRUE),
    FeedIn_kWh = sum(FeedInAfterBattery_kWh, na.rm = TRUE),
    
    BatteryChargeFromPV_kWh = sum(BatteryChargeFromPV_kWh, na.rm = TRUE),
    BatteryDischargeToLoad_kWh = sum(BatteryDischargeToLoad_kWh, na.rm = TRUE)
  ) |>
  mutate(
    SelfUsedElectricity_kWh = Consumption_kWh - GridImport_kWh,
    
    Autarky_percent = if_else(
      Consumption_kWh > 0,
      SelfUsedElectricity_kWh / Consumption_kWh * 100,
      NA_real_
    ),
    
    SelfConsumptionRate_percent = if_else(
      Production_kWh > 0,
      (Production_kWh - FeedIn_kWh) / Production_kWh * 100,
      NA_real_
    ),
    
    PVUsedOnSiteIncludingBatteryCharging_kWh = Production_kWh - FeedIn_kWh
  ) |>
  select(
    Scenario,
    BatteryCapacity_kWh,
    Production_kWh,
    Consumption_kWh,
    SelfUsedElectricity_kWh,
    GridImport_kWh,
    FeedIn_kWh,
    Autarky_percent,
    SelfConsumptionRate_percent,
    PVUsedOnSiteIncludingBatteryCharging_kWh,
    BatteryChargeFromPV_kWh,
    BatteryDischargeToLoad_kWh
  )


# ------------------------------------------------------------
# 5) Vergleichstabelle erstellen
# ------------------------------------------------------------

summary_comparison <- bind_rows(
  metrics_without_battery,
  metrics_with_battery
) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

print(summary_comparison)

# Plausibilitätsprüfung
if (
  summary_comparison$SelfUsedElectricity_kWh[
    summary_comparison$Scenario == "Without battery"
  ] >
  summary_comparison$SelfUsedElectricity_kWh[
    summary_comparison$Scenario == "With battery"
  ]
) {
  warning(
    "Selbst genutzter Strom ist ohne Batterie höher als mit Batterie. ",
    "Das ist ungewöhnlich und sollte geprüft werden."
  )
}

if (
  any(summary_comparison$FeedIn_kWh > summary_comparison$Production_kWh)
) {
  warning(
    "Einspeisung ist grösser als Produktion. ",
    "Das weist auf einen Berechnungsfehler hin."
  )
}
# ------------------------------------------------------------
# 6) Ergebnisse speichern
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  summary_comparison,
  output_summary_file
)

write_csv(
  df_with_battery,
  output_detail_file
)

message("Autarkie-Auswertung abgeschlossen.")
message("Zusammenfassung gespeichert unter: ", output_summary_file)
message("Detaildaten mit Batterie gespeichert unter: ", output_detail_file)