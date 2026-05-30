source("PowerSimulationFunction.R")

input_file <- "output/energy_15min_prepared.csv"
output_file <- "output/simulation_fixed_battery_output.csv"
plot_file <- "output/simulation_fixed_battery_plot.png"

charge_efficiency <- 0.97
discharge_efficiency <- 0.95
c_rate_max <- 0.5
battery_capacity_kWh <- 380
zero_grid_import_tolerance_kWh <- 1e-6

consumption_col <- "ConsumptionTotal_kWh"
simulation_year <- 2025

warmup_source_start <- ymd_hms("2025-10-01 00:00:00", tz = "Europe/Zurich")
warmup_source_end <- ymd_hms("2026-01-01 00:00:00", tz = "Europe/Zurich")

plot_interval_hours <- 24

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

simulation_result <- simulate_capacity(
  df_work_base = df_work_base,
  capacity_kWh = battery_capacity_kWh,
  charge_efficiency = charge_efficiency,
  discharge_efficiency = discharge_efficiency,
  c_rate_max = c_rate_max,
  consumption_col = consumption_col,
  zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh
)

df_result <- simulation_result$df_result
summary_result <- simulation_result$summary_result

print(summary_result)

export_simulation_result(df_result, output_file)

plot_simulation_result(
  df_result = df_result,
  plot_file = plot_file,
  simulation_year = simulation_year,
  battery_capacity_kWh = battery_capacity_kWh,
  c_rate_max = c_rate_max,
  plot_interval_hours = plot_interval_hours
)

message("Simulation mit fixer Batteriegrösse abgeschlossen.")
message("Verwendete Batteriegrösse: ", battery_capacity_kWh, " kWh")
message("Netzbezug nach Batterie: ", round(summary_result$GridImportAfterBattery_kWh, 6), " kWh")
message("SOC am Start der echten Simulation: ", round(summary_result$Start_SOC_percent, 2), " %")
message("Simulation gespeichert unter: ", output_file)
message("Plot gespeichert unter: ", plot_file)
