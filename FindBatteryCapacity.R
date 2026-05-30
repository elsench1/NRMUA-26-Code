source("PowerSimulationFunction.R")

input_file <- "output/energy_15min_prepared.csv"
output_file <- "output/simulation_minimum_battery_output.csv"
plot_file <- "output/simulation_minimum_battery_plot.png"
search_log_file <- "output/battery_capacity_search_log.csv"

charge_efficiency <- 0.97
discharge_efficiency <- 0.95
c_rate_max <- 0.5
start_battery_capacity_kWh <- 380

minimum_battery_accuracy_kWh <- 1
max_capacity_search_iterations <- 20
max_capacity_allowed_kWh <- 1e8
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

minimum_search <- find_minimum_capacity(
  df_work_base = df_work_base,
  start_capacity_kWh = start_battery_capacity_kWh,
  charge_efficiency = charge_efficiency,
  discharge_efficiency = discharge_efficiency,
  c_rate_max = c_rate_max,
  consumption_col = consumption_col,
  accuracy_kWh = minimum_battery_accuracy_kWh,
  zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh,
  max_iterations = max_capacity_search_iterations,
  max_capacity_allowed_kWh = max_capacity_allowed_kWh
)

minimum_battery_capacity_kWh <- minimum_search$minimum_capacity_kWh
df_result <- minimum_search$result$df_result
summary_result <- minimum_search$result$summary_result

print(minimum_search$search_log)
print(summary_result)

write_csv(minimum_search$search_log, search_log_file)
export_simulation_result(df_result, output_file)

plot_simulation_result(
  df_result = df_result,
  plot_file = plot_file,
  simulation_year = simulation_year,
  battery_capacity_kWh = minimum_battery_capacity_kWh,
  c_rate_max = c_rate_max,
  plot_interval_hours = plot_interval_hours
)

message("Minimale Batteriegrösse ohne Netzbezug: ", minimum_battery_capacity_kWh, " kWh")
message("Letzte nicht ausreichende Grösse: ", minimum_search$last_insufficient_capacity_kWh, " kWh")
message("Erster ausreichender Wert: ", minimum_search$first_sufficient_capacity_kWh, " kWh")
message("Netzbezug nach Batterie: ", round(summary_result$GridImportAfterBattery_kWh, 6), " kWh")
message("Simulation gespeichert unter: ", output_file)
message("Suchlog gespeichert unter: ", search_log_file)
message("Plot gespeichert unter: ", plot_file)