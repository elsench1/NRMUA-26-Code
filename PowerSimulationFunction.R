library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

parse_time_utc <- function(x) {
  parsed <- parse_date_time(
    x,
    orders = c("ymd HMS z", "ymd HMS", "ymd HM z", "ymd HM", "ymd"),
    tz = "UTC"
  )
  as_datetime(parsed, tz = "UTC")
}

load_energy_data <- function(input_file, consumption_col) {
  df_raw <- read_csv(
    input_file,
    show_col_types = FALSE,
    col_types = cols(
      Time = col_character(),
      .default = col_double()
    )
  )
  
  required_cols <- c("Time", "Production_kWh", consumption_col)
  missing_cols <- setdiff(required_cols, names(df_raw))
  
  if (length(missing_cols) > 0) {
    stop("Folgende Spalten fehlen in der CSV: ", paste(missing_cols, collapse = ", "))
  }
  
  df <- df_raw %>%
    mutate(
      Time_original = Time,
      Time_UTC = parse_time_utc(Time_original),
      Time_CH = with_tz(Time_UTC, tzone = "Europe/Zurich")
    ) %>%
    arrange(Time_CH)
  
  if (any(is.na(df$Time_UTC))) {
    bad_examples <- df %>%
      filter(is.na(Time_UTC)) %>%
      slice_head(n = 10) %>%
      pull(Time_original)
    
    stop(
      "Einige Zeitwerte konnten nicht gelesen werden. Beispiele: ",
      paste(bad_examples, collapse = ", ")
    )
  }
  
  df
}

prepare_simulation_data <- function(
    df,
    simulation_year,
    warmup_source_start,
    warmup_source_end
) {
  simulation_start <- ymd_hms(
    paste0(simulation_year, "-01-01 00:00:00"),
    tz = "Europe/Zurich"
  )
  
  simulation_end <- ymd_hms(
    paste0(simulation_year + 1, "-01-01 00:00:00"),
    tz = "Europe/Zurich"
  )
  
  df_simulation <- df %>%
    filter(Time_CH >= simulation_start, Time_CH < simulation_end) %>%
    mutate(
      IsWarmup = FALSE,
      Original_Time_CH = Time_CH,
      Original_Time_UTC = Time_UTC,
      Time_Output = format(Time_UTC, "%Y-%m-%dT%H:%M:%SZ")
    )
  
  if (nrow(df_simulation) == 0) {
    stop("Keine Daten für das Simulationsjahr ", simulation_year, " gefunden.")
  }
  
  df_warmup <- df %>%
    filter(Time_CH >= warmup_source_start, Time_CH < warmup_source_end) %>%
    mutate(
      Original_Time_CH = Time_CH,
      Original_Time_UTC = Time_UTC,
      Time_CH = Time_CH - years(1),
      Time_UTC = with_tz(Time_CH, tzone = "UTC"),
      Time_Output = format(Time_UTC, "%Y-%m-%dT%H:%M:%SZ"),
      IsWarmup = TRUE
    )
  
  if (nrow(df_warmup) == 0) {
    stop(
      "Keine Daten für den künstlichen Warm-up gefunden. Erwartet werden Werte von ",
      warmup_source_start, " bis ", warmup_source_end, "."
    )
  }
  
  df_work_base <- bind_rows(df_warmup, df_simulation) %>%
    arrange(Time_CH) %>%
    mutate(
      next_time = lead(Time_CH),
      timestep_h = as.numeric(difftime(next_time, Time_CH, units = "hours"))
    )
  
  median_timestep <- median(
    df_work_base$timestep_h[df_work_base$timestep_h > 0],
    na.rm = TRUE
  )
  
  if (is.na(median_timestep)) {
    stop("Der Zeitschritt konnte nicht bestimmt werden.")
  }
  
  df_work_base %>%
    mutate(
      timestep_h = if_else(
        is.na(timestep_h) | timestep_h <= 0,
        median_timestep,
        timestep_h
      )
    )
}

run_battery_simulation <- function(
    df_input,
    battery_capacity_kWh,
    charge_efficiency,
    discharge_efficiency,
    c_rate_max,
    consumption_col,
    initial_soc_kWh = battery_capacity_kWh
) {
  df_work <- df_input %>%
    arrange(Time_CH) %>%
    mutate(
      Battery_SOC_kWh = NA_real_,
      Battery_SOC_percent = NA_real_,
      BatteryChargeFromPV_kWh = 0,
      BatteryDischargeToLoad_kWh = 0,
      GridImportAfterBattery_kWh = NA_real_,
      FeedInAfterBattery_kWh = NA_real_
    )
  
  soc_kWh <- min(initial_soc_kWh, battery_capacity_kWh)
  max_power_kW <- battery_capacity_kWh * c_rate_max
  
  for (i in seq_len(nrow(df_work))) {
    production_kWh <- df_work$Production_kWh[i]
    consumption_kWh <- df_work[[consumption_col]][i]
    timestep_h <- df_work$timestep_h[i]
    
    if (is.na(production_kWh) || is.na(consumption_kWh) || is.na(timestep_h)) {
      df_work$Battery_SOC_kWh[i] <- soc_kWh
      df_work$Battery_SOC_percent[i] <- ifelse(
        battery_capacity_kWh > 0,
        soc_kWh / battery_capacity_kWh * 100,
        NA_real_
      )
      next
    }
    
    max_energy_per_step_kWh <- max_power_kW * timestep_h
    surplus_kWh <- production_kWh - consumption_kWh
    
    battery_charge_from_pv_kWh <- 0
    battery_discharge_to_load_kWh <- 0
    grid_import_after_battery_kWh <- 0
    feed_in_after_battery_kWh <- 0
    
    if (surplus_kWh > 0) {
      battery_headroom_kWh <- battery_capacity_kWh - soc_kWh
      
      max_charge_ac_kWh <- min(
        surplus_kWh,
        max_energy_per_step_kWh,
        battery_headroom_kWh / charge_efficiency
      )
      
      soc_kWh <- soc_kWh + max_charge_ac_kWh * charge_efficiency
      
      battery_charge_from_pv_kWh <- max_charge_ac_kWh
      feed_in_after_battery_kWh <- surplus_kWh - max_charge_ac_kWh
      grid_import_after_battery_kWh <- 0
      
    } else if (surplus_kWh < 0) {
      deficit_kWh <- abs(surplus_kWh)
      
      max_discharge_dc_kWh <- min(
        soc_kWh,
        max_energy_per_step_kWh,
        deficit_kWh / discharge_efficiency
      )
      
      delivered_to_load_kWh <- max_discharge_dc_kWh * discharge_efficiency
      soc_kWh <- soc_kWh - max_discharge_dc_kWh
      
      battery_discharge_to_load_kWh <- delivered_to_load_kWh
      grid_import_after_battery_kWh <- deficit_kWh - delivered_to_load_kWh
      feed_in_after_battery_kWh <- 0
      
    } else {
      grid_import_after_battery_kWh <- 0
      feed_in_after_battery_kWh <- 0
    }
    
    soc_kWh <- max(0, min(battery_capacity_kWh, soc_kWh))
    
    df_work$Battery_SOC_kWh[i] <- soc_kWh
    df_work$Battery_SOC_percent[i] <- ifelse(
      battery_capacity_kWh > 0,
      soc_kWh / battery_capacity_kWh * 100,
      NA_real_
    )
    df_work$BatteryChargeFromPV_kWh[i] <- battery_charge_from_pv_kWh
    df_work$BatteryDischargeToLoad_kWh[i] <- battery_discharge_to_load_kWh
    df_work$GridImportAfterBattery_kWh[i] <- grid_import_after_battery_kWh
    df_work$FeedInAfterBattery_kWh[i] <- feed_in_after_battery_kWh
  }
  
  df_work %>%
    mutate(
      ProductionPower_sim_kW = Production_kWh / timestep_h,
      ConsumptionPower_sim_kW = .data[[consumption_col]] / timestep_h
    )
}

make_simulation_summary <- function(df_result, consumption_col) {
  df_result %>%
    summarise(
      SimulationStart = min(Time_CH),
      SimulationEnd = max(Time_CH),
      Start_SOC_kWh = first(Battery_SOC_kWh),
      Start_SOC_percent = first(Battery_SOC_percent),
      End_SOC_kWh = last(Battery_SOC_kWh),
      End_SOC_percent = last(Battery_SOC_percent),
      Mean_SOC_kWh = mean(Battery_SOC_kWh, na.rm = TRUE),
      Mean_SOC_percent = mean(Battery_SOC_percent, na.rm = TRUE),
      Min_SOC_percent = min(Battery_SOC_percent, na.rm = TRUE),
      Max_SOC_percent = max(Battery_SOC_percent, na.rm = TRUE),
      Production_kWh = sum(Production_kWh, na.rm = TRUE),
      Consumption_kWh = sum(.data[[consumption_col]], na.rm = TRUE),
      BatteryChargeFromPV_kWh = sum(BatteryChargeFromPV_kWh, na.rm = TRUE),
      BatteryDischargeToLoad_kWh = sum(BatteryDischargeToLoad_kWh, na.rm = TRUE),
      GridImportAfterBattery_kWh = sum(GridImportAfterBattery_kWh, na.rm = TRUE),
      FeedInAfterBattery_kWh = sum(FeedInAfterBattery_kWh, na.rm = TRUE)
    )
}

simulate_capacity <- function(
    df_work_base,
    capacity_kWh,
    charge_efficiency,
    discharge_efficiency,
    c_rate_max,
    consumption_col,
    zero_grid_import_tolerance_kWh = 1e-6
) {
  df_sim_all <- run_battery_simulation(
    df_input = df_work_base,
    battery_capacity_kWh = capacity_kWh,
    charge_efficiency = charge_efficiency,
    discharge_efficiency = discharge_efficiency,
    c_rate_max = c_rate_max,
    consumption_col = consumption_col,
    initial_soc_kWh = capacity_kWh
  )
  
  df_result <- df_sim_all %>%
    filter(!IsWarmup) %>%
    arrange(Time_CH)
  
  summary_result <- make_simulation_summary(df_result, consumption_col)
  total_grid_import_kWh <- summary_result$GridImportAfterBattery_kWh
  
  list(
    capacity_kWh = capacity_kWh,
    sufficient = total_grid_import_kWh <= zero_grid_import_tolerance_kWh,
    total_grid_import_kWh = total_grid_import_kWh,
    df_result = df_result,
    summary_result = summary_result
  )
}

find_minimum_capacity <- function(
    df_work_base,
    start_capacity_kWh,
    charge_efficiency,
    discharge_efficiency,
    c_rate_max,
    consumption_col,
    accuracy_kWh = 1,
    zero_grid_import_tolerance_kWh = 1e-6,
    max_iterations = 20,
    max_capacity_allowed_kWh = 1e8
) {
  lower_capacity_kWh <- 0
  upper_capacity_kWh <- ceiling(start_capacity_kWh)
  
  search_log <- tibble(
    Phase = character(),
    Capacity_kWh = numeric(),
    GridImportAfterBattery_kWh = numeric(),
    Sufficient = logical()
  )
  
  for (iteration in seq_len(max_iterations)) {
    test <- simulate_capacity(
      df_work_base = df_work_base,
      capacity_kWh = upper_capacity_kWh,
      charge_efficiency = charge_efficiency,
      discharge_efficiency = discharge_efficiency,
      c_rate_max = c_rate_max,
      consumption_col = consumption_col,
      zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh
    )
    
    search_log <- bind_rows(
      search_log,
      tibble(
        Phase = "upper_bound_search",
        Capacity_kWh = upper_capacity_kWh,
        GridImportAfterBattery_kWh = test$total_grid_import_kWh,
        Sufficient = test$sufficient
      )
    )
    
    message(
      "Test obere Grenze: ", upper_capacity_kWh, " kWh -> Netzbezug ",
      round(test$total_grid_import_kWh, 6), " kWh"
    )
    
    if (test$sufficient) {
      break
    }
    
    lower_capacity_kWh <- upper_capacity_kWh
    upper_capacity_kWh <- upper_capacity_kWh * 10
    
    if (upper_capacity_kWh > max_capacity_allowed_kWh) {
      stop(
        "Auch eine sehr grosse Batterie reicht nicht aus. ",
        "Vermutlich ist die Jahresproduktion inklusive Wirkungsgradverluste zu klein. ",
        "Letzte getestete nicht ausreichende Grösse: ", lower_capacity_kWh, " kWh."
      )
    }
  }
  
  if (!test$sufficient) {
    stop("Keine ausreichende obere Batteriegrenze gefunden.")
  }
  
  while ((upper_capacity_kWh - lower_capacity_kWh) > accuracy_kWh) {
    mid_capacity_kWh <- floor((lower_capacity_kWh + upper_capacity_kWh) / 2)
    
    if (mid_capacity_kWh <= lower_capacity_kWh) {
      mid_capacity_kWh <- lower_capacity_kWh + accuracy_kWh
    }
    
    test <- simulate_capacity(
      df_work_base = df_work_base,
      capacity_kWh = mid_capacity_kWh,
      charge_efficiency = charge_efficiency,
      discharge_efficiency = discharge_efficiency,
      c_rate_max = c_rate_max,
      consumption_col = consumption_col,
      zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh
    )
    
    search_log <- bind_rows(
      search_log,
      tibble(
        Phase = "binary_search",
        Capacity_kWh = mid_capacity_kWh,
        GridImportAfterBattery_kWh = test$total_grid_import_kWh,
        Sufficient = test$sufficient
      )
    )
    
    message(
      "Binäre Suche: ", mid_capacity_kWh, " kWh -> Netzbezug ",
      round(test$total_grid_import_kWh, 6), " kWh"
    )
    
    if (test$sufficient) {
      upper_capacity_kWh <- mid_capacity_kWh
    } else {
      lower_capacity_kWh <- mid_capacity_kWh
    }
  }
  
  final_test <- simulate_capacity(
    df_work_base = df_work_base,
    capacity_kWh = upper_capacity_kWh,
    charge_efficiency = charge_efficiency,
    discharge_efficiency = discharge_efficiency,
    c_rate_max = c_rate_max,
    consumption_col = consumption_col,
    zero_grid_import_tolerance_kWh = zero_grid_import_tolerance_kWh
  )
  
  if (!final_test$sufficient) {
    stop("Interner Fehler: Die ermittelte Batteriegrösse reicht nicht aus.")
  }
  
  list(
    minimum_capacity_kWh = ceiling(upper_capacity_kWh),
    last_insufficient_capacity_kWh = lower_capacity_kWh,
    first_sufficient_capacity_kWh = upper_capacity_kWh,
    result = final_test,
    search_log = search_log
  )
}

export_simulation_result <- function(df_result, output_file) {
  wanted_cols <- c(
    "Time_Output",
    "Time_UTC",
    "Time_CH",
    "Original_Time_CH",
    "Production_kWh",
    "ProductionPower_kW",
    "ProductionPower_sim_kW",
    "ConsumptionTotal_kWh",
    "ConsumptionEV_kWh",
    "ConsumptionWithoutEV_kWh",
    "ConsumptionPower_sim_kW",
    "NetConsumption_kWh",
    "SelfConsumptionPotential_kWh",
    "FeedInPotential_kWh",
    "GridImportPotential_kWh",
    "Battery_SOC_kWh",
    "Battery_SOC_percent",
    "BatteryChargeFromPV_kWh",
    "BatteryDischargeToLoad_kWh",
    "GridImportAfterBattery_kWh",
    "FeedInAfterBattery_kWh"
  )
  
  existing_cols <- intersect(wanted_cols, names(df_result))
  
  df_out <- df_result %>%
    select(all_of(existing_cols)) %>%
    rename(Time = Time_Output)
  
  write_csv(df_out, output_file)
  invisible(df_out)
}

plot_simulation_result <- function(
    df_result,
    plot_file,
    simulation_year,
    battery_capacity_kWh,
    c_rate_max,
    plot_interval_hours = 24
) {
  plot_data <- df_result %>%
    mutate(
      PlotTime_CH = floor_date(Time_CH, unit = paste(plot_interval_hours, "hours"))
    ) %>%
    group_by(PlotTime_CH) %>%
    summarise(
      ConsumptionPower_avg_kW = mean(ConsumptionPower_sim_kW, na.rm = TRUE),
      ProductionPower_avg_kW = mean(ProductionPower_sim_kW, na.rm = TRUE),
      Battery_SOC_avg_percent = mean(Battery_SOC_percent, na.rm = TRUE),
      .groups = "drop"
    )
  
  max_power_for_plot <- max(
    plot_data$ProductionPower_avg_kW,
    plot_data$ConsumptionPower_avg_kW,
    na.rm = TRUE
  )
  
  if (!is.finite(max_power_for_plot) || max_power_for_plot <= 0) {
    max_power_for_plot <- 1
  }
  
  soc_scale_factor <- max_power_for_plot / 100
  
  plot_result <- ggplot(plot_data, aes(x = PlotTime_CH)) +
    geom_line(aes(
      y = ConsumptionPower_avg_kW,
      color = "Totaler Verbrauch"),
      linewidth = 1) + # 0.45
    geom_line(aes(
      y = ProductionPower_avg_kW,
      color = "PV-Produktion"),
      linewidth = 1) + # 0.45
    geom_line(aes(
      y = Battery_SOC_avg_percent * soc_scale_factor,
      color = "Batterie-SOC"), 
      linewidth = 1) + # 0.55
    scale_y_continuous(
      name = "Leistung [kW]",
      sec.axis = sec_axis(
        trans = ~ . / soc_scale_factor,
        name = "Batterie-SOC [%]",
        breaks = seq(0, 100, by = 20)
      )
    ) +
    scale_x_datetime(
      name = "Zeit",
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Totaler Verbrauch" = "black",
        "PV-Produktion" = "red",
        "Batterie-SOC" = "blue"
      )
    ) +
    labs(
      title = "PV-Produktion, totaler Verbrauch und Batterie-SOC",
      subtitle = paste0(
        plot_interval_hours, "-Stunden-Mittelwerte | ",
        "Simulation ", simulation_year,
        " | Batterie: ", battery_capacity_kWh, " kWh",
        " | C-Rate: ", c_rate_max
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(plot_result)
  
  ggsave(
    filename = plot_file,
    plot = plot_result,
    width = 14,
    height = 7,
    dpi = 150
  )
  
  invisible(plot_result)
}
