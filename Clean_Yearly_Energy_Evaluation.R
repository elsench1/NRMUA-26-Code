# Clean_Yearly_Energy_Evaluation.R
#
# Zweck:
# - berechnet alle Jahreswerte sauber neu fuer genau ein Zieljahr
# - filtert Verbrauchsdaten explizit auf target_year
# - verwendet keine Mittelung ueber mehrere Jahre
# - berechnet aktuelle PV-Anlage, Polysun ohne Akku und Polysun mit Akku

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)

# ------------------------------------------------------------
# 0) Einstellungen
# ------------------------------------------------------------

target_year <- 2025
tz_local <- "Europe/Zurich"

path_consumption_total <- "Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv"
path_consumption_ev <- "Verbrauch/Verbrauch_E Mobility_2025.csv"
path_current_pv <- "PV_MGH_Winterthur_15min-Werte_2025.xlsx"

path_pv_simulations <- c(
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_1_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_1_West_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_2_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_3_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_4_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_4_West_15Minutes.csv"
)

pv_simulation_yield_col <- "Yield Photovoltaics AC [kWh]"
pv_simulation_yield_fallback_col <- "Yield Photovoltaics DC [kWh]"
pv_simulation_date_order <- "dm"

battery_capacity_kWh <- 400
charge_efficiency <- 0.95
discharge_efficiency <- 0.95
c_rate_max <- 0.5
initial_soc_kWh <- battery_capacity_kWh

output_csv <- "output/clean_yearly_energy_summary.csv"
output_checks_csv <- "output/clean_yearly_energy_checks.csv"
output_xlsx <- "output/clean_yearly_energy_summary.xlsx"

# ------------------------------------------------------------
# 1) Hilfsfunktionen
# ------------------------------------------------------------

year_start <- ymd_hms(paste0(target_year, "-01-01 00:00:00"), tz = tz_local)
year_end <- ymd_hms(paste0(target_year + 1, "-01-01 00:00:00"), tz = tz_local)

to_number <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  text <- trimws(as.character(x))
  text <- gsub("\\s", "", text)
  text <- gsub("'", "", text, fixed = TRUE)
  both <- grepl(",", text, fixed = TRUE) & grepl(".", text, fixed = TRUE)
  text[both] <- gsub(".", "", text[both], fixed = TRUE)
  text <- gsub(",", ".", text, fixed = TRUE)
  suppressWarnings(as.numeric(text))
}

parse_fixed_datetime <- function(text, formats, tz = tz_local) {
  text <- trimws(as.character(text))
  text <- gsub("T", " ", text, fixed = TRUE)
  text <- sub("Z$", "", text)
  out <- as.POSIXct(rep(NA_real_, length(text)), origin = "1970-01-01", tz = tz)

  for (fmt in formats) {
    idx <- is.na(out) & !is.na(text) & text != ""
    if (!any(idx)) {
      break
    }
    parsed <- suppressWarnings(as.POSIXct(strptime(text[idx], format = fmt, tz = tz)))
    out[idx] <- parsed
  }

  out
}

parse_timestamp_ch <- function(x, tz = tz_local) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.POSIXct(x, tz = tz))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }
  if (is.numeric(x)) {
    # Excel-Datumsseriennummern, falls Excel solche Werte liefert.
    return(as.POSIXct((as.numeric(x) - 25569) * 86400, origin = "1970-01-01", tz = tz))
  }

  parse_fixed_datetime(
    x,
    formats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d",
      "%d.%m.%Y %H:%M:%S",
      "%d.%m.%Y %H:%M",
      "%d.%m.%Y",
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %H:%M",
      "%d/%m/%Y",
      "%m/%d/%Y %H:%M:%S",
      "%m/%d/%Y %H:%M",
      "%m/%d/%Y",
      "%Y%m%d %H:%M:%S",
      "%Y%m%d %H:%M"
    ),
    tz = tz
  )
}

filter_target_year <- function(data, time_col = "Time") {
  data |>
    filter(.data[[time_col]] >= year_start, .data[[time_col]] < year_end)
}

create_15min_year_grid <- function() {
  tibble(
    Time = seq(
      from = year_start,
      to = year_end - minutes(15),
      by = "15 min"
    )
  )
}

input_check <- function(data, source, value_col) {
  tibble(
    Source = source,
    Rows = nrow(data),
    FirstTime = min(data$Time, na.rm = TRUE),
    LastTime = max(data$Time, na.rm = TRUE),
    Value_kWh = sum(as.numeric(data[[value_col]]), na.rm = TRUE)
  )
}

read_consumption_year <- function(path, source_name) {
  data_raw <- read_csv(path, show_col_types = FALSE)
  required <- c("Timestamp", "Volume")
  missing <- setdiff(required, names(data_raw))
  if (length(missing) > 0) {
    stop("Fehlende Spalten in ", path, ": ", paste(missing, collapse = ", "))
  }
  data_raw |>
    transmute(
      Time = floor_date(parse_timestamp_ch(Timestamp), unit = "15 minutes"),
      Value_kWh = to_number(Volume),
      Source = source_name
    ) |>
    filter(!is.na(Time)) |>
    filter_target_year("Time") |>
    group_by(Time, Source) |>
    summarise(Value_kWh = sum(Value_kWh, na.rm = TRUE), .groups = "drop") |>
    arrange(Time)
}

read_current_pv_year <- function(path) {
  data_raw <- read_excel(path, col_names = FALSE, skip = 1)
  names(data_raw) <- c("Time", "Production_kWh")
  data_raw |>
    transmute(
      Time = floor_date(parse_timestamp_ch(Time), unit = "15 minutes"),
      Production_kWh = to_number(Production_kWh),
      Source = "current_pv"
    ) |>
    filter(!is.na(Time)) |>
    filter_target_year("Time") |>
    group_by(Time, Source) |>
    summarise(Production_kWh = sum(Production_kWh, na.rm = TRUE), .groups = "drop") |>
    arrange(Time)
}

read_pv_simulation_file <- function(path) {
  first_bytes <- readBin(path, what = "raw", n = 20)
  is_utf16le <- length(first_bytes) >= 2 &&
    (identical(first_bytes[1:2], as.raw(c(0xFF, 0xFE))) || any(first_bytes == as.raw(0x00)))
  is_utf16be <- length(first_bytes) >= 2 && identical(first_bytes[1:2], as.raw(c(0xFE, 0xFF)))

  if (is_utf16le) {
    lines <- readLines(file(path, open = "r", encoding = "UTF-16LE"), warn = FALSE)
    text_utf8 <- paste(lines, collapse = "\n")
    read_delim(I(text_utf8), delim = ";", locale = locale(decimal_mark = ".", grouping_mark = "'"), trim_ws = TRUE, show_col_types = FALSE)
  } else if (is_utf16be) {
    lines <- readLines(file(path, open = "r", encoding = "UTF-16BE"), warn = FALSE)
    text_utf8 <- paste(lines, collapse = "\n")
    read_delim(I(text_utf8), delim = ";", locale = locale(decimal_mark = ".", grouping_mark = "'"), trim_ws = TRUE, show_col_types = FALSE)
  } else {
    read_delim(path, delim = ";", locale = locale(decimal_mark = ".", grouping_mark = "'"), trim_ws = TRUE, show_col_types = FALSE)
  }
}

parse_pv_simulation_time <- function(date_value, time_value) {
  datetime_text <- paste(date_value, target_year, time_value)
  if (pv_simulation_date_order == "dm") {
    formats <- c(
      "%d.%m. %Y %H:%M:%S",
      "%d.%m. %Y %H:%M",
      "%d.%m %Y %H:%M:%S",
      "%d.%m %Y %H:%M",
      "%d/%m %Y %H:%M:%S",
      "%d/%m %Y %H:%M",
      "%d-%m %Y %H:%M:%S",
      "%d-%m %Y %H:%M",
      "%d.%m. %Y %I:%M %p",
      "%d.%m %Y %I:%M %p",
      "%d/%m %Y %I:%M %p"
    )
  } else {
    formats <- c(
      "%m/%d %Y %H:%M:%S",
      "%m/%d %Y %H:%M",
      "%m-%d %Y %H:%M:%S",
      "%m-%d %Y %H:%M",
      "%m/%d %Y %I:%M %p"
    )
  }
  parse_fixed_datetime(datetime_text, formats = formats, tz = tz_local)
}

read_polysun_year <- function(path) {
  data_raw <- read_pv_simulation_file(path) |>
    select(where(~ !all(is.na(.x))))

  if (!"Date" %in% names(data_raw) || !"Time" %in% names(data_raw)) {
    stop("Date/Time fehlt in Polysun-Datei: ", path)
  }

  if (pv_simulation_yield_col %in% names(data_raw)) {
    selected_yield_col <- pv_simulation_yield_col
  } else if (pv_simulation_yield_fallback_col %in% names(data_raw)) {
    selected_yield_col <- pv_simulation_yield_fallback_col
    warning("AC-Spalte fehlt, verwende DC-Spalte in: ", path)
  } else {
    stop("Keine PV-Ertragsspalte gefunden in: ", path)
  }

  data_raw |>
    transmute(
      Time = floor_date(parse_pv_simulation_time(Date, Time), unit = "15 minutes"),
      Production_kWh = to_number(.data[[selected_yield_col]]),
      Source = basename(path)
    ) |>
    filter(!is.na(Time)) |>
    filter_target_year("Time") |>
    group_by(Time, Source) |>
    summarise(Production_kWh = sum(Production_kWh, na.rm = TRUE), .groups = "drop") |>
    arrange(Time)
}

combine_production <- function(data, source_name) {
  data |>
    group_by(Time) |>
    summarise(
      Production_kWh = sum(Production_kWh, na.rm = TRUE),
      Source = source_name,
      .groups = "drop"
    ) |>
    arrange(Time)
}

build_energy_table <- function(production_data, consumption_total_data, consumption_ev_data) {
  create_15min_year_grid() |>
    left_join(
      production_data |> select(Time, Production_kWh),
      by = "Time"
    ) |>
    left_join(
      consumption_total_data |> transmute(Time, ConsumptionTotal_kWh = Value_kWh),
      by = "Time"
    ) |>
    left_join(
      consumption_ev_data |> transmute(Time, ConsumptionEV_kWh = Value_kWh),
      by = "Time"
    ) |>
    mutate(
      Production_kWh = replace_na(Production_kWh, 0),
      ConsumptionTotal_kWh = replace_na(ConsumptionTotal_kWh, 0),
      ConsumptionEV_kWh = replace_na(ConsumptionEV_kWh, 0),
      ConsumptionWithoutEV_kWh = ConsumptionTotal_kWh - ConsumptionEV_kWh,
      DirectSelfUsed_kWh = pmin(Production_kWh, ConsumptionTotal_kWh),
      GridImportWithoutBattery_kWh = pmax(ConsumptionTotal_kWh - Production_kWh, 0),
      FeedInWithoutBattery_kWh = pmax(Production_kWh - ConsumptionTotal_kWh, 0)
    )
}

simulate_battery <- function(data) {
  out <- data |>
    arrange(Time) |>
    mutate(
      Battery_SOC_kWh = NA_real_,
      BatteryChargeFromPV_kWh = 0,
      BatteryDischargeToLoad_kWh = 0,
      GridImportAfterBattery_kWh = 0,
      FeedInAfterBattery_kWh = 0
    )

  soc_kWh <- min(initial_soc_kWh, battery_capacity_kWh)
  max_energy_per_step_kWh <- battery_capacity_kWh * c_rate_max * 0.25

  for (i in seq_len(nrow(out))) {
    surplus_kWh <- out$Production_kWh[i] - out$ConsumptionTotal_kWh[i]

    if (surplus_kWh > 0) {
      battery_headroom_kWh <- battery_capacity_kWh - soc_kWh
      charge_from_pv_kWh <- min(surplus_kWh, max_energy_per_step_kWh, battery_headroom_kWh / charge_efficiency)
      soc_kWh <- soc_kWh + charge_from_pv_kWh * charge_efficiency
      out$BatteryChargeFromPV_kWh[i] <- charge_from_pv_kWh
      out$FeedInAfterBattery_kWh[i] <- surplus_kWh - charge_from_pv_kWh
    } else if (surplus_kWh < 0) {
      deficit_kWh <- abs(surplus_kWh)
      discharge_dc_kWh <- min(soc_kWh, max_energy_per_step_kWh, deficit_kWh / discharge_efficiency)
      delivered_to_load_kWh <- discharge_dc_kWh * discharge_efficiency
      soc_kWh <- soc_kWh - discharge_dc_kWh
      out$BatteryDischargeToLoad_kWh[i] <- delivered_to_load_kWh
      out$GridImportAfterBattery_kWh[i] <- deficit_kWh - delivered_to_load_kWh
    }

    soc_kWh <- max(0, min(battery_capacity_kWh, soc_kWh))
    out$Battery_SOC_kWh[i] <- soc_kWh
  }

  out
}

make_metrics <- function(data, scenario, battery = FALSE) {
  if (battery) {
    grid_import <- sum(data$GridImportAfterBattery_kWh, na.rm = TRUE)
    feed_in <- sum(data$FeedInAfterBattery_kWh, na.rm = TRUE)
    battery_charge <- sum(data$BatteryChargeFromPV_kWh, na.rm = TRUE)
    battery_discharge <- sum(data$BatteryDischargeToLoad_kWh, na.rm = TRUE)
    capacity <- battery_capacity_kWh
  } else {
    grid_import <- sum(data$GridImportWithoutBattery_kWh, na.rm = TRUE)
    feed_in <- sum(data$FeedInWithoutBattery_kWh, na.rm = TRUE)
    battery_charge <- 0
    battery_discharge <- 0
    capacity <- 0
  }

  production <- sum(data$Production_kWh, na.rm = TRUE)
  consumption <- sum(data$ConsumptionTotal_kWh, na.rm = TRUE)
  self_used_electricity <- consumption - grid_import
  pv_used_on_site <- production - feed_in

  tibble(
    Scenario = scenario,
    Year = target_year,
    BatteryCapacity_kWh = capacity,
    Production_kWh = production,
    Consumption_kWh = consumption,
    SelfUsedElectricity_kWh = self_used_electricity,
    GridImport_kWh = grid_import,
    FeedIn_kWh = feed_in,
    Autarky_percent = if_else(consumption > 0, self_used_electricity / consumption * 100, NA_real_),
    SelfConsumptionRate_percent = if_else(production > 0, pv_used_on_site / production * 100, NA_real_),
    PVUsedOnSiteIncludingBatteryCharging_kWh = pv_used_on_site,
    BatteryChargeFromPV_kWh = battery_charge,
    BatteryDischargeToLoad_kWh = battery_discharge
  )
}

# ------------------------------------------------------------
# 2) Daten laden und konsequent auf target_year filtern
# ------------------------------------------------------------

consumption_total <- read_consumption_year(path_consumption_total, "consumption_total")
consumption_ev <- read_consumption_year(path_consumption_ev, "consumption_ev")
current_pv <- read_current_pv_year(path_current_pv)

missing_polysun <- path_pv_simulations[!file.exists(path_pv_simulations)]
if (length(missing_polysun) > 0) {
  stop("Folgende Polysun-Dateien fehlen:\n", paste(missing_polysun, collapse = "\n"))
}

polysun_pv <- map(path_pv_simulations, read_polysun_year) |>
  bind_rows() |>
  combine_production("polysun_total")

current_pv_total <- combine_production(current_pv, "current_pv_total")

# ------------------------------------------------------------
# 3) Szenarien berechnen
# ------------------------------------------------------------

current_table <- build_energy_table(current_pv_total, consumption_total, consumption_ev)
polysun_table <- build_energy_table(polysun_pv, consumption_total, consumption_ev)
polysun_battery_table <- simulate_battery(polysun_table)

summary_clean <- bind_rows(
  make_metrics(current_table, "Aktuelle PV-Anlage", battery = FALSE),
  make_metrics(polysun_table, "Polysun ohne Akku", battery = FALSE),
  make_metrics(polysun_battery_table, "Polysun mit Akku", battery = TRUE)
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

checks <- bind_rows(
  input_check(consumption_total, "Verbrauch gesamt gefiltert", "Value_kWh"),
  input_check(consumption_ev, "E-Mobility gefiltert", "Value_kWh"),
  input_check(current_pv, "Aktuelle PV gefiltert", "Production_kWh"),
  input_check(polysun_pv, "Polysun PV total gefiltert", "Production_kWh")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(summary_clean)
print(checks)

if (n_distinct(summary_clean$Consumption_kWh) != 1) {
  warning("Die Verbrauchswerte der Szenarien sind nicht identisch. Das sollte bei diesem Skript nicht passieren.")
}

# ------------------------------------------------------------
# 4) Ausgabe
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(summary_clean, output_csv)
write_csv(checks, output_checks_csv)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(
      Jahreswerte = summary_clean,
      Checks = checks
    ),
    output_xlsx
  )
  message("Excel-Datei geschrieben: ", output_xlsx)
} else {
  message("Paket 'writexl' ist nicht installiert. CSV-Dateien wurden geschrieben.")
}

message("Saubere Jahresauswertung abgeschlossen.")
message("Zusammenfassung: ", output_csv)
message("Checks: ", output_checks_csv)
