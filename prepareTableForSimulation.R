library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# ------------------------------------------------------------
# 0) Einstellungen
# ------------------------------------------------------------

tz_local <- "Europe/Zurich"

# File Path
path_consumption_total <- "Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv"
path_consumption_ev    <- "Verbrauch/Verbrauch_E Mobility_2025.csv"
path_pvgis             <- "PV_Production_Simulation_Data/Timeseries_47.505_8.765_SA3_24kWp_crystSi_14_10deg_76deg_2020_2023.csv"
path_pvgis_2           <- "PV_Production_Simulation_Data/Timeseries_47.505_8.765_SA3_19kWp_crystSi_14_10deg_-104deg_2020_2023.csv"

# Anzahl gleicher Daecher / Wiederholungen der Ost-West-Anlage
pv_roof_multiplier <- 9

# Spaltennamen der Verbrauchsdateien
consumption_time_col <- "Timestamp"
consumption_value_col <- "Volume"

ev_time_col <- "Timestamp"
ev_value_col <- "Volume"

# ------------------------------------------------------------
# 1) Hilfsfunktionen fuer den Import
# ------------------------------------------------------------

parse_timestamp <- function(x, tz = tz_local) {
  
  if (inherits(x, "POSIXct")) {
    return(with_tz(x, tz))
  }
  
  parsed <- parse_date_time(
    x,
    orders = c(
      "ymd HMS", "ymd HM", "ymd",
      "ymdT HMS", "ymdT HM",
      "dmY HMS", "dmY HM", "dmY",
      "Ymd HM", "Ymd HMS"
    ),
    tz = tz
  )
  
  as.POSIXct(parsed, tz = tz)
}

import_consumption_csv <- function(path,
                                   time_col = "Timestamp",
                                   value_col = "Volume",
                                   source_name = "consumption",
                                   tz = tz_local) {
  data_raw <- read_csv(path, show_col_types = FALSE)
  
  if (!time_col %in% names(data_raw)) {
    stop("Zeitspalte nicht gefunden: ", time_col)
  }
  
  if (!value_col %in% names(data_raw)) {
    stop("Wertspalte nicht gefunden: ", value_col)
  }
  
  data_raw |>
    transmute(
      Time = parse_timestamp(.data[[time_col]], tz = tz),
      Value_kWh = as.numeric(.data[[value_col]]),
      Source = source_name
    ) |>
    filter(!is.na(Time)) |>
    arrange(Time)
}

find_pvgis_header_line <- function(path) {
  lines <- read_lines(path, progress = FALSE)
  header_line <- which(str_detect(lines, "^time,"))[1]
  
  if (is.na(header_line)) {
    stop("PVGIS-Header nicht gefunden. Erwartet wird eine Zeile, die mit 'time,' beginnt.")
  }
  
  header_line
}

read_pvgis_data_lines <- function(path) {
  # PVGIS-Dateien enthalten am Ende oft Fusszeilen/Metadaten.
  # Diese Funktion liest deshalb nur den echten CSV-Datenblock:
  # - Header beginnt mit "time,"
  # - Datenzeilen beginnen mit z.B. "20200101:0010,"
  
  lines <- read_lines(path, progress = FALSE)
  header_line <- find_pvgis_header_line(path)
  
  data_lines <- lines[header_line:length(lines)]
  keep_lines <- str_detect(data_lines, "^time,") |
    str_detect(data_lines, "^[0-9]{8}:[0-9]{4},")
  
  data_lines_clean <- data_lines[keep_lines]
  
  read_csv(
    I(paste(data_lines_clean, collapse = "
")),
    show_col_types = FALSE
  )
}

parse_pvgis_time <- function(x, tz = tz_local) {
  # PVGIS-Format z.B. 20200101:0010
  as.POSIXct(strptime(x, format = "%Y%m%d:%H%M", tz = tz))
}

import_pvgis_hourly <- function(path,
                                tz = tz_local,
                                source_name = "pv_production") {
  read_pvgis_data_lines(path) |>
    transmute(
      Time_original = parse_pvgis_time(time, tz = tz),
      Time_hour = floor_date(Time_original, unit = "hour"),
      ProductionPower_W = as.numeric(P),
      Irradiance_W_m2 = as.numeric(`G(i)`),
      SunHeight_deg = as.numeric(H_sun),
      Temperature_C = as.numeric(T2m),
      WindSpeed_m_s = as.numeric(WS10m),
      Interpolated = as.numeric(Int),
      Source = source_name
    ) |>
    filter(!is.na(Time_hour)) |>
    arrange(Time_hour)
}

split_pvgis_hourly_to_15min <- function(pvgis_hourly) {
  # Annahme:
  # PVGIS-P ist eine mittlere Leistung in Watt fuer die jeweilige Stunde.
  # Energie pro 15-Minuten-Intervall = P_W / 1000 * 0.25 h.
  # Die vier 15-Minuten-Werte je Stunde summieren sich dadurch zur Stundenenergie.
  
  pvgis_hourly |>
    tidyr::crossing(Quarter = 0:3) |>
    mutate(
      Time = Time_hour + minutes(Quarter * 15),
      ProductionPower_kW = ProductionPower_W / 1000,
      Production_kWh = ProductionPower_kW * 0.25
    ) |>
    select(
      Time,
      Production_kWh,
      ProductionPower_kW,
      Irradiance_W_m2,
      SunHeight_deg,
      Temperature_C,
      WindSpeed_m_s,
      Interpolated,
      Source
    ) |>
    arrange(Time)
}

combine_pv_systems_15min <- function(...,
                                     multiplier = 1,
                                     source_name = "pv_production_total") {
  # Fasst mehrere PV-Anlagen zu einer gemeinsamen Anlage zusammen.
  # Energie und Leistung werden addiert und danach mit dem Multiplikator skaliert.
  # Wetter-/Einstrahlungswerte werden als Durchschnitt behalten, da sie keine Anlagenleistung sind.
  
  bind_rows(...) |>
    group_by(Time) |>
    summarise(
      Production_kWh = sum(Production_kWh, na.rm = TRUE) * multiplier,
      ProductionPower_kW = sum(ProductionPower_kW, na.rm = TRUE) * multiplier,
      Irradiance_W_m2 = mean(Irradiance_W_m2, na.rm = TRUE),
      SunHeight_deg = mean(SunHeight_deg, na.rm = TRUE),
      Temperature_C = mean(Temperature_C, na.rm = TRUE),
      WindSpeed_m_s = mean(WindSpeed_m_s, na.rm = TRUE),
      Interpolated = max(Interpolated, na.rm = TRUE),
      Source = source_name,
      .groups = "drop"
    ) |>
    arrange(Time)
}

align_to_15min_grid <- function(data,
                                time_col = "Time",
                                value_col = "Value_kWh",
                                source_name = NULL,
                                tz = tz_local) {
  # Rundet Zeitpunkte auf 15-Minuten-Raster und summiert doppelte Zeitpunkte.
  # Das ist sinnvoll fuer Energiewerte in kWh.
  
  out <- data |>
    mutate(
      Time = floor_date(.data[[time_col]], unit = "15 minutes"),
      Value_kWh = as.numeric(.data[[value_col]])
    ) |>
    group_by(Time) |>
    summarise(
      Value_kWh = sum(Value_kWh, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Time)
  
  if (!is.null(source_name)) {
    out <- out |> mutate(Source = source_name)
  }
  
  out
}

# ------------------------------------------------------------
# 2) Import aller Rohdaten
# ------------------------------------------------------------

consumption_total_raw <- import_consumption_csv(
  path = path_consumption_total,
  time_col = consumption_time_col,
  value_col = consumption_value_col,
  source_name = "consumption_total",
  tz = tz_local
)

consumption_ev_raw <- import_consumption_csv(
  path = path_consumption_ev,
  time_col = ev_time_col,
  value_col = ev_value_col,
  source_name = "consumption_ev",
  tz = tz_local
)

pvgis_hourly_east_raw <- import_pvgis_hourly(
  path = path_pvgis,
  tz = tz_local,
  source_name = "pv_east"
)

pvgis_hourly_west_raw <- import_pvgis_hourly(
  path = path_pvgis_2,
  tz = tz_local,
  source_name = "pv_west"
)

# Gemeinsamer Rohdatensatz beider PV-Anlagen, noch ohne Multiplikator.
# Dieser Datensatz dient vor allem fuer Kontrollen der Zeitspanne.
pvgis_hourly_raw <- bind_rows(
  pvgis_hourly_east_raw,
  pvgis_hourly_west_raw
)

# ------------------------------------------------------------
# 3) Aufbereitung auf einheitliches 15-Minuten-Raster
# ------------------------------------------------------------

consumption_total_15min <- consumption_total_raw |>
  align_to_15min_grid(
    time_col = "Time",
    value_col = "Value_kWh",
    source_name = "consumption_total",
    tz = tz_local
  ) |>
  rename(ConsumptionTotal_kWh = Value_kWh)

consumption_ev_15min <- consumption_ev_raw |>
  align_to_15min_grid(
    time_col = "Time",
    value_col = "Value_kWh",
    source_name = "consumption_ev",
    tz = tz_local
  ) |>
  rename(ConsumptionEV_kWh = Value_kWh)

production_east_15min <- pvgis_hourly_east_raw |>
  split_pvgis_hourly_to_15min()

production_west_15min <- pvgis_hourly_west_raw |>
  split_pvgis_hourly_to_15min()

# Beide PV-Anlagen werden addiert und anschliessend mit 9 multipliziert,
# da 9 gleiche Daecher vorhanden sind.
# Ab hier wird die PV-Produktion als eine gemeinsame Anlage behandelt.
production_15min <- combine_pv_systems_15min(
  production_east_15min,
  production_west_15min,
  multiplier = pv_roof_multiplier,
  source_name = "pv_production_total"
)

# ------------------------------------------------------------
# 4) Gemeinsame Tabelle fuer genau ein Jahr erstellen
# ------------------------------------------------------------

# Ziel:
# - Alle Datenquellen werden auf ein typisches 15-Minuten-Jahresprofil gebracht.
# - Das echte Jahr der Originaldaten wird ignoriert.
# - Gleiche Kalender-Zeitpunkte ueber mehrere Jahre werden gemittelt.
#   Beispiel: alle Werte fuer 01.01. 00:00 werden zu einem Durchschnittswert.
# - Danach wird alles auf ein Zieljahr geschrieben.

# Standard: Das Zieljahr wird aus dem ersten Verbrauchswert genommen.
# Bei deinen Daten ist das aktuell 2025.
target_year <- year(min(consumption_total_15min$Time, na.rm = TRUE))

create_15min_year_grid <- function(target_year, tz = tz_local) {
  year_start <- ymd_hms(
    paste0(target_year, "-01-01 00:00:00"),
    tz = tz
  )
  
  year_end <- ymd_hms(
    paste0(target_year + 1, "-01-01 00:00:00"),
    tz = tz
  )
  
  tibble(
    Time = seq(
      from = year_start,
      to = year_end - minutes(15),
      by = "15 min"
    )
  ) |>
    mutate(
      Month = month(Time),
      Day = day(Time),
      Hour = hour(Time),
      Minute = minute(Time)
    )
}

make_annual_15min_profile <- function(data,
                                      value_cols,
                                      time_col = "Time") {
  # Erstellt ein durchschnittliches Jahresprofil.
  # Gruppiert wird nach Monat, Tag, Stunde und Minute.
  # Falls mehrere Jahre vorhanden sind, wird je Kalender-Zeitpunkt gemittelt.
  
  data |>
    mutate(
      Month = month(.data[[time_col]]),
      Day = day(.data[[time_col]]),
      Hour = hour(.data[[time_col]]),
      Minute = minute(.data[[time_col]])
    ) |>
    group_by(Month, Day, Hour, Minute) |>
    summarise(
      across(
        all_of(value_cols),
        ~ mean(as.numeric(.x), na.rm = TRUE)
      ),
      N_values = n(),
      YearsUsed = paste(sort(unique(year(.data[[time_col]]))), collapse = ", "),
      .groups = "drop"
    )
}

year_grid_15min <- create_15min_year_grid(
  target_year = target_year,
  tz = tz_local
)

consumption_total_profile <- consumption_total_15min |>
  make_annual_15min_profile(
    value_cols = c("ConsumptionTotal_kWh"),
    time_col = "Time"
  )

consumption_ev_profile <- consumption_ev_15min |>
  make_annual_15min_profile(
    value_cols = c("ConsumptionEV_kWh"),
    time_col = "Time"
  )

production_profile <- production_15min |>
  make_annual_15min_profile(
    value_cols = c("Production_kWh", "ProductionPower_kW"),
    time_col = "Time"
  )

energy_15min <- year_grid_15min |>
  left_join(
    production_profile |>
      select(Month, Day, Hour, Minute, Production_kWh, ProductionPower_kW),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  left_join(
    consumption_total_profile |>
      select(Month, Day, Hour, Minute, ConsumptionTotal_kWh),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  left_join(
    consumption_ev_profile |>
      select(Month, Day, Hour, Minute, ConsumptionEV_kWh),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  select(
    Time,
    Production_kWh,
    ProductionPower_kW,
    ConsumptionTotal_kWh,
    ConsumptionEV_kWh
  ) |>
  arrange(Time) |>
  mutate(
    ConsumptionTotal_kWh = replace_na(ConsumptionTotal_kWh, 0),
    ConsumptionEV_kWh = replace_na(ConsumptionEV_kWh, 0),
    Production_kWh = replace_na(Production_kWh, 0),
    ProductionPower_kW = replace_na(ProductionPower_kW, 0),
    ConsumptionWithoutEV_kWh = ConsumptionTotal_kWh - ConsumptionEV_kWh,
    NetConsumption_kWh = ConsumptionTotal_kWh - Production_kWh,
    SelfConsumptionPotential_kWh = pmin(ConsumptionTotal_kWh, Production_kWh),
    FeedInPotential_kWh = pmax(Production_kWh - ConsumptionTotal_kWh, 0),
    GridImportPotential_kWh = pmax(ConsumptionTotal_kWh - Production_kWh, 0)
  )

# Kontrolltabelle: Wie viele Originalwerte je Kalender-Zeitpunkt vorhanden waren?
# Das hilft zu sehen, wo Durchschnitte aus mehreren Jahren entstanden sind.
profile_coverage <- bind_rows(
  consumption_total_profile |>
    transmute(Source = "Consumption total", N_values, YearsUsed),
  consumption_ev_profile |>
    transmute(Source = "Consumption EV", N_values, YearsUsed),
  production_profile |>
    transmute(Source = "PV production", N_values, YearsUsed)
) |>
  group_by(Source, N_values, YearsUsed) |>
  summarise(
    Slots = n(),
    .groups = "drop"
  ) |>
  arrange(Source, N_values, YearsUsed)

print(profile_coverage)

# ------------------------------------------------------------
# 5) Zeitspannen der einzelnen Dateien pruefen
# ------------------------------------------------------------

get_time_span <- function(data,
                          time_col = "Time",
                          source_name = "unknown") {
  time_values <- data[[time_col]]
  time_values <- time_values[!is.na(time_values)]
  
  if (length(time_values) == 0) {
    return(tibble(
      Source = source_name,
      Rows = nrow(data),
      FirstTime = as.POSIXct(NA, tz = tz_local),
      LastTime = as.POSIXct(NA, tz = tz_local),
      DaysCovered = NA_real_,
      YearsCovered = NA_character_
    ))
  }
  
  first_time <- min(time_values, na.rm = TRUE)
  last_time <- max(time_values, na.rm = TRUE)
  
  tibble(
    Source = source_name,
    Rows = nrow(data),
    FirstTime = first_time,
    LastTime = last_time,
    DaysCovered = as.numeric(difftime(last_time, first_time, units = "days")),
    YearsCovered = paste(sort(unique(year(time_values))), collapse = ", ")
  )
}

time_spans_raw <- bind_rows(
  get_time_span(
    data = consumption_total_raw,
    time_col = "Time",
    source_name = "Raw total consumption"
  ),
  get_time_span(
    data = consumption_ev_raw,
    time_col = "Time",
    source_name = "Raw EV consumption"
  ),
  get_time_span(
    data = pvgis_hourly_east_raw,
    time_col = "Time_hour",
    source_name = "Raw PVGIS production east hourly"
  ),
  get_time_span(
    data = pvgis_hourly_west_raw,
    time_col = "Time_hour",
    source_name = "Raw PVGIS production west hourly"
  ),
  get_time_span(
    data = production_east_15min,
    time_col = "Time",
    source_name = "Prepared PV production east 15min"
  ),
  get_time_span(
    data = production_west_15min,
    time_col = "Time",
    source_name = "Prepared PV production west 15min"
  ),
  get_time_span(
    data = production_15min,
    time_col = "Time",
    source_name = "Prepared PV production total 15min x9"
  ),
  get_time_span(
    data = energy_15min,
    time_col = "Time",
    source_name = "Combined prepared 15min data"
  )
)

print(time_spans_raw)

# Optional als CSV speichern
# write_csv(time_spans_raw, "output/time_spans_raw.csv")

# ------------------------------------------------------------
# 5b) Jahresprofil pruefen
# ------------------------------------------------------------

expected_rows_one_year <- nrow(year_grid_15min)
actual_rows_energy <- nrow(energy_15min)

annual_profile_check <- tibble(
  Check = c(
    "Target year",
    "Expected 15min rows",
    "Actual 15min rows",
    "First timestamp",
    "Last timestamp",
    "Total days"
  ),
  Value = c(
    as.character(target_year),
    as.character(expected_rows_one_year),
    as.character(actual_rows_energy),
    as.character(min(energy_15min$Time, na.rm = TRUE)),
    as.character(max(energy_15min$Time, na.rm = TRUE)),
    as.character(as.numeric(difftime(
      max(energy_15min$Time, na.rm = TRUE),
      min(energy_15min$Time, na.rm = TRUE),
      units = "days"
    )))
  )
)

print(annual_profile_check)

if (actual_rows_energy != expected_rows_one_year) {
  warning(
    "Die kombinierte 15-Minuten-Tabelle hat nicht die erwartete Anzahl Zeilen fuer ein Jahr."
  )
}

# Fehlende Werte im Jahresprofil pruefen.
# Nach replace_na() sind die Analyse-Spalten zwar 0,
# diese Kontrolle zeigt aber, wie viele Kalender-Zeitpunkte in den Profilen
# vor dem Auffuellen nicht vorhanden waren.

missing_profile_slots <- year_grid_15min |>
  left_join(
    production_profile |>
      mutate(HasProductionProfile = TRUE) |>
      select(Month, Day, Hour, Minute, HasProductionProfile),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  left_join(
    consumption_total_profile |>
      mutate(HasConsumptionTotalProfile = TRUE) |>
      select(Month, Day, Hour, Minute, HasConsumptionTotalProfile),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  left_join(
    consumption_ev_profile |>
      mutate(HasConsumptionEVProfile = TRUE) |>
      select(Month, Day, Hour, Minute, HasConsumptionEVProfile),
    by = c("Month", "Day", "Hour", "Minute")
  ) |>
  summarise(
    MissingProductionSlots = sum(is.na(HasProductionProfile)),
    MissingConsumptionTotalSlots = sum(is.na(HasConsumptionTotalProfile)),
    MissingConsumptionEVSlots = sum(is.na(HasConsumptionEVProfile))
  )

print(missing_profile_slots)

# ------------------------------------------------------------
# 5c) Parsing-Probleme der CSV-Dateien anzeigen
# ------------------------------------------------------------

# Wenn read_csv() eine Warnung zu parsing issues ausgibt,
# kann man mit problems() nachsehen, welche Zeilen/Spalten betroffen sind.
# Die PVGIS-Dateien werden hier bewusst ueber read_pvgis_data_lines() eingelesen,
# damit Fusszeilen/Metadaten am Dateiende ignoriert werden.

consumption_total_import_check <- read_csv(
  path_consumption_total,
  show_col_types = FALSE
)

consumption_ev_import_check <- read_csv(
  path_consumption_ev,
  show_col_types = FALSE
)

pvgis_east_import_check <- read_pvgis_data_lines(path_pvgis)
pvgis_west_import_check <- read_pvgis_data_lines(path_pvgis_2)

parsing_problems <- bind_rows(
  problems(consumption_total_import_check) |>
    mutate(Source = "Raw total consumption"),
  problems(consumption_ev_import_check) |>
    mutate(Source = "Raw EV consumption"),
  problems(pvgis_east_import_check) |>
    mutate(Source = "Raw PVGIS production east"),
  problems(pvgis_west_import_check) |>
    mutate(Source = "Raw PVGIS production west")
) |>
  select(Source, everything())

print(parsing_problems)

# Optional speichern
# write_csv(parsing_problems, "output/parsing_problems.csv")

# ------------------------------------------------------------
# 6) Erste Kontrollen
# ------------------------------------------------------------


check_totals <- tibble(
  Metric = c(
    "Total consumption",
    "EV consumption",
    "PV production",
    "Consumption without EV",
    "Potential self-consumption",
    "Potential feed-in",
    "Potential grid import"
  ),
  Value_kWh = c(
    sum(energy_15min$ConsumptionTotal_kWh, na.rm = TRUE),
    sum(energy_15min$ConsumptionEV_kWh, na.rm = TRUE),
    sum(energy_15min$Production_kWh, na.rm = TRUE),
    sum(energy_15min$ConsumptionWithoutEV_kWh, na.rm = TRUE),
    sum(energy_15min$SelfConsumptionPotential_kWh, na.rm = TRUE),
    sum(energy_15min$FeedInPotential_kWh, na.rm = TRUE),
    sum(energy_15min$GridImportPotential_kWh, na.rm = TRUE)
  )
)

print(check_totals)

# ------------------------------------------------------------
# 7) Aufbereitete Daten speichern
# ------------------------------------------------------------

# Ausgabeordner erstellen, falls er noch nicht existiert
if (!dir.exists("output")) {
  dir.create("output")
}

# Hauptdatei fuer die Weiterverarbeitung in neuen Skripten
write_csv(
  energy_15min,
  "output/energy_15min_prepared.csv"
)

# Kontrolltabellen ebenfalls speichern
write_csv(
  check_totals,
  "output/check_totals.csv"
)

write_csv(
  time_spans_raw,
  "output/time_spans_raw.csv"
)

write_csv(
  annual_profile_check,
  "output/annual_profile_check.csv"
)

write_csv(
  missing_profile_slots,
  "output/missing_profile_slots.csv"
)

# Optional: Parsing-Probleme speichern, falls vorhanden
write_csv(
  parsing_problems,
  "output/parsing_problems.csv"
)

message("Dateien wurden im Ordner 'output' gespeichert.")
message("Hauptdatei: output/energy_15min_prepared.csv")

