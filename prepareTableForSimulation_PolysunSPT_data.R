library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# 0) Einstellungen
# ------------------------------------------------------------

tz_local <- "Europe/Zurich"

# Verbrauchsdaten
path_consumption_total <- "Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv"
path_consumption_ev    <- "Verbrauch/Verbrauch_E Mobility_2025.csv"

# Neue PV-Simulationsdaten
# Diese Dateien ersetzen die alten PVGIS-Dateien.
# Wichtig: Die Werte werden NICHT mehr skaliert.
path_pv_simulations <- c(
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_1_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_1_West_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_2_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_3_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_4_East_15Minutes.csv",
  "PV_Production_Simulation_Data/PolysunSPT/Giesserei_Neuhegi_West_4_West_15Minutes.csv"
)

# Spaltennamen der Verbrauchsdateien
consumption_time_col <- "Timestamp"
consumption_value_col <- "Volume"

ev_time_col <- "Timestamp"
ev_value_col <- "Volume"

# Spalte aus den Simulationsdateien, die als PV-Produktion verwendet wird.
# In deinem Beispiel ist das die AC-Produktion.
pv_simulation_yield_col <- "Yield Photovoltaics AC [kWh]"

# Falls die AC-Spalte nicht vorhanden ist, kann alternativ DC verwendet werden.
pv_simulation_yield_fallback_col <- "Yield Photovoltaics DC [kWh]"

# Datumsformat der PV-Simulationsdateien:
# Bei Schweizer/DE-Exporten ist meistens "dm" korrekt.
# Falls deine Datei Monat/Tag verwendet, auf "md" ändern.
pv_simulation_date_order <- "dm"

# ------------------------------------------------------------
# 1) Hilfsfunktionen für den Import
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
      "mdY HMS", "mdY HM", "mdY",
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
    stop("Zeitspalte nicht gefunden: ", time_col, " in Datei: ", path)
  }
  
  if (!value_col %in% names(data_raw)) {
    stop("Wertspalte nicht gefunden: ", value_col, " in Datei: ", path)
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

parse_pv_simulation_time <- function(date_value,
                                     time_value,
                                     target_year,
                                     date_order = "dm",
                                     tz = tz_local) {
  
  datetime_text <- paste(date_value, target_year, time_value)
  
  if (date_order == "dm") {
    parsed <- parse_date_time(
      datetime_text,
      orders = c(
        "dm Y I:M p",
        "dm Y H:M",
        "dm Y HMS",
        "dm Y HM"
      ),
      tz = tz
    )
  } else if (date_order == "md") {
    parsed <- parse_date_time(
      datetime_text,
      orders = c(
        "md Y I:M p",
        "md Y H:M",
        "md Y HMS",
        "md Y HM"
      ),
      tz = tz
    )
  } else {
    stop("date_order muss entweder 'dm' oder 'md' sein.")
  }
  
  as.POSIXct(parsed, tz = tz)
}
read_pv_simulation_file <- function(path) {
  
  first_bytes <- readBin(path, what = "raw", n = 20)
  
  is_utf16le <- length(first_bytes) >= 2 &&
    (
      identical(first_bytes[1:2], as.raw(c(0xFF, 0xFE))) ||
        any(first_bytes == as.raw(0x00))
    )
  
  is_utf16be <- length(first_bytes) >= 2 &&
    identical(first_bytes[1:2], as.raw(c(0xFE, 0xFF)))
  
  if (is_utf16le) {
    lines <- readLines(
      file(path, open = "r", encoding = "UTF-16LE"),
      warn = FALSE
    )
    
    text_utf8 <- paste(lines, collapse = "\n")
    
    data_raw <- read_delim(
      file = I(text_utf8),
      delim = ";",
      locale = locale(decimal_mark = ".", grouping_mark = "'"),
      trim_ws = TRUE,
      show_col_types = FALSE
    )
    
  } else if (is_utf16be) {
    lines <- readLines(
      file(path, open = "r", encoding = "UTF-16BE"),
      warn = FALSE
    )
    
    text_utf8 <- paste(lines, collapse = "\n")
    
    data_raw <- read_delim(
      file = I(text_utf8),
      delim = ";",
      locale = locale(decimal_mark = ".", grouping_mark = "'"),
      trim_ws = TRUE,
      show_col_types = FALSE
    )
    
  } else {
    data_raw <- read_delim(
      file = path,
      delim = ";",
      locale = locale(decimal_mark = ".", grouping_mark = "'"),
      trim_ws = TRUE,
      show_col_types = FALSE
    )
  }
  
  # Leere Spalten entfernen, die durch ein abschliessendes Semikolon entstehen können
  data_raw |>
    select(where(~ !all(is.na(.x))))
}


import_pv_simulation_15min <- function(path,
                                       target_year,
                                       yield_col = pv_simulation_yield_col,
                                       fallback_yield_col = pv_simulation_yield_fallback_col,
                                       date_order = pv_simulation_date_order,
                                       tz = tz_local,
                                       source_name = NULL) {
  
  if (is.null(source_name)) {
    source_name <- basename(path)
  }
  
  data_raw <- read_pv_simulation_file(path)
  
  if (!"Date" %in% names(data_raw)) {
    stop(
      "Spalte 'Date' nicht gefunden in Datei: ", path,
      "\nGefundene Spalten:\n",
      paste(names(data_raw), collapse = "\n")
    )
  }
  
  if (!"Time" %in% names(data_raw)) {
    stop(
      "Spalte 'Time' nicht gefunden in Datei: ", path,
      "\nGefundene Spalten:\n",
      paste(names(data_raw), collapse = "\n")
    )
  }
  
  if (yield_col %in% names(data_raw)) {
    selected_yield_col <- yield_col
  } else if (fallback_yield_col %in% names(data_raw)) {
    selected_yield_col <- fallback_yield_col
    warning(
      "Spalte '", yield_col, "' nicht gefunden. Verwende stattdessen '",
      fallback_yield_col, "' in Datei: ", path
    )
  } else {
    stop(
      "Keine PV-Ertragsspalte gefunden. Erwartet wurde '",
      yield_col, "' oder '", fallback_yield_col, "' in Datei: ", path,
      "\nGefundene Spalten:\n",
      paste(names(data_raw), collapse = "\n")
    )
  }
  
  data_raw |>
    transmute(
      Time = parse_pv_simulation_time(
        date_value = Date,
        time_value = Time,
        target_year = target_year,
        date_order = date_order,
        tz = tz
      ),
      Production_kWh = as.numeric(.data[[selected_yield_col]]),
      Source = source_name
    ) |>
    filter(!is.na(Time)) |>
    mutate(
      Time = floor_date(Time, unit = "15 minutes"),
      Production_kWh = replace_na(Production_kWh, 0),
      ProductionPower_kW = Production_kWh / 0.25
    ) |>
    group_by(Time, Source) |>
    summarise(
      Production_kWh = sum(Production_kWh, na.rm = TRUE),
      ProductionPower_kW = sum(ProductionPower_kW, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Time)
}
combine_pv_simulations_15min <- function(pv_data_list,
                                         source_name = "pv_production_total") {
  bind_rows(pv_data_list) |>
    group_by(Time) |>
    summarise(
      Production_kWh = sum(Production_kWh, na.rm = TRUE),
      ProductionPower_kW = sum(ProductionPower_kW, na.rm = TRUE),
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

# ------------------------------------------------------------
# 2) Import der Verbrauchsdaten
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

# Zieljahr wird aus den Verbrauchsdaten genommen.
# Bei deinen aktuellen Verbrauchsdaten ist das vermutlich 2025.
target_year <- year(min(consumption_total_raw$Time, na.rm = TRUE))

# ------------------------------------------------------------
# 3) Import der neuen PV-Simulationsdaten
# ------------------------------------------------------------

pv_simulation_files_missing <- path_pv_simulations[!file.exists(path_pv_simulations)]

if (length(pv_simulation_files_missing) > 0) {
  stop(
    "Folgende PV-Simulationsdateien wurden nicht gefunden:\n",
    paste(pv_simulation_files_missing, collapse = "\n")
  )
}

pv_simulation_list <- map(
  path_pv_simulations,
  ~ import_pv_simulation_15min(
    path = .x,
    target_year = target_year,
    yield_col = pv_simulation_yield_col,
    fallback_yield_col = pv_simulation_yield_fallback_col,
    date_order = pv_simulation_date_order,
    tz = tz_local,
    source_name = basename(.x)
  )
)

pv_simulation_raw <- bind_rows(pv_simulation_list)

# Keine Skalierung.
# Die PV-Dateien werden direkt addiert.
production_15min <- combine_pv_simulations_15min(
  pv_data_list = pv_simulation_list,
  source_name = "pv_production_total"
)

# ------------------------------------------------------------
# 4) Verbrauch auf 15-Minuten-Raster bringen
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

# ------------------------------------------------------------
# 5) Gemeinsame Tabelle für genau ein Jahr erstellen
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 6) Kontrolltabellen
# ------------------------------------------------------------

profile_coverage <- bind_rows(
  consumption_total_profile |>
    transmute(Source = "Consumption total", N_values, YearsUsed),
  consumption_ev_profile |>
    transmute(Source = "Consumption EV", N_values, YearsUsed),
  production_profile |>
    transmute(Source = "PV production simulation", N_values, YearsUsed)
) |>
  group_by(Source, N_values, YearsUsed) |>
  summarise(
    Slots = n(),
    .groups = "drop"
  ) |>
  arrange(Source, N_values, YearsUsed)

print(profile_coverage)

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
    data = pv_simulation_raw,
    time_col = "Time",
    source_name = "Raw PV simulation files 15min"
  ),
  get_time_span(
    data = production_15min,
    time_col = "Time",
    source_name = "Prepared PV production total 15min no scaling"
  ),
  get_time_span(
    data = energy_15min,
    time_col = "Time",
    source_name = "Combined prepared 15min data"
  )
)

print(time_spans_raw)

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
    "Die kombinierte 15-Minuten-Tabelle hat nicht die erwartete Anzahl Zeilen für ein Jahr."
  )
}

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
# 7) Parsing-Probleme anzeigen
# ------------------------------------------------------------

consumption_total_import_check <- read_csv(
  path_consumption_total,
  show_col_types = FALSE
)

consumption_ev_import_check <- read_csv(
  path_consumption_ev,
  show_col_types = FALSE
)

pv_simulation_import_checks <- map(
  path_pv_simulations,
  ~ read_pv_simulation_file(.x)
)

parsing_problems_pv <- map2_dfr(
  pv_simulation_import_checks,
  path_pv_simulations,
  ~ problems(.x) |>
    mutate(Source = basename(.y))
)

parsing_problems <- bind_rows(
  problems(consumption_total_import_check) |>
    mutate(Source = "Raw total consumption"),
  problems(consumption_ev_import_check) |>
    mutate(Source = "Raw EV consumption"),
  parsing_problems_pv
) |>
  select(Source, everything())

print(parsing_problems)

# ------------------------------------------------------------
# 8) Gesamtsummen prüfen
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

# Zusatzkontrolle je PV-Datei
pv_totals_by_file <- pv_simulation_raw |>
  group_by(Source) |>
  summarise(
    Rows = n(),
    FirstTime = min(Time, na.rm = TRUE),
    LastTime = max(Time, na.rm = TRUE),
    Production_kWh = sum(Production_kWh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(Source)

print(pv_totals_by_file)

# ------------------------------------------------------------
# 9) Aufbereitete Daten speichern
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  energy_15min,
  "output/energy_15min_prepared.csv"
)

write_csv(
  check_totals,
  "output/check_totals.csv"
)

write_csv(
  pv_totals_by_file,
  "output/pv_totals_by_file.csv"
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

write_csv(
  parsing_problems,
  "output/parsing_problems.csv"
)

message("Dateien wurden im Ordner 'output' gespeichert.")
message("Hauptdatei: output/energy_15min_prepared.csv")
message("PV-Simulationsdaten wurden nicht skaliert, sondern direkt addiert.")