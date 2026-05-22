total_value <- function(data, value_col) {
  value_sym <- sym(value_col)
  
  data |>
    summarise(
      Total = sum(as.numeric(!!value_sym), na.rm = TRUE)
    ) |>
    pull(Total)
}

aggregate_values <- function(data,
                             time_col,
                             value_col,
                             period = c("daily", "weekly", "monthly", "yearly"),
                             tz = "Europe/Zurich",
                             week_start = 1,
                             source_name = NULL) {
  
  period <- match.arg(period)
  
  time_sym <- sym(time_col)
  value_sym <- sym(value_col)
  
  result <- data |>
    mutate(
      Time_clean = as.POSIXct(!!time_sym, tz = tz),
      Value_clean = as.numeric(!!value_sym)
    )
  
  if (period == "daily") {
    result <- result |>
      mutate(Date = as.Date(Time_clean)) |>
      group_by(Date) |>
      summarise(
        Value = sum(Value_clean, na.rm = TRUE),
        .groups = "drop"
      ) |>
      rename(Time = Date)
  }
  
  if (period == "weekly") {
    result <- result |>
      mutate(
        Time = floor_date(as.Date(Time_clean), unit = "week", week_start = week_start),
        Week = format(Time, "%G-W%V")
      ) |>
      group_by(Time, Week) |>
      summarise(
        Value = sum(Value_clean, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (period == "monthly") {
    result <- result |>
      mutate(Time = floor_date(as.Date(Time_clean), unit = "month")) |>
      group_by(Time) |>
      summarise(
        Value = sum(Value_clean, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (period == "yearly") {
    result <- result |>
      mutate(Time = floor_date(as.Date(Time_clean), unit = "year")) |>
      group_by(Time) |>
      summarise(
        Value = sum(Value_clean, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (!is.null(source_name)) {
    result <- result |>
      mutate(Source = source_name)
  }
  
  result
}


plot_values <- function(data,
                        title = "Plot",
                        x_label = "Date",
                        y_label = "Value",
                        plot_type = c("bar", "line")) {
  
  plot_type <- match.arg(plot_type)
  
  p <- ggplot(data, aes(x = Time, y = Value))
  
  if (plot_type == "bar") {
    p <- p + geom_col()
  }
  
  if (plot_type == "line") {
    p <- p + geom_line() +
      geom_point()
  }
  
  p +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
}
