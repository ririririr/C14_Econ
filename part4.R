#' # Part 4 — International energy prices and domestic inflation
#'
#' This executable R file is written in `knitr::spin()` Markdown style. It
#' creates the five report figures in `graphs/`. All inputs are read from
#' `DATASETS/`.
#' Run with `Rscript part4.R` or source it in RStudio; plots are also printed
#' to the Plots pane in an interactive session.
#'
#+ setup, include=FALSE
required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the following packages before running: ",
       paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

find_project_root <- function() {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  starts <- character()
  if (length(script_arg)) {
    starts <- c(starts, dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
  }
  starts <- c(starts, getwd())

  for (start in unique(starts)) {
    current <- normalizePath(start, mustWork = FALSE)
    repeat {
      if (file.exists(file.path(current, "DATASETS", "inflation.xlsx"))) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }
  stop("Could not locate the project folder containing DATASETS/inflation.xlsx.")
}

project_dir <- find_project_root()
data_dir <- file.path(project_dir, "DATASETS")
output_dir <- file.path(project_dir, "graphs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
input_file <- file.path(data_dir, "inflation.xlsx")

raw_monthly <- read_excel(
  input_file,
  sheet = "Monthly series",
  .name_repair = "minimal"
)
if (ncol(raw_monthly) < 2L) stop("The Monthly series sheet has no month columns.")

series_labels <- as.character(raw_monthly[[1]])
month_dates <- as.Date(substr(names(raw_monthly)[-1], 1, 10))
monthly <- as.data.frame(t(as.matrix(raw_monthly[-1])), check.names = FALSE)
names(monthly) <- series_labels
monthly$date <- month_dates
monthly <- monthly %>%
  mutate(across(-date, as.numeric)) %>%
  arrange(date)

global_name <- "IMF global energy index YoY change (%)"
india_energy_name <- "India energy-related CPI proxy YoY change (%)"
saudi_energy_name <- "Saudi electricity, gas and other fuels CPI YoY change (%)"
saudi_food_name <- "Saudi food CPI YoY change (%)"
required_series <- c(global_name, india_energy_name, saudi_energy_name, saudi_food_name)
missing_series <- setdiff(required_series, names(monthly))
if (length(missing_series)) {
  stop("Missing expected series in inflation.xlsx: ",
       paste(missing_series, collapse = "; "))
}

make_pair <- function(x_name, y_name, start_date, end_date) {
  pair <- monthly %>%
    transmute(
      date,
      x = .data[[x_name]],
      y = .data[[y_name]]
    ) %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date)) %>%
    arrange(date)

  if (!nrow(pair)) stop("No observations in requested period: ", x_name, " / ", y_name)
  if (anyNA(pair$x) || anyNA(pair$y)) {
    stop("Missing monthly values in requested period for: ", x_name, " / ", y_name)
  }
  expected_dates <- seq.Date(min(pair$date), max(pair$date), by = "month")
  if (!identical(as.Date(pair$date), expected_dates)) {
    stop("The observations are not a complete monthly sequence for: ", x_name, " / ", y_name)
  }
  pair
}

standardize <- function(x) {
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) stop("Cannot standardize a constant or incomplete series.")
  (x - mean(x)) / s
}

make_changes <- function(pair) {
  pair %>%
    arrange(date) %>%
    mutate(dx = x - dplyr::lag(x), dy = y - dplyr::lag(y)) %>%
    filter(!is.na(dx), !is.na(dy)) %>%
    transmute(date, x = dx, y = dy)
}

# Paired moving-block bootstrap for a simultaneous max-t band over lags 0:K.
# Positive k pairs the global change at month t with the CPI change at month t+k.
lagged_max_t_band <- function(changes, max_lag = 12L, block_length = 12L,
                              replicates = 5000L, seed = 20261119L) {
  x <- changes$x
  y <- changes$y
  n <- length(x)
  if (n <= max_lag + 2L) stop("Too few observations for the requested lag range.")
  if (block_length < 1L) stop("block_length must be positive.")

  correlation_at_lags <- function(x_values, y_values) {
    n_pair <- min(length(x_values), length(y_values))
    vapply(0:max_lag, function(k) {
      ix <- seq_len(n_pair - k)
      stats::cor(x_values[ix], y_values[ix + k])
    }, numeric(1))
  }

  estimate <- correlation_at_lags(x, y)
  L <- min(as.integer(block_length), n)
  blocks_needed <- ceiling(n / L)
  max_start <- n - L + 1L
  bootstrap <- matrix(NA_real_, nrow = replicates, ncol = max_lag + 1L)

  set.seed(seed)
  for (b in seq_len(replicates)) {
    starts <- sample.int(max_start, blocks_needed, replace = TRUE)
    index <- unlist(lapply(starts, function(s) seq.int(s, s + L - 1L)),
                    use.names = FALSE)[seq_len(n)]
    bootstrap[b, ] <- correlation_at_lags(x[index], y[index])
  }

  standard_errors <- apply(bootstrap, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(standard_errors)) || any(standard_errors == 0)) {
    stop("Could not estimate a finite bootstrap standard error at every lag.")
  }
  standardized_deviations <- sweep(bootstrap, 2, estimate, FUN = "-")
  standardized_deviations <- sweep(standardized_deviations, 2, standard_errors, FUN = "/")
  max_t <- apply(abs(standardized_deviations), 1, max, na.rm = TRUE)
  critical_value <- unname(stats::quantile(max_t, probs = 0.90, type = 7, na.rm = TRUE))

  tibble(
    lag_months = 0:max_lag,
    correlation = estimate,
    bootstrap_se = standard_errors,
    simultaneous_low = pmax(-1, estimate - critical_value * standard_errors),
    simultaneous_high = pmin(1, estimate + critical_value * standard_errors),
    n_pairs = n - (0:max_lag),
    confidence_level = 0.90,
    critical_value = critical_value
  )
}

plot_lag_band <- function(result, title, subtitle, x_label, caption) {
  peak <- result %>% slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE)
  ggplot(result, aes(x = lag_months, y = correlation)) +
    geom_ribbon(
      aes(ymin = simultaneous_low, ymax = simultaneous_high),
      fill = "#8AB7C0",
      alpha = 0.36
    ) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.45) +
    geom_vline(xintercept = peak$lag_months, colour = "#C04B3D",
               linetype = "dashed", linewidth = 0.45) +
    geom_line(colour = "#087E8B", linewidth = 0.85) +
    geom_point(colour = "#087E8B", size = 2) +
    geom_point(data = peak, colour = "#C04B3D", size = 3) +
    scale_x_continuous(breaks = sort(unique(result$lag_months))) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = "Correlation of monthly changes in YoY rates",
      caption = caption
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(colour = "#4B5563"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 8, colour = "#555555", hjust = 0)
    )
}

save_plot <- function(plot, filename, width = 10, height = 6.5) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "white"
  )
  if (interactive()) print(plot)
}
#+

#' ## Graph 1 — Standardized India energy inflation and global energy prices
#'
#' Each series is shown in standard deviations from its mean. To keep the
#' long-run Indian series comparable across the 2012 CPI rebasing break, this
#' graph uses the existing pre-break energy proxy through December 2025.
#+ india-standardized-series
india_long_pair <- make_pair(
  global_name,
  india_energy_name,
  "2012-01-01",
  "2025-12-01"
)
india_long <- bind_rows(
  tibble(date = india_long_pair$date,
         series = "Global energy index",
         value = india_long_pair$x),
  tibble(date = india_long_pair$date,
         series = "India energy CPI proxy",
         value = india_long_pair$y)
) %>%
  group_by(series) %>%
  mutate(standard_deviations = standardize(value)) %>%
  ungroup()

india_standardized_plot <- ggplot(
  india_long,
  aes(x = date, y = standard_deviations, colour = series, group = series)
) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(
    "Global energy index" = "#D28A00",
    "India energy CPI proxy" = "#087E8B"
  )) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Global energy prices and India energy inflation",
    subtitle = "Standardized YoY rates, January 2012–December 2025",
    x = NULL,
    y = "Standard deviations from each series' mean",
    colour = NULL,
    caption = paste(
      "India uses the pre-rebase energy CPI proxy through 2025.",
      "The separate 2024-base CPI group begins in 2026 and is not spliced into this history."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 8, colour = "#555555", hjust = 0)
  )
save_plot(india_standardized_plot, "india_prelag.png")
#+

#' ## Graph 2 — India lagged correlations with simultaneous 90% intervals
#'
#' Correlations are calculated between monthly changes in the two YoY rates.
#' The paired moving-block bootstrap uses a max-t band across lags 0–12.
#+ india-lagged-correlation
india_changes <- make_changes(india_long_pair)
india_lag_result <- lagged_max_t_band(
  india_changes,
  max_lag = 12L,
  block_length = 12L,
  replicates = 5000L,
  seed = 20261119L
)
india_peak <- india_lag_result %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE)
india_lag_plot <- plot_lag_band(
  india_lag_result,
  title = "India energy CPI and global energy: lagged correlations",
  subtitle = sprintf(
    "90%% simultaneous max-t block-bootstrap band; strongest |r| at k = %d (r = %.2f)",
    india_peak$lag_months,
    india_peak$correlation
  ),
  x_label = "Months after the global energy-rate change",
  caption = paste(
    "Positive lag k pairs the global energy-rate change in month t with",
    "the India energy CPI-rate change in month t+k. Sample ends December 2025",
    "to avoid crossing the Indian CPI rebasing break."
  )
)
save_plot(india_lag_plot, "india_lagf.png")
#+

#' ## Graph 3 — India series aligned at the selected lag
#'
#' The lag is selected from the monthly-change correlation function above.
#' For positive k, the global energy rate at t−k is paired with India's CPI rate
#' at t. This uses `lag()` on the global series, not a future value from `lead()`.
#+ india-lag-adjusted-series
india_best_lag <- as.integer(india_peak$lag_months)
india_aligned <- india_long_pair %>%
  mutate(global_energy_prior = dplyr::lag(x, n = india_best_lag)) %>%
  filter(!is.na(global_energy_prior)) %>%
  transmute(
    date,
    `Global energy index (earlier)` = global_energy_prior,
    `India energy CPI proxy` = y
  )
india_aligned_long <- india_aligned %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  group_by(series) %>%
  mutate(standard_deviations = standardize(value)) %>%
  ungroup()
india_lagged_line_plot <- ggplot(
  india_aligned_long,
  aes(x = date, y = standard_deviations, colour = series, group = series)
) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(
    "Global energy index (earlier)" = "#D28A00",
    "India energy CPI proxy" = "#087E8B"
  )) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "India energy inflation and global energy prices at the selected lag",
    subtitle = sprintf(
      "Global energy leads the Indian CPI proxy by %d month(s); standardized YoY rates",
      india_best_lag
    ),
    x = NULL,
    y = "Standard deviations from each aligned series' mean",
    colour = NULL,
    caption = paste(
      sprintf("At each date t, global energy is from t−%d and India CPI is from t.",
              india_best_lag),
      "The selected lag is based on correlations of monthly changes in YoY rates."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 8, colour = "#555555", hjust = 0)
  )
save_plot(india_lagged_line_plot, "india_postlag.png")
#+

#' ## Graph 4 — Saudi Arabia energy CPI and global energy prices, raw YoY rates
#'
#' This recent-period graph uses year-on-year percentage changes on the y-axis,
#' not standardized values. The two series share a common scale so their
#' magnitudes remain visible as reported.
#+ saudi-energy-yoy
saudi_energy_pair <- make_pair(
  global_name,
  saudi_energy_name,
  "2025-01-01",
  "2026-09-01"
)
saudi_energy_long <- bind_rows(
  tibble(date = saudi_energy_pair$date,
         series = "IMF global energy index",
         yoy_percent = saudi_energy_pair$x),
  tibble(date = saudi_energy_pair$date,
         series = "Saudi electricity, gas and other fuels CPI",
         yoy_percent = saudi_energy_pair$y)
)
saudi_energy_yoy_plot <- ggplot(
  saudi_energy_long,
  aes(x = date, y = yoy_percent, colour = series, group = series)
) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c(
    "IMF global energy index" = "#D28A00",
    "Saudi electricity, gas and other fuels CPI" = "#087E8B"
  )) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    limits = as.Date(c("2025-01-01", "2026-09-01"))
  ) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Saudi Arabia energy CPI and global energy prices",
    subtitle = "Year-on-year percentage changes, January 2025–September 2026",
    x = NULL,
    y = "YoY change (%)",
    colour = NULL,
    caption = paste(
      "Saudi CPI group: electricity, gas and other fuels.",
      "Both series are plotted as raw YoY rates; neither is standardized."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.caption = element_text(size = 8, colour = "#555555", hjust = 0)
  )
save_plot(saudi_energy_yoy_plot, "saudi_energy_line.png")
#+

#' ## Graph 5 — Saudi food CPI and global energy lagged correlations
#'
#' Correlations are calculated between monthly changes in the YoY rates, with
#' the same 90% simultaneous max-t paired moving-block bootstrap over lags 0–12.
#+ saudi-food-lagged-correlation
saudi_food_pair <- make_pair(
  global_name,
  saudi_food_name,
  "2012-01-01",
  "2026-09-01"
)
saudi_food_changes <- make_changes(saudi_food_pair)
saudi_food_lag_result <- lagged_max_t_band(
  saudi_food_changes,
  max_lag = 12L,
  block_length = 12L,
  replicates = 5000L,
  seed = 20261120L
)
saudi_food_peak <- saudi_food_lag_result %>%
  slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE)
saudi_food_lag_plot <- plot_lag_band(
  saudi_food_lag_result,
  title = "Saudi food CPI and global energy: lagged correlations",
  subtitle = sprintf(
    "90%% simultaneous max-t block-bootstrap band; strongest |r| at k = %d (r = %.2f)",
    saudi_food_peak$lag_months,
    saudi_food_peak$correlation
  ),
  x_label = "Months after the global energy-rate change",
  caption = paste(
    "Positive k: global energy change at t; Saudi food CPI change at t+k.",
    "Sample: Jan 2012–Sep 2026. Band is simultaneous across lags 0–12."
  )
)
save_plot(saudi_food_lag_plot, "saudi-lagf.png")
#+

message("Part 4 graphs saved in: ", output_dir)
