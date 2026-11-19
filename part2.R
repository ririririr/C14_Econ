#' # Part 2 — Top-decile wealth shares
#'
#' This executable R script is also formatted for `knitr::spin()`: the `#'`
#' lines are Markdown text and `#+` lines mark code chunks. Running the file
#' with `Rscript part2.R` or sourcing it in RStudio creates both figures in
#' `graphs/`; in an interactive R session, the plots also appear
#' in the Plots pane.
#'
#+ setup, include=FALSE
required_packages <- c("readr", "dplyr", "ggplot2", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the following packages before running: ",
       paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
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
      if (file.exists(file.path(current, "DATASETS", "india-peer.csv")) &&
          file.exists(file.path(current, "DATASETS", "saudi-comp.csv"))) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }
  stop("Could not locate the project folder containing DATASETS/india-peer.csv.")
}

project_dir <- find_project_root()
data_dir <- file.path(project_dir, "DATASETS")
output_dir <- file.path(project_dir, "graphs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_wealth_share <- function(filename) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("Missing input file: ", path)

  dat <- read_delim(
    path,
    delim = ";",
    skip = 1,
    trim_ws = TRUE,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  names(dat) <- trimws(names(dat))

  required <- c("Country", "Year", "Value", "Data quality")
  if (!all(required %in% names(dat))) {
    stop("Unexpected columns in ", filename, ". Found: ",
         paste(names(dat), collapse = ", "))
  }

  dat <- dat %>%
    transmute(
      Country = str_trim(Country),
      Year = as.integer(Year),
      Share = 100 * as.numeric(Value),
      Quality = as.integer(`Data quality`)
    ) %>%
    arrange(Country, Year)

  if (anyNA(dat$Country) || anyNA(dat$Year) || anyNA(dat$Share)) {
    stop("Missing country, year, or wealth-share values in ", filename, ".")
  }

  dat
}

make_peer_plot <- function(dat, focal_country, title, input_file) {
  countries <- unique(dat$Country)
  if (!focal_country %in% countries) {
    stop("Could not find ", focal_country, " in ", input_file, ".")
  }

  # Highlight the focal country while keeping peer colours distinct.
  peer_names <- setdiff(countries, focal_country)
  peer_palette <- setNames(
    rep(c("#4E79A7", "#59A14F", "#9C755F", "#B07AA1", "#76B7B2", "#F28E2B"),
        length.out = length(peer_names)),
    peer_names
  )
  palette <- c(peer_palette, setNames("#D62728", focal_country))

  ggplot(
    dat,
    aes(x = Year, y = Share, colour = Country, group = Country,
        linewidth = Country == focal_country)
  ) +
    geom_line(lineend = "round") +
    scale_colour_manual(values = palette) +
    scale_linewidth_manual(values = c(`FALSE` = 0.85, `TRUE` = 1.55), guide = "none") +
    scale_x_continuous(
      breaks = seq(1981, 2021, by = 5),
      limits = c(1981, 2024),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      limits = c(40, 75),
      breaks = seq(40, 75, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = "Top 10% share of net personal wealth, 1981–2024",
      x = NULL,
      y = "Share of total net personal wealth",
      colour = NULL,
      caption = paste0(
        "Source: World Inequality Database (WID), ", input_file,
        ". Most observations have WID data-quality grade 0 (modelled wealth distribution)."
      )
    ) +
    guides(colour = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(colour = "#4B5563"),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.caption = element_text(size = 8, colour = "#555555", hjust = 0),
      plot.margin = margin(12, 16, 10, 12)
    )
}
#+

#' ## Graph 1 — India and South Asian peers
#'
#' Peer series: Bangladesh, Nepal, Pakistan, and Sri Lanka.
#+ india-wealth-share
india_data <- read_wealth_share("india-peer.csv")
india_plot <- make_peer_plot(
  india_data,
  focal_country = "India",
  title = "India and South Asian peers",
  input_file = "india-peer.csv"
)
ggsave(
  file.path(output_dir, "india_top10.png"),
  plot = india_plot,
  width = 12,
  height = 7.1,
  dpi = 220,
  bg = "white"
)
if (interactive()) print(india_plot)
#+

#' ## Graph 2 — Saudi Arabia and regional peers
#'
#' Peer series: Bahrain, Iran, Oman, Qatar, Turkey, and the United Arab Emirates.
#+ saudi-wealth-share
saudi_data <- read_wealth_share("saudi-comp.csv")
saudi_plot <- make_peer_plot(
  saudi_data,
  focal_country = "Saudi Arabia",
  title = "Saudi Arabia and regional peers",
  input_file = "saudi-comp.csv"
)
ggsave(
  file.path(output_dir, "saudi_top10.png"),
  plot = saudi_plot,
  width = 12,
  height = 7.1,
  dpi = 220,
  bg = "white"
)
if (interactive()) print(saudi_plot)
#+

message("Part 2 graphs saved in: ", output_dir)
