# Economic Factors Data Collection
# Run this script directly in VSCode — no Pandoc/rmarkdown needed

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))

# ── Output folder ─────────────────────────────────────────────────────────────
output_dir <- file.path(getwd(), "data_economic_factors")
if (!dir.exists(output_dir)) dir.create(output_dir)
message("Saving plots to: ", output_dir)

save_plot <- function(plot, filename) {
  ggsave(file.path(output_dir, filename), plot = plot, width = 12, height = 6, dpi = 150)
}

# ── Parameters ────────────────────────────────────────────────────────────────
start_date_param  <- as.Date("2023-01-01")
end_date_param    <- Sys.Date()
write_objects     <- TRUE

# ── Libraries ─────────────────────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(skimr)
library(readr)
library(tidyr)
library(ggplot2)
library(quantmod)
library(tibble)
library(httr)
library(jsonlite)
library(here)
library(rsdmx)
library(googledrive)
library(googlesheets4)

# ── Auth & source external helpers ────────────────────────────────────────────
options(gargle_oauth_cache = "~/Library/Caches/gargle")
drive_auth(
  scopes = "https://www.googleapis.com/auth/drive",
  cache  = "~/Library/Caches/gargle"
)

temp_r_file <- tempfile(fileext = ".R")
drive_download(
  file = "external_collection.R",
  path = temp_r_file,
  overwrite = TRUE
)
source(temp_r_file)

# ── Setup ─────────────────────────────────────────────────────────────────────
start_date <- ymd(start_date_param)
end_date   <- ymd(end_date_param)

api_start       <- format(start_date, "%Y-%m")
api_end_current <- format(Sys.Date(), "%Y-%m")

options(scipen = 20)
options("getSymbols.warning4.0" = FALSE)

date_range <- tibble(start_date = seq(start_date, end_date, by = "weeks"))

# ── Business Confidence Index (BCI) ───────────────────────────────────────────
bci_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES@DF_CLI,/GBR+USA.M.BCICP...AA...H?",
  "startPeriod=", api_start,
  "&endPeriod=", api_end_current,
  "&dimensionAtObservation=AllDimensions"
)

bci_sdmx <- readSDMX(bci_url)
bci_df   <- as.data.frame(bci_sdmx)

bci_clean <- bci_df %>%
  select(TIME_PERIOD, obsValue, REF_AREA) %>%
  rename(month_date = TIME_PERIOD, bci = obsValue, country = REF_AREA) %>%
  mutate(
    month_date = as.Date(paste0(month_date, "-01")),
    bci        = as.numeric(bci),
    country    = recode(country, "GBR" = "UNITED KINGDOM", "USA" = "UNITED STATES")
  )

bci_weekly <- bci_clean %>%
  rowwise() %>%
  mutate(
    week_seq = list({
      first_day    <- month_date
      last_day     <- ceiling_date(first_day, unit = "month") - days(1)
      start_monday <- first_day + days((1 - wday(first_day, week_start = 1)) %% 7)
      end_monday   <- last_day  - days((wday(last_day,  week_start = 1) - 1) %% 7)
      if (start_monday <= end_monday) seq(from = start_monday, to = end_monday, by = "1 week") else first_day
    })
  ) %>%
  unnest(week_seq) %>%
  rename(week_date = week_seq) %>%
  ungroup() %>%
  arrange(country, week_date) %>%
  mutate(week_date = as.Date(week_date))

save_plot(
  ggplot(bci_weekly, aes(x = week_date, y = bci, color = country)) +
    geom_line(size = 1.1) +
    labs(title = "Weekly Business Confidence Indicator (BCI)", x = "Date", y = "BCI Value", color = "Country") +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "bci.png"
)

# ── Composite Leading Indicator (CLI) ─────────────────────────────────────────
cli_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES@DF_CLI,/USA+GBR.M.LI...AA...H?",
  "startPeriod=", api_start,
  "&endPeriod=", api_end_current
)

cli_sdmx <- readSDMX(cli_url)
cli_df   <- as.data.frame(cli_sdmx)

cli_clean <- cli_df %>%
  select(obsTime, obsValue, REF_AREA) %>%
  rename(month_date = obsTime, cli = obsValue, country = REF_AREA) %>%
  mutate(
    month_date = as.Date(paste0(month_date, "-01")),
    cli        = as.numeric(cli),
    country    = recode(country, "GBR" = "UNITED KINGDOM", "USA" = "UNITED STATES")
  )

cli_weekly <- cli_clean %>%
  rowwise() %>%
  mutate(
    week_seq = list({
      first_day    <- month_date
      last_day     <- ceiling_date(first_day, unit = "month") - days(1)
      start_monday <- first_day + days((1 - wday(first_day, week_start = 1)) %% 7)
      end_monday   <- last_day  - days((wday(last_day,  week_start = 1) - 1) %% 7)
      if (start_monday <= end_monday) seq(from = start_monday, to = end_monday, by = "1 week") else first_day
    })
  ) %>%
  unnest(week_seq) %>%
  rename(week_date = week_seq) %>%
  ungroup() %>%
  arrange(country, week_date) %>%
  mutate(week_date = as.Date(week_date))

save_plot(
  ggplot(cli_weekly, aes(x = week_date, y = cli, color = country)) +
    geom_line(size = 1.1) +
    labs(title = "Weekly Composite Leading Indicator (CLI)", x = "Date", y = "CLI Value", color = "Country") +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "cli.png"
)

# ── Unemployment ──────────────────────────────────────────────────────────────
unemployment_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_LFS@DF_IALFS_INDIC,1.0/USA+GBR.UNE_LF_M...Y._T.Y_GE15..M?",
  "startPeriod=", api_start,
  "&endPeriod=", api_end_current,
  "&dimensionAtObservation=AllDimensions"
)

unemp_sdmx <- readSDMX(unemployment_url)
unemp_df   <- as.data.frame(unemp_sdmx)

unemp_clean <- unemp_df %>%
  select(month_date = TIME_PERIOD, country = REF_AREA, unemployment_rate = obsValue) %>%
  mutate(
    month_date        = as.Date(paste0(month_date, "-01")),
    unemployment_rate = as.numeric(unemployment_rate),
    country           = recode(country, "GBR" = "UNITED KINGDOM", "USA" = "UNITED STATES")
  )

unemp_weekly <- unemp_clean %>%
  rowwise() %>%
  mutate(
    week_seq = list({
      first_day    <- month_date
      last_day     <- ceiling_date(first_day, unit = "month") - days(1)
      start_monday <- first_day + days((1 - wday(first_day, week_start = 1)) %% 7)
      end_monday   <- last_day  - days((wday(last_day,  week_start = 1) - 1) %% 7)
      if (start_monday <= end_monday) seq(from = start_monday, to = end_monday, by = "1 week") else first_day
    })
  ) %>%
  unnest(week_seq) %>%
  rename(week_date = week_seq) %>%
  ungroup() %>%
  arrange(country, week_date) %>%
  mutate(week_date = as.Date(week_date))

save_plot(
  ggplot(unemp_weekly, aes(x = week_date, y = unemployment_rate, color = country)) +
    geom_line(size = 1.2) +
    labs(title = "Weekly Unemployment Rate", x = "Date", y = "Unemployment Rate (%)", color = "Country") +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "unemployment.png"
)

# ── CPI ───────────────────────────────────────────────────────────────────────
cpi_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/USA+GBR.M.N.CPI.._T.N.GY+_Z?",
  "startPeriod=", api_start,
  "&endPeriod=", api_end_current,
  "&dimensionAtObservation=AllDimensions"
)

cpi_df <- as.data.frame(readSDMX(cpi_url)) %>%
  mutate(
    TIME_PERIOD  = as.Date(paste0(TIME_PERIOD, "-01")),
    country      = case_when(REF_AREA == "USA" ~ "UNITED STATES", REF_AREA == "GBR" ~ "UNITED KINGDOM", TRUE ~ REF_AREA),
    measure_type = case_when(
      UNIT_MEASURE == "PA" & TRANSFORMATION == "GY" ~ "cpi_yoy",
      UNIT_MEASURE == "IX" & TRANSFORMATION == "_Z" ~ "cpi_index",
      TRUE ~ "other"
    )
  ) %>%
  filter(measure_type %in% c("cpi_yoy", "cpi_index"))

cpi_wide <- cpi_df %>%
  select(country, TIME_PERIOD, measure_type, obsValue) %>%
  pivot_wider(names_from = measure_type, values_from = obsValue) %>%
  arrange(country, TIME_PERIOD)

cpi_weekly <- cpi_wide %>%
  rename(month_date = TIME_PERIOD) %>%
  rowwise() %>%
  mutate(
    week_seq = list({
      first_day    <- month_date
      last_day     <- ceiling_date(first_day, unit = "month") - days(1)
      start_monday <- first_day + days((1 - wday(first_day, week_start = 1)) %% 7)
      end_monday   <- last_day  - days((wday(last_day,  week_start = 1) - 1) %% 7)
      if (start_monday <= end_monday) seq(from = start_monday, to = end_monday, by = "1 week") else first_day
    })
  ) %>%
  unnest(week_seq) %>%
  rename(week_date = week_seq) %>%
  ungroup() %>%
  arrange(country, week_date) %>%
  mutate(week_date = as.Date(week_date))

save_plot(
  ggplot(cpi_weekly, aes(x = week_date, y = cpi_yoy, color = country)) +
    geom_line(size = 1.2) +
    labs(title = "Weekly CPI Inflation Rate (YoY %)", x = "Week Start Date", y = "YoY Inflation (%)", color = "Country") +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "cpi_yoy.png"
)

save_plot(
  ggplot(cpi_weekly, aes(x = week_date, y = cpi_index, color = country)) +
    geom_line(size = 1.2) +
    labs(title = "Weekly CPI Index (Base Year 2015)", x = "Week Start Date", y = "CPI Index", color = "Country") +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "cpi_index.png"
)

# ── VIX ───────────────────────────────────────────────────────────────────────
VIX_US_data <- quantmod::getSymbols(
  "^VIX",
  src         = "yahoo",
  from        = start_date,
  to          = end_date + days(1),
  auto.assign = FALSE,
  periodicity = "daily"
)

stocks_VIX <- as.data.frame(VIX_US_data) %>%
  tibble::rownames_to_column("date") %>%
  mutate(
    date      = as.Date(date),
    date_week = floor_date(date, unit = "week", week_start = 1)
  ) %>%
  group_by(date_week) %>%
  summarise(VIX_min = min(VIX.Close, na.rm = TRUE), VIX = mean(VIX.Close, na.rm = TRUE), VIX_max = max(VIX.Close, na.rm = TRUE), .groups = "drop") %>%
  mutate(date_week = as.Date(date_week))

save_plot(
  ggplot(stocks_VIX, aes(x = date_week, y = VIX)) +
    geom_line(color = "steelblue", size = 1.2) +
    labs(title = "Weekly VIX (Volatility Index)", x = "Week (Monday)", y = "VIX (avg close)", caption = "Source: Yahoo Finance") +
    theme_minimal(base_size = 14),
  "vix.png"
)

# ── Combine all indicators ────────────────────────────────────────────────────
vix_clean <- stocks_VIX %>%
  rename(week_date = date_week) %>%
  mutate(week_date = as.Date(week_date))

vix_repeated <- vix_clean %>%
  select(week_date, vix = VIX) %>%
  mutate(country = "UNITED STATES") %>%
  bind_rows(vix_clean %>% select(week_date, vix = VIX) %>% mutate(country = "UNITED KINGDOM"))

# Build a full week spine so indicators with publication lags (e.g. UK BCI)
# don't silently truncate all other series that are available more recently.
week_spine_start <- floor_date(start_date, unit = "week", week_start = 1)
all_mondays <- tibble(week_date = seq(week_spine_start, end_date, by = "weeks"))

date_spine <- bind_rows(
  all_mondays %>% mutate(country = "UNITED STATES"),
  all_mondays %>% mutate(country = "UNITED KINGDOM")
)

macro_table <- date_spine %>%
  left_join(bci_weekly   %>% select(country, week_date, bci),                by = c("country", "week_date")) %>%
  left_join(cli_weekly   %>% select(country, week_date, cli),                by = c("country", "week_date")) %>%
  left_join(unemp_weekly %>% select(country, week_date, unemployment_rate),  by = c("country", "week_date")) %>%
  left_join(cpi_weekly   %>% select(country, week_date, cpi_yoy, cpi_index), by = c("country", "week_date")) %>%
  left_join(vix_repeated,                                                     by = c("country", "week_date")) %>%
  select(week_date, country, vix, unemployment_rate, cli, bci, cpi_yoy, cpi_index) %>%
  arrange(country, week_date)

# ── Export to Google Sheets ───────────────────────────────────────────────────
if (write_objects) {

  gs4_auth(email = env$user_email)

  sheet_id <- sheets$macro_indicators

  macro_table_clean <- macro_table %>%
    as.data.frame() %>%
    mutate(week_date = as.character(week_date))

  tryCatch({
    write_sheet(macro_table_clean, ss = sheet_id, sheet = "Sheet1")
    message("Successfully wrote ", nrow(macro_table_clean), " rows to Google Sheets")
  }, error = function(e) {
    cat("Write failed. Error:\n")
    print(e)
  })
}
