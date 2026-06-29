# ============================================================================
# CHANNEL PERFORMANCE — MONTHLY VIEW
# Combines LTV + ACQ decomp to show spend, contribution and efficiency
# metrics (ROAS from LTV, CPA from ACQ) by channel, year and month.
#
# Filtered to August–December for 2023, 2024 and 2025.
# Output columns:
#   label | year | month | spend | contribution_ltv | contribution_acq |
#   roas  | cpa
# ============================================================================

library(dplyr)
library(tidyr)
library(googlesheets4)
library(googledrive)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("--file=", "", file_arg)))
    } else {
      dirname(normalizePath(sys.frames()[[1]]$ofile))
    }
  }
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# Source all functions from 07 without running the export loop
SOURCING_ONLY <- TRUE
source(file.path(script_dir, "07_dashboard_functions.r"))
rm(SOURCING_ONLY)

# ============================================================================
# CONFIGURATION — derived from config.R; edit run_folder/selected_sol there
# ============================================================================

SHEET_ID <- sheets$channel_performance

PERF_CONFIGS <- list(
  uk = list(
    ltv       = list(run_folder = dashboard$uk_ltv$run_folder, selected_sol = dashboard$uk_ltv$selected_sol),
    acq       = list(run_folder = dashboard$uk_acq$run_folder, selected_sol = dashboard$uk_acq$selected_sol),
    sheet_tab = "uk"
  ),
  us = list(
    ltv       = list(run_folder = dashboard$us_ltv$run_folder, selected_sol = dashboard$us_ltv$selected_sol),
    acq       = list(run_folder = dashboard$us_acq$run_folder, selected_sol = dashboard$us_acq$selected_sol),
    sheet_tab = "us"
  )
)

TARGET_MONTHS <- 8:12
TARGET_YEARS  <- c(2023, 2024, 2025)

MONTH_LABELS <- c(
  "8"  = "August",    "9"  = "September", "10" = "October",
  "11" = "November",  "12" = "December"
)

# ============================================================================
# FUNCTION: build_monthly_performance
# Loads decomp for LTV and ACQ models, filters to Aug–Dec for the target
# years, aggregates by label + year + month, and computes ROAS and CPA.
# ============================================================================

build_monthly_performance <- function(cfg_ltv, cfg_acq) {

  # -- 1. Load and filter decomp for both models --
  decomp_ltv <- build_decomp_long(cfg_ltv$run_folder, cfg_ltv$selected_sol)
  decomp_acq <- build_decomp_long(cfg_acq$run_folder, cfg_acq$selected_sol)

  # Keep paid media only
  decomp_ltv <- decomp_ltv[decomp_ltv$type == "paid_media", ]
  decomp_acq <- decomp_acq[decomp_acq$type == "paid_media", ]

  # Add year and month_num
  for (df_name in c("decomp_ltv", "decomp_acq")) {
    df <- get(df_name)
    df$year      <- as.integer(format(df$week, "%Y"))
    df$month_num <- as.integer(format(df$week, "%m"))
    assign(df_name, df)
  }

  # Filter to target months and years
  decomp_ltv <- decomp_ltv[
    decomp_ltv$month_num %in% TARGET_MONTHS &
    decomp_ltv$year      %in% TARGET_YEARS,  ]
  decomp_acq <- decomp_acq[
    decomp_acq$month_num %in% TARGET_MONTHS &
    decomp_acq$year      %in% TARGET_YEARS,  ]

  # Readable month name
  decomp_ltv$month <- MONTH_LABELS[as.character(decomp_ltv$month_num)]
  decomp_acq$month <- MONTH_LABELS[as.character(decomp_acq$month_num)]

  # -- 2. Aggregate by platform + year + month --
  agg_ltv <- aggregate(
    cbind(spend, contribution) ~ platform + year + month + month_num,
    data   = decomp_ltv,
    FUN    = sum,
    na.rm  = TRUE
  )
  names(agg_ltv)[names(agg_ltv) == "contribution"] <- "contribution_ltv"

  agg_acq <- aggregate(
    contribution ~ platform + year + month + month_num,
    data   = decomp_acq,
    FUN    = sum,
    na.rm  = TRUE
  )
  names(agg_acq)[names(agg_acq) == "contribution"] <- "contribution_acq"

  # -- 3. Join LTV and ACQ on platform + year + month --
  result <- merge(
    agg_ltv,
    agg_acq[, c("platform", "year", "month", "contribution_acq")],
    by  = c("platform", "year", "month"),
    all = TRUE
  )

  # -- 4. ROAS (LTV contribution / spend) and CPA (spend / ACQ contribution) --
  result$roas <- ifelse(
    is.na(result$spend) | result$spend == 0, NA_real_,
    result$contribution_ltv / result$spend
  )
  result$cpa  <- ifelse(
    is.na(result$contribution_acq) | result$contribution_acq == 0, NA_real_,
    result$spend / result$contribution_acq
  )

  # -- 5. Sort: platform → year → month order --
  result <- result[order(result$platform, result$year, result$month_num), ]
  result$month_num <- NULL
  rownames(result) <- NULL

  result[, c(
    "platform", "year", "month",
    "spend", "contribution_ltv", "contribution_acq",
    "roas", "cpa"
  )]
}

# ============================================================================
# PLATFORM GROUPS
# Channels to aggregate before computing group-level ROAS / CPA.
# Keys = group label shown in output; values = platform names as they appear
# in the decomp data (lowercase, underscores stripped to spaces by Robyn).
# ============================================================================

PLATFORM_GROUPS <- list(
  "DV360"  = c("DV360", "DV360 Other"),
  "Google" = c("Google ACe", "Google ACi", "Google Dgen",
               "Google PMax", "Google Search Brand", "Google Search Other")
)

# ============================================================================
# FUNCTION: group_platforms
# Aggregates component channels into group rows, then recalculates
# ROAS = sum(contribution_ltv) / sum(spend)
# CPA  = sum(spend) / sum(contribution_acq)
# Returns a data frame in the same shape as build_monthly_performance output.
# ============================================================================

group_platforms <- function(perf_data) {
  group_map <- character(0)
  for (grp in names(PLATFORM_GROUPS)) {
    group_map[PLATFORM_GROUPS[[grp]]] <- grp
  }

  perf_data$group <- group_map[perf_data$platform]
  to_group <- perf_data[!is.na(perf_data$group), ]

  if (nrow(to_group) == 0) {
    warning("group_platforms: no matching platform names found — check PLATFORM_GROUPS")
    return(NULL)
  }

  agg <- aggregate(
    cbind(spend, contribution_ltv, contribution_acq) ~ group + year + month,
    data  = to_group,
    FUN   = sum,
    na.rm = TRUE
  )
  names(agg)[1] <- "platform"

  agg$roas <- ifelse(agg$spend == 0,              NA_real_, agg$contribution_ltv / agg$spend)
  agg$cpa  <- ifelse(agg$contribution_acq == 0,   NA_real_, agg$spend / agg$contribution_acq)

  agg[, c("platform", "year", "month", "spend", "contribution_ltv", "contribution_acq", "roas", "cpa")]
}

# ============================================================================
# FUNCTION: build_wide_performance
# Pivots the long output so years are column groups.
# Each year has 4 columns: Spend | Inc LTV | ROAS | CPA
# Rows: one per platform × month combination.
# ============================================================================

MONTH_NUM_LOOKUP <- c(
  "August" = 8, "September" = 9, "October" = 10,
  "November" = 11, "December" = 12
)

build_wide_performance <- function(perf_data, years = TARGET_YEARS) {

  # All platform × month combos present across all years
  base <- unique(perf_data[, c("platform", "month")])
  base$month_num <- MONTH_NUM_LOOKUP[base$month]
  base <- base[order(base$platform, base$month_num), ]
  base$month_num <- NULL

  for (yr in years) {
    yr_sub <- perf_data[
      perf_data$year == yr,
      c("platform", "month", "spend", "contribution_ltv", "roas", "cpa")
    ]
    names(yr_sub)[3:6] <- c(
      paste0(yr, "_spend"),
      paste0(yr, "_inc_ltv"),
      paste0(yr, "_roas"),
      paste0(yr, "_cpa")
    )
    base <- merge(base, yr_sub, by = c("platform", "month"), all.x = TRUE)
  }

  # Re-sort after merges
  base$month_num <- MONTH_NUM_LOOKUP[base$month]
  base <- base[order(base$platform, base$month_num), ]
  base$month_num <- NULL
  rownames(base) <- NULL
  base
}

# ============================================================================
# FUNCTION: export_wide_to_gsheet
# Writes wide data with a two-row header:
#   Row 1 — year group labels  (2023 | | | | 2024 | ...)
#   Row 2 — metric sub-headers (Spend | Inc LTV | ROAS | CPA | ...)
#   Row 3+ — data
# ============================================================================

export_wide_to_gsheet <- function(wide_data, years, sheet_id, sheet_tab) {

  # Build header rows
  header_row1 <- c("Platform", "Month")
  header_row2 <- c("", "")
  col_order   <- c("platform", "month")

  for (yr in years) {
    header_row1 <- c(header_row1, as.character(yr), "", "", "")
    header_row2 <- c(header_row2, "Spend", "Inc LTV", "ROAS", "CPA")
    col_order   <- c(col_order,
      paste0(yr, "_spend"),
      paste0(yr, "_inc_ltv"),
      paste0(yr, "_roas"),
      paste0(yr, "_cpa")
    )
  }

  # Re-order columns and sanitise
  wide_data <- wide_data[, intersect(col_order, names(wide_data))]
  num_cols  <- sapply(wide_data, is.numeric)
  wide_data[num_cols] <- lapply(wide_data[num_cols], function(x) {
    x[!is.finite(x)] <- NA
    x
  })

  # Create or clear sheet tab
  existing <- sheet_names(ss = sheet_id)
  if (sheet_tab %in% existing) {
    range_clear(ss = sheet_id, sheet = sheet_tab)
  } else {
    sheet_add(ss = sheet_id, sheet = sheet_tab)
  }

  # Write two-row header then data
  header_df <- as.data.frame(rbind(header_row1, header_row2), stringsAsFactors = FALSE)
  range_write(ss = sheet_id, data = header_df, sheet = sheet_tab,
              range = "A1", col_names = FALSE)
  range_write(ss = sheet_id, data = wide_data, sheet = sheet_tab,
              range = "A3", col_names = FALSE)

  cat("   Wide tab written:", sheet_tab, "(", nrow(wide_data), "rows )\n")
}

# ============================================================================
# MAIN
# ============================================================================

gs4_auth()

for (region in names(PERF_CONFIGS)) {

  cfg <- PERF_CONFIGS[[region]]

  cat("\n════════════════════════════════════════════\n")
  cat("Region :", region, "\n")
  cat("════════════════════════════════════════════\n")

  perf <- build_monthly_performance(cfg$ltv, cfg$acq)
  cat("Rows:", nrow(perf), "\n")

  # Long format (one row per platform × year × month)
  export_to_gsheet(perf, SHEET_ID, cfg$sheet_tab)

  # Wide format (one row per platform × month, years as column groups)
  wide <- build_wide_performance(perf, TARGET_YEARS)
  export_wide_to_gsheet(wide, TARGET_YEARS, SHEET_ID, paste0(cfg$sheet_tab, "_wide"))

  # Grouped wide format (DV360 and Google aggregated)
  grouped <- group_platforms(perf)
  if (!is.null(grouped)) {
    wide_grouped <- build_wide_performance(grouped, TARGET_YEARS)
    export_wide_to_gsheet(wide_grouped, TARGET_YEARS, SHEET_ID, paste0(cfg$sheet_tab, "_groups"))
  }
}

cat("\nChannel performance exported to Google Sheets.\n")
perf <- build_monthly_performance(PERF_CONFIGS$uk$ltv, PERF_CONFIGS$uk$acq)
sort(unique(perf$platform))
