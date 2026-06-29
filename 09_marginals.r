# ============================================================================
# MARGINALS EXPORT
# Builds a per-channel summary of average spend, marginal CPA and marginal
# ROAS for UK and US, using the response curve data from each model.
#
# Logic:
#  1. Build response curves for acq + ltv models per region
#  2. For each channel, compute average original_spend over the time window
#  3. Find the RC row where deadstocked_spend is closest to that average
#  4. Return the marginal CPA (from acq RC) and marginal ROAS (from ltv RC)
#     at that point
# ============================================================================

library(Robyn)
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
setwd(script_dir)

# Source all functions from 07 without running the export loop
SOURCING_ONLY <- TRUE
source(file.path(script_dir, "07_dashboard_functions.r"))
rm(SOURCING_ONLY)

# ============================================================================
# CONFIGURATION
# ============================================================================

SHEET_ID <- sheets$marginal_efficiency

MARGINAL_CONFIGS <- list(

  uk = list(
    date_start = NULL,          # e.g. "2024-01-01" — NULL uses all available weeks
    date_end   = NULL,          # e.g. "2024-12-30" — NULL uses all available weeks
    acq = list(
      run_folder   = paste0("model_refresh/uk_acq/Robyn_202602162334_init/",
                            "Robyn_202605141723_rf1"),
      selected_sol = "2_54_8"
    ),
    ltv = list(
      run_folder   = "model_results/uk_ltv/Robyn_202605131355_init",
      selected_sol = "2_95_19"
    )
  ),

  us = list(
    date_start = NULL,
    date_end   = NULL,
    acq = list(
      run_folder   = paste0("model_refresh/us_acq/Robyn_202602181221_init/",
                            "Robyn_202605130051_rf1"),
      selected_sol = "3_65_19"
    ),
    ltv = list(
      run_folder   = "model_results/us_ltv/Robyn_202605191525_init",
      selected_sol = "5_74_2"
    )
  )
)

# ============================================================================
# FUNCTION: build_marginals
# Takes RC data for acq and ltv models, an optional date window, and returns
# one row per channel with average spend and the marginal CPA / ROAS at that
# spend level.
# ============================================================================

build_marginals <- function(rc_acq, rc_ltv,
                            date_start = NULL, date_end = NULL) {

  # -- Optional time window filter --
  if (!is.null(date_start)) {
    rc_acq <- rc_acq[rc_acq$week >= as.Date(date_start), ]
    rc_ltv <- rc_ltv[rc_ltv$week >= as.Date(date_start), ]
  }
  if (!is.null(date_end)) {
    rc_acq <- rc_acq[rc_acq$week <= as.Date(date_end), ]
    rc_ltv <- rc_ltv[rc_ltv$week <= as.Date(date_end), ]
  }

  # Time range label for the output
  all_weeks      <- sort(unique(rc_acq$week))
  time_range_lbl <- paste(
    format(min(all_weeks), "%Y-%m-%d"),
    "to",
    format(max(all_weeks), "%Y-%m-%d")
  )

  # -- Average original_spend per channel, excluding zero-spend weeks --
  avg_spend_df <- aggregate(
    original_spend ~ label,
    data = rc_acq[rc_acq$original_spend > 0, ],
    FUN  = function(x) mean(x, na.rm = TRUE)
  )
  names(avg_spend_df)[2] <- "average_spend"

  # -- Lookup: find mroi at the row where deadstocked_spend ≈ average_spend --
  find_marginal <- function(rc, ch_label, avg_sp) {
    rows <- rc[rc$label == ch_label & !is.na(rc$deadstocked_spend), ]
    if (nrow(rows) == 0 || is.na(avg_sp)) return(NA_real_)
    rows$mroi[which.min(abs(rows$deadstocked_spend - avg_sp))]
  }

  # -- Build one row per channel --
  result <- do.call(rbind, lapply(avg_spend_df$label, function(ch) {
    avg_sp <- avg_spend_df$average_spend[avg_spend_df$label == ch]
    data.frame(
      channel       = ch,
      time_range    = time_range_lbl,
      average_spend = avg_sp,
      marginal_cpa  = find_marginal(rc_acq, ch, avg_sp),
      marginal_roas = find_marginal(rc_ltv, ch, avg_sp),
      stringsAsFactors = FALSE
    )
  }))

  result[order(result$channel), ]
}

# ============================================================================
# MAIN: Build and export marginals for each region
# ============================================================================

gs4_auth(
  email  = env$user_email,
  scopes = "https://www.googleapis.com/auth/spreadsheets"
)

for (region in names(MARGINAL_CONFIGS)) {

  cfg <- MARGINAL_CONFIGS[[region]]

  cat("\n════════════════════════════════════════════\n")
  cat("Region :", region, "\n")
  cat("════════════════════════════════════════════\n")

  # Build RC for acq model (mroi = marginal CPA)
  cat("  Building acq RC ...\n")
  media_acq <- build_media_transform(cfg$acq$run_folder, cfg$acq$selected_sol)
  rc_acq    <- build_response_curves(media_acq, metric_type = "acq")

  # Build RC for ltv model (mroi = marginal ROAS)
  cat("  Building ltv RC ...\n")
  media_ltv <- build_media_transform(cfg$ltv$run_folder, cfg$ltv$selected_sol)
  rc_ltv    <- build_response_curves(media_ltv, metric_type = "ltv")

  # Build marginals summary
  marginals <- build_marginals(rc_acq, rc_ltv,
                               date_start = cfg$date_start,
                               date_end   = cfg$date_end)
  cat("  Channels found:", nrow(marginals), "\n")

  # Export to Google Sheets (one tab per region)
  export_to_gsheet(marginals, SHEET_ID, region)
}

cat("\nMarginals exported to Google Sheets.\n")
