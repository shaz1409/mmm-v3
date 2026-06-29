# ============================================================================
# MARGINAL EFFICIENCY
# Reads the *_rc response curve tabs (written by 07_dashboard_functions.r)
# from the dashboard Google Sheet and, for each channel, finds:
#
#   Current mROI/mCPA  — marginal metric at the historical average spend
#   Recommended spend  — spend level that achieves the target mROI/mCPA
#
# RC tab columns used:
#   week              : week date
#   variable          : full channel variable name
#   label             : human-readable channel label
#   deadstocked_spend : de-carryover spend (x-axis of response curve)
#   original_spend    : actual raw weekly spend
#   mroi              : marginal ROI (LTV) or marginal CPA (ACQ)
#
# Output tabs written to sheets$dashboard_export: <model>_mroi
#   e.g. uk_ltv_mroi, uk_acq_mroi, us_ltv_mroi, us_acq_mroi
# ============================================================================

library(googlesheets4)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# Source 07 for export_to_gsheet
SOURCING_ONLY <- TRUE
source(file.path(script_dir, "07_dashboard_functions.r"))
rm(SOURCING_ONLY)

# ============================================================================
# CONFIGURATION — targets and period come from config.R (marginal_efficiency)
# ============================================================================

RC_SOURCE_SHEET_ID <- sheets$dashboard_export   # where 07 wrote the _rc tabs
OUTPUT_SHEET_ID    <- sheets$marginal_efficiency  # where this script writes results
AVG_SPEND_PERIOD   <- marginal_efficiency$avg_spend_period
# NULL  → average over all weeks in the model
# c("2024-01-01", "2024-12-31") → restrict to that date range

# ============================================================================
# HELPER: find the row where col_name is closest to target_val
# ============================================================================

find_closest <- function(df, col_name, target_val) {
  idx <- which.min(abs(df[[col_name]] - target_val))
  df[idx, , drop = FALSE]
}

# ============================================================================
# FUNCTION: build_mroi_table
# For one model:
#   1. Reads its _rc tab from the dashboard sheet
#   2. Filters to AVG_SPEND_PERIOD (or all weeks if NULL)
#   3. Per channel: avg original_spend → find closest deadstocked_spend
#      → current mROI/mCPA
#   4. Per channel: find closest mroi to target → recommended spend + mroi
# ============================================================================

build_mroi_table <- function(model_key, is_acq) {

  me_cfg <- marginal_efficiency[[model_key]]
  if (is.null(me_cfg)) {
    cat("  No marginal_efficiency config for", model_key, "— skipping\n")
    return(NULL)
  }

  target_val   <- if (is_acq) me_cfg$target_cpa  else me_cfg$target_roi
  metric_label <- if (is_acq) "mCPA"             else "mROI"

  if (is.null(target_val)) {
    cat("  No target", metric_label, "set for", model_key, "— skipping\n")
    return(NULL)
  }

  # -- 1. Read RC tab --
  rc_tab <- paste0(model_key, "_rc")
  cat("  Reading", rc_tab, "...\n")
  rc <- as.data.frame(read_sheet(ss = RC_SOURCE_SHEET_ID, sheet = rc_tab))
  rc$week <- as.Date(rc$week)

  # -- 2. Filter to period for avg spend calculation --
  if (!is.null(AVG_SPEND_PERIOD)) {
    period_start <- as.Date(AVG_SPEND_PERIOD[1])
    period_end   <- as.Date(AVG_SPEND_PERIOD[2])
    rc_for_avg   <- rc[rc$week >= period_start & rc$week <= period_end, ]
    cat("  Avg spend period:", format(period_start), "to", format(period_end),
        "—", nrow(rc_for_avg), "rows\n")
  } else {
    rc_for_avg <- rc
    cat("  Avg spend period: full model history\n")
  }

  # -- 3. Average original_spend per channel --
  avg_df <- aggregate(original_spend ~ variable, data = rc_for_avg,
                      FUN = mean, na.rm = TRUE)

  # -- 4. Per-channel lookup --
  channels <- unique(rc$variable)

  rows <- lapply(channels, function(v) {

    ch_rc  <- rc[rc$variable == v & !is.na(rc$mroi), ]
    if (nrow(ch_rc) == 0) return(NULL)

    avg_sp <- avg_df$original_spend[avg_df$variable == v]
    if (length(avg_sp) == 0 || is.na(avg_sp)) return(NULL)

    # Current: row on curve closest to avg spend (by deadstocked_spend)
    current_row <- find_closest(ch_rc, "deadstocked_spend", avg_sp)

    # Recommended: if channel has no effect (all mroi = 0), default to avg spend
    has_effect <- any(ch_rc$mroi != 0, na.rm = TRUE)

    if (!has_effect) {
      rec_spend  <- avg_sp
      rec_metric <- 0
    } else {
      rec_row    <- find_closest(ch_rc, "mroi", target_val)
      rec_spend  <- rec_row$deadstocked_spend
      rec_metric <- rec_row$mroi
    }

    data.frame(
      label              = current_row$label,
      avg_spend          = round(avg_sp, 2),
      current_metric     = round(current_row$mroi, 4),
      recommended_spend  = round(rec_spend, 2),
      recommended_metric = round(rec_metric, 4),
      stringsAsFactors   = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))

  if (is.null(result) || nrow(result) == 0) {
    cat("  No results for", model_key, "\n")
    return(NULL)
  }

  # Rename metric columns to mROI / mCPA
  names(result)[3] <- paste0("current_", metric_label)
  names(result)[5] <- paste0("recommended_", metric_label)

  result <- result[order(result$label), ]
  rownames(result) <- NULL
  result
}

# ============================================================================
# MAIN
# ============================================================================

gs4_auth()

for (model_key in names(dashboard)) {

  is_acq <- grepl("acq", model_key, ignore.case = TRUE)

  cat("\n════════════════════════════════════════════\n")
  cat("Model  :", model_key, "\n")
  cat("Metric :", if (is_acq) "mCPA" else "mROI", "\n")
  cat("Target :", if (is_acq) marginal_efficiency[[model_key]]$target_cpa
                  else marginal_efficiency[[model_key]]$target_roi, "\n")
  cat("════════════════════════════════════════════\n")

  tbl <- build_mroi_table(model_key, is_acq)

  if (!is.null(tbl)) {
    cat("  Rows:", nrow(tbl), "\n")
    export_to_gsheet(tbl, OUTPUT_SHEET_ID, paste0(model_key, "_mroi"))
  }
}

cat("\nMarginal efficiency tables exported.\n")
