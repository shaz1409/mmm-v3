# ============================================================================
# BUDGET ALLOCATION EXPORT
# Runs robyn_allocator() for each model and exports to Google Sheets
# ============================================================================

library(Robyn)
library(dplyr)
library(googlesheets4)
library(googledrive)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# ============================================================================
# CONFIGURATION — derived from config.R; edit run_folder/selected_sol there
# ============================================================================

SHEET_ID <- sheets$budget_allocation

model_keys <- names(dashboard)
ALLOC_CONFIGS <- setNames(lapply(model_keys, function(m) {
  list(
    run_folder   = dashboard[[m]]$run_folder,
    selected_sol = dashboard[[m]]$selected_sol,
    date_range   = budget_allocation$date_range[[m]],
    sheet_tab    = m
  )
}), model_keys)

# ============================================================================
# FUNCTION: build_allocator
# Loads InputCollect.RDS / OutputCollect.RDS from run_folder, runs
# robyn_allocator(), and returns dt_optimOut enriched with strategy,
# channel, platform and label columns parsed from the channels column.
# ============================================================================

build_allocator <- function(run_folder, selected_sol, date_range = NULL) {

  InputCollect  <- readRDS(file.path(run_folder, "InputCollect.RDS"))
  OutputCollect <- readRDS(file.path(run_folder, "OutputCollect.RDS"))

  AllocatorCollect <- robyn_allocator(
    InputCollect        = InputCollect,
    OutputCollect       = OutputCollect,
    select_model        = selected_sol,
    date_range          = date_range,
    channel_constr_low  = budget_allocation$channel_constr_low,
    channel_constr_up   = budget_allocation$channel_constr_up,
    scenario            = budget_allocation$scenario,
    export              = FALSE  # we write to Sheets; skip local PNG/CSV output
  )

  alloc_out <- AllocatorCollect$dt_optimOut

  # -- Parse strategy / channel / platform / label from channels column --
  strip_prefix <- "^(total_spend_|total_impressions_|total_clicks_)"
  channel_keys <- sub(strip_prefix, "", alloc_out$channels)
  parts_list   <- strsplit(channel_keys, "_")

  strategy    <- sapply(parts_list, function(p) tolower(p[1]))
  raw_channel <- sapply(parts_list, function(p) tolower(p[2]))
  raw_channel <- gsub("paidsocial", "paid social", raw_channel)
  raw_channel <- gsub("paidsearch", "paid search", raw_channel)
  platform    <- sapply(
    parts_list,
    function(p) tolower(paste(p[seq(3, length(p))], collapse = " "))
  )

  alloc_out$strategy <- strategy
  alloc_out$channel  <- raw_channel
  alloc_out$platform <- platform
  alloc_out$label    <- tools::toTitleCase(
    paste(strategy, raw_channel, platform)
  )

  # Move label columns immediately after channels
  other_cols <- names(alloc_out)[
    !names(alloc_out) %in%
      c("channels", "strategy", "channel", "platform", "label")
  ]
  alloc_out <- alloc_out[, c(
    "channels", "strategy", "channel", "platform", "label", other_cols
  )]

  return(alloc_out)
}

# ============================================================================
# HELPER: sanitise Inf / NaN before writing to Google Sheets
# ============================================================================

export_to_gsheet <- function(data, sheet_id, sheet_tab) {
  num_cols <- sapply(data, is.numeric)
  data[num_cols] <- lapply(data[num_cols], function(x) {
    x[!is.finite(x)] <- NA
    x
  })
  cat("   Writing", nrow(data), "rows to tab '", sheet_tab, "' ...\n")
  sheet_write(data, ss = sheet_id, sheet = sheet_tab)
  cat("   Done.\n")
}

# ============================================================================
# MAIN: Authenticate and run allocator for all models
# ============================================================================

gs4_auth()

for (model_name in names(ALLOC_CONFIGS)) {

  cfg <- ALLOC_CONFIGS[[model_name]]

  cat("\n════════════════════════════════════════════\n")
  cat("Model      :", model_name, "\n")
  cat("Folder     :", cfg$run_folder, "\n")
  cat("solID      :", cfg$selected_sol, "\n")
  cat("date_range :", ifelse(is.null(cfg$date_range), "all", cfg$date_range), "\n")
  cat("Tab        :", cfg$sheet_tab, "\n")
  cat("════════════════════════════════════════════\n")

  alloc <- build_allocator(cfg$run_folder, cfg$selected_sol, cfg$date_range)
  cat("Alloc rows :", nrow(alloc), "\n")
  export_to_gsheet(alloc, SHEET_ID, cfg$sheet_tab)
}

cat("\nAll budget allocations exported to Google Sheets.\n")
