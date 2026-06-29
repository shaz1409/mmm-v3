# ================================================================
# QUARTERLY MMM Models
# Script 11.2: Model Refresh - US ACQ
# ================================================================

library(Robyn)
library(googledrive)
library(dplyr)
library(reticulate)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

py <- env$python_path
Sys.setenv(RETICULATE_PYTHON = py)
options(reticulate.conda_binary = "")
Sys.setenv(RETICULATE_MINICONDA_PATH = "")
options(googledrive_quiet = TRUE)
options(cli.unicode = FALSE)

cat("Working Directory:", getwd(), "\n\n")


# ================================================================
# RUN CONTROL - set to the model type to refresh
# ================================================================
REFRESH_MODEL <- "us_acq"


# ================================================================
# LOAD REFRESH CONFIG
# ================================================================

rf_cfg <- model_refresh[[REFRESH_MODEL]]
if (is.null(rf_cfg)) {
  stop("No model_refresh config found for: ", REFRESH_MODEL,
       "\nAvailable: ", paste(names(model_refresh), collapse = ", "))
}

INIT_FOLDER    <- rf_cfg$init_folder
SELECT_MODEL   <- rf_cfg$select_model
REFRESH_UNTIL  <- rf_cfg$refresh_until
REFRESH_STEPS  <- rf_cfg$refresh_steps
REFRESH_ITERS  <- rf_cfg$refresh_iters
REFRESH_TRIALS <- rf_cfg$refresh_trials
COUNTRY        <- rf_cfg$country

if (is.null(INIT_FOLDER) || is.null(SELECT_MODEL)) {
  stop("init_folder and select_model must be set in config$model_refresh$", REFRESH_MODEL)
}


# ================================================================
# LOAD DATA
# ================================================================

drive_auth(email = env$user_email, cache = ".secrets")

download_from_gdrive <- function(folder_id, filename, local_dir = "model_data") {
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  files      <- drive_ls(path = as_id(folder_id))
  file_match <- files %>% filter(name == filename)
  if (nrow(file_match) == 0) stop("File not found in Google Drive: ", filename)
  local_file <- file.path(local_dir, filename)
  drive_download(file = as_id(file_match$id), path = local_file, overwrite = TRUE)
  cat("Downloaded:", filename, "->", local_file, "\n")
  return(local_file)
}

cat("Loading", COUNTRY, "data...\n")
filename   <- project$file_map[[COUNTRY]]
local_dir  <- file.path("model_data", COUNTRY)
file_path  <- download_from_gdrive(drive_folders$model_data, filename, local_dir)

dt_input <- read.csv(file_path, stringsAsFactors = FALSE)
dt_input[[project$date_var]] <- as.Date(dt_input[[project$date_var]])

if (!is.null(REFRESH_UNTIL)) {
  until_date <- as.Date(REFRESH_UNTIL)
  dt_input   <- dt_input[dt_input[[project$date_var]] <= until_date, ]
  cat("Data capped at:", as.character(until_date), "\n")
}

cat("Rows:", nrow(dt_input), "| Columns:", ncol(dt_input), "\n")
cat("Date range:", as.character(min(dt_input[[project$date_var]])), "to", as.character(max(dt_input[[project$date_var]])), "\n\n")


# ================================================================
# VALIDATE / AUTO-DETECT REFRESH_STEPS
# ================================================================
# The original model's effective window_end (what Robyn actually used) is stored
# in InputCollect.RDS. refresh_steps MUST equal the number of weekly rows in
# dt_input that fall AFTER that window_end, otherwise Robyn cannot locate where
# new data begins inside the refresh rolling window.

ic_rds <- file.path(INIT_FOLDER, "InputCollect.RDS")
if (!file.exists(ic_rds)) {
  stop("InputCollect.RDS not found in INIT_FOLDER:\n  ", INIT_FOLDER)
}
ic_orig         <- readRDS(ic_rds)
orig_window_end <- as.Date(ic_orig$window_end)
date_var        <- project$date_var
all_dates       <- sort(dt_input[[date_var]])

# Robyn stores window_end as the last DAY of the training week (e.g. Sunday for
# Monday-commencing weekly data). Find the last week_commencing date that falls
# on or before that Sunday - that is the actual last training row in dt_input.
adapted_window_end <- max(all_dates[all_dates <= orig_window_end])

cat("------------------------------------------------------------------\n")
cat("Original model window_end :", as.character(orig_window_end), "\n")
cat("Adapted window_end (data) :", as.character(adapted_window_end), "\n")
cat("dt_input last 5 dates     :", paste(tail(all_dates, 5), collapse = ", "), "\n")

new_dates <- all_dates[all_dates > adapted_window_end]
cat("New dates available       :", length(new_dates), "\n")
if (length(new_dates) > 0)
  cat("  from", as.character(min(new_dates)), "to", as.character(max(new_dates)), "\n")
cat("------------------------------------------------------------------\n\n")

new_weeks <- length(new_dates)

if (new_weeks == 0) {
  stop("No data beyond adapted window_end (", adapted_window_end, ") found in dt_input.\n",
       "Make sure the downloaded dataset contains data past that date.")
}

if (is.null(REFRESH_STEPS)) {
  REFRESH_STEPS <- new_weeks
  cat("Auto-set REFRESH_STEPS    :", REFRESH_STEPS,
      "-> target date:", as.character(adapted_window_end + REFRESH_STEPS * 7), "\n\n")
} else {
  target_date <- adapted_window_end + REFRESH_STEPS * 7
  if (!target_date %in% all_dates) {
    cat("WARNING: REFRESH_STEPS =", REFRESH_STEPS, "targets", as.character(target_date),
        "which is NOT in dt_input - adjusting to nearest valid step count.\n")
    REFRESH_STEPS <- new_weeks
    cat("  Adjusted REFRESH_STEPS   :", REFRESH_STEPS,
        "-> target date:", as.character(adapted_window_end + REFRESH_STEPS * 7), "\n\n")
  } else {
    cat("REFRESH_STEPS             :", REFRESH_STEPS,
        "-> target date:", as.character(target_date), "\n\n")
  }
}


# ================================================================
# GENERATE MODEL-SPECIFIC JSON (if not already present)
# robyn_refresh() needs RobynModel-{id}.json from robyn_write(),
# NOT the aggregated RobynModel-models.json from robyn_outputs().
# ================================================================

JSON_FILE <- file.path(INIT_FOLDER, paste0("RobynModel-", SELECT_MODEL, ".json"))

cat("================================================================\n")
cat("  ROBYN REFRESH -", toupper(REFRESH_MODEL), "\n")
cat("================================================================\n")
cat("Init folder  :", INIT_FOLDER, "\n")
cat("JSON file    :", JSON_FILE, "\n")
cat("Refresh steps:", REFRESH_STEPS, "weeks\n")
cat("Iterations   :", REFRESH_ITERS, "\n")
cat("Trials       :", REFRESH_TRIALS, "\n")
cat("================================================================\n\n")

if (!file.exists(JSON_FILE)) {
  cat("Model-specific JSON not found - generating from RDS files...\n")

  rds_input  <- file.path(INIT_FOLDER, "InputCollect.RDS")
  rds_output <- file.path(INIT_FOLDER, "OutputCollect.RDS")

  if (!file.exists(rds_input) || !file.exists(rds_output)) {
    stop("Neither the model JSON nor the RDS files were found in:\n  ", INIT_FOLDER,
         "\nExpected: InputCollect.RDS, OutputCollect.RDS")
  }

  IC_prev <- readRDS(rds_input)
  OC_prev <- readRDS(rds_output)

  robyn_write(IC_prev, OC_prev, select_model = SELECT_MODEL,
              dir = INIT_FOLDER, export = TRUE)

  if (!file.exists(JSON_FILE)) {
    stop("robyn_write() did not produce ", JSON_FILE,
         "\nCheck that SELECT_MODEL '", SELECT_MODEL, "' exists in the model run.")
  }
  cat("JSON written:", JSON_FILE, "\n\n")
} else {
  cat("Using existing JSON:", JSON_FILE, "\n\n")
}


# Patch JSON window_end from Sunday to the last training Monday.
# Robyn stores window_end as the last day of the training week (Sunday).
# Its refresh check `window_end %in% totalDates` never matches because all dates
# in dt_input are Mondays. Patching to adapted_window_end (last Monday <= Sunday)
# makes the check fire and extends the window correctly by refresh_steps weeks.
json_text   <- paste(readLines(JSON_FILE, warn = FALSE), collapse = "\n")
old_end     <- as.character(orig_window_end)
new_end     <- as.character(adapted_window_end)
old_pattern <- paste0('"window_end":\\s*(?:"', old_end, '"|\\["', old_end, '"\\])')
new_value   <- paste0('"window_end": ["', new_end, '"]')
if (grepl(old_pattern, json_text, perl = TRUE)) {
  json_text <- sub(old_pattern, new_value, json_text, perl = TRUE)
  writeLines(json_text, JSON_FILE)
  cat("Patched JSON window_end:", old_end, "->", new_end, "\n\n")
} else {
  cat("JSON window_end already patched or not found - skipping.\n\n")
}


# Runtime patch for Robyn issue #1270: the installed code does
#   if (InputCollectRF$window_end %in% totalDates)
# but window_end is a character string from JSON while totalDates are Date objects.
# The %in% comparison silently returns FALSE (type mismatch), so the window end
# never extends. Fix: wrap window_end in as.Date() for the comparison.
local({
  f   <- getFromNamespace("robyn_refresh", "Robyn")
  b   <- deparse(body(f))
  old <- "if (InputCollectRF$window_end %in% totalDates) {"
  new <- "if (as.Date(InputCollectRF$window_end) %in% totalDates) {"
  if (any(grepl(old, b, fixed = TRUE))) {
    b <- gsub(old, new, b, fixed = TRUE)
    body(f) <- parse(text = paste(b, collapse = "\n"))[[1]]
    environment(f) <- asNamespace("Robyn")
    assignInNamespace("robyn_refresh", f, "Robyn")
    cat("Robyn patch applied: as.Date() fix for window_end check\n\n")
  } else {
    cat("Robyn patch not needed (already fixed or line not found)\n\n")
  }
})


# Apply column renames for compatibility with the original model's variable names.
# Only renames columns that are actually present — safe to run even if already renamed.
if (!is.null(rf_cfg$col_renames) && length(rf_cfg$col_renames) > 0) {
  present <- rf_cfg$col_renames[rf_cfg$col_renames %in% names(dt_input)]
  if (length(present) > 0) {
    dt_input <- dt_input %>% rename(any_of(present))
    cat("Column renames applied:", paste(names(present), "<-", present, collapse = ", "), "\n\n")
  }
}


# ================================================================
# CHANNEL AGGREGATION & COLUMN DROPS
# Must replicate transformations applied when the original model was trained.
# ================================================================

aggregate_cols <- function(data, new_col, src_cols) {
  metrics  <- c("total_spend", "total_impressions", "total_clicks",
                "total_qr", "total_vv", "total_installs")
  for (pfx in metrics) {
    new_name <- paste0(pfx, "_", new_col)
    src      <- paste0(pfx, "_", src_cols)
    present  <- src[src %in% names(data)]
    if (length(present) > 0) {
      data[[new_name]] <- rowSums(data[, present, drop = FALSE], na.rm = TRUE)
      to_remove <- present[present != new_name]
      data <- data[, !(names(data) %in% to_remove)]
    }
  }
  data
}

dt_input <- aggregate_cols(dt_input,
  new_col  = "CONSIDERATION_AUDIO_NEXUS_PANDORA_SIRIUSXM",
  src_cols = c("CONSIDERATION_AUDIO_NEXUS",
               "CONSIDERATION_AUDIO_PANDORA",
               "CONSIDERATION_AUDIO_SIRIUSXM")
)
dt_input <- aggregate_cols(dt_input,
  new_col  = "CONSIDERATION_OLV_NEXUS_SAMBA",
  src_cols = c("CONSIDERATION_OLV_SAMBA", "CONSIDERATION_OLV_NEXUS")
)
cat("Channel aggregations applied.\n\n")

# Drop columns excluded from the original model
if (!is.null(rf_cfg$col_drops) && length(rf_cfg$col_drops) > 0) {
  to_drop <- rf_cfg$col_drops[rf_cfg$col_drops %in% names(dt_input)]
  if (length(to_drop) > 0) {
    dt_input <- dt_input[, !(names(dt_input) %in% to_drop)]
    cat("Dropped columns:", paste(to_drop, collapse = ", "), "\n\n")
  }
}


RobynRefresh <- robyn_refresh(
  json_file      = JSON_FILE,
  dt_input       = dt_input,
  dt_holidays    = dt_prophet_holidays,
  refresh_steps  = REFRESH_STEPS,
  refresh_iters  = REFRESH_ITERS,
  refresh_trials = REFRESH_TRIALS
)


# ================================================================
# SAVE OUTPUTS
# ================================================================

# The refresh result is stored under listRefresh1, listRefresh2, etc.
# depending on how many refreshes have been chained
# Find the last listRefresh key (listRefresh1, listRefresh2, etc.)
# Robyn may append other keys (e.g. "refresh") — we want the listRefresh one.
listrefresh_keys <- grep("^listRefresh", names(RobynRefresh), value = TRUE)
if (length(listrefresh_keys) > 0) {
  refresh_key <- listrefresh_keys[length(listrefresh_keys)]
} else {
  refresh_key <- names(RobynRefresh)[length(names(RobynRefresh))]
  cat("WARNING: No listRefresh key found. Using last key:", refresh_key, "\n")
  cat("Available keys:", paste(names(RobynRefresh), collapse = ", "), "\n\n")
}
cat("\nRefresh key:", refresh_key, "\n")

InputCollectR  <- RobynRefresh[[refresh_key]]$InputCollect
OutputCollectR <- RobynRefresh[[refresh_key]]$OutputCollect
select_modelR  <- OutputCollectR$selectID

# Fallback: selectID is not always set after robyn_refresh() — try to
# derive it from the pareto results (the solution with the highest score).
if (is.null(select_modelR)) {
  fallback_ids <- unique(OutputCollectR$xDecompAgg$solID)
  if (length(fallback_ids) == 1) {
    select_modelR <- fallback_ids
    cat("selectID not set - inferred from single pareto solution:", select_modelR, "\n")
  } else if (length(fallback_ids) > 1) {
    stop("selectID is NULL and multiple pareto solutions exist (",
         paste(fallback_ids, collapse = ", "),
         ").\nInspect RobynRefresh.RDS and set select_modelR manually before proceeding.")
  } else {
    # Last resort: find the model JSON Robyn already wrote to the output folder
    plot_dir   <- OutputCollectR$plot_folder
    json_files <- if (!is.null(plot_dir) && dir.exists(plot_dir))
      list.files(plot_dir, pattern = "^RobynModel-(?!models).*\\.json$", perl = TRUE)
    else character(0)
    if (length(json_files) == 1) {
      select_modelR <- gsub("^RobynModel-|\\.json$", "", json_files)
      cat("selectID not set - inferred from existing JSON file:", select_modelR, "\n")
    } else {
      stop("selectID is NULL and no pareto solutions found in OutputCollect.",
           "\nInspect RobynRefresh.RDS manually.")
    }
  }
}

cat("Selected model:", select_modelR, "\n")

# Redirect outputs to:
# model_refresh/{model_type}/{init_folder_name}/{rf_subfolder}/
src_folder     <- normalizePath(OutputCollectR$plot_folder, winslash = "/", mustWork = FALSE)
refresh_parent <- normalizePath(
  file.path(env$working_dir, "model_refresh", REFRESH_MODEL, basename(INIT_FOLDER)),
  winslash = "/", mustWork = FALSE
)
rds_folder <- file.path(refresh_parent, basename(src_folder))

if (!dir.exists(refresh_parent)) dir.create(refresh_parent, recursive = TRUE)

# Move Robyn's auto-created output folder to the target location
if (dir.exists(src_folder)) {
  file.rename(src_folder, rds_folder)
  cat("Refresh output moved to:", rds_folder, "\n")
  # Remove the empty parent dir Robyn created under model_results (if now empty)
  src_parent <- dirname(src_folder)
  if (dir.exists(src_parent) && length(list.files(src_parent, recursive = TRUE)) == 0) {
    unlink(src_parent, recursive = TRUE)
    cat("Cleaned up empty folder:", src_parent, "\n")
  }
} else {
  dir.create(rds_folder, recursive = TRUE)
}

saveRDS(InputCollectR,  file.path(rds_folder, "InputCollect.RDS"))
saveRDS(OutputCollectR, file.path(rds_folder, "OutputCollect.RDS"))
saveRDS(RobynRefresh,   file.path(rds_folder, "RobynRefresh.RDS"))

cat("Saved RDS files to:", rds_folder, "\n")

# Generate model-specific JSON for the selected model.
# Required for chaining a second refresh and for robyn_allocator().
if (!is.null(select_modelR)) {
  robyn_write(InputCollectR, OutputCollectR, select_model = select_modelR,
              dir = rds_folder, export = TRUE)
  cat("Model JSON written:", file.path(rds_folder, paste0("RobynModel-", select_modelR, ".json")), "\n")
} else {
  warning("selectID is NULL - model JSON not written. Inspect RobynRefresh.RDS manually to find the solution ID.")
}

cat("================================================================\n")
cat("  REFRESH COMPLETE -", toupper(REFRESH_MODEL), "\n")
cat("================================================================\n")
cat("Selected model ID:", select_modelR, "\n")
cat("Output folder    :", rds_folder, "\n\n")
