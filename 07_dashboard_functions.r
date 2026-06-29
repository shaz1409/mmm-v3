# ============================================================================
# DASHBOARD DATA EXPORT
# Builds decomp_long for each model and exports to Google Sheets
# ============================================================================

library(dplyr)
library(tidyr)
library(googlesheets4)
library(googledrive)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# ============================================================================
# CONFIGURATION — edit run_folder / selected_sol in config.R (dashboard section)
# ============================================================================

SHEET_ID <- sheets$dashboard_export

# Build EXPORT_CONFIGS from config; sheet tab names are derived from model name.
model_keys  <- names(dashboard)
EXPORT_CONFIGS <- setNames(lapply(model_keys, function(m) {
  cfg <- dashboard[[m]]
  list(
    run_folder      = cfg$run_folder,
    selected_sol    = cfg$selected_sol,
    metric_type     = cfg$metric_type,
    sheet_tab       = m,
    sheet_tab_media = paste0(m, "_media"),
    sheet_tab_rc    = paste0(m, "_rc"),
    sheet_tab_decay = paste0(m, "_decay"),
    sheet_tab_fit   = paste0(m, "_fit")
  )
}), model_keys)

# Canonical platform display names.
# Keys are lowercase raw platform strings (post-split, spaces for multi-token).
# Both single-token (e.g. "bingsearch") and multi-token (e.g. "bing search")
# forms are included to handle either variable naming convention.
PLATFORM_NAME_MAP <- c(
  "bingsearch"          = "Bing Search",
  "bing search"         = "Bing Search",
  "googlepmax"          = "Google PMax",
  "google pmax"         = "Google PMax",
  "google search brand" = "Google Search Brand",
  "googlesearchbrand"   = "Google Search Brand",
  "google search other" = "Google Search Other",
  "googlesearchother"   = "Google Search Other",
  "googleace"           = "Google ACe",
  "google ace"          = "Google ACe",
  "googleaci"           = "Google ACi",
  "google aci"          = "Google ACi",
  "dv360other"          = "DV360 Other",
  "dv360 other"         = "DV360 Other",
  "dv360"               = "DV360",
  "amazonctv"           = "Amazon CTV",
  "amazon ctv"          = "Amazon CTV",
  "teadsctv"            = "Teads CTV",
  "teads ctv"           = "Teads CTV",
  "acastvoice"          = "Acast Voice",
  "acast voice"         = "Acast Voice",
  "jwplayer"            = "JW Player",
  "jw player"           = "JW Player",
  "googledgen"          = "Google Dgen",
  "google dgen"         = "Google Dgen",
  "finecastctv"         = "Finecast CTV",
  "finecast ctv"        = "Finecast CTV"
)

# Returns the canonical platform name for each raw (lowercase) platform string.
# Falls back to toTitleCase for any platform not in the map.
standardize_platform <- function(raw_platform) {
  mapped <- PLATFORM_NAME_MAP[raw_platform]
  ifelse(is.na(mapped), tools::toTitleCase(raw_platform), mapped)
}

# ============================================================================
# FUNCTION: build_decomp_long
# Loads pareto_alldecomp_matrix and raw_data for a given run folder,
# filters to one solID, reshapes to long format, and joins spend.
# Returns: data.frame with columns week | variable | contribution | spend
# ============================================================================

build_decomp_long <- function(run_folder, selected_sol) {

  # -- 1. Load & reshape decomp matrix --
  alldecomp <- read.csv(
    file.path(run_folder, "pareto_alldecomp_matrix.csv"),
    stringsAsFactors = FALSE
  )

  decomp_long <- alldecomp %>%
    dplyr::filter(solID == selected_sol) %>%
    dplyr::select(-dplyr::any_of(c("solID", "cluster", "top_sol", "X"))) %>%
    pivot_longer(
      cols      = -ds,
      names_to  = "variable",
      values_to = "contribution"
    ) %>%
    rename(week = ds) %>%
    mutate(week = as.Date(week))

  # -- 2. Load raw_data spend columns --
  raw_data   <- read.csv(
    file.path(run_folder, "raw_data.csv"),
    stringsAsFactors = FALSE
  )
  spend_cols <- names(raw_data)[grepl("^total_spend_", names(raw_data))]
  raw_spend  <- raw_data[, c("week_commencing", spend_cols)]
  raw_spend$week_commencing <- as.Date(raw_spend$week_commencing)

  # Pivot spend to long: week | channel_key | spend
  spend_long <- tidyr::pivot_longer(
    raw_spend,
    cols      = -week_commencing,
    names_to  = "spend_var",
    values_to = "spend"
  )
  spend_long$channel_key <- sub(
    "^total_spend_", "", spend_long$spend_var
  )
  spend_long$spend_var <- NULL
  names(spend_long)[names(spend_long) == "week_commencing"] <- "week"

  # -- 3. Extract channel_key for paid media rows --
  paid_prefixes <- c("total_impressions_", "total_clicks_", "total_spend_")
  decomp_long$channel_key <- NA_character_

  for (pfx in paid_prefixes) {
    is_match <- grepl(paste0("^", pfx), decomp_long$variable)
    decomp_long$channel_key[is_match] <- sub(
      paste0("^", pfx), "", decomp_long$variable[is_match]
    )
  }

  # -- 4. Join spend (split to avoid merge dropping NA keys) --
  paid_rows  <- decomp_long[!is.na(decomp_long$channel_key), ]
  other_rows <- decomp_long[is.na(decomp_long$channel_key), ]

  paid_rows <- merge(
    paid_rows, spend_long,
    by = c("week", "channel_key"), all.x = TRUE
  )
  other_rows$spend <- NA_real_

  decomp_long <- rbind(paid_rows, other_rows)
  decomp_long$channel_key <- NULL

  # Replace NA spend with 0
  decomp_long$spend[is.na(decomp_long$spend)] <- 0

  # -- 5. Label type: paid_media vs base --
  paid_pattern <- "^(total_spend_|total_impressions_|total_clicks_)"
  is_paid <- grepl(paid_pattern, decomp_long$variable)
  decomp_long$type <- ifelse(is_paid, "paid_media", "base")

  # -- 6. Parse strategy / channel / platform for paid_media rows --
  # Format after stripping metric prefix: STRATEGY_CHANNEL_PLATFORM(S)
  # e.g. ACQUISITION_PAIDSOCIAL_META  -> acquisition | paid social | meta
  #      CONSIDERATION_AUDIO_NEXUS_PANDORA -> consideration | audio | nexus pandora

  strip_prefix <- "^(total_spend_|total_impressions_|total_clicks_)"

  decomp_long$strategy <- NA_character_
  decomp_long$channel  <- NA_character_
  decomp_long$platform <- NA_character_

  if (any(is_paid)) {
    channel_keys <- sub(strip_prefix, "", decomp_long$variable[is_paid])
    parts_list   <- strsplit(channel_keys, "_")

    decomp_long$strategy[is_paid] <- tools::toTitleCase(sapply(
      parts_list, function(p) tolower(p[1])
    ))
    raw_channel <- sapply(parts_list, function(p) tolower(p[2]))
    raw_channel <- gsub("paidsocial",  "paid social",  raw_channel)
    raw_channel <- gsub("paidsearch",  "paid search",  raw_channel)
    decomp_long$channel[is_paid] <- tools::toTitleCase(raw_channel)
    raw_platform <- sapply(
      parts_list,
      function(p) tolower(paste(p[seq(3, length(p))], collapse = " "))
    )
    decomp_long$platform[is_paid] <- standardize_platform(raw_platform)
  }

  # -- 7. Combined label: "Acquisition Paid Social Meta" --
  decomp_long$label <- NA_character_
  decomp_long$label[is_paid] <- paste(
    decomp_long$strategy[is_paid],
    decomp_long$channel[is_paid],
    decomp_long$platform[is_paid]
  )

  # -- 8. Final column order and sort --
  decomp_long <- decomp_long[, c(
    "week", "variable", "type",
    "strategy", "channel", "platform", "label",
    "contribution", "spend"
  )]
  decomp_long <- decomp_long[
    order(decomp_long$week, decomp_long$variable),
  ]

  return(decomp_long)
}

# ============================================================================
# FUNCTION: build_media_transform
# Loads pareto_media_transform_matrix for a given run folder and solID.
# Returns: week | variable | decomp_media | adstocked_value | spend
# ============================================================================

build_media_transform <- function(run_folder, selected_sol) {

  # -- 1. Load & filter to solID --
  media_mat <- read.csv(
    file.path(run_folder, "pareto_media_transform_matrix.csv"),
    stringsAsFactors = FALSE
  )
  media_mat <- media_mat[media_mat$solID == selected_sol, ]

  # Identify media variable columns (exclude metadata)
  meta_cols  <- c("X", "ds", "type", "solID", "cluster", "top_sol")
  media_cols <- names(media_mat)[!names(media_mat) %in% meta_cols]

  # Keep only paid media cols
  paid_pattern <- "^(total_spend_|total_impressions_|total_clicks_)"
  media_cols   <- media_cols[grepl(paid_pattern, media_cols)]

  # -- 2. Split by type and pivot to long --
  adstocked <- media_mat[
    media_mat$type == "adstockedMedia", c("ds", media_cols)
  ]
  decomp <- media_mat[
    media_mat$type == "decompMedia", c("ds", media_cols)
  ]

  adstocked_long <- tidyr::pivot_longer(
    adstocked, cols = -ds,
    names_to = "variable", values_to = "adstocked_value"
  )
  decomp_long <- tidyr::pivot_longer(
    decomp, cols = -ds,
    names_to = "variable", values_to = "decomp_media"
  )

  # -- 3. Join adstocked + decomp side by side --
  media_long <- merge(adstocked_long, decomp_long, by = c("ds", "variable"))
  names(media_long)[names(media_long) == "ds"] <- "week"
  media_long$week <- as.Date(media_long$week)

  # -- 4. Join spend from raw_data (same logic as build_decomp_long) --
  raw_data   <- read.csv(
    file.path(run_folder, "raw_data.csv"),
    stringsAsFactors = FALSE
  )
  spend_cols <- names(raw_data)[grepl("^total_spend_", names(raw_data))]
  raw_spend  <- raw_data[, c("week_commencing", spend_cols)]
  raw_spend$week_commencing <- as.Date(raw_spend$week_commencing)

  spend_long <- tidyr::pivot_longer(
    raw_spend,
    cols      = -week_commencing,
    names_to  = "spend_var",
    values_to = "spend"
  )
  spend_long$channel_key <- sub("^total_spend_", "", spend_long$spend_var)
  spend_long$spend_var   <- NULL
  names(spend_long)[names(spend_long) == "week_commencing"] <- "week"

  paid_prefixes <- c("total_impressions_", "total_clicks_", "total_spend_")
  media_long$channel_key <- NA_character_
  for (pfx in paid_prefixes) {
    is_match <- grepl(paste0("^", pfx), media_long$variable)
    media_long$channel_key[is_match] <- sub(
      paste0("^", pfx), "", media_long$variable[is_match]
    )
  }

  media_long <- merge(
    media_long, spend_long,
    by = c("week", "channel_key"), all.x = TRUE
  )
  media_long$channel_key <- NULL
  media_long$spend[is.na(media_long$spend)] <- 0

  # -- 5. Join hyperparameters (alpha, gamma, theta) from pareto_hyperparameters --
  hyper <- read.csv(
    file.path(run_folder, "pareto_hyperparameters.csv"),
    stringsAsFactors = FALSE
  )
  hyper <- hyper[hyper$solID == selected_sol, ]

  # Select only alpha/gamma/theta columns
  param_cols <- names(hyper)[grepl("_(alphas|gammas|thetas)$", names(hyper))]
  hyper_row  <- hyper[1, param_cols, drop = FALSE]

  # Pivot to long: param_col | value
  hyper_long <- tidyr::pivot_longer(
    hyper_row,
    cols      = tidyr::everything(),
    names_to  = "param_col",
    values_to = "value"
  )

  # Split into variable name and param type
  hyper_long$variable <- sub("_(alphas|gammas|thetas)$", "",
                              hyper_long$param_col)
  hyper_long$param    <- sub("^.+_(alphas|gammas|thetas)$", "\\1",
                              hyper_long$param_col)
  hyper_long$param_col <- NULL
  hyper_long$param <- gsub("alphas", "alpha",
                     gsub("gammas", "gamma",
                     gsub("thetas", "theta", hyper_long$param)))

  # Pivot wider: variable | alpha | gamma | theta
  hyper_wide <- tidyr::pivot_wider(
    hyper_long,
    names_from  = "param",
    values_from = "value"
  )

  # Filter to paid media only and join
  hyper_wide <- hyper_wide[grepl(paid_pattern, hyper_wide$variable), ]
  media_long <- merge(media_long, hyper_wide, by = "variable", all.x = TRUE)

  # -- 6. Parse label from variable name (same logic as build_decomp_long) --
  strip_prefix <- "^(total_spend_|total_impressions_|total_clicks_)"
  channel_keys <- sub(strip_prefix, "", media_long$variable)
  parts_list   <- strsplit(channel_keys, "_")

  strategy    <- sapply(parts_list, function(p) tolower(p[1]))
  raw_channel <- sapply(parts_list, function(p) tolower(p[2]))
  raw_channel <- gsub("paidsocial", "paid social", raw_channel)
  raw_channel <- gsub("paidsearch", "paid search", raw_channel)
  platform    <- standardize_platform(sapply(
    parts_list,
    function(p) tolower(paste(p[seq(3, length(p))], collapse = " "))
  ))

  media_long$label <- paste(
    tools::toTitleCase(paste(strategy, raw_channel)),
    platform
  )

  # -- 7. Final column order and sort --
  media_long <- media_long[, c(
    "week", "variable", "label",
    "decomp_media", "adstocked_value", "spend",
    "alpha", "gamma", "theta"
  )]
  media_long <- media_long[order(media_long$week, media_long$variable), ]

  return(media_long)
}

# ============================================================================
# FUNCTION: build_response_curves
# Takes the output of build_media_transform and, for each channel, sorts by
# adstocked_value, de-adstocks spend (adstocked_value * (1 - theta)), and
# computes ROI and MROI (slope between consecutive sorted points).
# Returns: variable | spend_carryover_immediate | response_incremental |
#          deadstocked_spend | roi | mroi
# ============================================================================

build_response_curves <- function(media_long, metric_type = "ltv") {

  variables <- unique(media_long$variable)

  rc_list <- lapply(variables, function(v) {

    df <- media_long[media_long$variable == v, ]

    # Sort by adstocked spend ascending (builds the response curve)
    df <- df[order(df$adstocked_value), ]

    theta <- df$theta[1]

    # De-adstock: remove the carry-over portion
    df$deadstocked_spend <- df$adstocked_value * (1 - theta)

    # ROI / CPA at each point (guard against zero denominator)
    # ltv: ROAS = response / spend  |  acq: CPA = spend / response
    if (metric_type == "acq") {
      df$roi <- ifelse(
        df$decomp_media == 0, NA_real_,
        df$deadstocked_spend / df$decomp_media
      )
    } else {
      df$roi <- ifelse(
        df$deadstocked_spend == 0, NA_real_,
        df$decomp_media / df$deadstocked_spend
      )
    }

    # Marginal metric between consecutive sorted points
    # ltv: marginal ROAS = Δresponse / Δspend
    # acq: marginal CPA  = Δspend    / Δresponse
    n <- nrow(df)
    df$mroi <- NA_real_
    if (n > 1) {
      d_response <- diff(df$decomp_media)
      d_spend    <- diff(df$deadstocked_spend)
      if (metric_type == "acq") {
        df$mroi[2:n] <- ifelse(
          d_response == 0, NA_real_, d_spend / d_response
        )
      } else {
        df$mroi[2:n] <- ifelse(
          d_spend == 0, NA_real_, d_response / d_spend
        )
      }
    }

    df[, c(
      "week", "variable", "label", "adstocked_value", "decomp_media",
      "deadstocked_spend", "roi", "mroi", "spend"
    )]
  })

  rc <- do.call(rbind, rc_list)

  # Rename to descriptive column names
  names(rc)[names(rc) == "adstocked_value"] <- "spend_carryover_immediate"
  names(rc)[names(rc) == "decomp_media"]    <- "response_incremental"
  names(rc)[names(rc) == "spend"]           <- "original_spend"

  rc <- rc[order(rc$variable, rc$week), ]
  rownames(rc) <- NULL

  return(rc)
}

# ============================================================================
# FUNCTION: build_decay_table
# Reads theta parameters for all channels from pareto_hyperparameters and
# builds a wide decay table:
#   Row "parameter" : raw theta value
#   Rows 0..n_weeks : theta^week  (proportion — format as % in Sheets)
# Columns are one per channel variable (name includes _thetas suffix).
# ============================================================================

build_decay_table <- function(run_folder, selected_sol, n_weeks = 10) {

  hyper <- read.csv(
    file.path(run_folder, "pareto_hyperparameters.csv"),
    stringsAsFactors = FALSE
  )
  hyper <- hyper[hyper$solID == selected_sol, ]

  # Paid media theta columns only
  paid_pattern <- "^(total_spend_|total_impressions_|total_clicks_)"
  theta_cols   <- names(hyper)[grepl("_thetas$", names(hyper))]
  var_names    <- sub("_thetas$", "", theta_cols)
  theta_cols   <- theta_cols[grepl(paid_pattern, var_names)]
  var_names    <- var_names[grepl(paid_pattern, var_names)]

  theta_vals <- setNames(as.numeric(hyper[1, theta_cols]), theta_cols)

  # Build human-readable labels (lowercase)
  strip_prefix <- "^(total_spend_|total_impressions_|total_clicks_)"
  channel_keys <- sub(strip_prefix, "", var_names)
  parts_list   <- strsplit(channel_keys, "_")

  strategy    <- sapply(parts_list, function(p) tolower(p[1]))
  raw_channel <- sapply(parts_list, function(p) tolower(p[2]))
  raw_channel <- gsub("paidsocial", "paid social", raw_channel)
  raw_channel <- gsub("paidsearch", "paid search", raw_channel)
  platform    <- standardize_platform(sapply(
    parts_list,
    function(p) tolower(paste(p[seq(3, length(p))], collapse = " "))
  ))
  labels <- paste(tools::toTitleCase(paste(strategy, raw_channel)), platform)

  # Build long table: one row per week x channel
  weeks <- 0:n_weeks

  rows_list <- lapply(seq_along(theta_cols), function(i) {
    data.frame(
      week     = weeks,
      variable = labels[i],
      thetas   = theta_vals[i] ^ weeks,
      stringsAsFactors = FALSE
    )
  })

  decay_long <- do.call(rbind, rows_list)
  decay_long <- decay_long[order(decay_long$week, decay_long$variable), ]
  rownames(decay_long) <- NULL

  return(decay_long)
}

# ============================================================================
# FUNCTION: build_model_fit
# Extracts dep_var (actual) and depVarHat (fitted) from the decomp matrix,
# computes residuals, and joins rsq_train / rsq_val / decomp_rssd from pareto_aggregated.
# Returns: week | dep_var | depVarHat | residuals | rsq_train | rsq_val | decomp_rssd
# ============================================================================

build_model_fit <- function(run_folder, selected_sol) {

  # -- 1. Pull dep_var and depVarHat from alldecomp matrix --
  alldecomp <- read.csv(
    file.path(run_folder, "pareto_alldecomp_matrix.csv"),
    stringsAsFactors = FALSE
  )
  fit <- alldecomp[
    alldecomp$solID == selected_sol,
    c("ds", "dep_var", "depVarHat")
  ]
  names(fit)[names(fit) == "ds"] <- "week"
  fit$week      <- as.Date(fit$week)
  fit$residuals <- fit$dep_var - fit$depVarHat

  # -- 2. Join rsq_train and rsq_val from pareto_aggregated --
  pareto_agg <- read.csv(
    file.path(run_folder, "pareto_aggregated.csv"),
    stringsAsFactors = FALSE
  )
  model_row <- pareto_agg[pareto_agg$solID == selected_sol, ]

  fit$rsq_train   <- model_row$rsq_train[1]
  fit$rsq_val     <- model_row$rsq_val[1]
  fit$decomp_rssd <- model_row$decomp.rssd[1]

  fit <- fit[order(fit$week), ]
  rownames(fit) <- NULL

  return(fit)
}

# ============================================================================
# FUNCTION: export_to_gsheet
# Writes a data frame to a specific tab in the Google Sheet.
# Overwrites the tab if it already exists.
# ============================================================================

export_to_gsheet <- function(data, sheet_id, sheet_tab) {
  # Google Sheets rejects Inf / -Inf / NaN — replace with NA
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
# MAIN: Authenticate and process all models
# (skipped when this file is sourced by another script via SOURCING_ONLY)
# ============================================================================

if (!exists("SOURCING_ONLY") || !isTRUE(SOURCING_ONLY)) {

gs4_auth()

for (model_name in names(EXPORT_CONFIGS)) {

  cfg <- EXPORT_CONFIGS[[model_name]]

  cat("\n════════════════════════════════════════════\n")
  cat("Model    :", model_name, "\n")
  cat("Folder   :", cfg$run_folder, "\n")
  cat("solID    :", cfg$selected_sol, "\n")
  cat("Tab      :", cfg$sheet_tab, "\n")
  cat("════════════════════════════════════════════\n")

  # -- decomp long --
  decomp <- build_decomp_long(cfg$run_folder, cfg$selected_sol)
  cat("Decomp rows:", nrow(decomp), "\n")
  export_to_gsheet(decomp, SHEET_ID, cfg$sheet_tab)

  # -- media transform --
  media <- build_media_transform(cfg$run_folder, cfg$selected_sol)
  cat("Media rows :", nrow(media), "\n")
  export_to_gsheet(media, SHEET_ID, cfg$sheet_tab_media)

  # -- response curves --
  rc <- build_response_curves(media, cfg$metric_type)
  cat("RC rows    :", nrow(rc), "\n")
  export_to_gsheet(rc, SHEET_ID, cfg$sheet_tab_rc)

  # -- decay table --
  decay <- build_decay_table(cfg$run_folder, cfg$selected_sol)
  cat("Decay rows :", nrow(decay), "\n")
  export_to_gsheet(decay, SHEET_ID, cfg$sheet_tab_decay)

  # -- model fit --
  fit <- build_model_fit(cfg$run_folder, cfg$selected_sol)
  cat("Fit rows   :", nrow(fit), "\n")
  export_to_gsheet(fit, SHEET_ID, cfg$sheet_tab_fit)
}

cat("\nAll models exported to Google Sheets.\n")

} # end SOURCING_ONLY guard
