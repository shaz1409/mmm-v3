# ==============================================================================
# Robyn Model Selection Script
# Selects top models from pareto_aggregated.csv based on:
#   - R² train > threshold
#   - Decomp.RSSD < threshold
#   - Fewer zero-contribution paid media channels
#   - ACQ models: CPA between CPA_MIN-CPA_MAX
#   - LTV models: ROI between ROI_MIN-ROI_MAX
# ==============================================================================

library(dplyr)
library(googledrive)
library(openxlsx)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# ============================================================================
# CONFIGURATION - Define all model types here
# ============================================================================
# Set enabled = TRUE for model types you want to generate raw + flat tabs for.
# Set enabled = FALSE to skip (existing tabs in Excel will NOT be touched).
# sol_ids: manually picked models for the _summary tab (from config, edit there).

GDRIVE_FOLDER_ID <- drive_folders$model_selection

# Number of top-scored models to auto-export as JSON (for refresh / allocator).
# Set to 0 to skip export entirely.
EXPORT_TOP_N <- 0

# Scoring weights — defined in config.R under model_selection$weights
W_RSQ      <- model_selection$weights$r_squared
W_RSSD     <- model_selection$weights$decomp_rssd
W_CHANNELS <- model_selection$weights$channels
W_BIZ      <- model_selection$weights$business
W_MEDIA    <- model_selection$weights$media

# MODEL_CONFIG is built from config.R. Only 'enabled' is local —
# set TRUE/FALSE here to control which tabs are regenerated.
ms <- model_selection  # shorthand

MODEL_CONFIG <- list(

  uk_acq = list(
    enabled         = TRUE,
    export          = ms$uk_acq$export,
    folders         = as.character(ms$uk_acq$folders),
    top_n           = ms$uk_acq$top_n,
    min_rsq_train   = ms$uk_acq$min_r2_train,
    max_decomp_rssd = ms$uk_acq$max_decomp_rssd,
    cpa_min         = ms$uk_acq$cpa_range[1],
    cpa_max         = ms$uk_acq$cpa_range[2],
    sol_ids         = ms$uk_acq$solutions,
    pin_solutions   = ms$uk_acq$pin_solutions
  ),

  uk_ltv = list(
    enabled         = TRUE,
    export          = ms$uk_ltv$export,
    folders         = as.character(ms$uk_ltv$folders),
    top_n           = ms$uk_ltv$top_n,
    min_rsq_train   = ms$uk_ltv$min_r2_train,
    max_decomp_rssd = ms$uk_ltv$max_decomp_rssd,
    roi_min         = ms$uk_ltv$roi_range[1],
    roi_max         = ms$uk_ltv$roi_range[2],
    sol_ids         = ms$uk_ltv$solutions,
    pin_solutions   = ms$uk_ltv$pin_solutions
  ),

  us_acq = list(
    enabled         = TRUE,
    export          = ms$us_acq$export,
    folders         = as.character(ms$us_acq$folders),
    top_n           = ms$us_acq$top_n,
    min_rsq_train   = ms$us_acq$min_r2_train,
    max_decomp_rssd = ms$us_acq$max_decomp_rssd,
    cpa_min         = ms$us_acq$cpa_range[1],
    cpa_max         = ms$us_acq$cpa_range[2],
    sol_ids         = ms$us_acq$solutions,
    pin_solutions   = ms$us_acq$pin_solutions
  ),

  us_ltv = list(
    enabled         = TRUE,
    export          = ms$us_ltv$export,
    folders         = as.character(ms$us_ltv$folders),
    top_n           = ms$us_ltv$top_n,
    min_rsq_train   = ms$us_ltv$min_r2_train,
    max_decomp_rssd = ms$us_ltv$max_decomp_rssd,
    roi_min         = ms$us_ltv$roi_range[1],
    roi_max         = ms$us_ltv$roi_range[2],
    sol_ids         = ms$us_ltv$solutions,
    pin_solutions   = ms$us_ltv$pin_solutions
  )
)

# ============================================================================
# HELPER: Min-max normalise
# ============================================================================

normalise <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  (x - min(x)) / (max(x) - min(x))
}

# ============================================================================
# SCORE & SELECT FUNCTION (reusable per model type)
# ============================================================================

score_and_select <- function(cfg, model_type) {

  IS_ACQ <- grepl("acq", model_type, ignore.case = TRUE)

  # --- Load data from all folders ---
  pareto_list <- list()
  for (mf in cfg$folders) {
    csv_path <- file.path(mf, "pareto_aggregated.csv")
    if (!file.exists(csv_path)) {
      cat("  WARNING: File not found:", csv_path, "- skipping\n")
      next
    }
    cat("  Loading:", csv_path, "\n")
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$.source_folder <- mf
    pareto_list[[length(pareto_list) + 1]] <- df
  }
  if (length(pareto_list) == 0) {
    cat("  No pareto_aggregated.csv files found - skipping\n")
    return(NULL)
  }
  pareto <- bind_rows(pareto_list)
  cat("  Rows:", nrow(pareto), "| Unique models:", length(unique(pareto$solID)), "\n")

  # --- Identify paid media ---
  pareto$is_paid_media <- grepl("^(total_impressions_|total_spend_|total_clicks_)", pareto$rn)

  # --- Compute per-model metrics ---
  CPA_MIN <- cfg$cpa_min; CPA_MAX <- cfg$cpa_max
  ROI_MIN <- cfg$roi_min; ROI_MAX <- cfg$roi_max

  model_scores <- pareto %>%
    dplyr::filter(is_paid_media) %>%
    group_by(solID) %>%
    summarise(
      rsq_train    = first(rsq_train),
      nrmse_train  = first(nrmse_train),
      decomp.rssd  = first(decomp.rssd),
      mape         = first(mape),
      n_paid_channels     = n(),
      n_zero_channels     = sum(xDecompAgg == 0, na.rm = TRUE),
      n_positive_channels = n_paid_channels - n_zero_channels,
      pct_positive_channels = n_positive_channels / n_paid_channels,
      sum_xDecompAgg      = sum(xDecompAgg, na.rm = TRUE),
      n_cpa_in_range = sum(cpa_total >= CPA_MIN & cpa_total <= CPA_MAX & xDecompAgg > 0, na.rm = TRUE),
      n_nonzero_with_cpa = sum(xDecompAgg > 0 & !is.na(cpa_total)),
      pct_cpa_in_range = ifelse(n_nonzero_with_cpa > 0, n_cpa_in_range / n_nonzero_with_cpa, 0),
      n_roi_in_range = sum(roi_total >= ROI_MIN & roi_total <= ROI_MAX & xDecompAgg > 0, na.rm = TRUE),
      n_nonzero_with_roi = sum(xDecompAgg > 0 & !is.na(roi_total)),
      pct_roi_in_range = ifelse(n_nonzero_with_roi > 0, n_roi_in_range / n_nonzero_with_roi, 0),
      .groups = "drop"
    )

  model_scores$pct_biz_in_range <- if (IS_ACQ) model_scores$pct_cpa_in_range else model_scores$pct_roi_in_range

  # --- Hard filters ---
  filtered <- model_scores %>%
    dplyr::filter(rsq_train > cfg$min_rsq_train, decomp.rssd < cfg$max_decomp_rssd)

  cat("  After hard filters (rsq >", cfg$min_rsq_train, "& rssd <", cfg$max_decomp_rssd, "):",
      nrow(filtered), "models\n")

  if (nrow(filtered) == 0) {
    cat("  No models passed hard filters!\n")
    cat("  Best rsq_train:", max(model_scores$rsq_train), "\n")
    cat("  Best decomp.rssd:", min(model_scores$decomp.rssd), "\n")
    return(NULL)
  }

  # --- Composite score ---
  filtered <- filtered %>%
    mutate(
      norm_rsq   = normalise(rsq_train),
      norm_rssd  = normalise(1 - decomp.rssd),
      norm_media = normalise(sum_xDecompAgg),
      score = W_RSQ * norm_rsq +
              W_RSSD * norm_rssd +
              W_CHANNELS * pct_positive_channels +
              W_BIZ * pct_biz_in_range +
              W_MEDIA * norm_media
    ) %>%
    arrange(desc(score))

  # --- Select models ---
  has_solutions <- length(cfg$sol_ids) > 0
  if (isTRUE(cfg$pin_solutions) && has_solutions) {
    # Pinned solutions first, then top N from the rest
    pinned     <- filtered[filtered$solID %in% cfg$sol_ids, ]
    remaining  <- head(filtered[!filtered$solID %in% cfg$sol_ids, ], cfg$top_n)
    top_models <- bind_rows(pinned, remaining)
  } else if (!isTRUE(cfg$pin_solutions) && has_solutions) {
    # Only the manually specified solutions
    top_models <- filtered[filtered$solID %in% cfg$sol_ids, ]
  } else {
    # No solutions specified — top N by score
    top_models <- head(filtered, cfg$top_n)
  }

  biz_label <- ifelse(IS_ACQ, "pct_cpa_in_range", "pct_roi_in_range")
  cat("\n  ════════════════════════════════════════════════════════════════\n")
  cat("    TOP", nrow(top_models), "MODELS -", toupper(model_type), "\n")
  cat("    Business metric:", biz_label, "\n")
  cat("  ════════════════════════════════════════════════════════════════\n\n")

  top_models %>%
    dplyr::select(solID, score, rsq_train, decomp.rssd, mape,
           n_positive_channels, n_zero_channels, pct_biz_in_range) %>%
    dplyr::mutate(score = round(score, 4), rsq_train = round(rsq_train, 4),
           decomp.rssd = round(decomp.rssd, 4), mape = round(mape, 4),
           pct_biz_in_range = round(pct_biz_in_range, 2)) %>%
    print(n = cfg$top_n)

  # --- Build export table ---
  top_sol_ids <- top_models$solID

  export_data <- pareto %>%
    filter(solID %in% top_sol_ids) %>%
    dplyr::select(.source_folder, rn, xDecompAgg, xDecompPerc, spend_share, effect_share,
           rsq_train, nrmse_train, decomp.rssd, mape, solID, cpa_total, roi_total)

  rank_lookup <- top_models %>%
    mutate(model_rank = row_number()) %>%
    dplyr::select(solID, model_rank, score)

  export_data <- export_data %>%
    left_join(rank_lookup, by = "solID") %>%
    arrange(model_rank, rn)

  cat("  Export table:", nrow(export_data), "rows across", length(top_sol_ids), "models\n")

  return(list(pareto = pareto, top_sol_ids = top_sol_ids, export_data = export_data, is_acq = IS_ACQ))
}

# ============================================================================
# HELPER: Build pivoted summary and write to a sheet
# ============================================================================

build_pivoted_sheet <- function(wb, sheet_name, pareto_data, sol_ids, is_acq) {
  biz_label <- ifelse(is_acq, "CPA", "ROI")

  # Remove existing sheet
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }

  # Prepare paid media data
  pareto_data$is_paid_media <- grepl(
    "^(total_impressions_|total_spend_|total_clicks_)", pareto_data$rn
  )
  paid_top <- pareto_data %>%
    dplyr::filter(solID %in% sol_ids, is_paid_media) %>%
    dplyr::select(rn, solID, xDecompAgg, xDecompPerc, spend_share, effect_share,
           total_spend, cpa_total, roi_total, .source_folder)

  paid_top$channel <- gsub("^(total_spend_|total_impressions_|total_clicks_)", "", paid_top$rn)
  channels <- unique(paid_top$channel)
  ranked_ids <- sol_ids

  # Total ROI or CPA per model
  model_totals <- paid_top %>%
    group_by(solID) %>%
    summarise(
      sum_xDecompAgg = sum(xDecompAgg, na.rm = TRUE),
      sum_total_spend = sum(total_spend, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_roi = ifelse(sum_total_spend > 0, sum_xDecompAgg / sum_total_spend, NA),
      total_cpa = ifelse(sum_xDecompAgg > 0, sum_total_spend / sum_xDecompAgg, NA)
    )

  # Model-level metrics
  model_meta <- pareto_data %>%
    dplyr::filter(solID %in% sol_ids) %>%
    group_by(solID) %>%
    summarise(
      rsq_train   = first(rsq_train),
      decomp.rssd = first(decomp.rssd),
      mape        = first(mape),
      .groups = "drop"
    ) %>%
    left_join(model_totals, by = "solID")

  # solID -> source folder lookup
  sol_folder_lookup <- pareto_data %>%
    dplyr::filter(solID %in% sol_ids) %>%
    group_by(solID) %>%
    summarise(.source_folder = first(.source_folder), .groups = "drop")

  # Build wide matrix
  n_models <- length(ranked_ids)
  header_cols <- c("Variable", "Total Spend", "Spend Share")
  for (sid in ranked_ids) {
    header_cols <- c(header_cols,
                     paste0(sid, " Effect Share"),
                     paste0(sid, " ", biz_label))
  }
  n_cols <- length(header_cols)

  # Header rows
  folder_row <- c("Folder", "", "")
  for (sid in ranked_ids) {
    sf <- sol_folder_lookup$.source_folder[sol_folder_lookup$solID == sid]
    folder_row <- c(folder_row, sf, "")
  }

  model_id_row <- c("Model ID", "", "")
  for (sid in ranked_ids) model_id_row <- c(model_id_row, sid, "")

  rsq_row <- c("R Square", "", "")
  for (sid in ranked_ids) {
    meta <- model_meta[model_meta$solID == sid, ]
    rsq_row <- c(rsq_row, round(meta$rsq_train, 4), "")
  }

  rssd_row <- c("Decomp RSSD", "", "")
  for (sid in ranked_ids) {
    meta <- model_meta[model_meta$solID == sid, ]
    rssd_row <- c(rssd_row, round(meta$decomp.rssd, 4), "")
  }

  mape_row <- c("Calibrated MAPE", "", "")
  for (sid in ranked_ids) {
    meta <- model_meta[model_meta$solID == sid, ]
    mape_row <- c(mape_row, round(meta$mape, 4), "")
  }

  biz_total_row <- c(paste0("Total ", biz_label), "", "")
  for (sid in ranked_ids) {
    meta <- model_meta[model_meta$solID == sid, ]
    val <- if (is_acq) round(meta$total_cpa, 2) else round(meta$total_roi, 4)
    biz_total_row <- c(biz_total_row, val, "")
  }

  # Body rows (one per channel)
  body_rows <- list()
  for (ch in channels) {
    row <- c(ch)
    ch_data_first <- paid_top %>% dplyr::filter(channel == ch, solID == ranked_ids[1])
    if (nrow(ch_data_first) > 0) {
      row <- c(row, round(ch_data_first$total_spend[1], 2),
               round(ch_data_first$spend_share[1], 4))
    } else {
      row <- c(row, 0, 0)
    }
    for (sid in ranked_ids) {
      ch_data <- paid_top %>% dplyr::filter(channel == ch, solID == sid)
      if (nrow(ch_data) > 0) {
        eff <- round(ch_data$effect_share[1], 4)
        biz <- if (is_acq) round(ch_data$cpa_total[1], 2) else round(ch_data$roi_total[1], 4)
        row <- c(row, eff, biz)
      } else {
        row <- c(row, 0, NA)
      }
    }
    body_rows[[length(body_rows) + 1]] <- row
  }

  # Assemble
  all_rows <- list(folder_row, model_id_row, rsq_row, rssd_row, mape_row, biz_total_row)
  all_rows[[length(all_rows) + 1]] <- rep("", n_cols)
  all_rows[[length(all_rows) + 1]] <- header_cols
  for (br in body_rows) all_rows[[length(all_rows) + 1]] <- br

  summary_df <- do.call(rbind, lapply(all_rows, function(r) {
    as.data.frame(t(r), stringsAsFactors = FALSE)
  }))
  colnames(summary_df) <- paste0("V", 1:n_cols)

  # Write sheet
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, summary_df, colNames = FALSE)
  setColWidths(wb, sheet_name, cols = 1:n_cols, widths = "auto")

  bold_style <- createStyle(textDecoration = "bold")
  addStyle(wb, sheet_name, bold_style, rows = 1:6, cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, sheet_name, bold_style, rows = 8, cols = 1:n_cols, gridExpand = TRUE)

  cat("  Pivoted tab created:", sheet_name, "with", length(channels), "channels x", n_models, "models\n")
}

# ============================================================================
# EXPORT TO EXCEL — loop over all enabled model types
# ============================================================================

export_filename <- "top_models_selection.xlsx"
local_path <- file.path("model_results", export_filename)

# Load existing workbook or create new one
if (file.exists(local_path)) {
  cat("Loading existing Excel file to add/update tabs...\n")
  wb <- loadWorkbook(local_path)
} else {
  cat("Creating new Excel file...\n")
  wb <- createWorkbook()
}

for (model_type in names(MODEL_CONFIG)) {
  cfg <- MODEL_CONFIG[[model_type]]

  # --- Raw + Flat tabs: only for enabled model types ---
  if (isTRUE(cfg$enabled)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  SCORING:", toupper(model_type), "\n")
    cat("══════════════════════════════════════════════════════════════\n")

    result <- score_and_select(cfg, model_type)

    if (!is.null(result)) {
      # Tab 1: raw data (long format)
      raw_sheet <- model_type
      if (raw_sheet %in% names(wb)) removeWorksheet(wb, raw_sheet)
      addWorksheet(wb, raw_sheet)
      writeData(wb, raw_sheet, result$export_data)
      setColWidths(wb, raw_sheet, cols = 1:ncol(result$export_data), widths = "auto")
      cat("  Raw tab created:", raw_sheet, "\n")

      # Tab 2: flat (pivoted view of top N scored models)
      flat_sheet <- paste0(model_type, "_flat")
      build_pivoted_sheet(wb, flat_sheet, result$pareto, result$top_sol_ids, result$is_acq)

      # JSON export: runs only when export = TRUE in config and EXPORT_TOP_N > 0
      if (isTRUE(cfg$export) && EXPORT_TOP_N > 0) {
        library(Robyn)
        if (length(cfg$sol_ids) > 0) {
          to_export <- cfg$sol_ids
          cat("\n  Exporting", length(to_export), "config-specified models for", model_type, "...\n")
        } else {
          to_export <- head(result$top_sol_ids, EXPORT_TOP_N)
          cat("\n  Exporting top", length(to_export), "scored models for", model_type, "...\n")
        }

        for (sol in to_export) {
          sol_folder <- result$pareto %>%
            dplyr::filter(solID == sol) %>%
            pull(.source_folder) %>%
            first()

          rds_ic <- file.path(sol_folder, "InputCollect.RDS")
          rds_oc <- file.path(sol_folder, "OutputCollect.RDS")

          if (!file.exists(rds_ic) || !file.exists(rds_oc)) {
            cat("  SKIP", sol, "— RDS files not found in:", sol_folder, "\n")
            next
          }

          IC <- readRDS(rds_ic)
          OC <- readRDS(rds_oc)
          robyn_write(IC, OC, select_model = sol, dir = sol_folder, export = TRUE)
          cat("  Exported: RobynModel-", sol, ".json ->", sol_folder, "\n", sep = "")
        }
      }
    }
  } else {
    cat("\nSkipping raw/flat for", model_type, "(enabled = FALSE)\n")
  }

  # --- Summary tab: always process if sol_ids are set (regardless of enabled) ---
  if (length(cfg$sol_ids) > 0) {
    cat("\n--- Building summary tab:", model_type, "---\n")

    s_is_acq <- grepl("acq", model_type, ignore.case = TRUE)

    # Load pareto data from folders
    s_pareto_list <- list()
    for (sf in cfg$folders) {
      s_csv_path <- file.path(sf, "pareto_aggregated.csv")
      if (!file.exists(s_csv_path)) {
        cat("  WARNING: File not found:", s_csv_path, "- skipping\n")
        next
      }
      cat("  Loading:", s_csv_path, "\n")
      s_df <- read.csv(s_csv_path, stringsAsFactors = FALSE)
      s_df$.source_folder <- sf
      s_pareto_list[[length(s_pareto_list) + 1]] <- s_df
    }
    if (length(s_pareto_list) == 0) {
      cat("  No data files found - skipping summary\n")
      next
    }
    s_pareto <- bind_rows(s_pareto_list)

    # Validate solIDs
    s_sol_ids <- cfg$sol_ids
    missing_ids <- setdiff(s_sol_ids, unique(s_pareto$solID))
    if (length(missing_ids) > 0) {
      cat("  WARNING: solIDs not found in data:", paste(missing_ids, collapse = ", "), "\n")
    }
    s_sol_ids <- intersect(s_sol_ids, unique(s_pareto$solID))
    if (length(s_sol_ids) > 0) {
      summary_sheet <- paste0(model_type, "_summary")
      build_pivoted_sheet(wb, summary_sheet, s_pareto, s_sol_ids, s_is_acq)
    } else {
      cat("  No valid solIDs - skipping summary\n")
    }
  }
}

# Save locally
saveWorkbook(wb, local_path, overwrite = TRUE)
cat("\nSaved locally:", local_path, "\n")
cat("Tabs in file:", paste(names(loadWorkbook(local_path)), collapse = ", "), "\n")

# Upload to Google Drive
cat("Uploading to Google Drive...\n")
drive_auth()

drive_files <- drive_ls(as_id(GDRIVE_FOLDER_ID))
existing <- drive_files[drive_files$name == export_filename, ]

if (nrow(existing) > 0) {
  drive_update(file = as_id(existing$id[1]), media = local_path)
  cat("  Updated existing file on Drive\n")
} else {
  drive_upload(media = local_path, path = as_id(GDRIVE_FOLDER_ID), name = export_filename)
  cat("  Uploaded new file to Drive\n")
}

cat("\n════════════════════════════════════════════════════════════════\n")
cat("  DONE - Exported to Google Drive\n")
cat("  File:", export_filename, "\n")
cat("  Tabs:", paste(names(loadWorkbook(local_path)), collapse = ", "), "\n")
cat("════════════════════════════════════════════════════════════════\n")
