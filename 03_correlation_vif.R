# ==============================================================================
# QUARTERLY MMM CORRELATION & VIF ANALYSIS - FINAL VERSION
# Script 3: Correlation and multicollinearity analysis
# ==============================================================================

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(car)  # For VIF
library(ggplot2)
library(googledrive)  # For Google Drive export

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# Setup ------------------------------------------------------------------------
cat("\n=== STARTING CORRELATION & VIF ANALYSIS ===\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create output folder
output_base <- project$output_dirs$correlation
if (!dir.exists(output_base)) dir.create(output_base)

# ==============================================================================
# GOOGLE DRIVE SETUP
# ==============================================================================

cat("Setting up Google Drive connection...\n")

# Initialize gdrive_enabled as FALSE by default
gdrive_enabled <- FALSE

# Authenticate with Google Drive
tryCatch({
  # Load googledrive package
  if (!require(googledrive, quietly = TRUE)) {
    install.packages("googledrive", repos = "https://cloud.r-project.org")
    library(googledrive)
  }
  
  drive_auth()
  
  # FOLDER IDs
  source_folder_id <- drive_folders$data_clean
  target_folder_id <- drive_folders$model_data
  
  # Check if folders exist and are accessible
  source_folder_info <- drive_get(as_id(source_folder_id))
  target_folder_info <- drive_get(as_id(target_folder_id))
  
  if (nrow(source_folder_info) > 0 && nrow(target_folder_info) > 0) {
    cat("  ✓ Connected to source folder:", source_folder_info$name, "\n")
    cat("    (Source folder ID:", source_folder_id, ")\n")
    cat("  ✓ Connected to target folder:", target_folder_info$name, "\n")
    cat("    (Target folder ID:", target_folder_id, ")\n\n")
    gdrive_enabled <- TRUE
  } else {
    cat("  ⚠️  Warning: Could not access Google Drive folders\n")
    cat("  Analysis will continue with local files only\n\n")
    gdrive_enabled <- FALSE
  }
}, error = function(e) {
  cat("  ⚠️  Warning: Google Drive setup failed:", e$message, "\n")
  cat("  Analysis will continue with local files only\n\n")
  gdrive_enabled <- FALSE
})

cat("Google Drive import/export enabled:", gdrive_enabled, "\n\n")

# ==============================================================================
# LOAD DATA FROM GOOGLE DRIVE
# ==============================================================================

cat("Loading MMM datasets...\n")

if (gdrive_enabled) {
  cat("  Attempting to load from Google Drive...\n\n")
  
  # List all files in the source folder
  all_files <- drive_ls(as_id(source_folder_id))
  
  # Filter for MMM dataset files
  mmm_files <- all_files %>%
    filter(grepl("mmm_dataset_.*\\.csv$", name))
  
  if (nrow(mmm_files) == 0) {
    cat("  ⚠️  No MMM dataset files found in Google Drive\n")
    cat("  Falling back to local files...\n\n")
    use_local <- TRUE
  } else {
    cat("  Found", nrow(mmm_files), "MMM dataset files in Google Drive\n\n")
    use_local <- FALSE
  }
  
} else {
  cat("  Google Drive not available, using local files...\n\n")
  use_local <- TRUE
}

# Determine available countries
if (use_local) {
  # Use local files
  countries_available <- list.dirs("data_exploration", recursive = FALSE, full.names = FALSE)
  cat("Found", length(countries_available), "countries (local):\n")
  cat("  ", paste(countries_available, collapse = ", "), "\n\n")
  
} else {
  # Extract countries from Google Drive filenames
  countries_available <- unique(gsub("_mmm_dataset_.*", "", mmm_files$name))
  cat("Found", length(countries_available), "countries (Google Drive):\n")
  cat("  ", paste(countries_available, collapse = ", "), "\n\n")
}

# ==============================================================================
# PROCESS EACH COUNTRY
# ==============================================================================

for (current_country in countries_available) {
  
  cat("#" %>% rep(80) %>% paste(collapse = ""), "\n")
  cat("### ANALYZING: ", toupper(current_country), "\n", sep = "")
  cat("#" %>% rep(80) %>% paste(collapse = ""), "\n\n")
  
  # Create country-specific folder
  country_folder <- file.path(output_base, current_country)
  if (!dir.exists(country_folder)) dir.create(country_folder, recursive = TRUE)
  
  # =========================================================================
  # LOAD MMM DATASET (FROM GOOGLE DRIVE OR LOCAL)
  # =========================================================================
  
  if (!use_local && gdrive_enabled) {
    # Load from Google Drive
    cat("Loading MMM dataset from Google Drive...\n")
    
    # Find files for this country
    country_files <- mmm_files %>%
      filter(grepl(paste0("^", current_country, "_mmm_dataset_"), name)) %>%
      arrange(desc(name))  # Sort by name (timestamps in name)
    
    if (nrow(country_files) == 0) {
      cat("⚠️  No MMM dataset found for", current_country, "in Google Drive - skipping\n\n")
      next
    }
    
    # Get the most recent file
    latest_file <- country_files[1, ]
    cat("  Found latest file:", latest_file$name, "\n")
    
    # Download to temp location
    temp_file <- tempfile(fileext = ".csv")
    drive_download(
      as_id(latest_file$id),
      path = temp_file,
      overwrite = TRUE
    )
    
    # Read the data
    mmm_data <- read.csv(temp_file, stringsAsFactors = FALSE)
    mmm_data$week_commencing <- as.Date(mmm_data$week_commencing)
    
    cat("  ✓ Downloaded and loaded:", latest_file$name, "\n")
    cat("    Rows:", nrow(mmm_data), "\n")
    cat("    Columns:", ncol(mmm_data), "\n\n")
    
    # Clean up temp file
    unlink(temp_file)
    
  } else {
    # Load from local files
    cat("Loading MMM dataset from local files...\n")
    
    mmm_files_local <- list.files(
      path = file.path("data_exploration", current_country),
      pattern = "^mmm_dataset_.*\\.csv$",
      full.names = TRUE
    )
    
    if (length(mmm_files_local) == 0) {
      cat("⚠️  No MMM dataset found for", current_country, "locally - skipping\n\n")
      next
    }
    
    # Load most recent file
    mmm_data <- read.csv(mmm_files_local[length(mmm_files_local)], stringsAsFactors = FALSE)
    mmm_data$week_commencing <- as.Date(mmm_data$week_commencing)
    
    cat("  ✓ Loaded:", basename(mmm_files_local[length(mmm_files_local)]), "\n")
    cat("    Rows:", nrow(mmm_data), "\n")
    cat("    Columns:", ncol(mmm_data), "\n\n")
  }
  
  # ===========================================================================
  # IDENTIFY VARIABLES
  # ===========================================================================
  
  cat("Identifying variables...\n")
  
  # Get all numeric columns
  all_numeric <- names(mmm_data)[sapply(mmm_data, is.numeric)]
  all_numeric <- setdiff(all_numeric, c(project$date_var, "country"))
  
  # Identify LTV variables to EXCLUDE from VIF
  ltv_vars <- var_selection$ltv_exclude
  ltv_vars <- ltv_vars[ltv_vars %in% all_numeric]

  # Identify acquisition components to EXCLUDE from VIF
  acq_components <- var_selection$acq_exclude
  acq_components <- acq_components[acq_components %in% all_numeric]
  
  # Identify media metrics to EXCLUDE from VIF (keeping only spend)
  impressions_vars <- grep("^total_impressions_", all_numeric, value = TRUE)
  clicks_vars <- grep("^total_clicks_", all_numeric, value = TRUE)
  installs_vars <- grep("^total_installs_", all_numeric, value = TRUE)
  qr_vars <- grep("^total_qr_", all_numeric, value = TRUE)
  vv_vars <- grep("^total_vv_", all_numeric, value = TRUE)
  spend_vars <- grep("^total_spend_", all_numeric, value = TRUE)
  
  # Variables to EXCLUDE from VIF analysis
  vars_to_exclude_vif <- unique(c(ltv_vars, acq_components, impressions_vars, clicks_vars, installs_vars, qr_vars, vv_vars, spend_vars))
  
  # Identify dependent variable
  dep_var <- project$dep_vars[1]

  if (!dep_var %in% names(mmm_data)) {
    cat("  ⚠️  WARNING:", dep_var, "not found!\n\n")
    next
  }

  cat("  ✓ Using", dep_var, "as dependent variable\n")

  # ALL PREDICTORS (for correlation analysis) - exclude all dep vars, not just the primary one
  all_predictors <- setdiff(all_numeric, project$dep_vars)
  
  # Remove zero-variance columns from all predictors
  zero_var <- sapply(all_predictors, function(col) {
    sd(mmm_data[[col]], na.rm = TRUE) == 0
  })
  zero_var_list <- names(zero_var[zero_var])
  all_predictors <- all_predictors[!zero_var]
  
  # PREDICTORS FOR VIF (excluding media metrics, LTV, acq components)
  predictors_for_vif <- setdiff(all_predictors, vars_to_exclude_vif)
  
  cat("  Total variables (excluding DV):", length(all_predictors), "\n")
  cat("  Variables for VIF analysis:", length(predictors_for_vif), "\n")
  cat("  Variables excluded from VIF:", length(vars_to_exclude_vif), "\n")
  cat("  Zero-variance variables removed:", length(zero_var_list), "\n\n")
  
  # ===========================================================================
  # INITIALIZE DROPPED VARIABLES TRACKER
  # ===========================================================================
  
  cat("Initializing dropped variables tracker...\n")
  
  # Use list to collect tracking entries
  dropped_list <- list()
  
  # Track all dependent variables
  dep_vars_present <- project$dep_vars[project$dep_vars %in% all_numeric]
  dropped_list[[length(dropped_list) + 1]] <- data.frame(
    variable = dep_vars_present,
    drop_reason = "Dependent variable",
    drop_stage = "0_Dependent_Variable",
    stringsAsFactors = FALSE
  )
  
  # Track zero variance
  if (length(zero_var_list) > 0) {
    dropped_list[[length(dropped_list) + 1]] <- data.frame(
      variable = zero_var_list,
      drop_reason = "Zero variance",
      drop_stage = "1_Zero_Variance",
      stringsAsFactors = FALSE
    )
  }
  
  # Track variables excluded from VIF (but NOT dropped from final dataset)
  if (length(vars_to_exclude_vif) > 0) {
    
    reasons <- character(length(vars_to_exclude_vif))
    reasons[vars_to_exclude_vif %in% ltv_vars] <- "Excluded from VIF only (LTV variable)"
    reasons[vars_to_exclude_vif %in% acq_components] <- "Excluded from VIF only (Acquisition component)"
    reasons[vars_to_exclude_vif %in% impressions_vars] <- "Excluded from VIF only (Impressions metric)"
    reasons[vars_to_exclude_vif %in% clicks_vars] <- "Excluded from VIF only (Clicks metric)"
    reasons[vars_to_exclude_vif %in% installs_vars] <- "Excluded from VIF only (Installs metric)"
    reasons[vars_to_exclude_vif %in% qr_vars] <- "Excluded from VIF only (Quality reads metric)"
    reasons[vars_to_exclude_vif %in% vv_vars]    <- "Excluded from VIF only (Video views metric)"
    reasons[vars_to_exclude_vif %in% spend_vars] <- "Excluded from VIF only (Spend metric)"

    dropped_list[[length(dropped_list) + 1]] <- data.frame(
      variable = vars_to_exclude_vif,
      drop_reason = reasons,
      drop_stage = "1a_Excluded_from_VIF_Only",
      stringsAsFactors = FALSE
    )
  }
  
  cat("  Initial tracking complete\n\n")
  
  # ===========================================================================
  # STEP 1: CORRELATION PAIRS (ALL PREDICTORS)
  # ===========================================================================

  cat("STEP 1: Finding correlation pairs >", var_selection$corr_pair_threshold, "(all variables)...\n\n")
  
  if (length(all_predictors) > 1) {
    
    # Calculate correlation matrix
    cor_matrix_all <- cor(mmm_data[, all_predictors, drop = FALSE], use = "pairwise.complete.obs")
    
    # Find pairs with |r| > 0.7
    cor_upper <- cor_matrix_all
    cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
    
    high_cor_indices <- which(abs(cor_upper) > var_selection$corr_pair_threshold, arr.ind = TRUE)
    
    if (nrow(high_cor_indices) > 0) {
      high_cor_pairs <- data.frame(
        variable1 = all_predictors[high_cor_indices[, 1]],
        variable2 = all_predictors[high_cor_indices[, 2]],
        correlation = round(cor_upper[high_cor_indices], 3),
        abs_correlation = round(abs(cor_upper[high_cor_indices]), 3),
        stringsAsFactors = FALSE
      ) %>% arrange(desc(abs_correlation))
      
      # Export
      write.csv(
        high_cor_pairs,
        file.path(country_folder, "1_correlation_pairs_above_0.7.csv"),
        row.names = FALSE
      )
      
      cat("  ✓ Found", nrow(high_cor_pairs), "pairs with |r| >", var_selection$corr_pair_threshold, "\n")
      cat("  ✓ Exported to: 1_correlation_pairs_above_0.7.csv\n\n")
      
      # Show top 10
      cat("  Top 10 correlated pairs:\n")
      print(head(high_cor_pairs %>% select(variable1, variable2, correlation), 10))
      cat("\n")
      
    } else {
      cat("  ✓ No pairs with |r| >", var_selection$corr_pair_threshold, "found\n\n")
      
      write.csv(
        data.frame(variable1 = character(), variable2 = character(), 
                   correlation = numeric(), abs_correlation = numeric()),
        file.path(country_folder, "1_correlation_pairs_above_0.7.csv"),
        row.names = FALSE
      )
    }
    
  } else {
    cat("  ⚠️  Not enough predictors for correlation analysis\n\n")
  }
  
  # ===========================================================================
  # STEP 2: VIF ANALYSIS (EXCLUDING MEDIA METRICS, LTV, ACQ COMPONENTS)
  # ===========================================================================
  
  cat("STEP 2: Calculating VIF (excluding media metrics, LTV, acq components)...\n\n")
  
  vif_table <- data.frame()
  
  if (length(predictors_for_vif) < 2) {
    cat("  ⚠️  Not enough predictors for VIF analysis\n\n")
  } else {
    
    # -------------------------------------------------------------------------
    # STEP 2.1: PREPARE ACTIVE PREDICTORS
    # -------------------------------------------------------------------------
    
    # Filter predictors with variance
    has_variance <- sapply(mmm_data[predictors_for_vif], function(x) {
      !all(is.na(x)) && sd(x, na.rm = TRUE) > 0
    })
    predictors_active <- predictors_for_vif[has_variance]
    
    cat("  Starting with", length(predictors_active), "active predictors for VIF\n\n")
    
    if (length(predictors_active) < 2) {
      cat("  ⚠️  Not enough active predictors\n\n")
    } else {
      
      # -----------------------------------------------------------------------
      # STEP 2.2: CALCULATE INITIAL CORRELATION MATRIX
      # -----------------------------------------------------------------------
      
      cat("  Calculating initial correlation matrix...\n")
      cor_matrix <- cor(mmm_data[, predictors_active, drop = FALSE], use = "pairwise.complete.obs")
      cat("    ✓ Initial correlation matrix ready\n\n")
      
      # -----------------------------------------------------------------------
      # STEP 2.3: DROP SPECIFIC _DIFF VARIABLES
      # -----------------------------------------------------------------------
      
      cat("  Step 2.3: Removing specific traffic _diff variables...\n")
      
      # Define specific diff variables to remove
      specific_diff_vars <- var_selection$diff_vars_remove
      
      # Find which ones are actually present
      diff_vars <- specific_diff_vars[specific_diff_vars %in% predictors_active]
      
      if (length(diff_vars) > 0) {
        cat("    Removing", length(diff_vars), "specific traffic _diff variables\n")
        
        # TRACK DROPPED _DIFF VARS
        dropped_list[[length(dropped_list) + 1]] <- data.frame(
          variable = diff_vars,
          drop_reason = "Specific traffic difference variable (_diff suffix)",
          drop_stage = "2_VIF_Cleanup_Diff_Vars",
          stringsAsFactors = FALSE
        )
        
        predictors_active <- setdiff(predictors_active, diff_vars)
      } else {
        cat("    No specific traffic _diff variables found\n")
      }
      cat("\n")
      
      if (length(predictors_active) < 2) {
        cat("  ⚠️  Not enough predictors remaining\n\n")
      } else {
        
        # ---------------------------------------------------------------------
        # STEP 2.4: HANDLE PROPORTION VARIABLES
        # ---------------------------------------------------------------------
        
        cat("  Step 2.4: Handling proportion variables...\n")
        
        # Define proportion groups
        proportion_groups <- list(
          share_vars = c("share_bundle", "share_digital_premium", "share_digital_standard",
                         "share_digital_other", "share_print"),
          cluster_vars = c("pct_cluster_email_only", "pct_cluster_advocates", "pct_cluster_disengaged",
                           "pct_cluster_fans", "pct_cluster_readers_with_email",
                           "pct_cluster_readers_wo_email", "pct_cluster_skimmers", "pct_cluster_superfans"),
          discount_seen_vars = c("seen_1_10_discount", "seen_11_25_discount",
                                 "seen_26_33_discount", "seen_34_50_discount"),
          discount_prop_vars = c("prop_1_10_discount", "prop_11_25_discount", "prop_26_33_discount",
                                 "prop_34_50_discount", "prop_50_plus_discount", "prop_rrp")
        )

        # Identify variables to drop
        vars_to_drop_proportion <- c()
        
        for (group_name in names(proportion_groups)) {
          group_vars_present <- proportion_groups[[group_name]][proportion_groups[[group_name]] %in% predictors_active]
          
          if (length(group_vars_present) > 1) {
            cat("    Group:", group_name, "(", length(group_vars_present), "vars) - ", sep = "")
            
            # Calculate correlations with total_acquisitions
            cors <- sapply(mmm_data[group_vars_present], function(x) {
              cor(x, mmm_data[[dep_var]], use = "pairwise.complete.obs")
            })
            
            # Drop variable with lowest absolute correlation
            lowest <- names(cors)[which.min(abs(cors))]
            vars_to_drop_proportion <- c(vars_to_drop_proportion, lowest)
            cat("drop ", lowest, " (|r| = ", round(abs(cors[lowest]), 3), ")\n", sep = "")
          }
        }
        
        vars_to_drop_proportion <- unique(vars_to_drop_proportion)
        
        if (length(vars_to_drop_proportion) > 0) {
          
          # TRACK DROPPED PROPORTION VARS
          dropped_list[[length(dropped_list) + 1]] <- data.frame(
            variable = vars_to_drop_proportion,
            drop_reason = "Proportion variable (lowest |r| with DV in group)",
            drop_stage = "3_VIF_Cleanup_Proportion_Vars",
            stringsAsFactors = FALSE
          )
          
          predictors_active <- setdiff(predictors_active, vars_to_drop_proportion)
          cat("  Dropped", length(vars_to_drop_proportion), "proportion variables\n\n")
        } else {
          cat("  No proportion variables to drop\n\n")
        }
        
        # ---------------------------------------------------------------------
        # STEP 2.5: RECALCULATE CORRELATION MATRIX
        # ---------------------------------------------------------------------
        
        cat("  Step 2.5: Recalculating correlation matrix...\n")
        cor_matrix <- cor(mmm_data[, predictors_active, drop = FALSE], use = "pairwise.complete.obs")
        cat("    ✓ Updated correlation matrix\n\n")
        
        # ---------------------------------------------------------------------
        # STEP 2.6: REMOVE PERFECT CORRELATIONS (SKIP SPEND-METRIC PAIRS)
        # ---------------------------------------------------------------------
        
        cat("  Step 2.6: Checking for perfect correlations (excluding spend-metric pairs)...\n")
        
        # Use vectorized approach
        cor_upper <- cor_matrix
        cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
        perfect_indices <- which(abs(cor_upper) >= var_selection$perfect_corr_threshold, arr.ind = TRUE)
        
        if (nrow(perfect_indices) > 0) {
          perfect_cors <- data.frame(
            var1 = predictors_active[perfect_indices[, 1]],
            var2 = predictors_active[perfect_indices[, 2]],
            correlation = round(cor_upper[perfect_indices], 5),
            stringsAsFactors = FALSE
          )
          
          # FILTER OUT SPEND-METRIC PAIRS (we want to keep these)
          is_spend_metric_pair <- sapply(1:nrow(perfect_cors), function(i) {
            v1 <- perfect_cors$var1[i]
            v2 <- perfect_cors$var2[i]
            
            # Check if one is spend and other is metric (impressions, clicks, installs, qr, vv)
            (grepl("^total_spend_", v1) && grepl("^total_(impressions|clicks|installs|qr|vv)_", v2)) ||
              (grepl("^total_spend_", v2) && grepl("^total_(impressions|clicks|installs|qr|vv)_", v1))
          })
          
          # Keep only non-spend-metric pairs
          perfect_cors_to_drop <- perfect_cors[!is_spend_metric_pair, ]
          perfect_cors_kept <- perfect_cors[is_spend_metric_pair, ]
          
          if (nrow(perfect_cors_kept) > 0) {
            cat("    ℹ️  Found", nrow(perfect_cors_kept), "spend-metric perfect correlations (keeping these)\n")
          }
          
          if (nrow(perfect_cors_to_drop) > 0) {
            cat("    ⚠️  Found", nrow(perfect_cors_to_drop), "other perfectly correlated pairs (removing)\n")
            
            # Export
            write.csv(
              perfect_cors_to_drop,
              file.path(country_folder, "2a_perfect_correlations_removed.csv"),
              row.names = FALSE
            )
            
            # Remove second variable
            vars_to_remove <- unique(perfect_cors_to_drop$var2)
            
            # TRACK DROPPED PERFECT CORRELATION VARS
            dropped_list[[length(dropped_list) + 1]] <- data.frame(
              variable = vars_to_remove,
              drop_reason = paste0("Perfect correlation (|r| >= ", var_selection$perfect_corr_threshold, ")"),
              drop_stage = "4_VIF_Cleanup_Perfect_Cors",
              stringsAsFactors = FALSE
            )
            
            predictors_active <- setdiff(predictors_active, vars_to_remove)
            cat("    Removed", length(vars_to_remove), "variables\n")
            
            # Recalculate
            cor_matrix <- cor(mmm_data[, predictors_active, drop = FALSE], use = "pairwise.complete.obs")
          } else {
            cat("    ✓ No perfect correlations to remove (spend-metric pairs kept)\n")
          }
        } else {
          cat("    ✓ No perfect correlations found\n")
        }
        cat("\n")
        
        # ---------------------------------------------------------------------
        # STEP 2.7: REMOVE LINEAR DEPENDENCIES
        # ---------------------------------------------------------------------
        
        cat("  Step 2.7: Checking for linear dependencies...\n")
        
        if (length(predictors_active) >= 2) {
          test_formula <- as.formula(paste(dep_var, "~", paste(predictors_active, collapse = " + ")))
          test_model <- lm(test_formula, data = mmm_data)
          aliased_check <- alias(test_model)
          
          if (!is.null(aliased_check$Complete) && nrow(aliased_check$Complete) > 0) {
            aliased_vars <- rownames(aliased_check$Complete)
            cat("    ⚠️  Found", length(aliased_vars), "linearly dependent variables\n")
            
            # TRACK DROPPED LINEAR DEPENDENCY VARS
            dropped_list[[length(dropped_list) + 1]] <- data.frame(
              variable = aliased_vars,
              drop_reason = "Linear dependency",
              drop_stage = "5_VIF_Cleanup_Linear_Deps",
              stringsAsFactors = FALSE
            )
            
            predictors_active <- setdiff(predictors_active, aliased_vars)
            cat("    Removed", length(aliased_vars), "variables\n")
            
            # Recalculate
            cor_matrix <- cor(mmm_data[, predictors_active, drop = FALSE], use = "pairwise.complete.obs")
          } else {
            cat("    ✓ No linear dependencies found\n")
          }
        }
        cat("\n")
        
        cat("  Predictors after all cleanup:", length(predictors_active), "\n\n")
        
        # ---------------------------------------------------------------------
        # STEP 2.8: CALCULATE VIF
        # ---------------------------------------------------------------------
        
        vif_pairs <- NULL

        n_obs  <- nrow(mmm_data)
        n_pred <- length(predictors_active)
        cat("  n/p check — observations:", n_obs, "| predictors:", n_pred,
            "| residual df:", n_obs - n_pred - 1, "\n\n")

        if (length(predictors_active) < 2) {
          cat("  ⚠️  Not enough predictors remaining for VIF\n\n")
        } else if ((n_obs - n_pred - 1) <= 5) {
          cat("  ⚠️  Near-saturated model (n/p =", round(n_obs / n_pred, 2),
              ") — VIF will produce NaN. Skipping VIF for", current_country, "\n")
          cat("     Reduce the number of predictors or increase the observation window.\n\n")
        } else {

          cat("  Step 2.8: Calculating VIF...\n")
          cat("    Using", length(predictors_active), "predictors\n")
          
          tryCatch({
            formula_vif <- as.formula(paste(dep_var, "~", paste(predictors_active, collapse = " + ")))
            model_vif   <- lm(formula_vif, data = mmm_data)
            vif_values  <- vif(model_vif)

            # ---------------------------------------------------------------
            # CREATE VIF SUMMARY TABLE
            # ---------------------------------------------------------------
            
            vif_table <- data.frame(
              country = current_country,
              variable = names(vif_values),
              VIF = as.numeric(vif_values),
              stringsAsFactors = FALSE
            )
            
            # Add number of correlation pairs for each variable
            cat("    Calculating correlation pairs...\n")
            
            vif_table$num_pairs_above_0.7 <- sapply(vif_table$variable, function(var) {
              if (var %in% rownames(cor_matrix)) {
                cors <- cor_matrix[var, ]
                sum(names(cors) != var & abs(cors) > var_selection$corr_pair_threshold, na.rm = TRUE)
              } else {
                NA
              }
            })
            
            # Add VIF category
            vif_table <- vif_table %>%
              mutate(
                VIF_category = case_when(
                  VIF < 5 ~ "Low (< 5)",
                  VIF >= 5 & VIF < 10 ~ "Moderate (5-10)",
                  VIF >= 10 ~ "High (>= 10)"
                )
              ) %>%
              arrange(desc(VIF))
            
            # Export VIF summary table
            write.csv(
              vif_table,
              file.path(country_folder, "2_vif_analysis.csv"),
              row.names = FALSE
            )
            
            cat("    ✓ VIF summary table exported\n")
            
            # ---------------------------------------------------------------
            # CREATE VIF CORRELATION PAIRS TABLE
            # ---------------------------------------------------------------
            
            cat("    Creating detailed correlation pairs table...\n")
            
            # Pre-filter high VIF variables
            high_vif_vars <- vif_table$variable[vif_table$num_pairs_above_0.7 > 0]
            
            if (length(high_vif_vars) > 0) {
              # Build pairs list efficiently
              pairs_list <- lapply(high_vif_vars, function(var) {
                cors <- cor_matrix[var, ]
                high_cors <- cors[names(cors) != var & abs(cors) > var_selection$corr_pair_threshold]
                
                if (length(high_cors) > 0) {
                  data.frame(
                    country = current_country,
                    variable = var,
                    VIF = vif_table$VIF[vif_table$variable == var],
                    correlated_with = names(high_cors),
                    correlation = round(high_cors, 3),
                    abs_correlation = round(abs(high_cors), 3),
                    stringsAsFactors = FALSE
                  )
                } else {
                  NULL
                }
              })
              
              # Combine all pairs
              vif_pairs <- do.call(rbind, pairs_list)
              
              if (!is.null(vif_pairs) && nrow(vif_pairs) > 0) {
                write.csv(
                  vif_pairs,
                  file.path(country_folder, "2b_vif_correlation_pairs.csv"),
                  row.names = FALSE
                )
                cat("    ✓ VIF correlation pairs table exported\n")
              }
            }
            
            cat("    ✓ VIF analysis complete\n\n")
            
            # Print summary
            cat("  VIF Summary:\n")
            cat("    Low (< 5):", sum(vif_table$VIF < 5), "variables\n")
            cat("    Moderate (5-10):", sum(vif_table$VIF >= 5 & vif_table$VIF < 10), "variables\n")
            cat("    High (>= 10):", sum(vif_table$VIF >= 10), "variables\n\n")
            
            if (sum(vif_table$VIF >= 10) > 0) {
              cat("  Variables with high VIF (>= 10):\n")
              high_vif <- vif_table %>% 
                filter(VIF >= 10) %>% 
                select(variable, VIF, num_pairs_above_0.7)
              print(head(high_vif, 10))
              cat("\n")
              
              # Summary of correlation pairs for high VIF variables
              cat("  High VIF variables - correlation pairs:\n")
              cat("    Total variables with VIF >= 10:", nrow(high_vif), "\n")
              cat("    Total correlation pairs (> 0.7):", sum(high_vif$num_pairs_above_0.7, na.rm = TRUE), "\n")
              if (exists("vif_pairs") && !is.null(vif_pairs) && nrow(vif_pairs) > 0) {
                cat("    See detailed pairs in: 2b_vif_correlation_pairs.csv\n")
              }
              cat("\n")
            }
            
          }, error = function(e) {
            cat("    ⚠️  VIF calculation failed:", e$message, "\n\n")
          })
          
          # -------------------------------------------------------------------
          # STEP 2.9: EXPORT CLEANED DATASET
          # -------------------------------------------------------------------
          
          cat("  Step 2.9: Exporting cleaned dataset...\n")
          
          # Get variables actually dropped in VIF (not excluded from VIF)
          vars_dropped_in_vif <- do.call(rbind, dropped_list) %>%
            filter(drop_stage %in% c("2_VIF_Cleanup_Diff_Vars", "3_VIF_Cleanup_Proportion_Vars",
                                     "4_VIF_Cleanup_Perfect_Cors", "5_VIF_Cleanup_Linear_Deps")) %>%
            pull(variable)
          
          # Start with ALL predictors, remove only VIF-dropped variables
          final_predictors <- setdiff(all_predictors, vars_dropped_in_vif)
          
          cat("    Variables in cleaned dataset:", length(final_predictors), "\n")
          cat("    Variables dropped from original:", length(vars_dropped_in_vif), "\n")
          
          # Create cleaned dataset — include ALL dep_vars so both LTV and ACQ
          # models can use the same file
          all_dep_vars_present <- project$dep_vars[project$dep_vars %in% names(mmm_data)]
          cleaned_data <- mmm_data %>%
            select(all_of(c(project$date_var, "country")),
                   all_of(all_dep_vars_present),
                   all_of(final_predictors))
          
          #Double check NA values
          if (any(is.na(cleaned_data))) {
            na_summary <- cleaned_data %>%
              summarise(across(everything(), ~sum(is.na(.)))) %>%
              pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
              filter(na_count > 0) %>%
              arrange(desc(na_count))
            
            cat("    ⚠️  WARNING: NAs detected in cleaned dataset!\n")
            cat("    Variables with NAs:\n")
            print(na_summary)
            cat("\n")
            
            # Optional: uncomment to stop execution
            # stop("Execution halted due to NAs in cleaned dataset for ", current_country)
          } else {
            cat("    ✓ No NAs detected in cleaned dataset\n")
          }
          
          # Export locally
          cleaned_file_path <- file.path(country_folder, "3_cleaned_dataset_for_modeling.csv")
          write.csv(
            cleaned_data,
            cleaned_file_path,
            row.names = FALSE
          )
          
          cat("    ✓ Exported cleaned dataset (", nrow(cleaned_data), " rows x ", ncol(cleaned_data), " cols)\n", sep = "")
          cat("    ✓ File: 3_cleaned_dataset_for_modeling.csv\n")
          
          # -------------------------------------------------------------------
          # UPLOAD TO GOOGLE DRIVE
          # -------------------------------------------------------------------
          
          if (gdrive_enabled) {
            cat("    Uploading to Google Drive...\n")
            
            tryCatch({
              drive_upload(
                media = cleaned_file_path,
                path = as_id(target_folder_id),
                name = paste0(current_country, "_cleaned_dataset.csv"),
                overwrite = TRUE
              )
              
              cat("    ✓ Uploaded to Google Drive as:", paste0(current_country, "_cleaned_dataset.csv"), "\n")
              
            }, error = function(e) {
              cat("    ⚠️  Google Drive upload failed:", e$message, "\n")
            })
          }
          
          cat("\n")
        }
      }
    }
  }
  
  # ===========================================================================
  # STEP 3: SPEND vs EXPOSURE METRIC R-SQUARED ANALYSIS
  # ===========================================================================

  cat("Step 3: R² analysis — spend vs exposure metrics per channel...\n\n")

  spend_cols    <- grep("^total_spend_", names(mmm_data), value = TRUE)
  channel_names <- sub("^total_spend_", "", spend_cols)
  exp_prefixes  <- c("total_impressions", "total_clicks", "total_vv", "total_qr")

  rsq_rows <- list()

  for (ch in channel_names) {
    spend_col <- paste0("total_spend_", ch)

    for (pfx in exp_prefixes) {
      exp_col <- paste0(pfx, "_", ch)
      if (!exp_col %in% names(mmm_data)) next

      df <- mmm_data[complete.cases(mmm_data[, c(spend_col, exp_col)]), c(spend_col, exp_col)]
      if (nrow(df) < 5) next
      if (var(df[[spend_col]]) == 0 || var(df[[exp_col]]) == 0) next

      fit     <- lm(as.formula(paste(exp_col, "~", spend_col)), data = df)
      r2      <- summary(fit)$r.squared
      cor_val <- cor(df[[spend_col]], df[[exp_col]])

      rsq_rows[[length(rsq_rows) + 1]] <- data.frame(
        country         = current_country,
        channel         = ch,
        exposure_metric = sub("^total_", "", pfx),
        n_obs           = nrow(df),
        r_squared       = round(r2, 4),
        correlation     = round(cor_val, 4),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rsq_rows) > 0) {
    rsq_long <- do.call(rbind, rsq_rows)

    # Wide format: one row per channel, one col per exposure metric
    rsq_wide <- rsq_long %>%
      tidyr::pivot_wider(
        id_cols     = c(country, channel),
        names_from  = exposure_metric,
        values_from = r_squared,
        names_prefix = "r2_"
      ) %>%
      arrange(channel)

    rsq_file <- file.path(country_folder, "4_spend_vs_exposure_rsquared.csv")
    write.csv(rsq_wide, rsq_file, row.names = FALSE)
    cat("  Exported:", rsq_file, "\n")
    cat("  Channels analysed:", nrow(rsq_wide), "\n\n")
    print(rsq_wide)
    cat("\n")
  } else {
    cat("  No matching spend/exposure column pairs found.\n\n")
  }

  # ===========================================================================
  # EXPORT DROPPED VARIABLES TRACKER
  # ===========================================================================
  
  cat("Exporting dropped variables summary...\n")
  
  # Combine all dropped entries
  dropped_vars_tracker <- do.call(rbind, dropped_list)
  
  # Create summary
  dropped_summary <- dropped_vars_tracker %>%
    group_by(drop_stage, drop_reason) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(drop_stage)
  
  # Export detailed tracker
  dropped_detail_path <- file.path(country_folder, "0_dropped_variables_detail.csv")
  write.csv(
    dropped_vars_tracker %>% arrange(drop_stage, variable),
    dropped_detail_path,
    row.names = FALSE
  )
  
  # Export summary
  dropped_summary_path <- file.path(country_folder, "0_dropped_variables_summary.csv")
  write.csv(
    dropped_summary,
    dropped_summary_path,
    row.names = FALSE
  )
  
  cat("  ✓ Dropped variables detail: 0_dropped_variables_detail.csv\n")
  cat("  ✓ Dropped variables summary: 0_dropped_variables_summary.csv\n")
  
  # Upload dropped variables tracking to Google Drive
  if (gdrive_enabled) {
    cat("  Uploading dropped variables tracking to Google Drive...\n")
    
    tryCatch({
      drive_upload(
        media = dropped_detail_path,
        path = as_id(target_folder_id),
        name = paste0(current_country, "_dropped_variables_detail.csv"),
        overwrite = TRUE
      )
      
      drive_upload(
        media = dropped_summary_path,
        path = as_id(target_folder_id),
        name = paste0(current_country, "_dropped_variables_summary.csv"),
        overwrite = TRUE
      )
      
      cat("  ✓ Uploaded tracking files to Google Drive\n")
      
    }, error = function(e) {
      cat("  ⚠️  Google Drive upload failed:", e$message, "\n")
    })
  }
  
  cat("\n")
  
  # Print summary
  cat("  Dropped variables by stage:\n")
  print(dropped_summary)
  cat("\n")
  
  # ===========================================================================
  # SUMMARY FOR THIS COUNTRY
  # ===========================================================================
  
  cat("✓ Analysis complete for", toupper(current_country), "\n")
  cat("  Output folder:", country_folder, "\n")
  cat("  Files created:\n")
  cat("    0a. 0_dropped_variables_detail.csv\n")
  cat("    0b. 0_dropped_variables_summary.csv\n")
  cat("    1. 1_correlation_pairs_above_0.7.csv\n")
  
  if (file.exists(file.path(country_folder, "2a_perfect_correlations_removed.csv"))) {
    cat("    2a. 2a_perfect_correlations_removed.csv\n")
  }
  
  if (file.exists(file.path(country_folder, "2_vif_analysis.csv"))) {
    cat("    2. 2_vif_analysis.csv\n")
  }
  
  if (file.exists(file.path(country_folder, "2b_vif_correlation_pairs.csv"))) {
    cat("    2b. 2b_vif_correlation_pairs.csv\n")
  }
  
  if (file.exists(file.path(country_folder, "3_cleaned_dataset_for_modeling.csv"))) {
    cat("    3. 3_cleaned_dataset_for_modeling.csv\n")
  }
  
  if (gdrive_enabled) {
    cat("  \n")
    cat("  ✓ Files also uploaded to Google Drive:\n")
    cat("    Location: Data & Analytics - Mark... > MMM > MMM V3 > Data > correlation_analysis\n")
    cat("    URL: https://drive.google.com/drive/folders/", drive_folders$model_data, "\n", sep = "")
  }
  
  cat("\n")
  
} # End country loop

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("ANALYSIS COMPLETE - ALL COUNTRIES\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("Countries analyzed:", length(countries_available), "\n")
cat("Output location:", output_base, "/\n")

if (gdrive_enabled) {
  cat("Google Drive output location: https://drive.google.com/drive/folders/", drive_folders$model_data, "\n", sep = "")
}

cat("\n✓ Correlation and VIF analysis complete\n")
