# ==============================================================================
# TIERED CONTEXT VARIABLE SELECTION FOR ROBYN
# Combines tiered clustering with VIF, Elastic Net, and Stepwise methods
# ==============================================================================

library(dplyr)
library(tidyr)
library(car)
library(openxlsx)
library(googledrive)
library(glmnet)
library(MASS)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

# ==============================================================================
# CLUSTER CONFIGURATION
# ==============================================================================

cluster_config <- list(
  tier1_threshold        = var_selection$tier1$corr_threshold,
  tier2_threshold        = var_selection$tier2$corr_threshold,
  tier1_max_per_cluster  = var_selection$tier1$max_per_cluster,
  tier2_max_per_cluster  = var_selection$tier2$max_per_cluster,
  tier1_max_per_category = var_selection$tier1$max_per_category,
  tier2_max_per_category = var_selection$tier2$max_per_category
)

# ==============================================================================
# DATA CONFIGURATION
# ==============================================================================

GDRIVE_FOLDER_ID <- drive_folders$model_data
countries        <- project$countries
dependent_vars   <- project$dep_vars
output_dir       <- project$output_dirs$variable_selection

# VIF, Elastic Net, Stepwise Configuration
vif_threshold <- var_selection$vif_threshold
alpha_enet    <- var_selection$elastic_net_alpha

# ==============================================================================
# GOOGLE DRIVE AUTH
# ==============================================================================

drive_auth(email = env$user_email, cache = ".secrets")

# ==============================================================================
# GOOGLE DRIVE FUNCTIONS
# ==============================================================================

download_from_gdrive <- function(folder_id, filename, local_path = "data_exploration") {
  if (!dir.exists(local_path)) {
    dir.create(local_path, recursive = TRUE)
  }

  files <- drive_ls(path = as_id(folder_id))
  file_match <- files %>% filter(name == filename)
  
  if (nrow(file_match) == 0) {
    stop("File not found in Google Drive: ", filename)
  }
  
  local_file <- file.path(local_path, filename)
  drive_download(
    file = as_id(file_match$id),
    path = local_file,
    overwrite = TRUE
  )
  
  cat("Downloaded:", filename, "\n")
  cat("   To:", local_file, "\n\n")
  
  return(local_file)
}

load_model_data <- function(country, gdrive_folder_id = NULL, use_gdrive = TRUE) {
  filename <- project$file_map[[country]]
  
  if (is.null(filename)) {
    stop("No data file mapping found for country: ", country)
  }
  
  if (use_gdrive && !is.null(gdrive_folder_id)) {
    local_path <- file.path("data_exploration", country)
    file_path <- download_from_gdrive(gdrive_folder_id, filename, local_path)
  } else {
    file_path <- file.path("data_exploration", country, filename)
    if (!file.exists(file_path)) {
      stop("Local file not found: ", file_path)
    }
  }
  
  cat("Loading dataset from:", file_path, "\n")
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data[[project$date_var]] <- as.Date(data[[project$date_var]])

  cat("   Rows:", nrow(data), "| Columns:", ncol(data), "\n")
  cat("   Date range:", min(data[[project$date_var]]), "to", max(data[[project$date_var]]), "\n\n")
  
  return(data)
}

# ==============================================================================
# VARIABLE CATEGORIES
# ==============================================================================

variable_categories <- list(
  
  # TIER 1: MANDATORY CATEGORIES (Critical business drivers - must be represented)

  economics = c(
    "cpi_index",
    "cpi_yoy",
    "bci",
    "unemployment_rate",
    "cli",
    "vix"
  ),

  discount = c(
    # seen_ family
    "seen_1_10_discount",
    "seen_11_25_discount",
    "seen_26_33_discount",
    "seen_34_50_discount",
    # prop_ family
    "prop_1_10_discount",
    "prop_11_25_discount",
    "prop_26_33_discount",
    "prop_34_50_discount",
    "prop_50_plus_discount",
    "prop_rrp"
  ),

  product_mix = c(
    "share_bundle",
    "share_digital_premium",
    "share_digital_standard",
    "share_digital_other",
    "share_print"
  ),

  organic = c(
    # Absolute levels
    "organic_search_traffic",
    "organic_social_traffic",
    "organic_push_notification",
    "organic_social_search_push",
    # WoW
    "organic_social_search_push_wow_diff",
    "organic_social_search_push_wow_pct",
    # MoM
    "organic_social_search_push_mom_diff",
    "organic_social_search_push_mom_pct",
    # YoY
    "organic_social_search_push_yoy_diff",
    "organic_social_search_push_yoy_pct"
  ),

  sale = c("sale"),

  app = c (
    "app_unique_users",
    "app_pct_app_use",
    "app_yoy_ratio_diff",
    "app_mom_4w_ratio_diff",
    "app_wow_ratio_diff"
   ),

  # TIER 2: CONTEXTUAL CATEGORIES (Important but can be heavily clustered)
  
    brand_awareness = c(
    "tma_new_york_times",
    "tma_apple_news",
    "tma_financial_times",
    "tma_economist",
    "unaided_awareness_washington_post",
    "unaided_awareness_financial_times",
    "unaided_awareness_wall_st_journal",
    "unaided_awareness_bloomberg",
    "unaided_awareness_the_economist",
    "aided_awareness_apple_news",
    "aided_awareness_bloomberg",
    "aided_awareness_financial_times",
    "aided_awareness_politico_europe",
    "aided_awareness_the_economist",
    "aided_awareness_the_new_york_times",
    "aided_awareness_the_times_sunday_times",
    "aided_awareness_wall_st_journal"
  ),

  editorial = c(
    "edit_companies_views",
    "edit_companies_narticles",
    "edit_opinion_views",
    "edit_opinion_narticles",
    "edit_uknews_views",
    "edit_uknews_narticles",
    "edit_weekend_views",
    "edit_weekend_narticles",
    "edit_world_news_views",
    "edit_world_news_narticles",
    "total_articles"
  ),

  user_behaviour = c(
    "article_page_views",
    "newsletter_open_rate",
    "habitual_percentage",
    "pct_quality_reads"
  ),

  news_agenda = c(
    # Direct traffic
    "direct_traffic",
    "direct_traffic_wow_pct",
    "direct_traffic_mom_pct",
    "direct_traffic_yoy_pct",
    # Internal traffic
    "internal_traffic",
    "internal_traffic_wow_pct",
    "internal_traffic_mom_pct",
    "internal_traffic_yoy_pct",
    # Combined organic + direct + internal
    "organic_direct_internal",
    "organic_direct_internal_wow_pct",
    "organic_direct_internal_mom_pct",
    "organic_direct_internal_yoy_pct"
  ),
  
  engagement = c(
    # Reader behaviour
    "weekly_avg_rfv",
    "weekly_share_engaged_users",
    # Engagement clusters
    "pct_cluster_email_only",
    "pct_cluster_advocates",
    "pct_cluster_disengaged",
    "pct_cluster_fans",
    "pct_cluster_readers_with_email",
    "pct_cluster_readers_wo_email",
    "pct_cluster_skimmers",
    "pct_cluster_superfans"
  ),
  
  
   paywall = c(
    "total_opportunity_visits",
    "barrier_opportunity_visits",
    "cvr_opportunity_visits"
  )
  
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

get_numeric_vars <- function(data, var_names) {
  numeric_vars <- c()
  for (var in var_names) {
    if (var %in% names(data) && is.numeric(data[[var]])) {
      numeric_vars <- c(numeric_vars, var)
    }
  }
  return(numeric_vars)
}


# ==============================================================================
# TIERED CLUSTERING SELECTION
# ==============================================================================

tiered_variable_selection <- function(data, dep_var, categories, config,
                                      mandatory_categories = var_selection$mandatory_categories) {
  
  cat("\n=== TIERED VARIABLE SELECTION (CLUSTERING METHOD) ===\n")
  cat("Dependent variable:", dep_var, "\n")
  cat("Configuration:\n")
  cat("  Tier 1 threshold:", config$tier1_threshold, "| Max per cluster:", 
      config$tier1_max_per_cluster, "| Max per category:", config$tier1_max_per_category, "\n")
  cat("  Tier 2 threshold:", config$tier2_threshold, "| Max per cluster:", 
      config$tier2_max_per_cluster, "| Max per category:", config$tier2_max_per_category, "\n\n")
  
  selected_vars <- list()
  selection_summary <- data.frame(
    category = character(),
    tier = character(),
    initial_count = integer(),
    selected_count = integer(),
    selected_vars = character(),
    correlation_with_dep = character(),
    selection_method = character(),
    stringsAsFactors = FALSE
  )
  
  # -------------------------------------------------------------------------
  # TIER 1: MANDATORY CATEGORIES
  # -------------------------------------------------------------------------
  cat("TIER 1: MANDATORY CATEGORIES (Critical business drivers)\n")
  cat("========================================================\n")
  
  for (cat_name in mandatory_categories) {
    cat("\nCategory:", toupper(cat_name), "\n")
    cat(strrep("-", 60), "\n")
    cat_vars <- categories[[cat_name]]
    
    available_vars <- intersect(cat_vars, names(data))
    cat("  Available:", length(available_vars), "/", length(cat_vars), "variables\n")
    
    if (length(available_vars) == 0) {
      cat("  WARNING: No variables available in data!\n")
      next
    }
    
    numeric_vars <- get_numeric_vars(data, available_vars)
    
    if (length(numeric_vars) == 0) {
      cat("  WARNING: No numeric variables!\n")
      next
    }
    
    if (length(numeric_vars) == 1) {
      selected_vars[[cat_name]] <- numeric_vars
      cor_val <- cor(data[[numeric_vars]], data[[dep_var]], use = "pairwise.complete.obs")
      cat("  ✓ Selected (only variable):", numeric_vars, 
          "| Cor:", round(cor_val, 3), "\n")
      
      selection_summary <- rbind(selection_summary, data.frame(
        category = cat_name,
        tier = "Tier 1 (Mandatory)",
        initial_count = length(cat_vars),
        selected_count = 1,
        selected_vars = numeric_vars,
        correlation_with_dep = as.character(round(cor_val, 3)),
        selection_method = "Only variable available",
        stringsAsFactors = FALSE
      ))
      
    } else {
      cat("  Clustering", length(numeric_vars), "variables (threshold:", 
          config$tier1_threshold, ")...\n")
      
      cor_matrix <- cor(data[, numeric_vars], use = "pairwise.complete.obs")
      dist_matrix <- as.dist(1 - abs(cor_matrix))
      hclust_result <- hclust(dist_matrix, method = "average")
      clusters <- cutree(hclust_result, h = 1 - config$tier1_threshold)
      
      n_clusters <- max(clusters)
      cat("  Found", n_clusters, "clusters\n")
      
      cluster_selected <- c()
      for (cluster_id in 1:n_clusters) {
        cluster_members <- names(clusters[clusters == cluster_id])
        
        if (length(cluster_members) == 1) {
          cluster_selected <- c(cluster_selected, cluster_members)
          cat("    Cluster", cluster_id, "(n=1):", cluster_members, "\n")
        } else {
          scores <- sapply(cluster_members, function(var) {
            cor_dep <- abs(cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs"))
            missing_pct <- sum(is.na(data[[var]])) / nrow(data)
            score <- cor_dep * 0.7 + (1 - missing_pct) * 0.3
            return(score)
          })
          
          n_to_keep <- min(config$tier1_max_per_cluster, length(cluster_members))
          top_vars <- names(sort(scores, decreasing = TRUE)[1:n_to_keep])
          cluster_selected <- c(cluster_selected, top_vars)
          
          cat("    Cluster", cluster_id, "(n=", length(cluster_members), 
              "): kept", paste(top_vars, collapse = ", "), 
              "(cor:", paste(round(scores[top_vars], 3), collapse = ", "), ")\n")
        }
      }
      
      if (length(cluster_selected) > config$tier1_max_per_category) {
        cat("  Applying category cap:", length(cluster_selected), "→", 
            config$tier1_max_per_category, "variables\n")
        
        all_cors <- sapply(cluster_selected, function(var) {
          abs(cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs"))
        })
        cluster_selected <- names(sort(all_cors, decreasing = TRUE)[1:config$tier1_max_per_category])
      }
      
      selected_vars[[cat_name]] <- cluster_selected
      
      # Get correlations for selected vars
      selected_cors <- sapply(cluster_selected, function(var) {
        cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
      })
      cors_string <- paste(paste0(cluster_selected, " (", round(selected_cors, 3), ")"), 
                           collapse = ", ")
      
      cat("  FINAL SELECTION:", length(cluster_selected), "variables\n")
      cat("     →", paste(cluster_selected, collapse = ", "), "\n")
      
      selection_summary <- rbind(selection_summary, data.frame(
        category = cat_name,
        tier = "Tier 1 (Mandatory)",
        initial_count = length(cat_vars),
        selected_count = length(cluster_selected),
        selected_vars = paste(cluster_selected, collapse = ", "),
        correlation_with_dep = cors_string,
        selection_method = paste0("Clustering (threshold=", config$tier1_threshold, 
                                  ", max_per_cluster=", config$tier1_max_per_cluster,
                                  ", max_per_category=", config$tier1_max_per_category, ")"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # -------------------------------------------------------------------------
  # TIER 2: CONTEXTUAL CATEGORIES
  # -------------------------------------------------------------------------
  cat("\n\nTIER 2: CONTEXTUAL CATEGORIES (Can be heavily clustered)\n")
  cat("========================================================\n")
  
  tier2_categories <- setdiff(names(categories), c(mandatory_categories))
  
  for (cat_name in tier2_categories) {
    cat("\nCategory:", toupper(cat_name), "\n")
    cat(strrep("-", 60), "\n")
    cat_vars <- categories[[cat_name]]
    
    available_vars <- intersect(cat_vars, names(data))
    cat("  Available:", length(available_vars), "/", length(cat_vars), "variables\n")
    
    if (length(available_vars) == 0) {
      cat("  Skipping - no variables available\n")
      next
    }
    
    numeric_vars <- get_numeric_vars(data, available_vars)
    
    if (length(numeric_vars) == 0) {
      cat("  Skipping - no numeric variables\n")
      next
    }
    
    if (length(numeric_vars) <= config$tier2_max_per_category) {
      selected_vars[[cat_name]] <- numeric_vars
      
      # Get correlations
      selected_cors <- sapply(numeric_vars, function(var) {
        cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
      })
      cors_string <- paste(paste0(numeric_vars, " (", round(selected_cors, 3), ")"), 
                           collapse = ", ")
      
      cat("  Selected all", length(numeric_vars), "variables (below threshold)\n")
      cat("     →", paste(numeric_vars, collapse = ", "), "\n")
      
      selection_summary <- rbind(selection_summary, data.frame(
        category = cat_name,
        tier = "Tier 2 (Contextual)",
        initial_count = length(cat_vars),
        selected_count = length(numeric_vars),
        selected_vars = paste(numeric_vars, collapse = ", "),
        correlation_with_dep = cors_string,
        selection_method = "All kept (below max threshold)",
        stringsAsFactors = FALSE
      ))
      
    } else {
      cat("  Clustering", length(numeric_vars), "variables (target: max", 
          config$tier2_max_per_category, ")...\n")
      
      cor_matrix <- cor(data[, numeric_vars], use = "pairwise.complete.obs")
      dist_matrix <- as.dist(1 - abs(cor_matrix))
      hclust_result <- hclust(dist_matrix, method = "complete")
      clusters <- cutree(hclust_result, h = 1 - config$tier2_threshold)
      
      n_clusters <- max(clusters)
      cat("  Found", n_clusters, "clusters\n")
      
      cluster_selected <- c()
      for (cluster_id in 1:n_clusters) {
        cluster_members <- names(clusters[clusters == cluster_id])
        
        if (length(cluster_members) == 1) {
          cluster_selected <- c(cluster_selected, cluster_members)
        } else {
          scores <- sapply(cluster_members, function(var) {
            cor_dep <- abs(cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs"))
            missing_pct <- sum(is.na(data[[var]])) / nrow(data)
            score <- cor_dep * 0.7 + (1 - missing_pct) * 0.3
            return(score)
          })
          
          n_to_keep <- min(config$tier2_max_per_cluster, length(cluster_members))
          top_vars <- names(sort(scores, decreasing = TRUE)[1:n_to_keep])
          cluster_selected <- c(cluster_selected, top_vars)
        }
      }
      
      if (length(cluster_selected) > config$tier2_max_per_category) {
        cat("  Applying category cap:", length(cluster_selected), "→", 
            config$tier2_max_per_category, "variables\n")
        
        all_cors <- sapply(cluster_selected, function(var) {
          abs(cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs"))
        })
        cluster_selected <- names(sort(all_cors, decreasing = TRUE)[1:config$tier2_max_per_category])
      }
      
      selected_vars[[cat_name]] <- cluster_selected
      
      # Get correlations
      selected_cors <- sapply(cluster_selected, function(var) {
        cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
      })
      cors_string <- paste(paste0(cluster_selected, " (", round(selected_cors, 3), ")"), 
                           collapse = ", ")
      
      cat("  FINAL SELECTION:", length(cluster_selected), "variables\n")
      cat("     →", paste(cluster_selected, collapse = ", "), "\n")
      
      selection_summary <- rbind(selection_summary, data.frame(
        category = cat_name,
        tier = "Tier 2 (Contextual)",
        initial_count = length(cat_vars),
        selected_count = length(cluster_selected),
        selected_vars = paste(cluster_selected, collapse = ", "),
        correlation_with_dep = cors_string,
        selection_method = paste0("Clustering + Top-", config$tier2_max_per_category, 
                                  " by correlation"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # -------------------------------------------------------------------------
  # FINAL SUMMARY
  # -------------------------------------------------------------------------
  all_selected <- unlist(selected_vars)
  
  cat("\n\n", strrep("=", 80), "\n")
  cat("FINAL SELECTION SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat("Total selected variables:", length(all_selected), "\n\n")
  
  cat("By tier:\n")
  tier_counts <- selection_summary %>% 
    group_by(tier) %>% 
    summarise(
      n_categories = n(),
      n_vars = sum(selected_count),
      .groups = "drop"
    )
  print(tier_counts)
  
  cat("\nBy category:\n")
  cat_summary <- selection_summary %>%
    dplyr::select(category, tier, selected_count, selected_vars) %>%
    dplyr::arrange(tier, category)
  print(cat_summary, row.names = FALSE)
  
  return(list(
    selected_vars = all_selected,
    selected_by_category = selected_vars,
    summary = selection_summary
  ))
}

# ==============================================================================
# VIF SELECTION
# ==============================================================================

calculate_vif <- function(data, dep_var, context_vars) {
  numeric_vars <- get_numeric_vars(data, context_vars)
  
  if (length(numeric_vars) != length(context_vars)) {
    removed <- setdiff(context_vars, numeric_vars)
    cat("Removed", length(removed), "non-numeric variables\n")
  }
  
  if (length(numeric_vars) < 2) {
    cat("Not enough numeric variables for VIF calculation\n")
    return(NULL)
  }
  
  var_check <- sapply(numeric_vars, function(var) {
    var(data[[var]], na.rm = TRUE)
  })
  
  constant_vars <- names(var_check[var_check == 0 | is.na(var_check)])
  if (length(constant_vars) > 0) {
    cat("Removing constant/NA variance variables:", paste(constant_vars, collapse = ", "), "\n")
    numeric_vars <- setdiff(numeric_vars, constant_vars)
  }
  
  if (length(numeric_vars) < 2) {
    cat("Not enough valid variables for VIF calculation after removing constants\n")
    return(NULL)
  }
  
  tryCatch({
    formula_str <- paste(dep_var, "~", paste(numeric_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = data)
    vif_values <- vif(model)
    
    if (is.matrix(vif_values)) {
      vif_values <- vif_values[, "GVIF^(1/(2*Df))"]^2
    }
    
    return(data.frame(
      variable = names(vif_values),
      VIF = as.numeric(vif_values),
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    cat("Error calculating VIF:", e$message, "\n")
    return(NULL)
  })
}

iterative_vif_reduction <- function(data, dep_var, context_vars, threshold = 20) {
  cat("\n--- Starting VIF Reduction ---\n")
  cat("Initial variables:", length(context_vars), "\n")
  
  numeric_vars <- get_numeric_vars(data, context_vars)
  cat("Numeric variables:", length(numeric_vars), "\n")
  
  if (length(numeric_vars) != length(context_vars)) {
    cat("Removed", length(context_vars) - length(numeric_vars), "non-numeric variables\n")
  }
  
  dropped_vars <- data.frame(
    variable = character(),
    VIF = numeric(),
    correlation_with_dep = numeric(),
    abs_correlation_with_dep = numeric(),
    iteration = integer(),
    stringsAsFactors = FALSE
  )
  
  iteration <- 1
  current_vars <- numeric_vars
  final_vif <- NULL

  n_obs  <- nrow(data)
  n_pred <- length(current_vars)
  if ((n_obs - n_pred - 1) <= 5) {
    cat("Near-saturated model (n =", n_obs, ", p =", n_pred, ", residual df =",
        n_obs - n_pred - 1, "). VIF will produce NaN — skipping.\n")
    return(list(
      selected_vars   = current_vars,
      dropped_vars    = dropped_vars,
      final_vif       = NULL,
      all_vars_tested = current_vars
    ))
  }

  while(TRUE) {
    vif_results <- calculate_vif(data, dep_var, current_vars)
    
    if (is.null(vif_results)) {
      cat("VIF calculation failed. Cannot proceed with VIF reduction.\n")
      return(list(
        selected_vars = character(0),
        dropped_vars = dropped_vars,
        final_vif = NULL,
        all_vars_tested = current_vars
      ))
    }

    final_vif <- vif_results
    high_vif <- vif_results %>% filter(VIF > threshold)
    
    if (nrow(high_vif) == 0) {
      cat("No variables with VIF >", threshold, ". Process complete.\n")
      break
    }
    
    cat("\nIteration", iteration, "- Variables with VIF >", threshold, ":", nrow(high_vif), "\n")
    
    high_vif_vars <- high_vif$variable
    correlations <- sapply(high_vif_vars, function(var) {
      if (var %in% names(data) && is.numeric(data[[var]])) {
        cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
      } else {
        NA
      }
    })
    
    correlations <- correlations[!is.na(correlations)]
    
    if (length(correlations) == 0) {
      cat("No valid correlations calculated. Stopping.\n")
      break
    }
    
    var_to_remove <- names(correlations)[which.min(abs(correlations))]
    vif_of_removed <- high_vif$VIF[high_vif$variable == var_to_remove]
    cor_of_removed <- correlations[var_to_remove]
    abs_cor_of_removed <- abs(cor_of_removed)
    
    cat("Removing:", var_to_remove, 
        "| VIF:", round(vif_of_removed, 2),
        "| Abs. Correlation with", dep_var, ":", round(abs_cor_of_removed, 4),
        "| Actual Correlation:", round(cor_of_removed, 4), "\n")
    
    dropped_vars <- rbind(dropped_vars, data.frame(
      variable = var_to_remove,
      VIF = vif_of_removed,
      correlation_with_dep = cor_of_removed,
      abs_correlation_with_dep = abs_cor_of_removed,
      iteration = iteration,
      stringsAsFactors = FALSE
    ))
    
    current_vars <- setdiff(current_vars, var_to_remove)
    iteration <- iteration + 1
    
    if (length(current_vars) < 2) {
      cat("Only 1 variable left. Stopping.\n")
      break
    }
  }
  
  selected_vars <- current_vars
  
  cat("\nFinal selected variables (VIF ≤", threshold, "):", length(selected_vars), "\n")
  cat("Total dropped variables:", nrow(dropped_vars), "\n")
  
  return(list(
    selected_vars = selected_vars,
    dropped_vars = dropped_vars,
    final_vif = final_vif,
    all_vars_tested = numeric_vars
  ))
}

# ==============================================================================
# ELASTIC NET SELECTION
# ==============================================================================

elastic_net_selection <- function(data, dep_var, candidate_vars, alpha = 0.7) {
  cat("\n--- Starting Elastic Net Selection ---\n")
  cat("Initial variables:", length(candidate_vars), "\n")
  cat("Alpha (mixing parameter):", alpha, "\n")
  
  numeric_vars <- get_numeric_vars(data, candidate_vars)
  cat("Numeric variables:", length(numeric_vars), "\n")
  
  if (length(numeric_vars) < 2) {
    cat("Not enough numeric variables for Elastic Net\n")
    return(list(
      selected_vars = character(0),
      dropped_vars = data.frame(),
      coefficients = data.frame(),
      lambda_min = NA,
      all_vars_tested = numeric_vars
    ))
  }
  
  complete_data <- data[, c(dep_var, numeric_vars)]
  complete_cases <- complete.cases(complete_data)
  
  if (sum(complete_cases) < 10) {
    cat("Not enough complete cases for Elastic Net (less than 10)\n")
    return(list(
      selected_vars = character(0),
      dropped_vars = data.frame(),
      coefficients = data.frame(),
      lambda_min = NA,
      all_vars_tested = numeric_vars
    ))
  }
  
  complete_data <- complete_data[complete_cases, ]
  cat("Complete cases:", nrow(complete_data), "out of", nrow(data), "\n")
  
  X <- as.matrix(complete_data[, numeric_vars])
  y <- complete_data[[dep_var]]
  
  cat("Running cross-validation to select lambda...\n")
  
  set.seed(var_selection$enet_seed)
  cv_fit <- cv.glmnet(X, y, alpha = alpha, nfolds = var_selection$enet_nfolds, standardize = TRUE)
  
  lambda_min <- cv_fit$lambda.min
  lambda_1se <- cv_fit$lambda.1se
  
  cat("Optimal lambda (min):", round(lambda_min, 6), "\n")
  cat("Lambda 1SE:", round(lambda_1se, 6), "\n")
  
  final_fit <- glmnet(X, y, alpha = alpha, lambda = lambda_min, standardize = TRUE)
  
  coef_matrix <- as.matrix(coef(final_fit))
  coefficients <- data.frame(
    variable = rownames(coef_matrix),
    coefficient = as.numeric(coef_matrix),
    stringsAsFactors = FALSE
  )
  
  coefficients <- coefficients %>% filter(variable != "(Intercept)")
  
  selected_coefs <- coefficients %>% filter(coefficient != 0)
  dropped_coefs <- coefficients %>% filter(coefficient == 0)
  
  selected_vars <- selected_coefs$variable
  
  cat("\nVariables with non-zero coefficients:", nrow(selected_coefs), "\n")
  cat("Variables with zero coefficients:", nrow(dropped_coefs), "\n")
  
  if (length(selected_vars) > 0) {
    selected_correlations <- sapply(selected_vars, function(var) {
      cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
    })
    
    selected_coefs$correlation_with_dep <- selected_correlations[selected_coefs$variable]
    selected_coefs$abs_correlation_with_dep <- abs(selected_coefs$correlation_with_dep)
    selected_coefs <- selected_coefs %>% arrange(desc(abs(coefficient)))
  }
  
  if (nrow(dropped_coefs) > 0) {
    dropped_correlations <- sapply(dropped_coefs$variable, function(var) {
      cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
    })
    
    dropped_vars_df <- data.frame(
      variable = dropped_coefs$variable,
      coefficient = 0,
      correlation_with_dep = dropped_correlations,
      abs_correlation_with_dep = abs(dropped_correlations),
      reason = "Zero coefficient after elastic net regularization",
      stringsAsFactors = FALSE
    )
  } else {
    dropped_vars_df <- data.frame()
  }
  
  return(list(
    selected_vars = selected_vars,
    selected_coefs = selected_coefs,
    dropped_vars = dropped_vars_df,
    all_coefficients = coefficients,
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    cv_fit = cv_fit,
    all_vars_tested = numeric_vars
  ))
}

# ==============================================================================
# STEPWISE SELECTION
# ==============================================================================

stepwise_selection <- function(data, dep_var, candidate_vars) {
  cat("\n--- Starting Stepwise Regression Selection ---\n")
  cat("Initial variables:", length(candidate_vars), "\n")
  
  numeric_vars <- get_numeric_vars(data, candidate_vars)
  cat("Numeric variables:", length(numeric_vars), "\n")
  
  if (length(numeric_vars) < 2) {
    cat("Not enough numeric variables for Stepwise\n")
    return(list(
      selected_vars = character(0),
      dropped_vars = data.frame(),
      model_summary = data.frame(),
      all_vars_tested = numeric_vars
    ))
  }
  
  complete_data <- data[, c(dep_var, numeric_vars)]
  complete_cases <- complete.cases(complete_data)
  
  if (sum(complete_cases) < 10) {
    cat("Not enough complete cases for Stepwise (less than 10)\n")
    return(list(
      selected_vars = character(0),
      dropped_vars = data.frame(),
      model_summary = data.frame(),
      all_vars_tested = numeric_vars
    ))
  }
  
  complete_data <- complete_data[complete_cases, ]
  cat("Complete cases:", nrow(complete_data), "out of", nrow(data), "\n")

  # n/p guard: remove bottom-20% spend channels until model is identifiable
  repeat {
    if ((nrow(complete_data) - length(numeric_vars) - 1) > 5) break
    cat("Near-saturated model (n =", nrow(complete_data), ", p =", length(numeric_vars),
        "). Removing bottom-20% spend channels...\n")
    spend_in_vars <- grep("^total_spend_", numeric_vars, value = TRUE)
    if (length(spend_in_vars) == 0) {
      cat("  No spend variables to remove. Cannot fit stepwise model.\n")
      return(list(
        selected_vars   = character(0),
        dropped_vars    = data.frame(),
        model_summary   = data.frame(),
        all_vars_tested = numeric_vars
      ))
    }
    total_spend  <- sapply(spend_in_vars, function(v) sum(complete_data[[v]], na.rm = TRUE))
    cutoff       <- quantile(total_spend, 0.20)
    low_spend    <- names(total_spend[total_spend <= cutoff])
    cat("  Removing:", paste(low_spend, collapse = ", "), "\n")
    numeric_vars <- setdiff(numeric_vars, low_spend)
  }

  formula_full <- as.formula(paste(dep_var, "~", paste(numeric_vars, collapse = " + ")))
  
  cat("Fitting full model...\n")
  full_model <- lm(formula_full, data = complete_data)
  
  cat("Running stepwise selection (both directions)...\n")
  stepwise_model <- stepAIC(full_model, direction = "both", trace = 0)
  
  selected_vars <- names(coef(stepwise_model))[-1]
  coef_values <- coef(stepwise_model)[-1]
  
  selected_coefs <- data.frame(
    variable = selected_vars,
    coefficient = as.numeric(coef_values),
    stringsAsFactors = FALSE
  )
  
  if (length(selected_vars) > 0) {
    selected_correlations <- sapply(selected_vars, function(var) {
      cor(complete_data[[var]], complete_data[[dep_var]], use = "pairwise.complete.obs")
    })
    
    model_summary <- summary(stepwise_model)
    p_values <- model_summary$coefficients[-1, "Pr(>|t|)"]
    
    selected_coefs$correlation_with_dep <- selected_correlations
    selected_coefs$abs_correlation_with_dep <- abs(selected_correlations)
    selected_coefs$p_value <- p_values[selected_coefs$variable]
    selected_coefs <- selected_coefs %>% arrange(p_value)
  }
  
  dropped_vars <- setdiff(numeric_vars, selected_vars)
  
  if (length(dropped_vars) > 0) {
    dropped_correlations <- sapply(dropped_vars, function(var) {
      cor(complete_data[[var]], complete_data[[dep_var]], use = "pairwise.complete.obs")
    })
    
    dropped_vars_df <- data.frame(
      variable = dropped_vars,
      correlation_with_dep = dropped_correlations,
      abs_correlation_with_dep = abs(dropped_correlations),
      reason = "Removed during stepwise selection (did not improve AIC)",
      stringsAsFactors = FALSE
    )
  } else {
    dropped_vars_df <- data.frame()
  }
  
  model_stats <- data.frame(
    Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC", 
               "Residual Std Error", "F-statistic", "Num. Variables"),
    Value = c(
      summary(stepwise_model)$r.squared,
      summary(stepwise_model)$adj.r.squared,
      AIC(stepwise_model),
      BIC(stepwise_model),
      summary(stepwise_model)$sigma,
      summary(stepwise_model)$fstatistic[1],
      length(selected_vars)
    )
  )
  
  cat("\nSelected variables:", length(selected_vars), "\n")
  cat("Dropped variables:", length(dropped_vars), "\n")
  cat("Model R-squared:", round(summary(stepwise_model)$r.squared, 4), "\n")
  cat("Model AIC:", round(AIC(stepwise_model), 2), "\n")
  
  return(list(
    selected_vars = selected_vars,
    selected_coefs = selected_coefs,
    dropped_vars = dropped_vars_df,
    model_stats = model_stats,
    model_summary = stepwise_model,
    all_vars_tested = numeric_vars
  ))
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("VARIABLE SELECTION FOR ROBYN MMM\n")
cat("Methods: Tiered Clustering, VIF, Elastic Net, Stepwise\n")
cat(strrep("=", 80), "\n\n")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
all_results <- list()

for (country in countries) {
  cat("\n")
  cat(strrep("#", 80), "\n")
  cat("COUNTRY:", toupper(country), "\n")
  cat(strrep("#", 80), "\n")
  
  data <- load_model_data(country, gdrive_folder_id = GDRIVE_FOLDER_ID, use_gdrive = TRUE)
  
  all_vars <- setdiff(names(data), project$date_var)
  
  for (dep_var in dependent_vars) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("PROCESSING:", toupper(country), "→", dep_var, "\n")
    cat(strrep("=", 80), "\n")
    
    if (!dep_var %in% names(data)) {
      cat("ERROR: Dependent variable", dep_var, "not found in data!\n")
      next
    }
    
    if (!is.numeric(data[[dep_var]])) {
      cat("ERROR: Dependent variable", dep_var, "is not numeric!\n")
      next
    }
    
    # Define exclusions
    media_metrics <- grep("^total_(spend|impressions|clicks|qr|vv|installs)_", 
                          all_vars, value = TRUE)
    
    other_dep_vars <- setdiff(c(var_selection$acq_exclude, var_selection$ltv_exclude), dep_var)
    
    # MODEL-SPECIFIC EXCLUSIONS
    model_specific_exclude <- c()
    
    if (grepl("acquisition", dep_var, ignore.case = TRUE)) {
      # Exclude sum_ltv_acquisition when modeling total_acquisitions
      ltv_exclude <- grep("^sum_ltv_", all_vars, value = TRUE)
      # Exclude share_ variables to avoid endogeneity
      share_exclude <- grep("^share_", all_vars, value = TRUE)
      model_specific_exclude <- c(model_specific_exclude, ltv_exclude, share_exclude)
      cat("ACQUISITIONS MODEL: Excluding", length(ltv_exclude), "LTV variables and", 
          length(share_exclude), "share_ variables (endogeneity)\n")
    }
    
    if (grepl("ltv", dep_var, ignore.case = TRUE)) {
      # Exclude total_acquisitions when modeling sum_ltv_acquisition
      model_specific_exclude <- c(model_specific_exclude, "total_acquisitions")
      cat("LTV MODEL: Excluding total_acquisitions variable\n")
    }
    
    common_exclude <- unique(c(dep_var, project$date_var, media_metrics,
                               other_dep_vars, model_specific_exclude))
    
    # Get all categories' variables
    all_category_vars <- unique(unlist(variable_categories))
    context_vars <- intersect(all_category_vars, setdiff(all_vars, common_exclude))
    
    cat("\nTotal variables in dataset:", length(all_vars), "\n")
    cat("Excluded variables:", length(common_exclude), "\n")
    cat("Context variables for selection:", length(context_vars), "\n")
    
    country_output_dir <- file.path(output_dir, country, dep_var)
    dir.create(country_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    headerStyle <- createStyle(textDecoration = "bold", border = "bottom")
    
    # =========================================================================
    # METHOD 1: TIERED CLUSTERING
    # =========================================================================
    cat("\n")
    cat("-----------------------------------------------------------------------------\n")
    cat("METHOD 1: TIERED CLUSTERING\n")
    cat("-----------------------------------------------------------------------------\n")
    
    tiered_result <- tiered_variable_selection(
      data = data,
      dep_var = dep_var,
      categories = variable_categories,
      config = cluster_config,
      mandatory_categories = var_selection$mandatory_categories
    )
    
    combo_name <- paste0(country, "_", dep_var, "_tiered_cluster")
    all_results[[combo_name]] <- list(
      method = "Tiered Clustering",
      selected_vars = tiered_result$selected_vars,
      summary = tiered_result$summary
    )
    
    wb_tiered <- createWorkbook()
    
    addWorksheet(wb_tiered, "Configuration")
    config_df <- data.frame(
      Parameter = c("Country", "Dependent Variable", "Total Variables Selected",
                    "Tier 1 Correlation Threshold", "Tier 1 Max Per Cluster", 
                    "Tier 1 Max Per Category",
                    "Tier 2 Correlation Threshold", "Tier 2 Max Per Cluster", 
                    "Tier 2 Max Per Category"),
      Value = c(country, dep_var, length(tiered_result$selected_vars),
                cluster_config$tier1_threshold, cluster_config$tier1_max_per_cluster,
                cluster_config$tier1_max_per_category,
                cluster_config$tier2_threshold, cluster_config$tier2_max_per_cluster,
                cluster_config$tier2_max_per_category)
    )
    writeData(wb_tiered, "Configuration", config_df)
    addStyle(wb_tiered, "Configuration", headerStyle, rows = 1, cols = 1:2)
    
    # Expand Selection_Summary so each variable gets its own row
    selection_summary_expanded <- tiered_result$summary %>%
      mutate(vars_list = strsplit(selected_vars, ", ")) %>%
      tidyr::unnest(vars_list) %>%
      dplyr::select(category, tier, initial_count, variable = vars_list, correlation_with_dep, selection_method) %>%
      mutate(
        # Extract correlation for this specific variable from correlation_with_dep string
        variable_correlation = sapply(1:n(), function(i) {
          cor_string <- correlation_with_dep[i]
          var_name <- variable[i]
          # Match pattern: variable_name (correlation)
          pattern <- paste0(var_name, " \\(([^)]+)\\)")
          match <- regmatches(cor_string, regexpr(pattern, cor_string))
          if (length(match) > 0) {
            cor_val <- gsub(paste0(var_name, " \\(|\\)"), "", match)
            return(cor_val)
          } else {
            return(NA)
          }
        })
      ) %>%
      dplyr::select(category, tier, initial_count, variable, correlation_with_dep = variable_correlation, selection_method)
    
    addWorksheet(wb_tiered, "Selection_Summary")
    writeData(wb_tiered, "Selection_Summary", selection_summary_expanded)
    addStyle(wb_tiered, "Selection_Summary", headerStyle, rows = 1, 
             cols = 1:ncol(selection_summary_expanded))
    
    final_list <- data.frame(
      variable = tiered_result$selected_vars,
      stringsAsFactors = FALSE
    )
    addWorksheet(wb_tiered, "Final_Variable_List")
    writeData(wb_tiered, "Final_Variable_List", final_list)
    addStyle(wb_tiered, "Final_Variable_List", headerStyle, rows = 1, cols = 1)
    
    excel_file_tiered <- file.path(country_output_dir, 
                                   paste0("TIERED_CLUSTER_selection_", dep_var, "_", timestamp, ".xlsx"))
    saveWorkbook(wb_tiered, excel_file_tiered, overwrite = TRUE)
    
    cat("\nTiered Clustering results exported to:", excel_file_tiered, "\n")
    
    # =========================================================================
    # METHOD 2: VIF-BASED SELECTION
    # =========================================================================
    cat("\n")
    cat("-----------------------------------------------------------------------------\n")
    cat("METHOD 2: VIF-BASED SELECTION\n")
    cat("-----------------------------------------------------------------------------\n")
    
    if (length(context_vars) >= 2) {
      vif_result <- iterative_vif_reduction(data, dep_var, context_vars, vif_threshold)
      
      if (length(vif_result$selected_vars) > 0) {
        selected_correlations <- sapply(vif_result$selected_vars, function(var) {
          tryCatch({
            cor(data[[var]], data[[dep_var]], use = "pairwise.complete.obs")
          }, error = function(e) NA)
        })
        
        selected_correlations <- selected_correlations[!is.na(selected_correlations)]
        
        vif_selected_df <- data.frame(
          variable = names(selected_correlations),
          correlation_with_dep = as.numeric(selected_correlations),
          abs_correlation_with_dep = abs(selected_correlations),
          stringsAsFactors = FALSE
        )
        
        if (!is.null(vif_result$final_vif)) {
          vif_selected_df <- vif_selected_df %>%
            left_join(vif_result$final_vif, by = "variable") %>%
            arrange(desc(abs_correlation_with_dep))
        }
        
        combo_name <- paste0(country, "_", dep_var, "_vif")
        all_results[[combo_name]] <- list(
          method = "VIF",
          selected_vars = vif_selected_df,
          dropped_vars = vif_result$dropped_vars
        )
        
        wb_vif <- createWorkbook()
        addWorksheet(wb_vif, "Selected_Variables")
        writeData(wb_vif, "Selected_Variables", vif_selected_df)
        addStyle(wb_vif, "Selected_Variables", headerStyle, rows = 1, cols = 1:ncol(vif_selected_df))
        
        if (nrow(vif_result$dropped_vars) > 0) {
          addWorksheet(wb_vif, "Dropped_Variables")
          writeData(wb_vif, "Dropped_Variables", vif_result$dropped_vars)
          addStyle(wb_vif, "Dropped_Variables", headerStyle, rows = 1, cols = 1:ncol(vif_result$dropped_vars))
        }
        
        summary_vif <- data.frame(
          Metric = c("Method", "Country", "Dependent Variable", "Total Variables", 
                     "Excluded Variables", "Context Variables", "Selected Variables", 
                     "Dropped Variables", "VIF Threshold"),
          Value = c("VIF-based", country, dep_var, length(all_vars), 
                    length(common_exclude), length(context_vars),
                    nrow(vif_selected_df), nrow(vif_result$dropped_vars), vif_threshold)
        )
        addWorksheet(wb_vif, "Summary")
        writeData(wb_vif, "Summary", summary_vif)
        addStyle(wb_vif, "Summary", headerStyle, rows = 1, cols = 1:2)
        
        excel_file_vif <- file.path(country_output_dir, 
                                    paste0("VIF_selection_", dep_var, "_", timestamp, ".xlsx"))
        saveWorkbook(wb_vif, excel_file_vif, overwrite = TRUE)
        cat("\nVIF results exported to:", excel_file_vif, "\n")
        
        cat("\nTop 10 VIF-selected variables:\n")
        print(head(vif_selected_df, 10))
      }
    }
    
    # =========================================================================
    # METHOD 3: ELASTIC NET
    # =========================================================================
    cat("\n")
    cat("-----------------------------------------------------------------------------\n")
    cat("METHOD 3: ELASTIC NET REGULARIZATION\n")
    cat("-----------------------------------------------------------------------------\n")
    
    if (length(context_vars) >= 2) {
      enet_result <- elastic_net_selection(data, dep_var, context_vars, alpha_enet)
      
      if (length(enet_result$selected_vars) > 0) {
        combo_name <- paste0(country, "_", dep_var, "_enet")
        all_results[[combo_name]] <- list(
          method = "Elastic Net",
          selected_vars = enet_result$selected_coefs,
          dropped_vars = enet_result$dropped_vars,
          all_coefficients = enet_result$all_coefficients
        )
        
        wb_enet <- createWorkbook()
        
        addWorksheet(wb_enet, "Selected_Variables")
        writeData(wb_enet, "Selected_Variables", enet_result$selected_coefs)
        addStyle(wb_enet, "Selected_Variables", headerStyle, rows = 1, cols = 1:ncol(enet_result$selected_coefs))
        
        if (nrow(enet_result$dropped_vars) > 0) {
          addWorksheet(wb_enet, "Dropped_Variables")
          writeData(wb_enet, "Dropped_Variables", enet_result$dropped_vars)
          addStyle(wb_enet, "Dropped_Variables", headerStyle, rows = 1, cols = 1:ncol(enet_result$dropped_vars))
        }
        
        addWorksheet(wb_enet, "All_Coefficients")
        writeData(wb_enet, "All_Coefficients", enet_result$all_coefficients %>% arrange(desc(abs(coefficient))))
        addStyle(wb_enet, "All_Coefficients", headerStyle, rows = 1, cols = 1:ncol(enet_result$all_coefficients))
        
        summary_enet <- data.frame(
          Metric = c("Method", "Country", "Dependent Variable", "Total Variables", 
                     "Excluded Variables", "Context Variables", 
                     "Selected Variables (non-zero coef)", "Dropped Variables (zero coef)", 
                     "Alpha", "Lambda (min)"),
          Value = c("Elastic Net", country, dep_var, length(all_vars), 
                    length(common_exclude), length(context_vars),
                    nrow(enet_result$selected_coefs), nrow(enet_result$dropped_vars), 
                    alpha_enet, round(enet_result$lambda_min, 6))
        )
        addWorksheet(wb_enet, "Summary")
        writeData(wb_enet, "Summary", summary_enet)
        addStyle(wb_enet, "Summary", headerStyle, rows = 1, cols = 1:2)
        
        excel_file_enet <- file.path(country_output_dir, 
                                     paste0("ENET_selection_", dep_var, "_", timestamp, ".xlsx"))
        saveWorkbook(wb_enet, excel_file_enet, overwrite = TRUE)
        cat("\nElastic Net results exported to:", excel_file_enet, "\n")
        
        cat("\nTop 10 Elastic Net-selected variables (by coefficient magnitude):\n")
        print(head(enet_result$selected_coefs, 10))
      }
    }
    
    # =========================================================================
    # METHOD 4: STEPWISE REGRESSION
    # =========================================================================
    cat("\n")
    cat("-----------------------------------------------------------------------------\n")
    cat("METHOD 4: STEPWISE REGRESSION\n")
    cat("-----------------------------------------------------------------------------\n")
    
    if (length(context_vars) >= 2) {
      stepwise_result <- stepwise_selection(data, dep_var, context_vars)
      
      if (length(stepwise_result$selected_vars) > 0) {
        combo_name <- paste0(country, "_", dep_var, "_stepwise")
        all_results[[combo_name]] <- list(
          method = "Stepwise",
          selected_vars = stepwise_result$selected_coefs,
          dropped_vars = stepwise_result$dropped_vars,
          model_stats = stepwise_result$model_stats
        )
        
        wb_stepwise <- createWorkbook()
        
        addWorksheet(wb_stepwise, "Selected_Variables")
        writeData(wb_stepwise, "Selected_Variables", stepwise_result$selected_coefs)
        addStyle(wb_stepwise, "Selected_Variables", headerStyle, rows = 1, cols = 1:ncol(stepwise_result$selected_coefs))
        
        if (nrow(stepwise_result$dropped_vars) > 0) {
          addWorksheet(wb_stepwise, "Dropped_Variables")
          writeData(wb_stepwise, "Dropped_Variables", stepwise_result$dropped_vars)
          addStyle(wb_stepwise, "Dropped_Variables", headerStyle, rows = 1, cols = 1:ncol(stepwise_result$dropped_vars))
        }
        
        addWorksheet(wb_stepwise, "Model_Statistics")
        writeData(wb_stepwise, "Model_Statistics", stepwise_result$model_stats)
        addStyle(wb_stepwise, "Model_Statistics", headerStyle, rows = 1, cols = 1:ncol(stepwise_result$model_stats))
        
        summary_stepwise <- data.frame(
          Metric = c("Method", "Country", "Dependent Variable", "Total Variables", 
                     "Excluded Variables", "Context Variables", 
                     "Selected Variables", "Dropped Variables", 
                     "R-squared", "AIC"),
          Value = c("Stepwise Regression", country, dep_var, length(all_vars), 
                    length(common_exclude), length(context_vars),
                    nrow(stepwise_result$selected_coefs), nrow(stepwise_result$dropped_vars),
                    round(as.numeric(stepwise_result$model_stats$Value[stepwise_result$model_stats$Metric == "Adjusted R-squared"]), 4),
                    round(as.numeric(stepwise_result$model_stats$Value[stepwise_result$model_stats$Metric == "AIC"]), 2))
        )
        addWorksheet(wb_stepwise, "Summary")
        writeData(wb_stepwise, "Summary", summary_stepwise)
        addStyle(wb_stepwise, "Summary", headerStyle, rows = 1, cols = 1:2)
        
        excel_file_stepwise <- file.path(country_output_dir, 
                                         paste0("STEPWISE_selection_", dep_var, "_", timestamp, ".xlsx"))
        saveWorkbook(wb_stepwise, excel_file_stepwise, overwrite = TRUE)
        cat("\nStepwise results exported to:", excel_file_stepwise, "\n")
        
        cat("\nTop 10 Stepwise-selected variables (by p-value):\n")
        print(head(stepwise_result$selected_coefs, 10))
      }
    }
  }
}

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================
cat("\n")
cat(strrep("=", 80), "\n")
cat("VARIABLE SELECTION COMPLETE\n")
cat(strrep("=", 80), "\n")
cat("Results saved to:", output_dir, "\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

summary_data <- lapply(names(all_results), function(combo) {
  result <- all_results[[combo]]
  if ("selected_vars" %in% names(result)) {
    if (is.data.frame(result$selected_vars)) {
      selected_count <- nrow(result$selected_vars)
    } else {
      selected_count <- length(result$selected_vars)
    }
  } else {
    selected_count <- 0
  }
  
  if ("dropped_vars" %in% names(result)) {
    dropped_count <- nrow(result$dropped_vars)
  } else {
    dropped_count <- 0
  }
  
  data.frame(
    combination = combo,
    method = result$method,
    selected_count = selected_count,
    dropped_count = dropped_count,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

summary_file <- file.path(output_dir, 
                          paste0("selection_summary_", 
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
write.csv(summary_data, summary_file, row.names = FALSE)

cat("\nOverall summary:\n")
print(summary_data)