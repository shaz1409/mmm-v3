# ==============================================================================
# VARIABLE SELECTION - US LTV (TEMPORARY)
# Same logic as 04_variable_selection.R but:
#   - US only, sum_ltv_acquisition only
#   - Data filtered to 2023-07-10 onwards
#   - prop_*, seen_*, share_* variables excluded from candidate pool
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
# RUN CONTROL
# ==============================================================================

COUNTRY       <- "united_states"
DEP_VAR       <- "sum_ltv_acquisition"
START_DATE    <- as.Date("2023-07-10")
OUTPUT_SUFFIX <- "us_ltv_from_2023_07_10"

# Variables to exclude from candidates on top of the standard exclusions
EXTRA_EXCLUDE_PATTERNS <- c("^prop_", "^seen_", "^share_")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

cluster_config <- list(
  tier1_threshold        = var_selection$tier1$corr_threshold,
  tier2_threshold        = var_selection$tier2$corr_threshold,
  tier1_max_per_cluster  = var_selection$tier1$max_per_cluster,
  tier2_max_per_cluster  = var_selection$tier2$max_per_cluster,
  tier1_max_per_category = var_selection$tier1$max_per_category,
  tier2_max_per_category = var_selection$tier2$max_per_category
)

vif_threshold <- var_selection$vif_threshold
alpha_enet    <- var_selection$elastic_net_alpha
output_dir    <- file.path(project$output_dirs$variable_selection, OUTPUT_SUFFIX)

# ==============================================================================
# DATA LOAD
# ==============================================================================

drive_auth(email = env$user_email, cache = ".secrets")

filename   <- project$file_map[[COUNTRY]]
local_dir  <- file.path("data_exploration", COUNTRY)
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

files      <- drive_ls(path = as_id(drive_folders$model_data))
file_match <- files %>% filter(name == filename)
if (nrow(file_match) == 0) stop("File not found in Google Drive: ", filename)
local_file <- file.path(local_dir, filename)
drive_download(file = as_id(file_match$id), path = local_file, overwrite = TRUE)

data <- read.csv(local_file, stringsAsFactors = FALSE)
data[[project$date_var]] <- as.Date(data[[project$date_var]])
data <- data[data[[project$date_var]] >= START_DATE, ]

cat("Data filtered from", as.character(START_DATE), "onwards\n")
cat("Rows:", nrow(data), "| Date range:",
    as.character(min(data[[project$date_var]])), "to",
    as.character(max(data[[project$date_var]])), "\n\n")

# ==============================================================================
# VARIABLE CATEGORIES
# ==============================================================================

variable_categories <- list(

  economics = c("cpi_index", "cpi_yoy", "bci", "unemployment_rate", "cli", "vix"),

  discount = c(
    "seen_1_10_discount", "seen_11_25_discount",
    "seen_26_33_discount", "seen_34_50_discount",
    "prop_1_10_discount", "prop_11_25_discount",
    "prop_26_33_discount", "prop_34_50_discount",
    "prop_50_plus_discount", "prop_rrp"
  ),

  product_mix = c(
    "share_bundle", "share_digital_premium", "share_digital_standard",
    "share_digital_other", "share_print"
  ),

  organic = c(
    "organic_search_traffic", "organic_social_traffic",
    "organic_push_notification", "organic_social_search_push",
    "organic_social_search_push_wow_diff", "organic_social_search_push_wow_pct",
    "organic_social_search_push_mom_diff", "organic_social_search_push_mom_pct",
    "organic_social_search_push_yoy_diff", "organic_social_search_push_yoy_pct"
  ),

  sale = c("sale"),

  app = c(
    "app_unique_users", "app_pct_app_use", "app_yoy_ratio_diff",
    "app_mom_4w_ratio_diff", "app_wow_ratio_diff"
  ),

  brand_awareness = c(
    "tma_new_york_times", "tma_apple_news", "tma_financial_times", "tma_economist",
    "unaided_awareness_washington_post", "unaided_awareness_financial_times",
    "unaided_awareness_wall_st_journal", "unaided_awareness_bloomberg",
    "unaided_awareness_the_economist",
    "aided_awareness_apple_news", "aided_awareness_bloomberg",
    "aided_awareness_financial_times", "aided_awareness_politico_europe",
    "aided_awareness_the_economist", "aided_awareness_the_new_york_times",
    "aided_awareness_the_times_sunday_times", "aided_awareness_wall_st_journal"
  ),

  editorial = c(
    "edit_companies_views", "edit_companies_narticles",
    "edit_opinion_views", "edit_opinion_narticles",
    "edit_uknews_views", "edit_uknews_narticles",
    "edit_weekend_views", "edit_weekend_narticles",
    "edit_world_news_views", "edit_world_news_narticles",
    "total_articles"
  ),

  user_behaviour = c(
    "article_page_views", "newsletter_open_rate",
    "habitual_percentage", "pct_quality_reads"
  ),

  news_agenda = c(
    "direct_traffic", "direct_traffic_wow_pct",
    "direct_traffic_mom_pct", "direct_traffic_yoy_pct",
    "internal_traffic", "internal_traffic_wow_pct",
    "internal_traffic_mom_pct", "internal_traffic_yoy_pct",
    "organic_direct_internal", "organic_direct_internal_wow_pct",
    "organic_direct_internal_mom_pct", "organic_direct_internal_yoy_pct"
  ),

  engagement = c(
    "weekly_avg_rfv", "weekly_share_engaged_users",
    "pct_cluster_email_only", "pct_cluster_advocates",
    "pct_cluster_disengaged", "pct_cluster_fans",
    "pct_cluster_readers_with_email", "pct_cluster_readers_wo_email",
    "pct_cluster_skimmers", "pct_cluster_superfans"
  ),

  paywall = c(
    "total_opportunity_visits", "barrier_opportunity_visits", "cvr_opportunity_visits"
  )
)

# ==============================================================================
# HELPER FUNCTIONS  (identical to 04_variable_selection.R)
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

tiered_variable_selection <- function(data, dep_var, categories, config,
                                      mandatory_categories = var_selection$mandatory_categories) {
  cat("\n=== TIERED VARIABLE SELECTION ===\n")
  cat("Dependent variable:", dep_var, "\n\n")

  selected_vars    <- list()
  selection_summary <- data.frame(
    category = character(), tier = character(),
    initial_count = integer(), selected_count = integer(),
    selected_vars = character(), correlation_with_dep = character(),
    selection_method = character(), stringsAsFactors = FALSE
  )

  process_category <- function(cat_name, cat_vars, threshold, max_per_cluster,
                                max_per_category, tier_label, hclust_method = "average") {
    available_vars <- intersect(cat_vars, names(data))
    cat("  Available:", length(available_vars), "/", length(cat_vars), "variables\n")
    if (length(available_vars) == 0) { cat("  WARNING: No variables available!\n"); return(NULL) }

    numeric_vars <- get_numeric_vars(data, available_vars)
    if (length(numeric_vars) == 0) { cat("  WARNING: No numeric variables!\n"); return(NULL) }

    if (length(numeric_vars) == 1) {
      cor_val <- cor(data[[numeric_vars]], data[[dep_var]], use = "pairwise.complete.obs")
      cat("  Selected (only variable):", numeric_vars, "| Cor:", round(cor_val, 3), "\n")
      return(list(
        selected = numeric_vars,
        summary_row = data.frame(category = cat_name, tier = tier_label,
          initial_count = length(cat_vars), selected_count = 1,
          selected_vars = numeric_vars,
          correlation_with_dep = as.character(round(cor_val, 3)),
          selection_method = "Only variable available", stringsAsFactors = FALSE)
      ))
    }

    cor_matrix   <- cor(data[, numeric_vars], use = "pairwise.complete.obs")
    dist_matrix  <- as.dist(1 - abs(cor_matrix))
    clusters     <- cutree(hclust(dist_matrix, method = hclust_method), h = 1 - threshold)
    n_clusters   <- max(clusters)
    cat("  Found", n_clusters, "clusters (threshold:", threshold, ")\n")

    cluster_selected <- c()
    for (cid in 1:n_clusters) {
      members <- names(clusters[clusters == cid])
      if (length(members) == 1) {
        cluster_selected <- c(cluster_selected, members)
      } else {
        scores <- sapply(members, function(v) {
          abs(cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs")) * 0.7 +
            (1 - sum(is.na(data[[v]])) / nrow(data)) * 0.3
        })
        cluster_selected <- c(cluster_selected,
                              names(sort(scores, decreasing = TRUE)[1:min(max_per_cluster, length(members))]))
      }
    }

    if (length(cluster_selected) > max_per_category) {
      all_cors <- sapply(cluster_selected, function(v)
        abs(cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs")))
      cluster_selected <- names(sort(all_cors, decreasing = TRUE)[1:max_per_category])
    }

    sel_cors   <- sapply(cluster_selected, function(v) cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs"))
    cors_str   <- paste(paste0(cluster_selected, " (", round(sel_cors, 3), ")"), collapse = ", ")
    cat("  FINAL:", length(cluster_selected), "vars →", paste(cluster_selected, collapse = ", "), "\n")

    list(
      selected = cluster_selected,
      summary_row = data.frame(category = cat_name, tier = tier_label,
        initial_count = length(cat_vars), selected_count = length(cluster_selected),
        selected_vars = paste(cluster_selected, collapse = ", "),
        correlation_with_dep = cors_str,
        selection_method = paste0("Clustering (threshold=", threshold,
                                  ", max_per_cluster=", max_per_cluster,
                                  ", max_per_category=", max_per_category, ")"),
        stringsAsFactors = FALSE)
    )
  }

  cat("TIER 1: MANDATORY CATEGORIES\n", strrep("=", 60), "\n")
  for (cn in mandatory_categories) {
    cat("\nCategory:", toupper(cn), "\n", strrep("-", 40), "\n")
    res <- process_category(cn, categories[[cn]], config$tier1_threshold,
                            config$tier1_max_per_cluster, config$tier1_max_per_category,
                            "Tier 1 (Mandatory)", "average")
    if (!is.null(res)) {
      selected_vars[[cn]]  <- res$selected
      selection_summary    <- rbind(selection_summary, res$summary_row)
    }
  }

  cat("\n\nTIER 2: CONTEXTUAL CATEGORIES\n", strrep("=", 60), "\n")
  for (cn in setdiff(names(categories), mandatory_categories)) {
    cat("\nCategory:", toupper(cn), "\n", strrep("-", 40), "\n")
    cat_vars     <- categories[[cn]]
    numeric_vars <- get_numeric_vars(data, intersect(cat_vars, names(data)))
    if (length(numeric_vars) == 0) { cat("  Skipping - no numeric variables\n"); next }

    if (length(numeric_vars) <= config$tier2_max_per_category) {
      sel_cors <- sapply(numeric_vars, function(v) cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs"))
      cat("  Selected all", length(numeric_vars), "vars (below cap)\n")
      selected_vars[[cn]] <- numeric_vars
      selection_summary   <- rbind(selection_summary, data.frame(
        category = cn, tier = "Tier 2 (Contextual)",
        initial_count = length(cat_vars), selected_count = length(numeric_vars),
        selected_vars = paste(numeric_vars, collapse = ", "),
        correlation_with_dep = paste(paste0(numeric_vars, " (", round(sel_cors, 3), ")"), collapse = ", "),
        selection_method = "All kept (below max threshold)", stringsAsFactors = FALSE))
    } else {
      res <- process_category(cn, cat_vars, config$tier2_threshold,
                              config$tier2_max_per_cluster, config$tier2_max_per_category,
                              "Tier 2 (Contextual)", "complete")
      if (!is.null(res)) {
        selected_vars[[cn]] <- res$selected
        selection_summary   <- rbind(selection_summary, res$summary_row)
      }
    }
  }

  all_selected <- unlist(selected_vars)
  cat("\n\nFINAL SELECTION:", length(all_selected), "variables\n")
  list(selected_vars = all_selected, selected_by_category = selected_vars, summary = selection_summary)
}

iterative_vif_reduction <- function(data, dep_var, context_vars, threshold = 20) {
  cat("\n--- VIF Reduction ---\n")
  numeric_vars <- get_numeric_vars(data, context_vars)
  dropped_vars <- data.frame(variable = character(), VIF = numeric(),
                             correlation_with_dep = numeric(), abs_correlation_with_dep = numeric(),
                             iteration = integer(), stringsAsFactors = FALSE)
  n_obs  <- nrow(data); n_pred <- length(numeric_vars)
  if ((n_obs - n_pred - 1) <= 5) {
    cat("Near-saturated model — skipping VIF\n")
    return(list(selected_vars = numeric_vars, dropped_vars = dropped_vars, final_vif = NULL, all_vars_tested = numeric_vars))
  }
  current_vars <- numeric_vars; final_vif <- NULL; iteration <- 1
  while (TRUE) {
    formula_str <- paste(dep_var, "~", paste(current_vars, collapse = " + "))
    vif_vals <- tryCatch({
      m <- lm(as.formula(formula_str), data = data)
      v <- vif(m)
      if (is.matrix(v)) v <- v[, "GVIF^(1/(2*Df))"]^2
      data.frame(variable = names(v), VIF = as.numeric(v), stringsAsFactors = FALSE)
    }, error = function(e) { cat("VIF error:", e$message, "\n"); NULL })
    if (is.null(vif_vals)) break
    final_vif <- vif_vals
    high_vif  <- vif_vals %>% filter(VIF > threshold)
    if (nrow(high_vif) == 0) { cat("All VIF ≤", threshold, "\n"); break }
    cors <- sapply(high_vif$variable, function(v)
      cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs"))
    cors <- cors[!is.na(cors)]
    to_rm <- names(cors)[which.min(abs(cors))]
    cat("Iter", iteration, "- removing:", to_rm, "| VIF:", round(high_vif$VIF[high_vif$variable == to_rm], 2), "\n")
    dropped_vars <- rbind(dropped_vars, data.frame(variable = to_rm,
      VIF = high_vif$VIF[high_vif$variable == to_rm],
      correlation_with_dep = cors[to_rm], abs_correlation_with_dep = abs(cors[to_rm]),
      iteration = iteration, stringsAsFactors = FALSE))
    current_vars <- setdiff(current_vars, to_rm)
    iteration <- iteration + 1
    if (length(current_vars) < 2) break
  }
  list(selected_vars = current_vars, dropped_vars = dropped_vars, final_vif = final_vif, all_vars_tested = numeric_vars)
}

elastic_net_selection <- function(data, dep_var, candidate_vars, alpha = 0.7) {
  cat("\n--- Elastic Net ---\n")
  numeric_vars  <- get_numeric_vars(data, candidate_vars)
  if (length(numeric_vars) < 2) return(list(selected_vars = character(0), dropped_vars = data.frame(), selected_coefs = data.frame(), all_coefficients = data.frame(), lambda_min = NA, all_vars_tested = numeric_vars))
  complete_data <- data[complete.cases(data[, c(dep_var, numeric_vars)]), c(dep_var, numeric_vars)]
  if (nrow(complete_data) < 10) return(list(selected_vars = character(0), dropped_vars = data.frame(), selected_coefs = data.frame(), all_coefficients = data.frame(), lambda_min = NA, all_vars_tested = numeric_vars))
  X <- as.matrix(complete_data[, numeric_vars]); y <- complete_data[[dep_var]]
  set.seed(var_selection$enet_seed)
  cv_fit    <- cv.glmnet(X, y, alpha = alpha, nfolds = var_selection$enet_nfolds, standardize = TRUE)
  final_fit <- glmnet(X, y, alpha = alpha, lambda = cv_fit$lambda.min, standardize = TRUE)
  coef_df   <- data.frame(variable = rownames(as.matrix(coef(final_fit))),
                          coefficient = as.numeric(coef(final_fit)), stringsAsFactors = FALSE) %>%
    filter(variable != "(Intercept)")
  sel_coefs <- coef_df %>% filter(coefficient != 0)
  drp_coefs <- coef_df %>% filter(coefficient == 0)
  if (nrow(sel_coefs) > 0) {
    sel_cors <- sapply(sel_coefs$variable, function(v) cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs"))
    sel_coefs$correlation_with_dep     <- sel_cors
    sel_coefs$abs_correlation_with_dep <- abs(sel_cors)
    sel_coefs <- sel_coefs %>% arrange(desc(abs(coefficient)))
  }
  drp_df <- if (nrow(drp_coefs) > 0) {
    drp_cors <- sapply(drp_coefs$variable, function(v) cor(data[[v]], data[[dep_var]], use = "pairwise.complete.obs"))
    data.frame(variable = drp_coefs$variable, coefficient = 0, correlation_with_dep = drp_cors,
               abs_correlation_with_dep = abs(drp_cors), reason = "Zero coef after elastic net", stringsAsFactors = FALSE)
  } else data.frame()
  cat("Selected:", nrow(sel_coefs), "| Dropped:", nrow(drp_coefs), "\n")
  list(selected_vars = sel_coefs$variable, selected_coefs = sel_coefs, dropped_vars = drp_df,
       all_coefficients = coef_df, lambda_min = cv_fit$lambda.min, all_vars_tested = numeric_vars)
}

stepwise_selection <- function(data, dep_var, candidate_vars) {
  cat("\n--- Stepwise Regression ---\n")
  numeric_vars  <- get_numeric_vars(data, candidate_vars)
  if (length(numeric_vars) < 2) return(list(selected_vars = character(0), dropped_vars = data.frame(), selected_coefs = data.frame(), model_stats = data.frame(), all_vars_tested = numeric_vars))
  complete_data <- data[complete.cases(data[, c(dep_var, numeric_vars)]), c(dep_var, numeric_vars)]
  if (nrow(complete_data) < 10) return(list(selected_vars = character(0), dropped_vars = data.frame(), selected_coefs = data.frame(), model_stats = data.frame(), all_vars_tested = numeric_vars))
  repeat {
    if ((nrow(complete_data) - length(numeric_vars) - 1) > 5) break
    spend_vars <- grep("^total_spend_", numeric_vars, value = TRUE)
    if (length(spend_vars) == 0) return(list(selected_vars = character(0), dropped_vars = data.frame(), selected_coefs = data.frame(), model_stats = data.frame(), all_vars_tested = numeric_vars))
    total_spend <- sapply(spend_vars, function(v) sum(complete_data[[v]], na.rm = TRUE))
    numeric_vars <- setdiff(numeric_vars, names(total_spend[total_spend <= quantile(total_spend, 0.20)]))
  }
  full_model     <- lm(as.formula(paste(dep_var, "~", paste(numeric_vars, collapse = " + "))), data = complete_data)
  stepwise_model <- stepAIC(full_model, direction = "both", trace = 0)
  sel_vars       <- names(coef(stepwise_model))[-1]
  sel_coefs      <- data.frame(variable = sel_vars, coefficient = as.numeric(coef(stepwise_model)[-1]), stringsAsFactors = FALSE)
  if (length(sel_vars) > 0) {
    sel_cors  <- sapply(sel_vars, function(v) cor(complete_data[[v]], complete_data[[dep_var]], use = "pairwise.complete.obs"))
    p_vals    <- summary(stepwise_model)$coefficients[-1, "Pr(>|t|)"]
    sel_coefs$correlation_with_dep     <- sel_cors
    sel_coefs$abs_correlation_with_dep <- abs(sel_cors)
    sel_coefs$p_value                  <- p_vals[sel_coefs$variable]
    sel_coefs <- sel_coefs %>% arrange(p_value)
  }
  drp_vars <- setdiff(numeric_vars, sel_vars)
  drp_df   <- if (length(drp_vars) > 0) {
    drp_cors <- sapply(drp_vars, function(v) cor(complete_data[[v]], complete_data[[dep_var]], use = "pairwise.complete.obs"))
    data.frame(variable = drp_vars, correlation_with_dep = drp_cors,
               abs_correlation_with_dep = abs(drp_cors), reason = "Removed by stepwise (AIC)", stringsAsFactors = FALSE)
  } else data.frame()
  model_stats <- data.frame(
    Metric = c("R-squared", "Adjusted R-squared", "AIC", "BIC", "Residual Std Error", "F-statistic", "Num. Variables"),
    Value  = c(summary(stepwise_model)$r.squared, summary(stepwise_model)$adj.r.squared,
               AIC(stepwise_model), BIC(stepwise_model), summary(stepwise_model)$sigma,
               summary(stepwise_model)$fstatistic[1], length(sel_vars))
  )
  cat("Selected:", length(sel_vars), "| R²:", round(summary(stepwise_model)$r.squared, 4), "\n")
  list(selected_vars = sel_vars, selected_coefs = sel_coefs, dropped_vars = drp_df,
       model_stats = model_stats, all_vars_tested = numeric_vars)
}

# ==============================================================================
# BUILD CANDIDATE VARIABLE LIST (with extra exclusions applied)
# ==============================================================================

all_vars <- setdiff(names(data), project$date_var)

media_metrics  <- grep("^total_(spend|impressions|clicks|qr|vv|installs)_", all_vars, value = TRUE)
other_dep_vars <- setdiff(c(var_selection$acq_exclude, var_selection$ltv_exclude), DEP_VAR)

extra_exclude  <- unique(unlist(lapply(EXTRA_EXCLUDE_PATTERNS, function(p) grep(p, all_vars, value = TRUE))))
cat("Extra excluded (prop_*/seen_*/share_*):", length(extra_exclude), "vars\n")
cat(paste(" ", sort(extra_exclude), collapse = "\n"), "\n\n")

common_exclude <- unique(c(DEP_VAR, project$date_var, media_metrics,
                           other_dep_vars, "total_acquisitions", extra_exclude))

context_vars <- intersect(unique(unlist(variable_categories)), setdiff(all_vars, common_exclude))

cat("Total vars in dataset       :", length(all_vars), "\n")
cat("Excluded vars               :", length(common_exclude), "\n")
cat("Context vars for selection  :", length(context_vars), "\n\n")

# ==============================================================================
# RUN SELECTION METHODS
# ==============================================================================

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
headerStyle <- createStyle(textDecoration = "bold", border = "bottom")

cat(strrep("=", 80), "\n")
cat("PROCESSING: US → sum_ltv_acquisition | from", as.character(START_DATE), "\n")
cat(strrep("=", 80), "\n")

# ---- METHOD 1: TIERED CLUSTERING --------------------------------------------
cat("\n--- METHOD 1: TIERED CLUSTERING ---\n")
tiered_result <- tiered_variable_selection(
  data = data, dep_var = DEP_VAR,
  categories = variable_categories, config = cluster_config,
  mandatory_categories = var_selection$mandatory_categories
)

wb_tiered <- createWorkbook()
addWorksheet(wb_tiered, "Configuration")
writeData(wb_tiered, "Configuration", data.frame(
  Parameter = c("Country", "Dependent Variable", "Start Date",
                "Extra Exclusion Patterns", "Total Variables Selected"),
  Value = c(COUNTRY, DEP_VAR, as.character(START_DATE),
            paste(EXTRA_EXCLUDE_PATTERNS, collapse = ", "),
            length(tiered_result$selected_vars))
))

sel_exp <- tiered_result$summary %>%
  mutate(vars_list = strsplit(selected_vars, ", ")) %>%
  tidyr::unnest(vars_list) %>%
  dplyr::select(category, tier, initial_count, variable = vars_list, correlation_with_dep, selection_method) %>%
  mutate(variable_correlation = sapply(1:n(), function(i) {
    m <- regmatches(correlation_with_dep[i], regexpr(paste0(variable[i], " \\(([^)]+)\\)"), correlation_with_dep[i]))
    if (length(m) > 0) gsub(paste0(variable[i], " \\(|\\)"), "", m) else NA
  })) %>%
  dplyr::select(category, tier, initial_count, variable,
                correlation_with_dep = variable_correlation, selection_method)

addWorksheet(wb_tiered, "Selection_Summary"); writeData(wb_tiered, "Selection_Summary", sel_exp)
addWorksheet(wb_tiered, "Final_Variable_List"); writeData(wb_tiered, "Final_Variable_List", data.frame(variable = tiered_result$selected_vars))
f_tiered <- file.path(output_dir, paste0("TIERED_CLUSTER_us_ltv_", timestamp, ".xlsx"))
saveWorkbook(wb_tiered, f_tiered, overwrite = TRUE)
cat("Saved:", f_tiered, "\n")

# ---- METHOD 2: VIF ----------------------------------------------------------
cat("\n--- METHOD 2: VIF REDUCTION ---\n")
if (length(context_vars) >= 2) {
  vif_result <- iterative_vif_reduction(data, DEP_VAR, context_vars, vif_threshold)
  if (length(vif_result$selected_vars) > 0) {
    sel_cors <- sapply(vif_result$selected_vars, function(v)
      tryCatch(cor(data[[v]], data[[DEP_VAR]], use = "pairwise.complete.obs"), error = function(e) NA))
    sel_cors  <- sel_cors[!is.na(sel_cors)]
    vif_sel   <- data.frame(variable = names(sel_cors), correlation_with_dep = as.numeric(sel_cors),
                            abs_correlation_with_dep = abs(sel_cors), stringsAsFactors = FALSE)
    if (!is.null(vif_result$final_vif))
      vif_sel <- vif_sel %>% left_join(vif_result$final_vif, by = "variable") %>% arrange(desc(abs_correlation_with_dep))
    wb_vif <- createWorkbook()
    addWorksheet(wb_vif, "Selected_Variables"); writeData(wb_vif, "Selected_Variables", vif_sel)
    if (nrow(vif_result$dropped_vars) > 0) { addWorksheet(wb_vif, "Dropped_Variables"); writeData(wb_vif, "Dropped_Variables", vif_result$dropped_vars) }
    f_vif <- file.path(output_dir, paste0("VIF_us_ltv_", timestamp, ".xlsx"))
    saveWorkbook(wb_vif, f_vif, overwrite = TRUE)
    cat("Saved:", f_vif, "\n")
  }
}

# ---- METHOD 3: ELASTIC NET --------------------------------------------------
cat("\n--- METHOD 3: ELASTIC NET ---\n")
if (length(context_vars) >= 2) {
  enet_result <- elastic_net_selection(data, DEP_VAR, context_vars, alpha_enet)
  if (length(enet_result$selected_vars) > 0) {
    wb_enet <- createWorkbook()
    addWorksheet(wb_enet, "Selected_Variables"); writeData(wb_enet, "Selected_Variables", enet_result$selected_coefs)
    if (nrow(enet_result$dropped_vars) > 0) { addWorksheet(wb_enet, "Dropped_Variables"); writeData(wb_enet, "Dropped_Variables", enet_result$dropped_vars) }
    addWorksheet(wb_enet, "All_Coefficients")
    writeData(wb_enet, "All_Coefficients", enet_result$all_coefficients %>% arrange(desc(abs(coefficient))))
    f_enet <- file.path(output_dir, paste0("ENET_us_ltv_", timestamp, ".xlsx"))
    saveWorkbook(wb_enet, f_enet, overwrite = TRUE)
    cat("Saved:", f_enet, "\n")
  }
}

# ---- METHOD 4: STEPWISE -----------------------------------------------------
cat("\n--- METHOD 4: STEPWISE ---\n")
if (length(context_vars) >= 2) {
  sw_result <- stepwise_selection(data, DEP_VAR, context_vars)
  if (length(sw_result$selected_vars) > 0) {
    wb_sw <- createWorkbook()
    addWorksheet(wb_sw, "Selected_Variables"); writeData(wb_sw, "Selected_Variables", sw_result$selected_coefs)
    if (nrow(sw_result$dropped_vars) > 0) { addWorksheet(wb_sw, "Dropped_Variables"); writeData(wb_sw, "Dropped_Variables", sw_result$dropped_vars) }
    addWorksheet(wb_sw, "Model_Statistics"); writeData(wb_sw, "Model_Statistics", sw_result$model_stats)
    f_sw <- file.path(output_dir, paste0("STEPWISE_us_ltv_", timestamp, ".xlsx"))
    saveWorkbook(wb_sw, f_sw, overwrite = TRUE)
    cat("Saved:", f_sw, "\n")
  }
}

cat("\n", strrep("=", 80), "\n")
cat("DONE - outputs in:", output_dir, "\n")
cat(strrep("=", 80), "\n")
