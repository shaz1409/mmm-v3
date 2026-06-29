# ================================================================
# QUARTERLY MMM Models
# Script 5: Run Robyn to get 4 models
# ================================================================

#Fix package issue: Error: <ggplot2::element_text> object properties are invalid:
# - @family must be <NULL> or <character>, not <logical>
#remove.packages("ggplot2")
#remotes::install_version("ggplot2", version = "3.5.2")
#packageVersion("ggplot2")

set.seed(123)

## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

# Set to FALSE to avoid the creation of files locally
create_files <- TRUE

# ================================================================
# RUN CONTROL - Set to TRUE/FALSE to choose which models to run
# ================================================================
RUN_UK_LTV <- TRUE
RUN_UK_ACQ <- FALSE

# Load required libraries
library(reticulate)
library(Robyn)
library(googledrive)
library(dplyr)
library(ggplot2)
library(car)


script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)

py <- env$python_path

Sys.setenv(RETICULATE_PYTHON = py)
options(reticulate.conda_binary = "")        # don't let reticulate call conda
Sys.setenv(RETICULATE_MINICONDA_PATH = "")   # ignore r-miniconda if it exists

reticulate::py_config()  # quick sanity check

cat("Working Directory:", getwd(), "\n\n")


# ================================================================
# DATA CONFIGURATION
# ================================================================


# Define Google Drive folder ID
GDRIVE_FOLDER_ID <- drive_folders$model_data

# Function to download data from Google Drive
download_from_gdrive <- function(folder_id, filename, local_dir = "model_data") {
  
  # Create local data folder if it doesn't exist
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  
  # Authenticate with Google Drive (will prompt browser authentication first time)
  drive_auth(email = env$user_email, cache = ".secrets")
  
  # Search for the file in the folder
  files <- drive_ls(path = as_id(folder_id))
  file_match <- files %>% filter(name == filename)
  
  if (nrow(file_match) == 0) {
    stop("File not found in Google Drive: ", filename)
  }
  
  # Download the file
  local_file <- file.path(local_dir, filename)
  drive_download(
    file = as_id(file_match$id),
    path = local_file,
    overwrite = TRUE
  )
  
  cat("Downloaded:", filename, "\n")
  cat("   To:", local_file, "\n\n")
  
  return(local_file)
}

# Function to load data (from Google Drive or local)
load_model_data <- function(country, gdrive_folder_id = NULL, use_gdrive = TRUE) {
  
  filename <- project$file_map[[country]]

  if (is.null(filename)) {
    stop("No data file mapping found for country: ", country)
  }
  
  # Download from Google Drive if enabled
  if (use_gdrive && !is.null(gdrive_folder_id)) {
    local_dir <- file.path("model_data", country)  # More descriptive local path
    file_path <- download_from_gdrive(gdrive_folder_id, filename, local_dir)
  } else {
    # Use local file
    file_path <- file.path("model_data", country, filename)
    if (!file.exists(file_path)) {
      stop("Local file not found: ", file_path)
    }
  }

  # Load the data
  cat("Loading dataset from:", file_path, "\n")
  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Convert date: handle both numeric Excel dates and character strings
  if (is.numeric(data[[project$date_var]])) {
    # Excel serial date (origin 1900-01-01, but with 1900 leap year bug)
    data[[project$date_var]] <- as.Date(data[[project$date_var]], origin = "1899-12-30")
  } else {
    # Try parsing as character string (various formats)
    data[[project$date_var]] <- as.Date(data[[project$date_var]])
  }

  cat("   Rows:", nrow(data), "| Columns:", ncol(data), "\n")
  cat("   Date range:", min(data[[project$date_var]], na.rm = TRUE), "to", max(data[[project$date_var]], na.rm = TRUE), "\n\n")
  
  return(data)
}

# ================================================================
# CHANNEL AGGREGATION CONFIGURATION
# ================================================================

# Define channel aggregations - Simple: just say which channels to merge
CHANNEL_AGGREGATIONS <- list(

  # UK Aggregations
  united_kingdom = list(
    list(
      new_name = "DEMAND_PAIDSEARCH_GOOGLEDGEN",
      channels_to_merge = c(
        "DEMAND_PAIDSEARCH_GOOGLEDGEN",
        "ACQUISITION_PAIDSEARCH_GOOGLEDGEN"
      )
    )
  )

)

# ================================================================
# AGGREGATION FUNCTION (AGGREGATES ALL METRICS)
# ================================================================

aggregate_channels <- function(data, country) {

  aggregations <- CHANNEL_AGGREGATIONS[[country]]
  
  if (is.null(aggregations) || length(aggregations) == 0) {
    cat("No channel aggregations defined for", country, "\n")
    return(data)
  }

  cat("\nAggregating channels for", country, "...\n")
  
  # ALL metrics to aggregate
  metrics <- c("total_spend", "total_impressions", "total_clicks", "total_qr", "total_vv", "total_installs")
  
  for (agg in aggregations) {
    new_name <- agg$new_name
    channels <- agg$channels_to_merge
    
    cat("   Merging:", paste(channels, collapse = " + "), "->", new_name, "\n")
    
    # Track which columns we actually found and aggregated
    aggregated_metrics <- c()
    
    # Aggregate each metric
    for (metric in metrics) {
      # Build column names for channels to merge
      cols_to_merge <- paste0(metric, "_", channels)
      
      # Check which columns exist
      existing_cols <- cols_to_merge[cols_to_merge %in% names(data)]
      
      if (length(existing_cols) > 0) {
        # Create new aggregated column
        new_col_name <- paste0(metric, "_", new_name)
        data[[new_col_name]] <- rowSums(data[, existing_cols, drop = FALSE], na.rm = TRUE)

        # Remove old columns (exclude new_col_name in case it shares a name with a source column)
        cols_to_remove <- existing_cols[existing_cols != new_col_name]
        data <- data[, !(names(data) %in% cols_to_remove)]
        
        aggregated_metrics <- c(aggregated_metrics, metric)
      }
    }
    
    cat("      Aggregated metrics:", paste(aggregated_metrics, collapse = ", "), "\n")
  }
  
  cat("   Channel aggregation complete\n\n")
  
  return(data)
}

# ================================================================
# MODEL CONFIGURATIONS
# ================================================================

# Define all model configurations
MODEL_CONFIGS <- list(
  
  # Model 1: UK LTV Acquisition
  uk_ltv = list(
    country = "united_kingdom",
    model_name = "ltv_uk",
    dep_var = "sum_ltv_acquisition",
    dep_var_type = "revenue",
    date_var = project$date_var,
    prophet_country = "UK",
    prophet_vars = c("trend", "season", "holiday"),
    context_vars = c(
    "aided_awareness_bloomberg",
    "aided_awareness_financial_times",
    "aided_awareness_the_economist",
    "aided_awareness_the_new_york_times",
    "aided_awareness_wall_st_journal",
    "app_mom_4w_ratio_diff",
    "app_pct_app_use",
    "app_wow_ratio_diff",
    "article_page_views",
    "barrier_opportunity_visits",
    "bci",
    "cvr_opportunity_visits",
    "direct_traffic_mom_pct",
    "direct_traffic_wow_pct",
    "edit_companies_views",
    "edit_opinion_views",
    "edit_uknews_views",
    "internal_traffic_mom_pct",
    "internal_traffic_yoy_pct",
    "pct_cluster_fans",
    "pct_cluster_readers_wo_email",
    "pct_quality_reads",
    "prop_rrp",
    "seen_11_25_discount",
    "seen_26_33_discount",
    "seen_34_50_discount",
    "tma_apple_news",
    "tma_new_york_times",
    "total_articles",
    "unaided_awareness_bloomberg",
    "unaided_awareness_financial_times",
    "unaided_awareness_the_economist",
    "unaided_awareness_wall_st_journal",
    "vix"

    ),
    
        paid_media_spends = c(
        # ACQUISITION
        "total_spend_ACQUISITION_DISPLAY_DV360",
        "total_spend_ACQUISITION_OLV_YOUTUBE",
        "total_spend_ACQUISITION_PAIDSEARCH_BINGPMAX",
        "total_spend_ACQUISITION_PAIDSEARCH_BINGSEARCH",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLEPMAX",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER",
        "total_spend_ACQUISITION_PAIDSOCIAL_LINKEDIN",
        "total_spend_ACQUISITION_PAIDSOCIAL_META",
        "total_spend_ACQUISITION_PAIDSOCIAL_REDDIT",
        
        # APPACQUISITION
        "total_spend_APPACQUISITION_APP_ASA",
        "total_spend_APPACQUISITION_APP_GOOGLEACE",
        "total_spend_APPACQUISITION_APP_GOOGLEACI",
        
        # AWARENESS
        "total_spend_AWARENESS_DISPLAY_DV360OTHER",
        "total_spend_AWARENESS_OLV_YOUTUBE",
        "total_spend_AWARENESS_PAIDSOCIAL_META",
        "total_spend_AWARENESS_PAIDSOCIAL_REDDIT",
        
        # CONSIDERATION
        "total_spend_CONSIDERATION_AUDIO_ACAST",
        "total_spend_CONSIDERATION_AUDIO_NEXUS",
        "total_spend_CONSIDERATION_AUDIO_SONY",
        "total_spend_CONSIDERATION_DISPLAY_DV360",
        "total_spend_CONSIDERATION_OLV_JWPLAYER",
        "total_spend_CONSIDERATION_OLV_NEXUS",
        "total_spend_CONSIDERATION_PAIDSOCIAL_META",
        "total_spend_CONSIDERATION_PAIDSOCIAL_TIKTOK",
        
        # DEMAND
        "total_spend_DEMAND_DISPLAY_CONDENAST",
        "total_spend_DEMAND_DISPLAY_DV360",
        "total_spend_DEMAND_DISPLAY_DV360OTHER",
        "total_spend_DEMAND_DISPLAY_OUTBRAIN",
        "total_spend_DEMAND_PAIDSEARCH_GOOGLEDGEN",
        "total_spend_DEMAND_PAIDSEARCH_GOOGLESEARCHOTHER",
        "total_spend_DEMAND_PAIDSOCIAL_LINKEDIN",
        "total_spend_DEMAND_PAIDSOCIAL_META",
        "total_spend_DEMAND_PAIDSOCIAL_REDDIT",
        
        # RETENTION
        "total_spend_RETENTION_PAIDSOCIAL_META"
      
    ),
    
    paid_media_vars = c(
      # ACQUISITION
      "total_impressions_ACQUISITION_DISPLAY_DV360",
      "total_impressions_ACQUISITION_OLV_YOUTUBE",
      "total_clicks_ACQUISITION_PAIDSEARCH_BINGPMAX",
      "total_impressions_ACQUISITION_PAIDSEARCH_BINGSEARCH",
      "total_spend_ACQUISITION_PAIDSEARCH_GOOGLEPMAX",
      "total_impressions_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
      "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER",
      "total_clicks_ACQUISITION_PAIDSOCIAL_LINKEDIN",
      "total_impressions_ACQUISITION_PAIDSOCIAL_META",
      "total_impressions_ACQUISITION_PAIDSOCIAL_REDDIT",

      # APPACQUISITION
      "total_clicks_APPACQUISITION_APP_ASA",
      "total_impressions_APPACQUISITION_APP_GOOGLEACE",
      "total_impressions_APPACQUISITION_APP_GOOGLEACI",

      # AWARENESS
      "total_impressions_AWARENESS_DISPLAY_DV360OTHER",
      "total_impressions_AWARENESS_OLV_YOUTUBE",
      "total_impressions_AWARENESS_PAIDSOCIAL_META",
      "total_impressions_AWARENESS_PAIDSOCIAL_REDDIT",

      # CONSIDERATION
      "total_impressions_CONSIDERATION_AUDIO_ACAST",
      "total_impressions_CONSIDERATION_AUDIO_NEXUS",
      "total_impressions_CONSIDERATION_AUDIO_SONY",
      "total_clicks_CONSIDERATION_DISPLAY_DV360",
      "total_impressions_CONSIDERATION_OLV_JWPLAYER",
      "total_impressions_CONSIDERATION_OLV_NEXUS",
      "total_impressions_CONSIDERATION_PAIDSOCIAL_META",
      "total_impressions_CONSIDERATION_PAIDSOCIAL_TIKTOK",

      # DEMAND
      "total_impressions_DEMAND_DISPLAY_CONDENAST",
      "total_impressions_DEMAND_DISPLAY_DV360",
      "total_impressions_DEMAND_DISPLAY_DV360OTHER",
      "total_impressions_DEMAND_DISPLAY_OUTBRAIN",
      "total_spend_DEMAND_PAIDSEARCH_GOOGLEDGEN",
      "total_impressions_DEMAND_PAIDSEARCH_GOOGLESEARCHOTHER",
      "total_impressions_DEMAND_PAIDSOCIAL_LINKEDIN",
      "total_impressions_DEMAND_PAIDSOCIAL_META",
      "total_impressions_DEMAND_PAIDSOCIAL_REDDIT",

      # RETENTION
      "total_impressions_RETENTION_PAIDSOCIAL_META"
    ),
    
    
    organic_vars = c(
      "organic_search_traffic",
      "organic_push_notification",
      "organic_social_traffic"
    ),
    adstock       = "geometric",
    train_size    = c(0.75, 0.9),
    window_start  = "2023-07-10",   # edit directly — first week of modelling window
    window_end    = "2026-03-30"    # edit directly — last week of modelling window
  ),

  # Model 2: UK Acquisition
  uk_acq = list(
    country = "united_kingdom",
    model_name = "acq_uk",
    dep_var = "total_acquisitions",
    dep_var_type = "conversion",
    date_var = project$date_var,
    prophet_country = "UK",
    prophet_vars = c("trend", "season", "holiday"),
    context_vars = c(
      
      "aided_awareness_apple_news", 
      "aided_awareness_politico_europe", 
      "aided_awareness_the_times_sunday_times", 
      #"aided_awareness_the_new_york_times", #NEW
      "unaided_awareness_the_economist", 
      #"unaided_awareness_financial_times", # NEW
      "tma_apple_news", 
      "tma_new_york_times", 
      
      "app_yoy_ratio_diff",        #-> corr cli .77
      
      "bci", 
      "cli", 
      "vix",
      
      "sale", 
      
      "internal_traffic",
      "internal_traffic_wow_pct",

      "pct_cluster_advocates",
      "pct_cluster_disengaged", # -> corr bci -.72
      "pct_cluster_skimmers",
      #"prop_11_25_discount", 
      "prop_34_50_discount",
      "prop_50_plus_discount",
      "prop_rrp", 
      "seen_11_25_discount", 
      "seen_34_50_discount", 
      
      #"share_digital_standard", 
      #"share_digital_premium", 
      
      "total_articles", 
      "article_page_views", 
      "edit_companies_views", 
      "edit_opinion_views",      
      "edit_uknews_views", 
      "edit_weekend_narticles",  
      "edit_weekend_views", 

      "barrier_opportunity_visits", 
      "conversions_percent_of_opportunities",
      "newsletter_open_rate"
      
    ),
    
    paid_media_spends = c(
        # ACQUISITION
        "total_spend_ACQUISITION_DISPLAY_DV360",
        "total_spend_ACQUISITION_OLV_YOUTUBE",
        "total_spend_ACQUISITION_PAIDSEARCH_BINGPMAX",
        "total_spend_ACQUISITION_PAIDSEARCH_BINGSEARCH",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLEPMAX",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER",
        "total_spend_ACQUISITION_PAIDSOCIAL_LINKEDIN",
        "total_spend_ACQUISITION_PAIDSOCIAL_META",
        "total_spend_ACQUISITION_PAIDSOCIAL_REDDIT",
        
        # APPACQUISITION
        "total_spend_APPACQUISITION_APP_ASA",
        "total_spend_APPACQUISITION_APP_GOOGLEACE",
        "total_spend_APPACQUISITION_APP_GOOGLEACI",
        
        # AWARENESS
        "total_spend_AWARENESS_DISPLAY_DV360OTHER",
        "total_spend_AWARENESS_OLV_YOUTUBE",
        "total_spend_AWARENESS_PAIDSOCIAL_META",
        "total_spend_AWARENESS_PAIDSOCIAL_REDDIT",
        
        # CONSIDERATION
        "total_spend_CONSIDERATION_AUDIO_ACAST",
        "total_spend_CONSIDERATION_AUDIO_NEXUS",
        "total_spend_CONSIDERATION_AUDIO_SONY",
        "total_spend_CONSIDERATION_DISPLAY_DV360",
        "total_spend_CONSIDERATION_OLV_JWPLAYER",
        "total_spend_CONSIDERATION_OLV_NEXUS",
        "total_spend_CONSIDERATION_PAIDSOCIAL_META",
        "total_spend_CONSIDERATION_PAIDSOCIAL_TIKTOK",
        
        # DEMAND
        "total_spend_DEMAND_DISPLAY_CONDENAST",
        "total_spend_DEMAND_DISPLAY_DV360",
        "total_spend_DEMAND_DISPLAY_DV360OTHER",
        "total_spend_DEMAND_DISPLAY_OUTBRAIN",
        "total_spend_DEMAND_PAIDSEARCH_GOOGLEDGEN",
        "total_spend_DEMAND_PAIDSEARCH_GOOGLESEARCHOTHER",
        "total_spend_DEMAND_PAIDSOCIAL_LINKEDIN",
        "total_spend_DEMAND_PAIDSOCIAL_META",
        "total_spend_DEMAND_PAIDSOCIAL_REDDIT",
        
        # RETENTION
        "total_spend_RETENTION_PAIDSOCIAL_META"
      
    ),
    
    paid_media_vars = c(
      # ACQUISITION
      "total_impressions_ACQUISITION_DISPLAY_DV360",
      "total_impressions_ACQUISITION_OLV_YOUTUBE",
      "total_clicks_ACQUISITION_PAIDSEARCH_BINGPMAX",
      "total_impressions_ACQUISITION_PAIDSEARCH_BINGSEARCH",
      "total_spend_ACQUISITION_PAIDSEARCH_GOOGLEPMAX",
      "total_impressions_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
      "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER",
      "total_clicks_ACQUISITION_PAIDSOCIAL_LINKEDIN",
      "total_impressions_ACQUISITION_PAIDSOCIAL_META",
      "total_impressions_ACQUISITION_PAIDSOCIAL_REDDIT",

      # APPACQUISITION
      "total_clicks_APPACQUISITION_APP_ASA",
      "total_impressions_APPACQUISITION_APP_GOOGLEACE",
      "total_impressions_APPACQUISITION_APP_GOOGLEACI",

      # AWARENESS
      "total_impressions_AWARENESS_DISPLAY_DV360OTHER",
      "total_impressions_AWARENESS_OLV_YOUTUBE",
      "total_impressions_AWARENESS_PAIDSOCIAL_META",
      "total_impressions_AWARENESS_PAIDSOCIAL_REDDIT",

      # CONSIDERATION
      "total_impressions_CONSIDERATION_AUDIO_ACAST",
      "total_impressions_CONSIDERATION_AUDIO_NEXUS",
      "total_impressions_CONSIDERATION_AUDIO_SONY",
      "total_clicks_CONSIDERATION_DISPLAY_DV360",
      "total_impressions_CONSIDERATION_OLV_JWPLAYER",
      "total_impressions_CONSIDERATION_OLV_NEXUS",
      "total_impressions_CONSIDERATION_PAIDSOCIAL_META",
      "total_impressions_CONSIDERATION_PAIDSOCIAL_TIKTOK",

      # DEMAND
      "total_impressions_DEMAND_DISPLAY_CONDENAST",
      "total_impressions_DEMAND_DISPLAY_DV360",
      "total_impressions_DEMAND_DISPLAY_DV360OTHER",
      "total_impressions_DEMAND_DISPLAY_OUTBRAIN",
      "total_spend_DEMAND_PAIDSEARCH_GOOGLEDGEN",
      "total_impressions_DEMAND_PAIDSEARCH_GOOGLESEARCHOTHER",
      "total_impressions_DEMAND_PAIDSOCIAL_LINKEDIN",
      "total_impressions_DEMAND_PAIDSOCIAL_META",
      "total_impressions_DEMAND_PAIDSOCIAL_REDDIT",

      # RETENTION
      "total_impressions_RETENTION_PAIDSOCIAL_META"
    ),


    organic_vars = c(
      "organic_search_traffic",
      "organic_push_notification",
      "organic_social_traffic"
    ),
    adstock       = "geometric",
    train_size    = c(0.75, 0.9),
    window_start  = "2023-04-03",   # edit directly — first week of modelling window
    window_end    = "2026-03-30"    # edit directly — last week of modelling window
  )


)

# ================================================================
# CALIBRATION CONFIGURATION
# ================================================================
# Effect size calibration uses experimental lift results to constrain the model.
# Populate each entry with a data.frame when you have lift/experiment data,
# or leave as NULL to run without calibration.
#
# Rules (from Robyn docs):
#   channel           - must match a name in paid_media_vars or organic_vars
#   liftStartDate     - start of experiment window (must be within data range)
#   liftEndDate       - end of experiment window (must be within data range)
#   liftAbs           - incremental result (point estimate, same unit as dep_var)
#   spend             - spend on that channel during the experiment window
#   confidence        - e.g. 1 - p_value from a frequentist test
#   metric            - must match dep_var column name (e.g. "sum_ltv_acquisition")
#   calibration_scope - "immediate" for lift experiments, "total" for other MMMs

CALIBRATION_INPUTS <- list(

   uk_ltv = NULL,
  #  uk_ltv = data.frame(
  #   channel           = c("total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
  #                        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER"),
  #   liftStartDate     = as.Date(c("2024-11-11", "2024-11-11")),
  #   liftEndDate       = as.Date(c("2024-12-09", "2024-12-09")),
  #   liftAbs           = c(327600, 140400),
  #   spend             = c(38500, 16500),
  #   confidence        = c(0.9),
  #   metric            = c("sum_ltv_acquisition"),
  #   calibration_scope = c("immediate")
  # ),

  uk_acq = NULL
  # uk_acq = data.frame(
  #   channel           = c("total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND",
  #                        "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER"),
  #   liftStartDate     = as.Date(c("2024-11-11", "2024-11-11")),
  #   liftEndDate       = as.Date(c("2024-12-09", "2024-12-09")),
  #   liftAbs           = c(840, 360),
  #   spend             = c(38500, 16500),
  #   confidence        = c(0.9),
  #   metric            = c("total_acquisitions"),
  #   calibration_scope = c("immediate")
  # )
)


# ================================================================
# HYPERPARAMETER CONFIGURATION SYSTEM
# ================================================================

# Alpha, gamma, and theta profiles are defined in config.R under hyperparams$alpha/gamma/theta

# ================================================================
# CHANNEL CLASSIFICATION - UK MODEL
# ================================================================


CHANNEL_CONFIG <- list(

  # ACQUISITION
  "total_impressions_ACQUISITION_DISPLAY_DV360"          = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_ACQUISITION_OLV_YOUTUBE"            = list(alpha = "slow",   gamma = "slow",   theta = "medium"),
  "total_clicks_ACQUISITION_PAIDSEARCH_BINGPMAX"         = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_ACQUISITION_PAIDSEARCH_BINGSEARCH"  = list(alpha = "medium", gamma = "fast",   theta = "digital"),
  "total_spend_ACQUISITION_PAIDSEARCH_GOOGLEPMAX"        = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_ACQUISITION_PAIDSEARCH_GOOGLESEARCHBRAND"  = list(alpha = "fast",   gamma = "fast",   theta = "digital"),
  "total_spend_ACQUISITION_PAIDSEARCH_GOOGLESEARCHOTHER" = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_clicks_ACQUISITION_PAIDSOCIAL_LINKEDIN"         = list(alpha = "medium", gamma = "fast",   theta = "digital"),
  "total_impressions_ACQUISITION_PAIDSOCIAL_META"        = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_ACQUISITION_PAIDSOCIAL_REDDIT"      = list(alpha = "medium", gamma = "medium", theta = "digital"),

  # APP ACQUISITION
  "total_clicks_APPACQUISITION_APP_ASA"           = list(alpha = "medium", gamma = "fast",   theta = "digital"),
  "total_impressions_APPACQUISITION_APP_GOOGLEACE" = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_APPACQUISITION_APP_GOOGLEACI" = list(alpha = "medium", gamma = "medium", theta = "digital"),

  # AWARENESS
  "total_impressions_AWARENESS_DISPLAY_DV360OTHER" = list(alpha = "slow", gamma = "slow", theta = "high"),
  "total_impressions_AWARENESS_OLV_YOUTUBE"        = list(alpha = "slow", gamma = "slow", theta = "high"),
  "total_impressions_AWARENESS_PAIDSOCIAL_META"    = list(alpha = "slow", gamma = "slow", theta = "high"),
  "total_impressions_AWARENESS_PAIDSOCIAL_REDDIT"  = list(alpha = "slow", gamma = "slow", theta = "high"),

  # CONSIDERATION
  "total_impressions_CONSIDERATION_AUDIO_ACAST"    = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_AUDIO_NEXUS"    = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_AUDIO_SONY"     = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_clicks_CONSIDERATION_DISPLAY_DV360"       = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_OLV_JWPLAYER"   = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_OLV_NEXUS"      = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_PAIDSOCIAL_META"   = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_CONSIDERATION_PAIDSOCIAL_TIKTOK" = list(alpha = "medium", gamma = "medium", theta = "medium"),

  # DEMAND
  "total_impressions_DEMAND_DISPLAY_CONDENAST"          = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_impressions_DEMAND_DISPLAY_DV360"              = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_DISPLAY_DV360OTHER"         = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_DISPLAY_OUTBRAIN"           = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "total_spend_DEMAND_PAIDSEARCH_GOOGLEDGEN"            = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_PAIDSEARCH_GOOGLESEARCHOTHER" = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_PAIDSOCIAL_LINKEDIN"        = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_PAIDSOCIAL_META"            = list(alpha = "medium", gamma = "medium", theta = "digital"),
  "total_impressions_DEMAND_PAIDSOCIAL_REDDIT"          = list(alpha = "medium", gamma = "medium", theta = "medium"),

  # RETENTION
  "total_impressions_RETENTION_PAIDSOCIAL_META" = list(alpha = "fast", gamma = "fast", theta = "digital"),

  # ORGANIC
  "organic_search_traffic"      = list(alpha = "medium", gamma = "medium", theta = "medium"),
  "organic_push_notification"   = list(alpha = "medium", gamma = "fast",   theta = "digital"),
  "organic_social_traffic"      = list(alpha = "slow",   gamma = "slow",   theta = "medium")
)

# ================================================================
# HYPERPARAMETER GENERATOR FUNCTION (CORRECTED)
# ================================================================

generate_hyperparameters <- function(paid_media_vars, organic_vars = c(), adstock_type = "geometric", train_size = c(0.75, 0.9)) {

  # First, create hyperparameters for all channels
  all_hyperparams <- list()

  # Process paid media channels using paid_media_vars (NOT paid_media_spends!)
  for (channel in paid_media_vars) {

    # Look up using FULL channel name
    ch_config <- CHANNEL_CONFIG[[channel]]

    if (!is.null(ch_config)) {
      alpha_range <- hyperparams$alpha[[ch_config$alpha]]
      gamma_range <- hyperparams$gamma[[ch_config$gamma]]
      theta_range <- hyperparams$theta[[ch_config$theta]]
    } else {
      # Fallback to defaults
      alpha_range <- c(0.5, 3)
      gamma_range <- c(0.3, 1)
      theta_range <- c(0.01, 0.7)
    }

    all_hyperparams[[channel]] <- list(
      alphas = alpha_range,
      gammas = gamma_range,
      thetas = theta_range
    )
  }

  # Process organic channels
  if (length(organic_vars) > 0) {
    for (organic_channel in organic_vars) {

      ch_config <- CHANNEL_CONFIG[[organic_channel]]

      if (!is.null(ch_config)) {
        alpha_range <- hyperparams$alpha[[ch_config$alpha]]
        gamma_range <- hyperparams$gamma[[ch_config$gamma]]
        theta_range <- hyperparams$theta[[ch_config$theta]]
      } else {
        alpha_range <- c(0.5, 3)
        gamma_range <- c(0.3, 1)
        theta_range <- c(0.01, 0.7)
      }

      all_hyperparams[[organic_channel]] <- list(
        alphas = alpha_range,
        gammas = gamma_range,
        thetas = theta_range
      )
    }
  }

  # Sort in alphabetical order (Robyn's sorting)
  sorted_channel_names <- sort(c(paid_media_vars, organic_vars))

  # Build final hyperparameters list in sorted order
  result_params <- list()

  for (channel in sorted_channel_names) {
    params <- all_hyperparams[[channel]]

    result_params[[paste0(channel, "_alphas")]] <- params$alphas
    result_params[[paste0(channel, "_gammas")]] <- params$gammas

    if (adstock_type == "geometric") {
      result_params[[paste0(channel, "_thetas")]] <- params$thetas
    }
  }

  result_params[["train_size"]] <- train_size

  return(result_params)
}




# ================================================================
# CONTEXT VIF CHECK FUNCTION
# ================================================================

check_context_vif <- function(model_data, context_vars, config_name) {

  cat("\n================================================================\n")
  cat("  VIF CHECK - CONTEXT VARIABLES:", toupper(config_name), "\n")
  cat("================================================================\n")

  # Keep only context vars present in data
  available_vars <- context_vars[context_vars %in% names(model_data)]
  missing_vars   <- setdiff(context_vars, available_vars)
  if (length(missing_vars) > 0) {
    cat("  Skipped (not in data):", paste(missing_vars, collapse = ", "), "\n")
  }
  cat("  Context vars:", length(available_vars), "\n\n")

  if (length(available_vars) == 0) {
    cat("  No context vars found in data — VIF skipped.\n")
    cat("  Sample columns in data:", paste(head(names(model_data), 10), collapse = ", "), "...\n")
    cat("================================================================\n\n")
    return(invisible(NULL))
  }

  context_data <- model_data[, available_vars, drop = FALSE]

  # Drop zero-variance columns
  has_variance <- vapply(context_data, function(x) var(x, na.rm = TRUE) > 0, logical(1))
  context_data <- context_data[, has_variance, drop = FALSE]

  # n/p guard: VIF becomes degenerate when predictors ~ observations
  n_obs  <- nrow(context_data)
  n_pred <- ncol(context_data)
  if ((n_obs - n_pred - 1) <= 5) {
    cat("  Near-saturated model (n =", n_obs, ", p =", n_pred,
        ")  VIF skipped.\n\n")
    return(invisible(NULL))
  }

  formula_str <- paste(names(context_data)[1], "~",
                       paste(names(context_data)[-1], collapse = " + "))
  lm_model   <- lm(as.formula(formula_str), data = context_data)
  vif_results <- vif(lm_model)

  vif_df <- data.frame(Variable = names(vif_results), VIF = round(vif_results, 2))
  vif_df <- vif_df[order(-vif_df$VIF), ]
  rownames(vif_df) <- NULL
  print(vif_df)

  critical     <- vif_df[vif_df$VIF > 10, ]
  warning_vars <- vif_df[vif_df$VIF > 5 & vif_df$VIF <= 10, ]

  cat("\n  CRITICAL (VIF > 10):", if (nrow(critical) > 0) paste(critical$Variable, collapse = ", ") else "None", "\n")
  cat("  WARNING  (VIF 5-10):", if (nrow(warning_vars) > 0) paste(warning_vars$Variable, collapse = ", ") else "None", "\n")
  cat("  GOOD     (VIF < 5 ):", sum(vif_df$VIF < 5), "variables\n")
  cat("================================================================\n\n")

  out_dir  <- file.path("model_results", config_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0("vif_context_", config_name, ".txt"))
  out_lines <- c(
    "================================================================",
    paste("  VIF CHECK - CONTEXT VARIABLES:", toupper(config_name)),
    "================================================================",
    paste("  Context vars:", length(available_vars)),
    "",
    capture.output(print(vif_df)),
    "",
    paste("  CRITICAL (VIF > 10):", if (nrow(critical) > 0) paste(critical$Variable, collapse = ", ") else "None"),
    paste("  WARNING  (VIF 5-10):", if (nrow(warning_vars) > 0) paste(warning_vars$Variable, collapse = ", ") else "None"),
    paste("  GOOD     (VIF < 5 ):", sum(vif_df$VIF < 5), "variables"),
    "================================================================"
  )
  writeLines(out_lines, out_file)
  cat("  VIF results saved to:", out_file, "\n\n")

  invisible(vif_df)
}

# ================================================================
# MAIN EXECUTION FUNCTION
# ================================================================

run_robyn_model <- function(
    config_name,
    use_gdrive = TRUE,
    iterations = 2000,
    trials = 6,
    ts_validation = FALSE,
    add_penalty_factor = FALSE,
    pareto_fronts = "auto",
    clusters = TRUE,
    csv_out = "pareto",
    plot_pareto = FALSE,
    calibration_constraint = 0.1
) {

  # Get configuration
  config <- MODEL_CONFIGS[[config_name]]

  # Calibration is controlled solely via CALIBRATION_INPUTS
  calibration_input <- CALIBRATION_INPUTS[[config_name]]
  
  if (is.null(config)) {
    stop("Configuration not found: ", config_name,
         "\nAvailable configs: ", paste(names(MODEL_CONFIGS), collapse = ", "))
  }
  
  cat("\n")
  cat("================================================================\n")
  cat("  ROBYN MMM - MODEL CONFIGURATION\n")
  cat("================================================================\n")
  cat("Config Name:", config_name, "\n")
  cat("Country:", config$country, "\n")
  cat("Model Name:", config$model_name, "\n")
  cat("Dependent Variable:", config$dep_var, "\n")
  cat("Paid Media Channels:", length(config$paid_media_spends), "\n")
  cat("Context Variables:", length(config$context_vars), "\n")
  cat("Calibration:", ifelse(is.null(calibration_input), "None",
      paste(nrow(calibration_input), "experiment(s)")), "\n")
  cat("================================================================\n\n")
  
  # Create output directory structure: model_results/config_name/
  base_output_dir <- "model_results"
  robyn_directory <- file.path(base_output_dir, config_name)
  
  if (!dir.exists(robyn_directory)) {
    dir.create(robyn_directory, recursive = TRUE)
    cat("Created output folder:", robyn_directory, "\n\n")
  }
  
  # Load data
  cat("Loading data...\n")
  model_data <- load_model_data(
    country = config$country,
    gdrive_folder_id = GDRIVE_FOLDER_ID,
    use_gdrive = use_gdrive
  )
  
  # Aggregate channels
  model_data <- aggregate_channels(model_data, config$country)

  # VIF check on context variables
  check_context_vif(model_data, config$context_vars, config_name)

  # Generate hyperparameters
  cat("Generating hyperparameters...\n")
  hyperparameters <- generate_hyperparameters(
    paid_media_vars = config$paid_media_vars,
    organic_vars = config$organic_vars,
    adstock_type = config$adstock,
    train_size = config$train_size
  )
  cat("   Generated parameters for", length(config$paid_media_vars), "paid channels\n")  #  FIXED
  if (length(config$organic_vars) > 0) {
    cat("   Generated parameters for", length(config$organic_vars), "organic channels\n")
  }
  cat("\n")
  
  # Step 1: Create InputCollect
  cat("Step 1: Creating InputCollect...\n")
  if (!config$dep_var %in% names(model_data)) {
    stop("dep_var '", config$dep_var, "' not found in data.\n",
         "Available columns containing 'ltv' or 'acq': ",
         paste(grep("ltv|acq|acquisition", names(model_data), value = TRUE, ignore.case = TRUE), collapse = ", "))
  }
  InputCollect <- robyn_inputs(
    dt_input        = model_data,
    dt_holidays     = dt_prophet_holidays,
    date_var        = config$date_var,
    dep_var         = config$dep_var,
    dep_var_type    = config$dep_var_type,
    prophet_vars    = config$prophet_vars,
    prophet_country = config$prophet_country,
    context_vars    = config$context_vars,
    paid_media_spends = config$paid_media_spends,
    paid_media_vars = config$paid_media_vars,
    organic_vars    = config$organic_vars,
    window_start    = config$window_start,
    window_end      = config$window_end,
    adstock         = config$adstock
  )
  print(InputCollect)
  
  # Step 2: Add hyperparameters (and optionally calibration)
  cat("\nStep 2: Adding hyperparameters...\n")
  InputCollect <- robyn_inputs(
    InputCollect = InputCollect,
    hyperparameters = hyperparameters,
    calibration_input = calibration_input
  )
  if (!is.null(calibration_input)) {
    cat("   Calibration added:", nrow(calibration_input), "experiment(s) across",
        length(unique(calibration_input$channel)), "channel(s)\n")
  }
  print(InputCollect)
  
  # Step 3: Run models
  cat("\nStep 3: Running Robyn models...\n")
  cat("This may take several minutes...\n\n")
  
  OutputModels <- robyn_run(
    InputCollect = InputCollect,
    cores = NULL,
    iterations = iterations,
    trials = trials,
    ts_validation = ts_validation,
    add_penalty_factor = add_penalty_factor
  )
  print(OutputModels)
  
  # Step 4: Generate Pareto outputs
  cat("\nStep 4: Generating Pareto outputs...\n")
  
  OutputCollect <- robyn_outputs(
    InputCollect,
    OutputModels,
    pareto_fronts = pareto_fronts,
    calibration_constraint = calibration_constraint,
    csv_out = csv_out,
    clusters = clusters,
    export = create_files,
    plot_folder = robyn_directory,
    plot_pareto = plot_pareto
  )
  print(OutputCollect)

  # Save RDS files for later use (response curves, budget allocation)
  rds_folder <- OutputCollect$plot_folder
  saveRDS(InputCollect,  file.path(rds_folder, "InputCollect.RDS"))
  saveRDS(OutputModels,  file.path(rds_folder, "OutputModels.RDS"))
  saveRDS(OutputCollect, file.path(rds_folder, "OutputCollect.RDS"))
  cat("\nSaved RDS files to:", rds_folder, "\n")

  cat("\n")
  cat("================================================================\n")
  cat("  MODEL COMPLETED SUCCESSFULLY!\n")
  cat("================================================================\n")
  cat("All Robyn outputs saved to:", robyn_directory, "\n")
  cat("================================================================\n\n")

  # Return results for further analysis
  return(list(
    InputCollect = InputCollect,
    OutputModels = OutputModels,
    OutputCollect = OutputCollect,
    config = config,
    output_dir = robyn_directory
  ))
}

# ================================================================
# EXECUTION - Run your models here
# ================================================================

if (RUN_UK_LTV) {
  results_uk_ltv <- run_robyn_model(
    config_name        = "uk_ltv",
    use_gdrive         = TRUE,
    add_penalty_factor = TRUE
  )
}

if (RUN_UK_ACQ) {
  results_uk_acq <- run_robyn_model(
    config_name        = "uk_acq",
    use_gdrive         = TRUE,
    add_penalty_factor = TRUE
  )
}

