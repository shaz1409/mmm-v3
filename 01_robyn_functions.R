################################################################################
#################### ROBYN MMM - HELPER FUNCTIONS ##############################
################################################################################
# Purpose: Reusable functions for data extraction, cleaning, and model running
# Usage: source("1_robyn_functions.R")
################################################################################

library(bigrquery)
library(glue)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
cat("Loading Robyn helper functions...\n")

# =============================================================================
# SECTION 1: DATA EXTRACTION FUNCTIONS
# =============================================================================

#' Extract paid media data from BigQuery
extract_paid_media <- function(bq_config) {
  cat("\n=== Extracting Paid Media Data ===\n")
  
  tryCatch({
    sql_query <- glue("SELECT * FROM `{bq_config$project_id}.{bq_config$paid_media_table}`")
    
    paid_data <- bq_table_download(
      bq_project_query(
        x = bq_config$project_id,
        query = sql_query,
        use_legacy_sql = FALSE,  # Added
        location = bq_config$location
      )
    )
    
    # Validate data
    if (is.null(paid_data) || nrow(paid_data) == 0) {
      stop("No data returned from paid media table")
    }
    
    # Validate required columns
    required_cols <- c("week_commencing", "country")
    missing_cols <- setdiff(required_cols, names(paid_data))
    if (length(missing_cols) > 0) {
      stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
    }
    
    cat(glue("✓ Paid media data extracted: {nrow(paid_data)} rows × {ncol(paid_data)} columns\n"))
    cat(glue("✓ Date range: {min(paid_data$week_commencing)} to {max(paid_data$week_commencing)}\n"))
    cat(glue("✓ Markets: {paste(unique(paid_data$country), collapse=', ')}\n\n"))
    
    return(paid_data)
    
  }, error = function(e) {
    stop(glue("Failed to extract paid media data: {e$message}"))
  })
}


#' Extract internal data from BigQuery
extract_internal_data <- function(bq_config) {
  cat("=== Extracting Internal Data ===\n")
  
  tryCatch({
    sql_query <- glue("SELECT * FROM `{bq_config$project_id}.{bq_config$internal_table}`")
    
    internal_data <- bq_table_download(
      bq_project_query(
        x = bq_config$project_id,
        query = sql_query,
        use_legacy_sql = FALSE,
        location = bq_config$location
      )
    )
    
    cat(glue("✓ Internal data extracted: {nrow(internal_data)} rows × {ncol(internal_data)} columns\n\n"))
    
    return(internal_data)
    
  }, error = function(e) {
    stop(glue("Failed to extract internal data: {e$message}"))
  })
}
