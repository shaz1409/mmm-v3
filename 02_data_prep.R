# ==============================================================================
# QUARTERLY MMM DATA PREP & REVIEW
# Script 2: Clean data and generate descriptive analysis
# ==============================================================================

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibble)
library(car)  # For vif() function
library(purrr)

# Pin dplyr::select before bigrquery can shadow it
select <- dplyr::select



# Setup ------------------------------------------------------------------------
cat("\n=== STARTING DATA PREP & REVIEW ===\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create folders
if (!dir.exists("data_clean")) dir.create("data_clean")

# Load Raw Data ----------------------------------------------------------------
cat("Loading raw data...\n")

# Set working directory
script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) dirname(normalizePath(sys.frames()[[1]]$ofile))
)
source(file.path(script_dir, "config.R"))
setwd(env$working_dir)
source(file.path(script_dir, "01_robyn_functions.R"))

options(gargle_oauth_cache = ".secrets")

bq_deauth()
bq_auth(
  email = env$user_email,
  scopes = c(
    "https://www.googleapis.com/auth/bigquery",
    "https://www.googleapis.com/auth/drive.readonly"
  ),
  cache = FALSE
)
cat("✓ BigQuery & Google Drive authenticated\n\n")


#Extract data
paid_data <- extract_paid_media(bq_config)

# Extract internal data
internal_data <- extract_internal_data(bq_config)


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
  
  # Set the target folder ID - data_clean folder
  target_folder_id <- drive_folders$data_clean
  
  # Check if folder exists and is accessible
  folder_info <- drive_get(as_id(target_folder_id))
  
  if (nrow(folder_info) > 0) {
    cat("  ✓ Connected to Google Drive folder:", folder_info$name, "\n")
    cat("  ✓ Folder path: .../MMM/MMM V3/Data/data_clean\n\n")
    gdrive_enabled <- TRUE
  } else {
    cat("  ⚠️  Warning: Could not access Google Drive folder\n")
    cat("  Files will be saved locally only\n\n")
    gdrive_enabled <- FALSE
  }
}, error = function(e) {
  cat("  ⚠️  Warning: Google Drive setup failed:", e$message, "\n")
  cat("  Files will be saved locally only\n\n")
  gdrive_enabled <<- FALSE
})

cat("Google Drive export enabled:", gdrive_enabled, "\n\n")

# ==============================================================================
# DATA CLEANING & TRANSFORMATIONS
# ==============================================================================

cat("=== DATA CLEANING & TRANSFORMATIONS ===\n\n")

# 1. Check and convert date columns --------------------------------------------
cat("Step 1: Converting date formats...\n")

# Convert week_commencing to Date format
paid_data$week_commencing <- as.Date(paid_data$week_commencing)
internal_data$week_commencing <- as.Date(internal_data$week_commencing)

cat("✓ Date columns converted\n\n")


# ==============================================================================
# STEP 1: DATA TYPE VALIDATION & NORMALIZATION
# ==============================================================================

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("STEP 1: DATA TYPE VALIDATION & NORMALIZATION\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# 1.1 Define data structure ----------------------------------------------------
cat("1.1. Defining expected data structure...\n\n")

# INTERNAL DATA STRUCTURE
# Date: week_commencing
# Categorical: region, country
# Numeric: ALL other columns (117 columns total)
categorical_cols_internal <- c("region", "country")
categorical_cols_internal <- categorical_cols_internal[categorical_cols_internal %in% names(internal_data)]
numeric_cols_internal <- setdiff(names(internal_data), c("week_commencing", categorical_cols_internal))

cat("INTERNAL DATA STRUCTURE:\n")
cat("  Total columns:", ncol(internal_data), "\n")
cat("  - Date (1): week_commencing\n")
if (length(categorical_cols_internal) > 0) {
  cat("  - Categorical (", length(categorical_cols_internal), "): ", 
      paste(categorical_cols_internal, collapse = ", "), "\n", sep = "")
} else {
  cat("  - Categorical (0): (region/country not found)\n")
}
cat("  - Numeric (", length(numeric_cols_internal), "): all other columns\n\n", sep = "")

# PAID DATA STRUCTURE
# Date: week_commencing
# Categorical: campaign_name, platform, channel, strategy, country, currency
# Numeric: spend, clicks, impressions, installs, quality_reads, video_views
categorical_cols_paid <- c("campaign_name", "platform", "channel", "strategy", "country", "currency")
categorical_cols_paid <- categorical_cols_paid[categorical_cols_paid %in% names(paid_data)]

numeric_cols_paid <- c("spend", "clicks", "impressions", "installs", "quality_reads", "video_views")
numeric_cols_paid <- numeric_cols_paid[numeric_cols_paid %in% names(paid_data)]

cat("PAID DATA STRUCTURE:\n")
cat("  Total columns:", ncol(paid_data), "\n")
cat("  - Date (1): week_commencing\n")
cat("  - Categorical (", length(categorical_cols_paid), "): ", 
    paste(categorical_cols_paid, collapse = ", "), "\n", sep = "")
cat("  - Numeric (", length(numeric_cols_paid), "): ", 
    paste(numeric_cols_paid, collapse = ", "), "\n\n", sep = "")

# Verify all columns are accounted for
internal_unaccounted <- setdiff(names(internal_data), c("week_commencing", categorical_cols_internal, numeric_cols_internal))
paid_unaccounted <- setdiff(names(paid_data), c("week_commencing", categorical_cols_paid, numeric_cols_paid))

if (length(internal_unaccounted) > 0) {
  cat("⚠️  Unaccounted INTERNAL columns:", paste(internal_unaccounted, collapse = ", "), "\n")
}
if (length(paid_unaccounted) > 0) {
  cat("⚠️  Unaccounted PAID columns:", paste(paid_unaccounted, collapse = ", "), "\n")
}

if (length(internal_unaccounted) == 0 && length(paid_unaccounted) == 0) {
  cat("✓ All columns accounted for\n")
}
cat("\n")


# 1.2 Display current data types -----------------------------------------------
cat("1.2. Checking current data types...\n\n")

cat("INTERNAL DATA - Sample of current types:\n")
internal_type_summary <- data.frame(
  column = names(internal_data),
  current_type = sapply(internal_data, function(x) class(x)[1]),
  expected_type = ifelse(
    names(internal_data) == "week_commencing", "Date",
    ifelse(names(internal_data) %in% categorical_cols_internal, "character", "numeric")
  ),
  stringsAsFactors = FALSE
)
print(head(internal_type_summary, 10))
cat("... (", nrow(internal_type_summary), " total columns)\n\n", sep = "")

cat("PAID DATA - All current types:\n")
paid_type_summary <- data.frame(
  column = names(paid_data),
  current_type = sapply(paid_data, function(x) class(x)[1]),
  expected_type = ifelse(
    names(paid_data) == "week_commencing", "Date",
    ifelse(names(paid_data) %in% categorical_cols_paid, "character", "numeric")
  ),
  stringsAsFactors = FALSE
)
print(paid_type_summary)
cat("\n")


# 1.3 Convert to correct data types --------------------------------------------
cat("1.3. Converting columns to correct data types...\n\n")

# Convert INTERNAL DATA numeric columns
cat("INTERNAL DATA - Converting to numeric:\n")
converted_count <- 0
for (col in numeric_cols_internal) {
  if (col %in% names(internal_data) && !is.numeric(internal_data[[col]])) {
    original_type <- class(internal_data[[col]])[1]
    suppressWarnings({
      internal_data[[col]] <- as.numeric(internal_data[[col]])
    })
    cat("  ✓", col, ":", original_type, "→ numeric\n")
    converted_count <- converted_count + 1
  }
}
if (converted_count == 0) {
  cat("  ✓ All", length(numeric_cols_internal), "numeric columns already correct type\n")
} else {
  cat("  ✓ Converted", converted_count, "columns to numeric\n")
}

# Convert INTERNAL DATA categorical columns
cat("\nINTERNAL DATA - Converting categoricals to character:\n")
converted_count <- 0
for (col in categorical_cols_internal) {
  if (col %in% names(internal_data) && !is.character(internal_data[[col]])) {
    original_type <- class(internal_data[[col]])[1]
    internal_data[[col]] <- as.character(internal_data[[col]])
    cat("  ✓", col, ":", original_type, "→ character\n")
    converted_count <- converted_count + 1
  }
}
if (converted_count == 0) {
  cat("  ✓ All", length(categorical_cols_internal), "categorical columns already character type\n")
}

# Convert PAID DATA numeric columns
cat("\nPAID DATA - Converting to numeric:\n")
converted_count <- 0
for (col in numeric_cols_paid) {
  if (col %in% names(paid_data) && !is.numeric(paid_data[[col]])) {
    original_type <- class(paid_data[[col]])[1]
    suppressWarnings({
      paid_data[[col]] <- as.numeric(paid_data[[col]])
    })
    cat("  ✓", col, ":", original_type, "→ numeric\n")
    converted_count <- converted_count + 1
  }
}
if (converted_count == 0) {
  cat("  ✓ All", length(numeric_cols_paid), "numeric columns already correct type\n")
} else {
  cat("  ✓ Converted", converted_count, "columns to numeric\n")
}

# Convert PAID DATA categorical columns
cat("\nPAID DATA - Converting categoricals to character:\n")
converted_count <- 0
for (col in categorical_cols_paid) {
  if (col %in% names(paid_data) && !is.character(paid_data[[col]])) {
    original_type <- class(paid_data[[col]])[1]
    paid_data[[col]] <- as.character(paid_data[[col]])
    cat("  ✓", col, ":", original_type, "→ character\n")
    converted_count <- converted_count + 1
  }
}
if (converted_count == 0) {
  cat("  ✓ All", length(categorical_cols_paid), "categorical columns already character type\n")
}

cat("\n✓ Data type conversion complete\n\n")


# 1.4 Verify final data types --------------------------------------------------
cat("1.4. Verifying final data types...\n\n")

# Check internal data
internal_issues <- 0
for (col in names(internal_data)) {
  expected <- if (col == "week_commencing") "Date" 
  else if (col %in% categorical_cols_internal) "character" 
  else "numeric"
  
  actual <- class(internal_data[[col]])[1]
  
  is_correct <- (expected == "Date" & actual == "Date") |
    (expected == "character" & actual == "character") |
    (expected == "numeric" & actual %in% c("numeric", "integer"))
  
  if (!is_correct) {
    cat("✗ INTERNAL:", col, "- expected", expected, "but got", actual, "\n")
    internal_issues <- internal_issues + 1
  }
}

if (internal_issues == 0) {
  cat("✓ INTERNAL DATA: All", ncol(internal_data), "columns have correct data types\n")
}

# Check paid data
paid_issues <- 0
for (col in names(paid_data)) {
  expected <- if (col == "week_commencing") "Date" 
  else if (col %in% categorical_cols_paid) "character" 
  else "numeric"
  
  actual <- class(paid_data[[col]])[1]
  
  is_correct <- (expected == "Date" & actual == "Date") |
    (expected == "character" & actual == "character") |
    (expected == "numeric" & actual %in% c("numeric", "integer"))
  
  if (!is_correct) {
    cat("✗ PAID:", col, "- expected", expected, "but got", actual, "\n")
    paid_issues <- paid_issues + 1
  }
}

if (paid_issues == 0) {
  cat("✓ PAID DATA: All", ncol(paid_data), "columns have correct data types\n")
}

cat("\n")

# ==============================================================================
# STEP 2: DATA COMPLETENESS & NULL CHECKS
# ==============================================================================

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("STEP 2: DATA COMPLETENESS & NULL CHECKS\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# 2.1 NULL and ZERO analysis ---------------------------------------------------
cat("2.1. Analyzing NULL and ZERO values...\n\n")

# Function to create completeness report
create_completeness_report <- function(data, data_name, numeric_cols) {
  
  cat("--- ", data_name, " ---\n", sep = "")
  
  total_rows <- nrow(data)
  
  # Create completeness dataframe
  completeness <- data.frame(
    column = names(data),
    total_rows = total_rows,
    null_count = sapply(data, function(x) sum(is.na(x))),
    stringsAsFactors = FALSE
  )
  
  # Add percentages
  completeness$null_pct <- round(completeness$null_count / total_rows * 100, 2)
  
  # Add zero analysis for numeric columns
  completeness$zero_count <- NA
  completeness$zero_pct <- NA
  completeness$non_null_non_zero_count <- NA
  completeness$non_null_non_zero_pct <- NA
  
  for (col in names(data)) {
    if (col %in% numeric_cols && is.numeric(data[[col]])) {
      # Count zeros
      zero_count <- sum(data[[col]] == 0, na.rm = TRUE)
      completeness[completeness$column == col, "zero_count"] <- zero_count
      completeness[completeness$column == col, "zero_pct"] <- 
        round(zero_count / total_rows * 100, 2)
      
      # Count non-null, non-zero
      non_null_non_zero <- sum(!is.na(data[[col]]) & data[[col]] != 0)
      completeness[completeness$column == col, "non_null_non_zero_count"] <- non_null_non_zero
      completeness[completeness$column == col, "non_null_non_zero_pct"] <- 
        round(non_null_non_zero / total_rows * 100, 2)
    }
  }
  
  # Print sample
  cat("\nSample (first 10 rows):\n")
  print(head(completeness, 10))
  cat("... (", nrow(completeness), " total columns)\n\n", sep = "")
  
  # Flag issues
  cat("ISSUES DETECTED:\n")
  
  # High null percentage
  high_nulls <- completeness %>%
    filter(null_pct > 5) %>%
    arrange(desc(null_pct))
  
  if (nrow(high_nulls) > 0) {
    cat("⚠️  Columns with >5% NULL values (",
        nrow(high_nulls), "):\n", sep = "")
    print(high_nulls %>% select(column, null_count, null_pct))
    cat("\n")
  } else {
    cat("✓ No columns with >30% NULL values\n\n")
  }
  
  # High zero percentage (for numeric columns)
  high_zeros <- completeness %>%
    filter(!is.na(zero_pct) & zero_pct > 80) %>%
    arrange(desc(zero_pct))
  
  if (nrow(high_zeros) > 0) {
    cat("⚠️  Numeric columns with >80% ZERO values (", nrow(high_zeros), "):\n", sep = "")
    print(high_zeros %>% select(column, zero_count, zero_pct))
    cat("\n")
  } else {
    cat("✓ No numeric columns with >80% ZERO values\n\n")
  }
  
  return(completeness)
}

# Generate reports
paid_completeness <- create_completeness_report(paid_data, "PAID DATA", numeric_cols_paid)
internal_completeness <- create_completeness_report(internal_data, "INTERNAL DATA", numeric_cols_internal)



# 2.2 Date continuity analysis -------------------------------------------------
cat("2.2. Analyzing date continuity...\n\n")

# Function to check date continuity
check_date_continuity <- function(data, data_name) {
  
  cat("--- ", data_name, " ---\n", sep = "")
  
  # Overall date range
  date_range <- range(data$week_commencing, na.rm = TRUE)
  cat("Overall date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
  
  # Calculate expected weeks
  expected_weeks <- as.numeric(difftime(date_range[2], date_range[1], units = "weeks")) + 1
  cat("Expected weeks (based on range):", ceiling(expected_weeks), "\n")
  
  # Actual unique weeks
  actual_weeks <- length(unique(data$week_commencing))
  cat("Actual unique weeks:", actual_weeks, "\n")
  
  # Create complete date spine
  date_spine <- seq.Date(
    from = floor_date(date_range[1], "week", week_start = 1),
    to = floor_date(date_range[2], "week", week_start = 1),
    by = "week"
  )
  
  cat("Complete date spine weeks:", length(date_spine), "\n\n")
  
  return(list(
    date_range = date_range,
    date_spine = date_spine,
    expected_weeks = ceiling(expected_weeks),
    actual_weeks = actual_weeks
  ))
}

# Check date continuity for paid media
paid_date_check <- check_date_continuity(paid_data, "PAID DATA")

# Check date continuity for internal data
internal_date_check <- check_date_continuity(internal_data, "INTERNAL DATA")

cat("✓ Date continuity analysis complete\n\n")


# ==============================================================================
# STEP 3: DATA CLEANING BY COUNTRY
# ==============================================================================

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("STEP 3: DATA CLEANING BY COUNTRY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# Identify countries -----------------------------------------------------------
cat("Identifying countries in data...\n\n")

paid_countries <- unique(paid_data$country[!is.na(paid_data$country)])
internal_countries <- unique(internal_data$country[!is.na(internal_data$country)])

cat("Paid data countries:", paste(paid_countries, collapse = ", "), "\n")
cat("Internal data countries:", paste(internal_countries, collapse = ", "), "\n")

# Get all unique countries
all_countries <- unique(c(paid_countries, internal_countries))
cat("\nCountries to process:", paste(all_countries, collapse = ", "), "\n")
cat("Total:", length(all_countries), "\n\n")

# Initialize storage for cleaned data
cleaned_paid_by_country <- list()
cleaned_internal_by_country <- list()
cleaning_reports <- list()


# ==============================================================================
# PROCESS EACH COUNTRY SEPARATELY
# ==============================================================================

for (current_country in all_countries) {
  
  cat("#" %>% rep(80) %>% paste(collapse = ""), "\n")
  cat("### PROCESSING: ", current_country, "\n", sep = "")
  cat("#" %>% rep(80) %>% paste(collapse = ""), "\n\n")
  
  # Filter data
  paid_country <- paid_data %>% filter(country == current_country)
  internal_country <- internal_data %>% filter(country == current_country)
  
  cat("Data for", current_country, ":\n")
  cat("  Paid data:", nrow(paid_country), "rows\n")
  cat("  Internal data:", nrow(internal_country), "rows\n\n")
  
  if (nrow(paid_country) == 0 || nrow(internal_country) == 0) {
    cat("⚠️  Skipping\n\n")
    next
  }
  
  # Initialize report (ONCE, HERE)
  report <- list()
  report$country <- current_country
  report$issues <- list()


# ============================================================================
# 3.1 - MISSING VALUE ANALYSIS
# ============================================================================

cat("3.1. Missing Value Analysis...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Analyze paid data
paid_missing <- data.frame(
  column = names(paid_country),
  null_count = colSums(is.na(paid_country)),
  null_pct = round(colSums(is.na(paid_country)) / nrow(paid_country) * 100, 2)
) %>%
  filter(null_count > 0) %>%
  arrange(desc(null_pct))

# Analyze internal data
internal_missing <- data.frame(
  column = names(internal_country),
  null_count = colSums(is.na(internal_country)),
  null_pct = round(colSums(is.na(internal_country)) / nrow(internal_country) * 100, 2)
) %>%
  filter(null_count > 0) %>%
  arrange(desc(null_pct))

cat("Paid data - columns with nulls:", nrow(paid_missing), "\n")
if (nrow(paid_missing) > 0) {
  print(paid_missing)
  cat("\n")
}

cat("Internal data - columns with nulls:", nrow(internal_missing), "\n")
if (nrow(internal_missing) > 0) {
  print(internal_missing)
  cat("\n")
}

# Identify columns to drop (>30% missing)
paid_drop_cols <- paid_missing %>% filter(null_pct > 30) %>% pull(column)
internal_drop_cols <- internal_missing %>% filter(null_pct > 30) %>% pull(column)

if (length(paid_drop_cols) > 0) {
  cat("⚠️  Dropping", length(paid_drop_cols), "paid data columns (>30% missing):\n")
  cat("   ", paste(paid_drop_cols, collapse = ", "), "\n\n")
  paid_country <- paid_country %>% select(-all_of(paid_drop_cols))
}

if (length(internal_drop_cols) > 0) {
  cat("⚠️  Dropping", length(internal_drop_cols), "internal data columns (>30% missing):\n")
  cat("   ", paste(internal_drop_cols, collapse = ", "), "\n\n")
  internal_country <- internal_country %>% select(-all_of(internal_drop_cols))
}

report$dropped_columns <- list(paid = paid_drop_cols, internal = internal_drop_cols)

# ============================================================================
# 3.2 - MISSING VALUE IMPUTATION
# ============================================================================

cat("3.2. Missing Value Imputation...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# PAID DATA: Replace nulls with 0 for numeric columns
cat("Paid data - replacing nulls with 0:\n")
imputed_count <- 0
for (col in numeric_cols_paid) {
  if (col %in% names(paid_country) && is.numeric(paid_country[[col]])) {
    na_count <- sum(is.na(paid_country[[col]]))
    if (na_count > 0) {
      paid_country[[col]][is.na(paid_country[[col]])] <- 0
      cat("  ✓", col, ":", na_count, "nulls → 0\n")
      imputed_count <- imputed_count + 1
    }
  }
}
if (imputed_count == 0) cat("  No nulls to impute\n")
cat("\n")

# INTERNAL DATA: Rolling average for numeric columns
cat("Internal data - rolling average imputation:\n")

# Get remaining missing columns
internal_missing_remaining <- names(internal_country)[
  sapply(internal_country, function(x) sum(is.na(x)) > 0)
]

# Filter to numeric columns only
internal_missing_numeric <- intersect(internal_missing_remaining, numeric_cols_internal)

if (length(internal_missing_numeric) > 0) {
  # Sort by date
  internal_country <- internal_country %>% arrange(week_commencing)
  
  # Rolling average function
  rolling_impute <- function(x, window = 3) {
    for (i in which(is.na(x))) {
      start_idx <- max(1, i - window)
      end_idx <- min(length(x), i + window)
      window_values <- x[start_idx:end_idx]
      window_values <- window_values[!is.na(window_values)]
      
      if (length(window_values) > 0) {
        x[i] <- mean(window_values)
      } else {
        x[i] <- mean(x, na.rm = TRUE)  # Fallback to column mean
      }
    }
    return(x)
  }
  
  # Apply imputation
  for (col in internal_missing_numeric) {
    na_count <- sum(is.na(internal_country[[col]]))
    internal_country[[col]] <- rolling_impute(internal_country[[col]], window = 3)
    cat("  ✓", col, ":", na_count, "nulls → rolling avg\n")
  }
} else {
  cat("  No nulls to impute\n")
}
cat("\n")


# ============================================================================
# 3.3 - NEGATIVE VALUE CHECK (UPDATED VERSION)
# ============================================================================

cat("3.3. Checking for Negative Values...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Check paid data
negative_paid <- data.frame()
spend_fixed_count <- 0

for (col in numeric_cols_paid) {
  if (col %in% names(paid_country) && is.numeric(paid_country[[col]])) {
    neg_count <- sum(paid_country[[col]] < 0, na.rm = TRUE)
    
    if (neg_count > 0) {
      # Special handling for spend - set to 0
      if (col == "spend") {
        cat("⚠️  Found", neg_count, "negative spend values - setting to 0\n")
        paid_country[[col]][paid_country[[col]] < 0] <- 0
        spend_fixed_count <- neg_count
      } else {
        # For other columns, just report
        negative_paid <- rbind(negative_paid, data.frame(
          column = col,
          negative_count = neg_count,
          min_value = min(paid_country[[col]], na.rm = TRUE)
        ))
      }
    }
  }
}

# Check internal data
negative_internal <- data.frame()
for (col in numeric_cols_internal) {
  if (col %in% names(internal_country) && is.numeric(internal_country[[col]])) {
    neg_count <- sum(internal_country[[col]] < 0, na.rm = TRUE)
    if (neg_count > 0) {
      negative_internal <- rbind(negative_internal, data.frame(
        column = col,
        negative_count = neg_count,
        min_value = min(internal_country[[col]], na.rm = TRUE)
      ))
    }
  }
}

# Report findings
if (spend_fixed_count > 0) {
  cat("  ✓ Fixed", spend_fixed_count, "negative spend values (set to 0)\n")
}

if (nrow(negative_paid) > 0) {
  cat("⚠️  Negative values found in other paid data columns:\n")
  print(negative_paid)
  cat("\n")
  report$issues$negative_paid <- negative_paid
} else {
  if (spend_fixed_count == 0) {
    cat("✓ No negative values in paid data\n")
  }
}

if (nrow(negative_internal) > 0) {
  cat("⚠️  Negative values found in internal data:\n")
  print(negative_internal)
  cat("\n")
  report$issues$negative_internal <- negative_internal
} else {
  cat("✓ No negative values in internal data\n")
}

cat("\n")

# ============================================================================
# 3.4 - OUTLIER DETECTION
# ============================================================================

cat("3.4. Detecting Outliers (Z-score > 3)...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Function to detect outliers
detect_outliers <- function(data, numeric_cols, threshold = 3) {
  outliers <- data.frame()
  
  for (col in numeric_cols) {
    if (col %in% names(data) && is.numeric(data[[col]])) {
      mean_val <- mean(data[[col]], na.rm = TRUE)
      sd_val <- sd(data[[col]], na.rm = TRUE)
      
      if (sd_val > 0) {
        z_scores <- abs((data[[col]] - mean_val) / sd_val)
        outlier_count <- sum(z_scores > threshold, na.rm = TRUE)
        
        if (outlier_count > 0) {
          outliers <- rbind(outliers, data.frame(
            column = col,
            outlier_count = outlier_count,
            outlier_pct = round(outlier_count / nrow(data) * 100, 2),
            max_z_score = round(max(z_scores, na.rm = TRUE), 2)
          ))
        }
      }
    }
  }
  
  return(outliers)
}

paid_outliers <- detect_outliers(paid_country, numeric_cols_paid)
internal_outliers <- detect_outliers(internal_country, numeric_cols_internal)

if (nrow(paid_outliers) > 0) {
  cat("Paid data outliers:\n")
  print(paid_outliers)
  cat("\n")
  report$issues$outliers_paid <- paid_outliers
} else {
  cat("✓ No significant outliers in paid data\n\n")
}

if (nrow(internal_outliers) > 0) {
  cat("Internal data outliers:\n")
  print(internal_outliers)
  cat("\n")
  report$issues$outliers_internal <- internal_outliers
} else {
  cat("✓ No significant outliers in internal data\n\n")
}



# ============================================================================
# 3.5 - DATA CONSISTENCY CHECKS
# ============================================================================

cat("3.5. Data Consistency Checks...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

consistency_issues <- list()
all_issues_list <- list()

# Prepare columns to show in reports
id_cols <- c("week_commencing", "platform", "campaign_name")
id_cols_available <- id_cols[id_cols %in% names(paid_country)]

# Check: Clicks > Impressions
if ("clicks" %in% names(paid_country) && "impressions" %in% names(paid_country)) {
  invalid_ctr <- paid_country %>%
    filter(clicks > impressions & impressions > 0) %>%
    select(all_of(c(id_cols_available, "clicks", "impressions"))) %>%
    mutate(issue_type = "clicks_greater_than_impressions")
  
  if (nrow(invalid_ctr) > 0) {
    cat("⚠️  Found", nrow(invalid_ctr), "rows where clicks > impressions\n")
    consistency_issues$clicks_gt_impressions <- invalid_ctr
    all_issues_list[["clicks_gt_impressions"]] <- invalid_ctr
  } else {
    cat("✓ No rows where clicks > impressions\n")
  }
}

# Check: Spend = 0 but activity > 0
activity_metrics <- c("impressions", "clicks")

for (metric in activity_metrics) {
  if (all(c("spend", metric) %in% names(paid_country))) {
    
    # Find issue rows
    issue_rows <- paid_country %>%
      filter(spend == 0 & .data[[metric]] > 0) %>%
      select(all_of(c(id_cols_available, "spend", metric))) %>%
      mutate(issue_type = paste0("spend_zero_", metric, "_positive"))
    
    issue_name <- paste0("spend_zero_", metric, "_positive")
    
    if (nrow(issue_rows) > 0) {
      cat("⚠️  Found", nrow(issue_rows), "rows where spend=0 but", metric, ">0\n")
      
      consistency_issues[[issue_name]] <- issue_rows
      all_issues_list[[issue_name]] <- issue_rows
    } else {
      cat("✓ No rows where spend=0 but", metric, ">0\n")
    }
  }
}

cat("\n")

# Export consistency issues to files if any found
if (length(all_issues_list) > 0) {
  
  cat("Exporting consistency issues to files...\n")
  
  # Create consistency_issues subfolder
  issues_dir <- file.path("reports", "consistency_issues")
  if (!dir.exists(issues_dir)) dir.create(issues_dir, recursive = TRUE)
  
  # Combine all issues into one dataframe
  all_issues_combined <- bind_rows(all_issues_list)
  
  # Export detailed CSV
  csv_filename <- file.path(issues_dir, paste0("consistency_issues_", 
                                               tolower(gsub(" ", "_", current_country)), 
                                               ".csv"))
  write.csv(all_issues_combined, csv_filename, row.names = FALSE)
  cat("  ✓ Exported detailed issues to:", csv_filename, "\n")
  
  # Create summary report by platform and campaign
  summary_report <- all_issues_combined %>%
    group_by(platform, campaign_name, issue_type) %>%
    summarise(
      occurrences = n(),
      first_occurrence = min(week_commencing, na.rm = TRUE),
      last_occurrence = max(week_commencing, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(issue_type, desc(occurrences))
  
  summary_csv <- file.path(issues_dir, paste0("consistency_summary_", 
                                              tolower(gsub(" ", "_", current_country)), 
                                              ".csv"))
  write.csv(summary_report, summary_csv, row.names = FALSE)
  cat("  ✓ Exported summary to:", summary_csv, "\n")
  
  # Create readable text report
  report_filename <- file.path(issues_dir, paste0("consistency_report_", 
                                                  tolower(gsub(" ", "_", current_country)), 
                                                  ".txt"))
  
  sink(report_filename)
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
  cat("DATA CONSISTENCY ISSUES REPORT\n")
  cat("Country:", current_country, "\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
  
  cat("SUMMARY OF ISSUES\n")
  cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")
  
  # Count by issue type
  issue_counts <- all_issues_combined %>%
    group_by(issue_type) %>%
    summarise(total_rows = n(), .groups = "drop")
  
  for (i in 1:nrow(issue_counts)) {
    cat("Issue:", issue_counts$issue_type[i], "\n")
    cat("  Total occurrences:", issue_counts$total_rows[i], "\n\n")
  }
  
  cat("\n")
  cat("DETAILED BREAKDOWN BY PLATFORM AND CAMPAIGN\n")
  cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")
  
  for (issue in names(all_issues_list)) {
    cat("\n")
    cat("=== ", toupper(gsub("_", " ", issue)), " ===\n\n", sep = "")
    
    issue_data <- all_issues_list[[issue]]
    
    # Summary by platform/campaign
    issue_summary <- issue_data %>%
      group_by(platform, campaign_name) %>%
      summarise(
        occurrences = n(),
        date_range = paste(min(week_commencing), "to", max(week_commencing)),
        .groups = "drop"
      ) %>%
      arrange(desc(occurrences))
    
    cat("Affected Platforms and Campaigns:\n\n")
    
    for (j in 1:nrow(issue_summary)) {
      cat(j, ". Platform:", issue_summary$platform[j], "\n")
      cat("   Campaign:", issue_summary$campaign_name[j], "\n")
      cat("   Occurrences:", issue_summary$occurrences[j], "\n")
      cat("   Date Range:", issue_summary$date_range[j], "\n\n")
    }
  }
  
  cat("\n")
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
  cat("END OF REPORT\n")
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
  
  sink()
  
  cat("  ✓ Exported readable report to:", report_filename, "\n\n")
  
  report$consistency_files <- list(
    detailed_csv = csv_filename,
    summary_csv = summary_csv,
    text_report = report_filename
  )
  
} else {
  cat("✓ No consistency issues found - no files exported\n\n")
}

if (length(consistency_issues) > 0) {
  report$issues$consistency <- consistency_issues
}



# ============================================================================
# 3.6 - DUPLICATE ROWS CHECK
# ============================================================================


cat("3.6. Checking for Duplicate Rows (all columns identical)...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Check paid data - find rows where ALL columns are identical
paid_dupes <- paid_country %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(paid_dupes) > 0) {
  unique_dupes <- paid_dupes %>%
    group_by(across(everything())) %>%
    slice(1) %>%
    ungroup()
  
  cat("⚠️  Found", nrow(paid_dupes), "duplicate rows in paid data\n")
  cat("   (", nrow(unique_dupes), "unique row patterns duplicated)\n")
  cat("   Removing duplicate rows...\n")
  
  # Remove duplicates - keep only distinct rows
  paid_country <- paid_country %>% distinct()
  
  cat("   ✓ Duplicates removed\n")
  cat("   Rows after deduplication:", nrow(paid_country), "\n\n")
  
  report$issues$duplicates_paid <- list(
    total_dupes = nrow(paid_dupes),
    unique_patterns = nrow(unique_dupes),
    rows_removed = nrow(paid_dupes) - nrow(unique_dupes)
  )
} else {
  cat("✓ No duplicate rows in paid data\n\n")
}

# Check internal data - find rows where ALL columns are identical
internal_dupes <- internal_country %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(internal_dupes) > 0) {
  unique_dupes <- internal_dupes %>%
    group_by(across(everything())) %>%
    slice(1) %>%
    ungroup()
  
  cat("⚠️  Found", nrow(internal_dupes), "duplicate rows in internal data\n")
  cat("   (", nrow(unique_dupes), "unique row patterns duplicated)\n")
  cat("   Removing duplicate rows...\n")
  
  # Remove duplicates - keep only distinct rows
  internal_country <- internal_country %>% distinct()
  
  cat("   ✓ Duplicates removed\n")
  cat("   Rows after deduplication:", nrow(internal_country), "\n\n")
  
  report$issues$duplicates_internal <- list(
    total_dupes = nrow(internal_dupes),
    unique_patterns = nrow(unique_dupes),
    rows_removed = nrow(internal_dupes) - nrow(unique_dupes)
  )
} else {
  cat("✓ No duplicate rows in internal data\n\n")
}


# ============================================================================
# 3.7 - REMOVE ZERO-VARIANCE COLUMNS
# ============================================================================

cat("3.7. Removing Zero-Variance Columns...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")


# Check internal data
zero_var_internal <- c()
for (col in numeric_cols_internal) {
  if (col %in% names(internal_country) && is.numeric(internal_country[[col]])) {
    if (var(internal_country[[col]], na.rm = TRUE) == 0 || 
        length(unique(internal_country[[col]][!is.na(internal_country[[col]])])) == 1) {
      zero_var_internal <- c(zero_var_internal, col)
    }
  }
}


if (length(zero_var_internal) > 0) {
  cat("⚠️  Removing", length(zero_var_internal), "zero-variance columns from internal data:\n")
  cat("   ", paste(zero_var_internal, collapse = ", "), "\n")
  internal_country <- internal_country %>% select(-all_of(zero_var_internal))
  report$zero_variance_internal <- zero_var_internal
} else {
  cat("✓ No zero-variance columns in internal data\n")
}

cat("\n")


# ============================================================================
# 3.8 - CHECK FOR Inf/NaN VALUES
# ============================================================================

cat("3.8. Checking for Inf/NaN Values...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Check and fix paid data
inf_count_paid <- 0
nan_count_paid <- 0

for (col in names(paid_country)) {
  if (is.numeric(paid_country[[col]])) {
    inf_count <- sum(is.infinite(paid_country[[col]]))
    nan_count <- sum(is.nan(paid_country[[col]]))
    
    if (inf_count > 0) {
      paid_country[[col]][is.infinite(paid_country[[col]])] <- NA
      inf_count_paid <- inf_count_paid + inf_count
      cat("  Fixed", inf_count, "Inf values in", col, "\n")
    }
    
    if (nan_count > 0) {
      paid_country[[col]][is.nan(paid_country[[col]])] <- NA
      nan_count_paid <- nan_count_paid + nan_count
      cat("  Fixed", nan_count, "NaN values in", col, "\n")
    }
  }
}

# Check and fix internal data
inf_count_internal <- 0
nan_count_internal <- 0

for (col in names(internal_country)) {
  if (is.numeric(internal_country[[col]])) {
    inf_count <- sum(is.infinite(internal_country[[col]]))
    nan_count <- sum(is.nan(internal_country[[col]]))
    
    if (inf_count > 0) {
      internal_country[[col]][is.infinite(internal_country[[col]])] <- NA
      inf_count_internal <- inf_count_internal + inf_count
      cat("  Fixed", inf_count, "Inf values in", col, "\n")
    }
    
    if (nan_count > 0) {
      internal_country[[col]][is.nan(internal_country[[col]])] <- NA
      nan_count_internal <- nan_count_internal + nan_count
      cat("  Fixed", nan_count, "NaN values in", col, "\n")
    }
  }
}

if (inf_count_paid == 0 && nan_count_paid == 0) {
  cat("✓ No Inf/NaN values in paid data\n")
}

if (inf_count_internal == 0 && nan_count_internal == 0) {
  cat("✓ No Inf/NaN values in internal data\n")
}

cat("\n")

# ============================================================================
# STORE CLEANED DATA
# ============================================================================

cleaned_paid_by_country[[current_country]] <- paid_country
cleaned_internal_by_country[[current_country]] <- internal_country
cleaning_reports[[current_country]] <- report

cat("✓ Cleaning complete for", current_country, "\n")
cat("  Final paid data:", nrow(paid_country), "rows x", ncol(paid_country), "cols\n")
cat("  Final internal data:", nrow(internal_country), "rows x", ncol(internal_country), "cols\n")

# ============================================================================
# EXPORT CLEANING REPORT TO data_clean AND GOOGLE DRIVE
# ============================================================================

cat("\nExporting cleaning report...\n")

# Create data_clean folder structure
data_clean_folder <- file.path("data_clean", tolower(gsub(" ", "_", current_country)))
if (!dir.exists(data_clean_folder)) dir.create(data_clean_folder, recursive = TRUE)

# Define file paths
paid_csv_path <- file.path(data_clean_folder, "paid_data_cleaned.csv")
internal_csv_path <- file.path(data_clean_folder, "internal_data_cleaned.csv")
report_file <- file.path(data_clean_folder, "cleaning_report.txt")

# Export cleaned CSVs locally
write.csv(paid_country, paid_csv_path, row.names = FALSE)
write.csv(internal_country, internal_csv_path, row.names = FALSE)

cat("  ✓ Saved locally to:", data_clean_folder, "\n")

# Export cleaning report as text
sink(report_file)

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("DATA CLEANING REPORT\n")
cat("Country:", current_country, "\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("FINAL DATA DIMENSIONS:\n")
cat("  Paid data:", nrow(paid_country), "rows x", ncol(paid_country), "columns\n")
cat("  Internal data:", nrow(internal_country), "rows x", ncol(internal_country), "columns\n\n")

cat("COLUMNS DROPPED (>30% missing):\n")
cat("  Paid:", length(report$dropped_columns$paid), "columns\n")
if (length(report$dropped_columns$paid) > 0) {
  cat("    ", paste("-", report$dropped_columns$paid, collapse = "\n     "), "\n")
}
cat("  Internal:", length(report$dropped_columns$internal), "columns\n")
if (length(report$dropped_columns$internal) > 0) {
  cat("    ", paste("-", report$dropped_columns$internal, collapse = "\n     "), "\n")
}
cat("\n")

if (length(report$issues) > 0) {
  cat("ISSUES DETECTED:\n")
  if (!is.null(report$issues$duplicates_paid)) {
    cat("  Duplicates in paid data:", report$issues$duplicates_paid$rows_removed, "rows removed\n")
  }
  if (!is.null(report$issues$duplicates_internal)) {
    cat("  Duplicates in internal data:", report$issues$duplicates_internal$rows_removed, "rows removed\n")
  }
  if (!is.null(report$issues$consistency)) {
    cat("  Consistency issues:", length(report$issues$consistency), "types detected\n")
  }
}

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
sink()

# Upload to Google Drive
if (gdrive_enabled) {
  cat("  Uploading to Google Drive...\n")
  
  tryCatch({
    # Upload paid data
    drive_upload(
      media = paid_csv_path,
      path = as_id(target_folder_id),
      name = paste0(tolower(gsub(" ", "_", current_country)), "_paid_data_cleaned.csv"),
      overwrite = TRUE
    )
    cat("    ✓ Uploaded paid_data_cleaned.csv\n")
    
    # Upload internal data
    drive_upload(
      media = internal_csv_path,
      path = as_id(target_folder_id),
      name = paste0(tolower(gsub(" ", "_", current_country)), "_internal_data_cleaned.csv"),
      overwrite = TRUE
    )
    cat("    ✓ Uploaded internal_data_cleaned.csv\n")
    
    # Upload cleaning report
    drive_upload(
      media = report_file,
      path = as_id(target_folder_id),
      name = paste0(tolower(gsub(" ", "_", current_country)), "_cleaning_report.txt"),
      overwrite = TRUE
    )
    cat("    ✓ Uploaded cleaning_report.txt\n")
    
    cat("  ✓ All files uploaded to Google Drive\n")
    
  }, error = function(e) {
    cat("  ⚠️  Google Drive upload failed:", e$message, "\n")
    cat("  Files remain available locally in:", data_clean_folder, "\n")
  })
}

cat("\n")

# ============================================================================
# 3.9 - DATA TRANSFORMATION & EXPLORATION FOR MMM
# ============================================================================

cat("3.9. Data Transformation & Exploration for MMM...\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Create timestamp for this run
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_date <- format(Sys.time(), "%Y-%m-%d")

# Load gridExtra if needed
if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra", repos = "https://cloud.r-project.org")
  library(gridExtra)
}

# Create country-specific folders INSIDE data_exploration
country_code <- tolower(gsub(" ", "_", current_country))
country_folder <- file.path("data_exploration", country_code)
country_plots_folder <- file.path("data_exploration", country_code, "plots")

if (!dir.exists(country_folder)) dir.create(country_folder, recursive = TRUE)
if (!dir.exists(country_plots_folder)) dir.create(country_plots_folder, recursive = TRUE)

cat("Processing", current_country, "...\n")
cat("  Timestamp:", run_timestamp, "\n")
cat("  Output folder:", country_folder, "\n\n")

# ============================================================================
# STEP 1: TRANSFORM PAID MEDIA DATA
# ============================================================================

cat("Step 1: Transforming paid media data...\n")

paid_data_grouped <- paid_country %>%
  mutate(
    # Clean platform names
    platform = case_when(
      platform == "npr.org" ~ "nprorg",
      TRUE ~ platform
    ),
    
    # Combined column: strategy_channel_platform
    combined = str_replace_all(
      paste(strategy, channel, platform, sep = "_"),
      " ",
      ""
    )
  )

# Show results
unique_channels <- length(unique(paid_data_grouped$combined))
cat("  Created", unique_channels, "unique channel combinations\n")

# Aggregate to weekly level
paid_data_weekly <- paid_data_grouped %>%
  group_by(week_commencing, country, combined) %>%
  summarise(
    total_spend = sum(spend, na.rm = TRUE),
    total_impressions = sum(impressions, na.rm = TRUE),
    total_clicks = sum(clicks, na.rm = TRUE),
    total_installs = sum(installs, na.rm = TRUE),
    total_qr = sum(quality_reads, na.rm = TRUE),
    total_vv = sum(video_views, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Aggregated to weekly level:", nrow(paid_data_weekly), "rows\n")

# Pivot to wide format
paid_data_wide <- paid_data_weekly %>%
  pivot_wider(
    names_from = combined,
    values_from = c(total_spend, total_impressions, total_clicks, total_installs, total_qr,total_vv ),
    values_fill = 0
  )

cat("  Pivoted to wide format:", ncol(paid_data_wide), "columns\n")

# Remove zero-variance columns (only for clicks, installs, qr, vv)
cat("  Checking for zero-variance columns in clicks, installs, qr, and vv...\n")

# Identify columns to check (only these metrics)
cols_to_check <- grep("^(total_clicks_|total_installs_|total_qr_|total_vv_)", names(paid_data_wide), value = TRUE)

# Find zero-variance columns
zero_var_cols <- c()
for (col in cols_to_check) {
  if (is.numeric(paid_data_wide[[col]])) {
    if (sd(paid_data_wide[[col]], na.rm = TRUE) == 0 || 
        length(unique(paid_data_wide[[col]][!is.na(paid_data_wide[[col]])])) == 1) {
      zero_var_cols <- c(zero_var_cols, col)
    }
  }
}

if (length(zero_var_cols) > 0) {
  
  # Categorize by metric type
  zero_var_clicks <- grep("^total_clicks_", zero_var_cols, value = TRUE)
  zero_var_installs <- grep("^total_installs_", zero_var_cols, value = TRUE)
  zero_var_qr <- grep("^total_qr_", zero_var_cols, value = TRUE)
  zero_var_vv <- grep("^total_vv_", zero_var_cols, value = TRUE)
  
  cat("  ⚠️  Removing", length(zero_var_cols), "zero-variance columns:\n")
  
  if (length(zero_var_clicks) > 0) {
    cat("    - Clicks:", length(zero_var_clicks), "columns\n")
  }
  if (length(zero_var_installs) > 0) {
    cat("    - Installs:", length(zero_var_installs), "columns\n")
  }
  if (length(zero_var_qr) > 0) {
    cat("    - Quality Reads:", length(zero_var_qr), "columns\n")
    cat("      Examples:", paste(head(zero_var_qr, 3), collapse = ", "), "\n")
  }
  if (length(zero_var_vv) > 0) {
    cat("    - Video Views:", length(zero_var_vv), "columns\n")
    cat("      Examples:", paste(head(zero_var_vv, 3), collapse = ", "), "\n")
  }
  
  # Remove zero-variance columns
  paid_data_wide <- paid_data_wide %>%
    select(-all_of(zero_var_cols))
  
  cat("  ✓ Removed", length(zero_var_cols), "zero-variance columns\n")
  cat("  Final paid data columns:", ncol(paid_data_wide), "\n\n")
  
} else {
  cat("  ✓ No zero-variance columns found\n\n")
}


# Export channel list
channel_list <- data.frame(
  channel_id = 1:length(unique(paid_data_grouped$combined)),
  combined_channel = sort(unique(paid_data_grouped$combined))
)

write.csv(channel_list, 
          file.path(country_folder, paste0("channel_list_", country_code, "_", run_timestamp, ".csv")),
          row.names = FALSE)
cat("  ✓ Exported channel list\n\n")

# ============================================================================
# STEP 2: CREATE TOTAL ACQUISITIONS & JOIN DATASETS
# ============================================================================

cat("Step 2: Creating total acquisitions and joining datasets...\n")

# Join datasets
mmm_data <- internal_country %>%
  inner_join(paid_data_wide, by = c("week_commencing", "country"))

cat("  ✓ Joined paid and internal data\n")
cat("  Final dataset:", nrow(mmm_data), "rows x", ncol(mmm_data), "columns\n\n")

# Export final dataset locally
mmm_dataset_path <- file.path(country_folder, paste0("mmm_dataset_", country_code, "_", run_timestamp, ".csv"))
write.csv(mmm_data, mmm_dataset_path, row.names = FALSE)
cat("  ✓ Saved final MMM dataset locally\n")

# Upload MMM dataset to Google Drive
if (gdrive_enabled) {
  tryCatch({
    drive_upload(
      media = mmm_dataset_path,
      path = as_id(target_folder_id),
      name = paste0(country_code, "_mmm_dataset_", run_timestamp, ".csv"),
      overwrite = TRUE
    )
    cat("  ✓ Uploaded MMM dataset to Google Drive\n")
  }, error = function(e) {
    cat("  ⚠️  Google Drive upload failed:", e$message, "\n")
    cat("  File remains available locally at:", mmm_dataset_path, "\n")
  })
}
cat("\n")

# ============================================================================
# STEP 3: DEPENDENT VARIABLE ANALYSIS - LTV & ACQUISITIONS
# ============================================================================

cat("Step 3: Analyzing dependent variables (LTV & Acquisitions)...\n")

# Define variables used throughout the analysis
spend_cols <- grep("^total_spend_", names(mmm_data), value = TRUE)

# Define dependent variables for this analysis
if ("sum_ltv_acquisition" %in% names(mmm_data)) {
  ltv_acquisition_col <- "sum_ltv_acquisition"
  cat("  ✓ Using sum_ltv_acquisition as LTV metric\n")
  
} else if ("sum_ltv_acquisition_capped_12m" %in% names(mmm_data)) {
  ltv_acquisition_col <- "sum_ltv_acquisition_capped_12m"
  cat("  ⚠️  sum_ltv_acquisition not found - using sum_ltv_acquisition_capped_12m instead\n")
  
} else {
  ltv_acquisition_col <- NULL
  cat("  ⚠️  WARNING: No LTV acquisition column found in data!\n")
  cat("     Looked for: sum_ltv_acquisition, sum_ltv_acquisition_capped_12m\n")
  cat("     Some analyses will be skipped.\n")
}

available_dependent <- c("total_acquisitions", ltv_acquisition_col)
available_dependent <- available_dependent[!is.null(available_dependent) & available_dependent %in% names(mmm_data)]

# 3A. Individual trend plots for key variables
cat("  Creating individual trend plots...\n")

# Plot 1: total_acquisitions
if ("total_acquisitions" %in% names(mmm_data)) {
  
  p_total_acq <- ggplot(mmm_data, aes(x = week_commencing, y = total_acquisitions)) +
    geom_line(color = "#1D3557", linewidth = 1.2) +
    geom_point(color = "#1D3557", size = 2.5, alpha = 0.7) +
    geom_smooth(method = "loess", color = "#E63946", linetype = "dashed", se = TRUE, alpha = 0.2) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste("Total Acquisitions Trend -", current_country),
      subtitle = paste(run_date, "|", run_timestamp),
      x = "Week",
      y = "Total Acquisitions"
    )
  
  ggsave(file.path(country_plots_folder, paste0("total_acquisitions_trend_", country_code, "_", run_timestamp, ".png")),
         p_total_acq, width = 12, height = 6, dpi = 300)
  cat("    ✓ Saved total_acquisitions plot\n")
}

# Plot 2: sum_ltv_acquisition (or first available LTV)
if (!is.null(ltv_acquisition_col)) {
  
  p_ltv_acq <- ggplot(mmm_data, aes(x = week_commencing, y = .data[[ltv_acquisition_col]])) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 2.5, alpha = 0.7) +
    geom_smooth(method = "loess", color = "#F1A208", linetype = "dashed", se = TRUE, alpha = 0.2) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste("LTV Acquisition Trend -", current_country),
      subtitle = paste(ltv_acquisition_col, "|", run_date, "|", run_timestamp),
      x = "Week",
      y = "LTV ($)"
    )
  
  ggsave(file.path(country_plots_folder, paste0("ltv_acquisition_trend_", country_code, "_", run_timestamp, ".png")),
         p_ltv_acq, width = 12, height = 6, dpi = 300)
  cat("    ✓ Saved ltv_acquisition plot\n")
}

# Plot 3: Combined LTV plot (all LTV metrics together)
ltv_cols <- c("sum_ltv_acquisition_capped_12m", "sum_ltv_acquisition", "sum_ltv_total")
available_ltv <- ltv_cols[ltv_cols %in% names(mmm_data)]

if (length(available_ltv) > 0) {
  
  ltv_long <- mmm_data %>%
    select(week_commencing, all_of(available_ltv)) %>%
    pivot_longer(cols = all_of(available_ltv), names_to = "ltv_type", values_to = "value")
  
  p_ltv_combined <- ggplot(ltv_long, aes(x = week_commencing, y = value, color = ltv_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.6) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_color_brewer(palette = "Set1", labels = function(x) gsub("sum_ltv_", "", x)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste("All LTV Metrics Comparison -", current_country),
      subtitle = paste(run_date, "|", run_timestamp),
      x = "Week",
      y = "LTV ($)",
      color = "LTV Type"
    )
  
  ggsave(file.path(country_plots_folder, paste0("ltv_all_metrics_combined_", country_code, "_", run_timestamp, ".png")),
         p_ltv_combined, width = 12, height = 6, dpi = 300)
  cat("    ✓ Saved combined LTV plot\n")
}

# Plot 4: Acquisitions breakdown
acq_cols <- c("new_acquisition", "recovery", "trialist_conversion")
available_acq <- acq_cols[acq_cols %in% names(mmm_data)]

if (length(available_acq) > 0) {
  
  acq_long <- mmm_data %>%
    select(week_commencing, all_of(available_acq)) %>%
    pivot_longer(cols = all_of(available_acq), names_to = "acq_type", values_to = "count")
  
  p_acq <- ggplot(acq_long, aes(x = week_commencing, y = count, color = acq_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.6) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_color_manual(
      values = c("new_acquisition" = "#E63946", 
                 "recovery" = "#F1A208", 
                 "trialist_conversion" = "#06A77D")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste("Acquisitions Breakdown -", current_country),
      subtitle = paste(run_date, "|", run_timestamp),
      x = "Week",
      y = "Count",
      color = "Type"
    )
  
  ggsave(file.path(country_plots_folder, paste0("acquisitions_breakdown_", country_code, "_", run_timestamp, ".png")),
         p_acq, width = 12, height = 6, dpi = 300)
  cat("    ✓ Saved acquisitions breakdown plot\n")
}

# 3B. Outlier Detection for Acquisitions AND LTV Metrics
cat("  Detecting outliers in acquisitions and LTV metrics...\n")

# Get all LTV columns
ltv_cols_all <- grep("ltv", names(mmm_data), value = TRUE, ignore.case = TRUE)

# Combine acquisition and LTV columns
all_dep_cols <- c("total_acquisitions", available_acq, ltv_cols_all)
all_dep_cols <- unique(all_dep_cols[all_dep_cols %in% names(mmm_data)])

outlier_results <- data.frame()

for (col in all_dep_cols) {
  if (col %in% names(mmm_data) && is.numeric(mmm_data[[col]])) {
    
    mean_val <- mean(mmm_data[[col]], na.rm = TRUE)
    sd_val <- sd(mmm_data[[col]], na.rm = TRUE)
    
    # Only calculate if there's variance
    if (sd_val > 0) {
      z_scores <- abs((mmm_data[[col]] - mean_val) / sd_val)
      
      Q1 <- quantile(mmm_data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(mmm_data[[col]], 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR_val
      upper_bound <- Q3 + 1.5 * IQR_val
      
      outliers_z <- sum(z_scores > 3, na.rm = TRUE)
      outliers_iqr <- sum(mmm_data[[col]] < lower_bound | mmm_data[[col]] > upper_bound, na.rm = TRUE)
      
      outlier_results <- rbind(outlier_results, data.frame(
        variable = col,
        variable_type = ifelse(grepl("ltv", col, ignore.case = TRUE), "LTV", "Acquisition"),
        mean = round(mean_val, 2),
        sd = round(sd_val, 2),
        outliers_zscore = outliers_z,
        outliers_iqr = outliers_iqr,
        lower_bound_iqr = round(lower_bound, 2),
        upper_bound_iqr = round(upper_bound, 2)
      ))
    }
  }
}

write.csv(outlier_results, 
          file.path(country_folder, paste0("outlier_analysis_all_dependent_vars_", country_code, "_", run_timestamp, ".csv")),
          row.names = FALSE)

cat("    ✓ Saved outlier analysis for", nrow(outlier_results), "variables\n")
if (nrow(outlier_results) > 0) {
  acq_count <- sum(outlier_results$variable_type == "Acquisition", na.rm = TRUE)
  ltv_count <- sum(outlier_results$variable_type == "LTV", na.rm = TRUE)
  cat("      - Acquisition metrics:", acq_count, "\n")
  cat("      - LTV metrics:", ltv_count, "\n")
  cat("    Total outliers (Z-score > 3):", sum(outlier_results$outliers_zscore), "\n")
  cat("    Total outliers (IQR method):", sum(outlier_results$outliers_iqr), "\n")
}
cat("\n")

# ============================================================================
# STEP 4: CORRELATION ANALYSIS
# ============================================================================

cat("Step 4: Correlation analysis...\n")

# Get all numeric columns except date and country
all_numeric_cols <- names(mmm_data)[sapply(mmm_data, is.numeric)]
all_numeric_cols <- setdiff(all_numeric_cols, c("week_commencing", "country"))

# Filter out zero-variance columns
non_zero_var_cols <- all_numeric_cols[sapply(all_numeric_cols, function(col) {
  sd(mmm_data[[col]], na.rm = TRUE) > 0
})]

if (length(non_zero_var_cols) == 0) {
  cat("  ⚠️  No variables with variance found for correlation analysis\n\n")
} else {
  
  # Identify LTV columns
  ltv_columns <- grep("ltv", non_zero_var_cols, value = TRUE, ignore.case = TRUE)
  
  # Identify acquisition component columns
  acq_component_cols <- c("new_acquisition", "recovery", "trialist_conversion")
  
  # Find the main ltv_acquisition column
  if (is.null(ltv_acquisition_col)) {
    if ("sum_ltv_acquisition" %in% names(mmm_data)) {
      ltv_acquisition_col <- "sum_ltv_acquisition"
    } else if ("sum_ltv_acquisition_capped_12m" %in% names(mmm_data)) {
      ltv_acquisition_col <- "sum_ltv_acquisition_capped_12m"
    }
  }
  
  if ("total_acquisitions" %in% names(mmm_data) && !is.null(ltv_acquisition_col)) {
    
    # Variables to correlate with total_acquisitions 
    # EXCLUDE: total_acquisitions, all LTVs, new_acquisition, trialist_conversion
    vars_for_total_acq <- setdiff(non_zero_var_cols, 
                                  c("total_acquisitions", ltv_columns, 
                                    "new_acquisition", "trialist_conversion"))
    
    # Variables to correlate with ltv_acquisition 
    # EXCLUDE: ltv_acquisition, other LTVs, total_acquisitions, all acquisition components
    vars_for_ltv <- setdiff(non_zero_var_cols, 
                            c(ltv_acquisition_col, "total_acquisitions", 
                              acq_component_cols, ltv_columns))
    
    # Calculate correlations
    correlation_results <- data.frame(variable = character(), stringsAsFactors = FALSE)
    
    # Get all unique variables
    all_vars_to_test <- unique(c(vars_for_total_acq, vars_for_ltv))
    
    for (var in all_vars_to_test) {
      
      row_data <- data.frame(variable = var, stringsAsFactors = FALSE)
      
      # Correlation with total_acquisitions
      if (var %in% vars_for_total_acq) {
        if (sd(mmm_data[[var]], na.rm = TRUE) > 0) {
          cor_total_acq <- cor(mmm_data[[var]], mmm_data$total_acquisitions, 
                               use = "complete.obs", method = "pearson")
          row_data$correlation_with_total_acquisitions <- round(cor_total_acq, 3)
        } else {
          row_data$correlation_with_total_acquisitions <- NA
        }
      } else {
        row_data$correlation_with_total_acquisitions <- NA
      }
      
      # Correlation with ltv_acquisition
      if (var %in% vars_for_ltv) {
        if (sd(mmm_data[[var]], na.rm = TRUE) > 0) {
          cor_ltv <- cor(mmm_data[[var]], mmm_data[[ltv_acquisition_col]], 
                         use = "complete.obs", method = "pearson")
          row_data$correlation_with_ltv_acquisition <- round(cor_ltv, 3)
        } else {
          row_data$correlation_with_ltv_acquisition <- NA
        }
      } else {
        row_data$correlation_with_ltv_acquisition <- NA
      }
      
      correlation_results <- rbind(correlation_results, row_data)
    }
    
    # Add absolute values for sorting
    correlation_results <- correlation_results %>%
      mutate(
        abs_cor_total_acq = abs(correlation_with_total_acquisitions),
        abs_cor_ltv = abs(correlation_with_ltv_acquisition)
      ) %>%
      arrange(desc(abs_cor_total_acq))
    
    # Export
    write.csv(correlation_results, 
              file.path(country_folder, paste0("correlations_", country_code, "_", run_timestamp, ".csv")),
              row.names = FALSE)
    
    cat("  ✓ Correlation analysis complete\n")
    cat("    Variables tested against total_acquisitions:", sum(!is.na(correlation_results$correlation_with_total_acquisitions)), "\n")
    cat("    Variables tested against ltv_acquisition:", sum(!is.na(correlation_results$correlation_with_ltv_acquisition)), "\n")
    
    # Create correlation plots
    
    # Plot 1: Top 20 correlations with total_acquisitions
    cor_total_acq_subset <- correlation_results %>%
      filter(!is.na(correlation_with_total_acquisitions)) %>%
      arrange(desc(abs_cor_total_acq)) %>%
      head(20)
    
    if (nrow(cor_total_acq_subset) > 0) {
      p_cor_total <- ggplot(cor_total_acq_subset, 
                            aes(x = reorder(variable, correlation_with_total_acquisitions), 
                                y = correlation_with_total_acquisitions)) +
        geom_col(aes(fill = correlation_with_total_acquisitions), show.legend = FALSE) +
        geom_text(aes(label = round(correlation_with_total_acquisitions, 2)), 
                  hjust = ifelse(cor_total_acq_subset$correlation_with_total_acquisitions > 0, -0.1, 1.1), 
                  size = 3) +
        scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", 
                             midpoint = 0, limits = c(-1, 1)) +
        coord_flip() +
        ylim(-1, 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 8)
        ) +
        labs(
          title = paste("Top 20 Correlations with Total Acquisitions -", current_country),
          subtitle = paste(run_date, "|", run_timestamp),
          x = "Variable",
          y = "Pearson Correlation"
        )
      
      ggsave(file.path(country_plots_folder, 
                       paste0("correlation_total_acquisitions_", country_code, "_", run_timestamp, ".png")),
             p_cor_total, width = 10, height = 12, dpi = 300)
    }
    
    # Plot 2: Top 20 correlations with ltv_acquisition
    cor_ltv_subset <- correlation_results %>%
      filter(!is.na(correlation_with_ltv_acquisition)) %>%
      arrange(desc(abs_cor_ltv)) %>%
      head(20)
    
    if (nrow(cor_ltv_subset) > 0) {
      p_cor_ltv <- ggplot(cor_ltv_subset, 
                          aes(x = reorder(variable, correlation_with_ltv_acquisition), 
                              y = correlation_with_ltv_acquisition)) +
        geom_col(aes(fill = correlation_with_ltv_acquisition), show.legend = FALSE) +
        geom_text(aes(label = round(correlation_with_ltv_acquisition, 2)), 
                  hjust = ifelse(cor_ltv_subset$correlation_with_ltv_acquisition > 0, -0.1, 1.1), 
                  size = 3) +
        scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", 
                             midpoint = 0, limits = c(-1, 1)) +
        coord_flip() +
        ylim(-1, 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 8)
        ) +
        labs(
          title = paste("Top 20 Correlations with LTV Acquisition -", current_country),
          subtitle = paste(run_date, "|", run_timestamp),
          x = "Variable",
          y = "Pearson Correlation"
        )
      
      ggsave(file.path(country_plots_folder, 
                       paste0("correlation_ltv_acquisition_", country_code, "_", run_timestamp, ".png")),
             p_cor_ltv, width = 10, height = 12, dpi = 300)
    }
    
    cat("  ✓ Saved correlation plots\n\n")
    
  } else {
    cat("  ⚠️  Skipping correlation analysis - missing required variables\n\n")
  }
}

# ============================================================================
# STEP 5: DISTRIBUTION ANALYSIS
# ============================================================================

cat("Step 5: Distribution analysis...\n")

# Load gridExtra if needed
if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra", repos = "https://cloud.r-project.org")
  library(gridExtra)
}

# ============================================================================
# 5A. KEY VARIABLES DISTRIBUTIONS
# ============================================================================
# Variables with "traffic" but NOT wow/mom/yoy comparisons
# Plus variables starting with: share, app, prop

cat("  5A. Key variables distributions...\n")

# Get all numeric columns
all_numeric <- names(mmm_data)[sapply(mmm_data, is.numeric)]
all_numeric <- setdiff(all_numeric, c("week_commencing", "country"))

# Define variables to EXCLUDE from key vars
ltv_cols_to_exclude <- grep("ltv", all_numeric, value = TRUE, ignore.case = TRUE)
vars_to_exclude <- c(ltv_cols_to_exclude, "total_acquisitions", "new_acquisition", "trialist_conversion")

# Also exclude spend, impressions, clicks (they get their own plots)
spend_impressions_clicks <- grep("^(total_spend_|total_impressions_|total_clicks_)", all_numeric, value = TRUE)
vars_to_exclude <- c(vars_to_exclude, spend_impressions_clicks)

# Get candidate key variables
candidate_vars <- setdiff(all_numeric, vars_to_exclude)

# Filter for key variables:
# 1. Has "traffic" in name AND does NOT have wow/mom/yoy
traffic_vars <- grep("traffic", candidate_vars, value = TRUE, ignore.case = TRUE)
traffic_vars <- traffic_vars[!grepl("wow|mom|yoy", traffic_vars, ignore.case = TRUE)]

# 2. Starts with "share", "app", or "prop"
share_vars <- grep("^share", candidate_vars, value = TRUE, ignore.case = TRUE)
app_vars <- grep("^app", candidate_vars, value = TRUE, ignore.case = TRUE)
prop_vars <- grep("^prop", candidate_vars, value = TRUE, ignore.case = TRUE)

# Combine all key variables
key_vars <- unique(c(traffic_vars, share_vars, app_vars, prop_vars))

if (length(key_vars) > 0) {
  
  cat("    Creating distribution plots for", length(key_vars), "key variables\n")
  cat("      Traffic vars (excl. wow/mom/yoy):", length(traffic_vars), "\n")
  cat("      Share vars:", length(share_vars), "\n")
  cat("      App vars:", length(app_vars), "\n")
  cat("      Prop vars:", length(prop_vars), "\n")
  
  key_plots <- list()
  
  for (i in seq_along(key_vars)) {
    var <- key_vars[i]
    
    p <- ggplot(mmm_data, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2E86AB", alpha = 0.6) +
      geom_density(color = "#E63946", linewidth = 1) +
      scale_x_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 7)
      ) +
      labs(
        title = substr(var, 1, 50),
        x = "Value",
        y = "Density"
      )
    
    key_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 3
  nrow_grid <- ceiling(length(key_plots) / ncol_grid)
  
  # Combine plots
  combined_key <- gridExtra::grid.arrange(
    grobs = key_plots, 
    ncol = ncol_grid,
    top = paste("Key Variable Distributions -", current_country, "|", run_timestamp)
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("distributions_key_vars_", country_code, "_", run_timestamp, ".png")),
    combined_key, 
    width = 15, 
    height = 4 * nrow_grid, 
    dpi = 300
  )
  
  cat("    ✓ Saved key variables distributions (", length(key_vars), " variables)\n", sep = "")
} else {
  cat("    ⚠️  No key variables found for distribution plots\n")
}

# ============================================================================
# 5B. MEDIA SPEND DISTRIBUTIONS (ALL)
# ============================================================================

cat("  5B. Media spend distributions...\n")

spend_cols <- grep("^total_spend_", names(mmm_data), value = TRUE)

if (length(spend_cols) > 0) {
  
  cat("    Creating distribution plots for", length(spend_cols), "spend channels\n")
  
  spend_plots <- list()
  
  for (i in seq_along(spend_cols)) {
    var <- spend_cols[i]
    channel_name <- gsub("^total_spend_", "", var)
    
    p <- ggplot(mmm_data, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "#06A77D", alpha = 0.6) +
      scale_x_continuous(labels = scales::dollar_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 6)
      ) +
      labs(
        title = substr(channel_name, 1, 45),
        x = "Spend",
        y = "Frequency"
      )
    
    spend_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 4
  nrow_grid <- ceiling(length(spend_plots) / ncol_grid)
  
  # Combine plots
  combined_spend <- gridExtra::grid.arrange(
    grobs = spend_plots, 
    ncol = ncol_grid,
    top = paste("All Media Spend Distributions -", current_country, "|", run_timestamp)
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("distributions_media_spend_", country_code, "_", run_timestamp, ".png")),
    combined_spend, 
    width = 20, 
    height = 4 * nrow_grid, 
    dpi = 300
  )
  
  cat("    ✓ Saved media spend distributions (", length(spend_cols), " channels)\n", sep = "")
} else {
  cat("    ⚠️  No spend columns found\n")
}

# ============================================================================
# 5C. IMPRESSIONS DISTRIBUTIONS (ALL)
# ============================================================================

cat("  5C. Impressions distributions...\n")

impressions_cols <- grep("^total_impressions_", names(mmm_data), value = TRUE)

if (length(impressions_cols) > 0) {
  
  cat("    Creating distribution plots for", length(impressions_cols), "impressions channels\n")
  
  impressions_plots <- list()
  
  for (i in seq_along(impressions_cols)) {
    var <- impressions_cols[i]
    channel_name <- gsub("^total_impressions_", "", var)
    
    p <- ggplot(mmm_data, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "#457B9D", alpha = 0.6) +
      scale_x_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 6)
      ) +
      labs(
        title = substr(channel_name, 1, 45),
        x = "Impressions",
        y = "Frequency"
      )
    
    impressions_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 4
  nrow_grid <- ceiling(length(impressions_plots) / ncol_grid)
  
  # Combine plots
  combined_impressions <- gridExtra::grid.arrange(
    grobs = impressions_plots, 
    ncol = ncol_grid,
    top = paste("All Impressions Distributions -", current_country, "|", run_timestamp)
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("distributions_impressions_", country_code, "_", run_timestamp, ".png")),
    combined_impressions, 
    width = 20, 
    height = 4 * nrow_grid, 
    dpi = 300
  )
  
  cat("    ✓ Saved impressions distributions (", length(impressions_cols), " channels)\n", sep = "")
} else {
  cat("    ⚠️  No impressions columns found\n")
}

# ============================================================================
# 5D. CLICKS DISTRIBUTIONS (ALL)
# ============================================================================

cat("  5D. Clicks distributions...\n")

clicks_cols <- grep("^total_clicks_", names(mmm_data), value = TRUE)

if (length(clicks_cols) > 0) {
  
  cat("    Creating distribution plots for", length(clicks_cols), "clicks channels\n")
  
  clicks_plots <- list()
  
  for (i in seq_along(clicks_cols)) {
    var <- clicks_cols[i]
    channel_name <- gsub("^total_clicks_", "", var)
    
    p <- ggplot(mmm_data, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "#E63946", alpha = 0.6) +
      scale_x_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 6)
      ) +
      labs(
        title = substr(channel_name, 1, 45),
        x = "Clicks",
        y = "Frequency"
      )
    
    clicks_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 4
  nrow_grid <- ceiling(length(clicks_plots) / ncol_grid)
  
  # Combine plots
  combined_clicks <- gridExtra::grid.arrange(
    grobs = clicks_plots, 
    ncol = ncol_grid,
    top = paste("All Clicks Distributions -", current_country, "|", run_timestamp)
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("distributions_clicks_", country_code, "_", run_timestamp, ".png")),
    combined_clicks, 
    width = 20, 
    height = 4 * nrow_grid, 
    dpi = 300
  )
  
  cat("    ✓ Saved clicks distributions (", length(clicks_cols), " channels)\n", sep = "")
} else {
  cat("    ⚠️  No clicks columns found\n")
}

cat("\n")

# ============================================================================
# STEP 6: TIME SERIES LINE PLOTS
# ============================================================================

cat("Step 6: Creating time series line plots...\n")

# Get all numeric columns
all_numeric <- names(mmm_data)[sapply(mmm_data, is.numeric)]
all_numeric <- setdiff(all_numeric, c("week_commencing", "country"))

# ============================================================================
# 6A. LINE PLOTS FOR NON-EXCLUDED NUMERICAL VARIABLES
# ============================================================================

cat("  6A. Creating line plots for numerical variables...\n")

# Define variables to EXCLUDE
vars_to_exclude <- c(
  # Dependent variables
  "total_acquisitions",
  "new_acquisition",
  "recovery",
  "trialist_conversion",
  # LTV variables
  "sum_ltv_acquisition",
  "sum_ltv_acquisition_capped_12m",
  "sum_ltv_total"
)

# Identify and exclude brand awareness metrics
brand_awareness_cols <- grep("^(tma_|unaided_awareness_|aided_awareness_)", all_numeric, value = TRUE)

# Identify and exclude media metrics (keeping only spend)
impressions_vars <- grep("^total_impressions_", all_numeric, value = TRUE)
spend_vars <- grep("^total_spend_", all_numeric, value = TRUE)
clicks_vars <- grep("^total_clicks_", all_numeric, value = TRUE)
installs_vars <- grep("^total_installs_", all_numeric, value = TRUE)
qr_vars <- grep("^total_qr_", all_numeric, value = TRUE)
vv_vars <- grep("^total_vv_", all_numeric, value = TRUE)

# Combine all exclusions
vars_to_exclude <- c(
  vars_to_exclude, 
  brand_awareness_cols,
  impressions_vars,
  spend_vars,
  clicks_vars,
  installs_vars,
  qr_vars,
  vv_vars
)

# Get variables for line plots
line_plot_vars <- setdiff(all_numeric, vars_to_exclude)

if (length(line_plot_vars) > 0) {
  
  cat("    Variables for line plots:", length(line_plot_vars), "\n")
  cat("    Excluded:\n")
  ltv_acq_base <- c(
    "total_acquisitions", "new_acquisition", "recovery",
    "trialist_conversion", "sum_ltv_acquisition",
    "sum_ltv_acquisition_capped_12m", "sum_ltv_total"
  )
  cat("      - LTV/Acquisition variables:",
      length(vars_to_exclude[vars_to_exclude %in% ltv_acq_base]), "\n")
  cat("      - Brand awareness metrics:", length(brand_awareness_cols), "\n")
  cat("      - Media impressions:", length(impressions_vars), "\n")
  cat("      - Media spend:", length(spend_vars), "\n")
  cat("      - Media clicks:", length(clicks_vars), "\n")
  cat("      - Media installs:", length(installs_vars), "\n")
  cat("      - Media quality reads:", length(qr_vars), "\n")
  cat("      - Media video views:", length(vv_vars), "\n\n")

  line_plots <- list()

  for (i in seq_along(line_plot_vars)) {
    var <- line_plot_vars[i]

    use_dollar <- grepl(
      "(spend|revenue|cost|price|ltv)", var, ignore.case = TRUE
    )
    
    p <- ggplot(mmm_data, aes(x = week_commencing, y = .data[[var]])) +
      geom_line(color = "#1D3557", linewidth = 0.8) +
      geom_point(color = "#1D3557", size = 1.5, alpha = 0.6) +
      {if (use_dollar) 
        scale_y_continuous(labels = scales::dollar_format())
        else 
          scale_y_continuous(labels = scales::comma_format())
      } +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 7)
      ) +
      labs(
        title = substr(var, 1, 50),
        x = "Week",
        y = "Value"
      )
    
    line_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 4
  nrow_grid <- ceiling(length(line_plots) / ncol_grid)
  
  # Calculate height (smaller rows, max 100 inches)
  plot_height <- min(3 * nrow_grid, 100)
  
  cat("    Creating combined plot (", nrow_grid, " rows x ", ncol_grid, " cols)\n", sep = "")
  cat("    Plot dimensions: 20 x ", plot_height, " inches\n", sep = "")
  
  # Combine plots
  combined_lines <- gridExtra::grid.arrange(
    grobs = line_plots, 
    ncol = ncol_grid,
    top = grid::textGrob(
      paste("Time Series - All Variables -", current_country, "|", run_timestamp),
      gp = grid::gpar(fontsize = 12, fontface = "bold")
    )
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("timeseries_all_numerical_vars_", country_code, "_", run_timestamp, ".png")),
    combined_lines, 
    width = 20, 
    height = plot_height, 
    dpi = 300,
    limitsize = FALSE
  )
  
  cat("    ✓ Saved line plots for", length(line_plot_vars), "variables\n")
  cat("    ✓ File: timeseries_all_numerical_vars_", country_code, "_", run_timestamp, ".png\n\n", sep = "")
  
} else {
  cat("    ⚠️  No variables found for line plots\n\n")
}

# ============================================================================
# 6B. LINE PLOTS FOR BRAND HEALTH METRICS ONLY
# ============================================================================
cat("  6B. Creating line plots for brand health metrics...\n")
# Get brand health metrics
brand_health_cols <- grep("^(tma_|unaided_awareness_|aided_awareness_)", all_numeric, value = TRUE)
if (length(brand_health_cols) > 0) {
  
  cat("    Found", length(brand_health_cols), "brand health metrics\n")
  cat("      TMA metrics:", length(grep("^tma_", brand_health_cols, value = TRUE)), "\n")
  cat("      Unaided awareness:", length(grep("^unaided_awareness_", brand_health_cols, value = TRUE)), "\n")
  cat("      Aided awareness:", length(grep("^aided_awareness_", brand_health_cols, value = TRUE)), "\n\n")
  
  brand_plots <- list()
  
  for (i in seq_along(brand_health_cols)) {
    var <- brand_health_cols[i]
    
    p <- ggplot(mmm_data, aes(x = week_commencing, y = .data[[var]])) +
      geom_line(color = "#2E86AB", linewidth = 0.8) +
      geom_point(color = "#2E86AB", size = 1.5, alpha = 0.6) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 7)
      ) +
      labs(
        title = var,  # ← CHANGED: Now uses full variable name
        x = "Week",
        y = "Value"
      )
    
    brand_plots[[i]] <- p
  }
  
  # Calculate grid dimensions
  ncol_grid <- 4
  nrow_grid <- ceiling(length(brand_plots) / ncol_grid)
  
  # Calculate height (smaller rows, max 100 inches)
  plot_height <- min(3 * nrow_grid, 100)
  
  cat("    Creating combined plot (", nrow_grid, " rows x ", ncol_grid, " cols)\n", sep = "")
  cat("    Plot dimensions: 20 x ", plot_height, " inches\n", sep = "")
  
  # Combine plots
  combined_brand <- gridExtra::grid.arrange(
    grobs = brand_plots, 
    ncol = ncol_grid,
    top = grid::textGrob(
      paste("Time Series - Brand Health Metrics -", current_country, "|", run_timestamp),
      gp = grid::gpar(fontsize = 12, fontface = "bold")
    )
  )
  
  ggsave(
    file.path(country_plots_folder, paste0("timeseries_brand_health_", country_code, "_", run_timestamp, ".png")),
    combined_brand, 
    width = 20, 
    height = plot_height, 
    dpi = 300,
    limitsize = FALSE
  )
  
  cat("    ✓ Saved brand health line plots for", length(brand_health_cols), "metrics\n")
  cat("    ✓ File: timeseries_brand_health_", country_code, "_", run_timestamp, ".png\n\n", sep = "")
  
} else {
  cat("    ⚠️  No brand health metrics found\n\n")
}
cat("✓ Time series visualization complete\n\n")


} # End country loop





