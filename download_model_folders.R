# ============================================================================
# DOWNLOAD MODEL FOLDERS FROM SHAREPOINT
# Downloads the 4 dashboard model folders to their correct local paths
# ============================================================================

# Install Microsoft365R if needed
if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
  install.packages("Microsoft365R")
}
library(Microsoft365R)

script_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("--file=", "", file_arg)))
    } else {
      dirname(normalizePath(sys.frames()[[1]]$ofile))
    }
  }
)
setwd(script_dir)

# ── SharePoint config ─────────────────────────────────────────────────────────
SITE_URL    <- "https://indigitalmarketing.sharepoint.com/sites/team"
BASE_PATH   <- paste0(
  "Drive/Clients/FT/Marketing Effectiveness/05. MMM/",
  "Andrea's Handover/MMM refresh & remodel (april 2026)"
)

# ── Folders to download: SharePoint path → local destination ─────────────────
FOLDERS <- list(
  list(
    remote = paste0(BASE_PATH,
      "/model_refresh/uk_acq/Robyn_202602162334_init/Robyn_202605141723_rf1"),
    local  = "model_refresh/uk_acq/Robyn_202602162334_init/Robyn_202605141723_rf1"
  ),
  list(
    remote = paste0(BASE_PATH,
      "/model_results/uk_ltv/Robyn_202605131355_init"),
    local  = "model_results/uk_ltv/Robyn_202605131355_init"
  ),
  list(
    remote = paste0(BASE_PATH,
      "/model_refresh/us_acq/Robyn_202602181221_init/Robyn_202605130051_rf1"),
    local  = "model_refresh/us_acq/Robyn_202602181221_init/Robyn_202605130051_rf1"
  ),
  list(
    remote = paste0(BASE_PATH,
      "/model_results/us_ltv/Robyn_202605191525_init"),
    local  = "model_results/us_ltv/Robyn_202605191525_init"
  )
)

# ── Recursively download a SharePoint folder ──────────────────────────────────
download_sp_folder <- function(sp_folder, local_path) {
  dir.create(local_path, recursive = TRUE, showWarnings = FALSE)
  items <- sp_folder$list_items()

  for (i in seq_len(nrow(items))) {
    item_name <- items$name[i]
    is_folder <- items$isdir[i]
    dest      <- file.path(local_path, item_name)

    if (is_folder) {
      sub_folder <- sp_folder$get_item(item_name)
      download_sp_folder(sub_folder, dest)
    } else {
      if (file.exists(dest)) {
        cat("  [skip]", file.path(local_path, item_name), "\n")
      } else {
        tryCatch({
          sp_folder$get_item(item_name)$download(dest = local_path,
                                                  overwrite = TRUE)
          cat("  [ok]  ", dest, "\n")
        }, error = function(e) {
          cat("  [err] ", dest, "--", e$message, "\n")
        })
      }
    }
  }
}

# ── Authenticate and run ──────────────────────────────────────────────────────
cat("Connecting to SharePoint (browser auth will open)...\n")
site <- get_sharepoint_site(SITE_URL)

for (f in FOLDERS) {
  cat("\n────────────────────────────────────────\n")
  cat("Downloading:", basename(f$remote), "\n")
  cat("        To:", f$local, "\n")

  tryCatch({
    sp_folder <- site$get_drive()$get_item(f$remote)
    download_sp_folder(sp_folder, f$local)
    cat("  Done.\n")
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
  })
}

cat("\n\nAll done.\n")
