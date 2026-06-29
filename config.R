################################################################################
# PROJECT CONFIG
# Edit this file when moving to a new machine or environment.
# Source it at the top of any script: source("config.R")
################################################################################


# ── ENVIRONMENT (change per machine) ──────────────────────────────────────────

#  env <- list(
#    python_path = "C:/Users/andre/anaconda3/python.exe",
#    working_dir = "C:/Users/andre/Marketing-Mix-Modelling/MMM V4",
#    user_email  = "andrea.rodriguez.idm@ft.com"
#  )

env <- list(
 python_path = "/Library/Frameworks/Python.framework/Versions/3.14/bin/python3",
 working_dir = "/Users/shazahmed/Marketing-Mix-Modelling/MMM V3",
 user_email  = "shaz.ahmed.indigital@ft.com"
)


# ── BIGQUERY ──────────────────────────────────────────────────────────────────

bq_config <- list(
  project_id        = "ft-customer-analytics",
  dataset           = "crg_arv",
  location          = "EU",
  paid_media_table  = "crg_arv.mmm_paid_media_master",
  internal_table    = "crg_arv.mmm_data_collection_refresh_Q12026_static"
)


# ── GOOGLE DRIVE FOLDER IDs ───────────────────────────────────────────────────

drive_folders <- list(
  data_clean          = "1GVf3-Us4RO6hIEtH4-ywti5oZ81oBL_k",
  model_data          = "17OpVknO-s0YZXim-2sPRklsp5DvUvHvt",
  model_selection     = "1SvaeBPhdco8MheHtzoZlSWybCeDJqX3v",
  external_collection = "external_collection.R"
)


# ── GOOGLE SHEETS ─────────────────────────────────────────────────────────────

sheets <- list(
  macro_indicators      = "1jqE7sFP5BDh9wZauhxZabN8HNCYd5-bHPxKlXI_BFSE",
  dashboard_export      = "1reh1Ob0w7fQ9fCyHnzz5cfJh4C0LmEKg2R4oT-941tg",
  budget_allocation     = "19p1XMdzMLscDD5jXkei5FiyB9tZ13u1vxQ3zaOijU_4",
  channel_performance   = "1ubeJtN-szxXEAxlCmQ1rnzPxJi_WmYlVPCuoMSgiQT8",
  marginal_efficiency   = "1Iihp7_XstRAIhGPrjzgAQN2F7uF0efNNDor6GNf6IeU"
)


# ── PROJECT SETTINGS ──────────────────────────────────────────────────────────

project <- list(
  countries       = c("united_states", "united_kingdom"),
  dep_vars        = c("total_acquisitions", "sum_ltv_acquisition"),
  date_var        = "week_commencing",
  start_date      = as.Date("2023-01-01"),

  file_map = list(
    united_states = "united_states_cleaned_dataset.csv",
    united_kingdom = "united_kingdom_cleaned_dataset.csv"
  ),

  output_dirs = list(
    variable_selection = "variable_selection_results",
    model_data         = "model_data",
    model_results      = "model_results",
    correlation        = "correlation_analysis",
    economic_plots     = "data_economic_factors"
  )
)


# ── VARIABLE SELECTION PARAMS ─────────────────────────────────────────────────

var_selection <- list(
  vif_threshold       = 20,
  elastic_net_alpha   = 0.85,
  enet_seed           = 123,
  enet_nfolds         = 10,

  tier1 = list(corr_threshold = 0.75, max_per_cluster = 1, max_per_category = 4),
  tier2 = list(corr_threshold = 0.60, max_per_cluster = 1, max_per_category = 6),

  mandatory_categories = c("economics", "discount", "news_agenda", "app", "organic", "sale"),

  corr_pair_threshold    = 0.70,
  perfect_corr_threshold = 0.999,

  ltv_exclude  = c("sum_ltv_acquisition_capped_12m", "sum_ltv_total"),
  acq_exclude  = c("new_acquisition", "recovery", "trialist_conversion"),

  diff_vars_remove = c(
    "direct_traffic_wow_diff", "internal_traffic_wow_diff",
    "organic_direct_internal_wow_diff", "organic_social_search_push_wow_diff",
    "direct_traffic_mom_diff", "internal_traffic_mom_diff",
    "organic_direct_internal_mom_diff", "organic_social_search_push_mom_diff",
    "direct_traffic_yoy_diff", "internal_traffic_yoy_diff",
    "organic_direct_internal_yoy_diff", "organic_social_search_push_yoy_diff"
  )
)


# ── HYPERPARAMETER PROFILES ───────────────────────────────────────────────────

hyperparams <- list(
  alpha = list(
      fast   = c(1.0, 3.0), 
      medium = c(0.5, 2.0), 
      slow = c(0.5, 1.5)),

  gamma = list(
    fast   = c(0.30, 0.70), 
    medium = c(0.40, 0.80), 
    slow = c(0.60, 1.00)),

  theta = list(
    high   = c(0.3, 0.75), 
    medium = c(0.1, 0.6),  
    digital = c(0.0, 0.4))
)


# ── MODEL SELECTION ───────────────────────────────────────────────────────────

model_selection <- list(

  weights = list(
    r_squared   = 0.15,  # % of variance explained by the model (higher = better fit)
    decomp_rssd = 0.20,  # alignment between spend share and effect share (lower rssd = better)
    channels    = 0.20,  # % of paid media channels with a positive contribution (penalises zero-effect channels)
    business    = 0.20,  # % of channels with CPA / ROI within the defined business target range
    media       = 0.25   # total paid media contribution share — how much of dep var is driven by paid media
  ),

  uk_acq = list(
    folders        = "model_refresh/uk_acq/Robyn_202602162334_init/Robyn_202605141723_rf1",
    top_n          = 20,
    min_r2_train   = 0.68,
    max_decomp_rssd = 0.55, #allowing more flexibility 
    cpa_range      = c(20, 600),
    solutions      = c("1_89_8", "2_76_2", "2_54_8", "3_85_6", "5_96_9", "2_94_2", "2_96_3"),
    pin_solutions  = TRUE, # include solutions on top of top_n in flat tab
    export         = TRUE
  ),

  uk_ltv = list(
    folders        = c("model_results/uk_ltv/Robyn_202605122235_init",
                       "model_results/uk_ltv/Robyn_202605131355_init"),
    top_n          = 15,
    min_r2_train   = 0.75,
    max_decomp_rssd = 0.34,
    roi_range      = c(0, 5),
    solutions      = c("5_82_2", "2_95_6", "2_95_19", "2_63_4"),
    pin_solutions  = FALSE, # include solutions on top of top_n in flat tab
    export         = TRUE
  ),

  us_acq = list(
    folders = c("model_refresh/us_acq/Robyn_202602181221_init/Robyn_202605130051_rf1"
    ),
    top_n          = 15,
    min_r2_train   = 0.75,
    max_decomp_rssd = 0.34,
    cpa_range      = c(50, 600),
    solutions      = c("3_65_19", "3_43_6", "5_91_14", "3_93_11", "1_51_15", "2_38_19"),
    pin_solutions  = TRUE, # include solutions on top of top_n in flat tab
    export         = TRUE
  ),

  us_ltv = list(
    folders        = c("model_results/us_ltv/Robyn_202605191525_init"),
    top_n          = 10,
    min_r2_train   = 0.75,
    max_decomp_rssd = 0.45,
    roi_range      = c(0, 5),
    solutions      = c("5_74_2", "5_74_4", "5_75_13"),
    pin_solutions  = FALSE, # include solutions on top of top_n in flat tab
    export         = TRUE
  )
)


# ── DASHBOARD ─────────────────────────────────────────────────────────────────
# sheet_id     : Google Sheet to write to
# run_folder   : relative path to the chosen Robyn run folder (from working_dir)
# selected_sol : pareto solution ID chosen for the dashboard
# metric_type  : "acq" (CPA) or "ltv" (ROAS)


dashboard <- list(

  uk_ltv = list(
    run_folder   = "model_results/uk_ltv/Robyn_202605131355_init",
    selected_sol = "2_95_19",
    metric_type  = "ltv"
  ),

  uk_acq = list(
    run_folder   = "model_refresh/uk_acq/Robyn_202602162334_init/Robyn_202605141723_rf1",
    selected_sol = "2_54_8",
    metric_type  = "acq"
  ),

  us_ltv = list(
    run_folder   = "model_results/us_ltv/Robyn_202605191525_init",
    selected_sol = "5_74_2",
    metric_type  = "ltv"
  ),

  us_acq = list(
    run_folder   = "model_refresh/us_acq/Robyn_202602181221_init/Robyn_202605130051_rf1",
    selected_sol = "3_65_19",
    metric_type  = "acq"
  )
)


# ── BUDGET ALLOCATION ─────────────────────────────────────────────────────────
# channel_constr_low/up : spend can vary between [low * historical, up * historical]
# scenario              : "max_response" (maximise outcome at same budget) or
#                         "target_efficiency" (hit a CPA/ROI target)
# date_range            : NULL = all weeks in the model; "last_N" = last N weeks

budget_allocation <- list(
  channel_constr_low = 0.5,
  channel_constr_up  = 1.5,
  scenario           = "max_response",

  date_range = list(
    uk_ltv = NULL,
    uk_acq = NULL,
    us_ltv = NULL,
    us_acq = NULL
  )
)



# ── MARGINAL EFFICIENCY ───────────────────────────────────────────────────────
# avg_spend_period : NULL = average over the full model period;
#                   c("YYYY-MM-DD", "YYYY-MM-DD") = restrict to a date range
# target_roi       : mROI target for LTV models — used to find recommended spend
# target_cpa       : mCPA target for ACQ models — used to find recommended spend

marginal_efficiency <- list(
  avg_spend_period = NULL,

  uk_ltv = list(target_roi = 1.25),
  uk_acq = list(target_cpa = 303),
  us_ltv = list(target_roi = 1.25),
  us_acq = list(target_cpa = 303)
)


# ── MODEL REFRESH ─────────────────────────────────────────────────────────────
# init_folder  : absolute path to the Robyn run folder used as the starting point
# select_model : pareto solution ID to refresh (must exist in that run)
# country      : must match a key in project$file_map
# refresh_until: cap data at this date before refresh (NULL = use all data)
# refresh_steps: weeks of new data to incorporate (NULL = auto-detect)
# col_renames  : named vector c(new_name = "old_name") for columns that were
#                renamed between the original model run and the current dataset

model_refresh <- list(

  uk_acq = list(
    init_folder    = "/Users/shazahmed/Documents/MMMM/Robyn_202602162334_init",
    select_model   = "4_94_1", # get this from model selection file
    country        = "united_kingdom",
    refresh_until  = "2026-03-30",
    refresh_steps  = NULL,
    refresh_iters  = 2500,
    refresh_trials = 10,
    col_renames    = c(conversions_percent_of_opportunities = "cvr_opportunity_visits")
  ),

  uk_ltv = list(
    init_folder    = NULL,
    select_model   = NULL,
    country        = "united_kingdom",
    refresh_until  = NULL,
    refresh_steps  = NULL,
    refresh_iters  = 2000,
    refresh_trials = 5,
    col_renames    = NULL
  ),

  us_acq = list(
    init_folder    = "C:/Users/andre/Marketing-Mix-Modelling/MMM V4/model_refresh/us_acq/Robyn_202602181221_init",
    select_model   = "2_143_18",
    country        = "united_states",
    refresh_until  = "2026-03-30",
    refresh_steps  = NULL,
    refresh_iters  = 2000,
    refresh_trials = 5,
    col_renames    = c(conversions_percent_of_opportunities = "cvr_opportunity_visits"),
    col_drops      = NULL
  ),

  us_ltv = list(
    init_folder    = NULL,
    select_model   = NULL,
    country        = "united_states",
    refresh_until  = NULL,
    refresh_steps  = NULL,
    refresh_iters  = 2000,
    refresh_trials = 5,
    col_renames    = NULL
  )
)


cat("Config loaded.\n")
