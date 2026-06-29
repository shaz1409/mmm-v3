library(bigrquery)
library(glue)

bq_deauth()
bq_auth(
  email   = "andrea.rodriguez@ft.com",
  scopes  = c("https://www.googleapis.com/auth/bigquery"),
  cache   = FALSE
)

project_id <- "t7s-looker-studio"

sql_query <- "
  SELECT *
  FROM   `t7s-looker-studio.FinancialTimes.ft_mmm_data`
  WHERE  day > '2025-12-31'
"

ft_data <- bq_table_download(
  bq_project_query(
    x              = project_id,
    query          = sql_query,
    use_legacy_sql = FALSE
  )
)

write.csv(ft_data, "ft_mmm_data_2026.csv", row.names = FALSE)

cat(glue("Exported {nrow(ft_data)} rows to ft_mmm_data_2026.csv\n"))
