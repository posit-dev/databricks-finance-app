# authenticate.R
library(dplyr)
library(dbplyr)
library(odbc)

# Sys.setenv("DATABRICKS_HOST" = "your-databricks-host.com")
# Sys.setenv("DATABRICKS_TOKEN" = "your-databricks-token")
con <-
  dbConnect(odbc::databricks(), httpPath = Sys.getenv("HTTP_PATH"))

# Return connection to lending club table
connect_to_lending_club_data_on_databricks <- function(){
  tbl(con, dbplyr::in_catalog("hive_metastore", "default", "lendingclub")) |>
    select(int_rate, term, all_util, bc_util, bc_open_to_buy) |> 
    filter(!is.na(int_rate)) |> 
    mutate(int_rate = REPLACE(int_rate, "%", ""),
           term = SUBSTRING(term, 2,2),
           across(everything(), ~ as.numeric(.)))
}