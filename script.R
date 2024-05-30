# Packages 
# ------------------------------------------------------------------------------

library(tidyverse)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)
library(glmnet)
library(dials)
library(tune)
library(broom)
library(vetiver)
library(pins)
# library(plumber)
# library(rsconnect)



# Clean Data
# ------------------------------------------------------------------------------ 
# We want to predict interests rates. before we do, we should remove variables
# that are irrelevant, incomplete, or will not be available when predicting
# interest rates

# library(tidyverse)

con <-
  DBI::dbConnect(odbc::databricks(), httpPath = "/sql/1.0/warehouses/300bd24ba12adf8e")

lendingclub_dat <- 
  dplyr::tbl(con, dbplyr::in_catalog("hive_metastore", "default", "lendingclub")) |> 
  select(-c("acc_now_delinq", "chargeoff_within_12_mths",
            "debt_settlement_flag", "debt_settlement_flag_date",
            "deferral_term",
            "delinq_amnt","desc","disbursement_method","emp_title",
            "funded_amnt","funded_amnt_inv","grade","hardship_amount",
            "hardship_dpd", "hardship_end_date", "hardship_flag",
            "hardship_last_payment_amount", "hardship_length",
            "hardship_loan_status", "hardship_payoff_balance_amount",
            "hardship_reason", "hardship_start_date", "hardship_status",
            "last_credit_pull_d",
            "hardship_type","id","initial_list_status","installment","issue_d",
            "last_pymnt_d", "last_pymnt_amnt", "loan_status",
            "member_id", "next_pymnt_d", "num_tl_30dpd", "num_tl_120dpd_2m", 
            "orig_projected_additional_accrued_interest",
            "out_prncp", "out_prncp_inv","payment_plan_start_date",
            "policy_code","purpose", "pymnt_plan", "revol_bal_joint",
            "revol_util", "sec_app_earliest_cr_line",
            "sec_app_inq_last_6mths", "sec_app_mort_acc", "sec_app_open_acc",
            "sec_app_revol_util", "sec_app_open_act_il",
            "sec_app_num_rev_accts", "sec_app_chargeoff_within_12_mths",
            "sec_app_collections_12_mths_ex_med",
            "sec_app_mths_since_last_major_derog","settlement_amount",
            "settlement_date", "settlement_percentage", "settlement_status",
            "settlement_term","sub_grade","title", "total_pymnt", "total_pymnt_inv",
            "total_rec_int", "total_rec_late_fee", "total_rec_prncp", # "total_rev_hi_lim",
            "url","verification_status",
            "verification_status_joint")) |>
  mutate(
    # Convert these columns into numeric
    across(c(starts_with("annual"), starts_with("dti"), starts_with("inq"),  
             starts_with("mo"), starts_with("mths"), starts_with("num"), 
             starts_with("open"), starts_with("percent"), starts_with("pct"), 
             starts_with("revol"), starts_with("tot"),  "acc_open_past_24mths", 
             "all_util", "avg_cur_bal","bc_open_to_buy", "bc_util", 
             "collections_12_mths_ex_med", "collection_recovery_fee", "delinq_2yrs", 
             "il_util", "loan_amnt", "max_bal_bc", "pub_rec", 
             "pub_rec_bankruptcies", "recoveries", "tax_liens"), 
           ~ as.numeric(.)),
    # Calculate a loan to income statistic
    loan_to_income = case_when(
      application_type == "Individual" ~ loan_amnt / annual_inc,
      .default = loan_amnt / annual_inc_joint
    ),
    # Calculate the percentage of the borrower's total income that current debt 
    # obligations, including this loan, will represent
    adjusted_dti = case_when(
      application_type == "Individual" ~ (loan_amnt + tot_cur_bal) / (annual_inc),
      .default = (loan_amnt + tot_cur_bal) / (annual_inc_joint)
    ),
    #  Calculate utilization on installment accounts excluding mortgage balance
    il_util_ex_mort = case_when(
      total_il_high_credit_limit > 0 ~ total_bal_ex_mort / total_il_high_credit_limit,
      .default = 0
    ),
    # Fill debt to income joint with individual debt to income where missing
    dti_joint = coalesce(dti_joint, dti),
    # Fill annual income joint with individual annual income where missing
    annual_inc_joint = coalesce(annual_inc_joint, annual_inc)) |> 
  collect()

lendingclub_dat_clean <-
  lendingclub_dat |>
  mutate(
    # Missing values for these columns seem most appropriate to fill with zero
    across(c("inq_fi", "dti", "all_util", "percent_bc_gt_75", "il_util", 
             "avg_cur_bal","all_util", "il_util", "inq_last_6mths", "inq_last_12m", 
             "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m"), 
           ~ replace_na(., 0)),
    # Missing values for these columns seem most appropriate to fill with the column max
    across(c("mo_sin_old_il_acct", "mths_since_last_major_derog", "mths_since_last_delinq", 
             "mths_since_recent_bc", "mths_since_last_record", "mths_since_rcnt_il", 
             "mths_since_recent_bc", "mths_since_recent_bc_dlq", "mths_since_recent_inq", 
             "mths_since_recent_revol_delinq", "mths_since_recent_revol_delinq"),  
           ~ replace_na(., max(., na.rm = TRUE))),
    # Remove percent sign
    int_rate = as.numeric(stringr::str_remove(int_rate, "%")),
    # Create variable for earliest line of credit
    earliest_cr_line = lubridate::parse_date_time2(paste("01", earliest_cr_line, sep = "-"), 
                                                   "dmy", cutoff_2000 = 50L),
    # Calculate time since earliest line of credit
    age_earliest_cr = lubridate::interval(as.Date(earliest_cr_line), 
                                          as.Date(lubridate::today())) %/% lubridate::days(1),
    # Convert characters to factors
    across(where(is.character), .fns = as.factor),
    # Encode ordered factors
    term = as.numeric(stringr::str_trim(stringr::str_remove(term, "months"))),
    emp_length = as.ordered(factor(emp_length, 
                                   levels = c("< 1 year", "1 year", "2 years", 
                                              "3 years", "4 years", "5 years",
                                              "6 years", "7 years", "8 years", 
                                              "9 years", "10+ years")))) |> 
  # drop date column
  select(!earliest_cr_line) |> 
  filter(!is.na(int_rate))


mean_impute_vals <- 
  c("bc_util", "num_rev_accts", "bc_open_to_buy", "emp_length", "percent_bc_gt_75", 
    "total_bal_il", "total_il_high_credit_limit", "total_cu_tl")

categorical_vars <- 
  c("addr_state", "application_type", "home_ownership", "emp_length", "term", 
    "zip_code")

# let's check that our data looks clean
colSums(is.na(lendingclub_dat_clean))
sum(colSums(is.na(lendingclub_dat_clean)) > 0)

lendingclub_dat_clean |>
  select(where(is.factor)) %>%
  select(where( ~ nlevels(.) < 2))



# Model selection
# ------------------------------------------------------------------------------
# We use Lasso to identify the set of variables likely to maximize performance
# without overfitting. We tune to find the ideal penalty parameter.

# library(tidyverse)
# library(rsample)
# library(recipes)
# library(parsnip)
# library(workflows)
# library(yardstick)
# library(glmnet)
# library(dials)
# library(tune)

set.seed(1234)
train_test_split <- initial_split(lendingclub_dat_clean)

lend_train <- training(train_test_split)
lend_test <- testing(train_test_split)

rec_obj <- recipe(int_rate ~ ., data = lend_train) |>
  step_normalize(all_numeric_predictors()) |>
  step_ordinalscore(emp_length) |> 
  step_integer(c("addr_state", "application_type", "home_ownership", "zip_code")) |> 
  step_impute_mean(all_of(mean_impute_vals))

# Review data
prep(rec_obj, lend_train) |>  bake(new_data = NULL)


# We will use cross validation to determine the optimum penaly parameter for lasso
lend_lasso <- 
  linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

lend_lasso_wflow <-
  workflow() |>
  add_model(lend_lasso) |>
  add_recipe(rec_obj)


# Find the best penalty parameter
lambda_grid <- 
  grid_regular(penalty(), levels = 50)

lasso_grid <- 
  lend_lasso_wflow |> 
  tune_grid(grid = lambda_grid, resamples = vfold_cv(lend_train))

lasso_grid |> 
  collect_metrics()

lasso_grid |> 
  autoplot()

# Every parameter below 0.01 seems similar, but 0.01 will give us the most
# parsimonious model. How well does that model do?
final_lasso_wflow <- 
  lend_lasso_wflow |> 
  finalize_workflow(list(penalty = 0.1))

lend_lasso_fit <-
  final_lasso_wflow |>
  fit(data = lend_train)

# Let's look at our predictions
predict(lend_lasso_fit, new_data = lend_train) 

# What is the rsq for this model?
lend_lasso_results <-
  bind_cols(predict(lend_lasso_fit, lend_train)) |>
  bind_cols(lend_train |> select(int_rate))

rsq(lend_lasso_results, truth = int_rate, estimate = .pred)
rmse(lend_lasso_results, truth = int_rate, estimate = .pred)


# How many variables are in this model?

# library(broom)

lend_lasso_fit |>
  extract_fit_parsnip() |> 
  tidy() |> 
  filter(estimate > 0) |> 
  nrow()
# 20


# Variable Selection
# ------------------------------------------------------------------------------
# Unfortunately, we think that clients who use our app will only have patience
# to input five variables. Hence we need to find the top 5.
lend_lasso_fit |> 
  extract_fit_parsnip() |> 
  autoplot()

lend_lasso_fit |> 
  extract_fit_parsnip() |> 
  autoplot(min_penalty = 1, top_n = 5)


# Sacrifice in performance
# ------------------------------------------------------------------------------
# How much performance do we sacrifice by only using four variables?

set.seed(1234)
lendingclub_dat_reduced <-
  lendingclub_dat_clean |> 
  select(int_rate, term, bc_open_to_buy, bc_util, all_util)

reduced_split <- initial_split(lendingclub_dat_reduced)

reduced_train <- training(reduced_split)
reduced_test <- testing(reduced_split)

red_rec_obj <- recipe(int_rate ~ ., data = reduced_train) |>
  step_normalize(all_numeric_predictors()) |>
  step_impute_mean(all_of(c("bc_open_to_buy", "bc_util")))

lend_linear <- 
  linear_reg()

lend_linear_wflow <-
  workflow() |>
  add_model(lend_linear) |>
  add_recipe(red_rec_obj)

lend_linear_fit <-
  lend_linear_wflow |>
  fit(data = reduced_train)

lend_linear_results <-
  bind_cols(predict(lend_linear_fit, reduced_train)) |>
  bind_cols(reduced_train |> select(int_rate))

rsq(lend_linear_results, truth = int_rate, estimate = .pred)
rmse(lend_linear_results, truth = int_rate, estimate = .pred)

# We will deal


# Deploy Model
# ------------------------------------------------------------------------------
# Now let's host the model as a pin

# library(vetiver)
# library(pins)
# library(plumber)
# library(rsconnect)

v <- 
  vetiver_model(lend_linear_fit, "lending_club_model")

v |> 
  write_rds(file = "app/vetiver-model.RDS")


# Deploy a Model API
# ------------------------------------------------------------------------------
# We'd like to build an API for our app to use the model. We will make a plumber
# API and then pin that to our board.

# board <-
#   pins::board_connect()
# 
# 
# board |>
#   vetiver_pin_write(v)
# 
# rsconnect::addServer(Sys.getenv("CONNECT_SERVER"))
# 
# 
# rsconnect::connectApiUser(
#   server = Sys.getenv("CONNECT_SERVER_WITHOUT_HTTP"),
#   account = Sys.getenv("CONNECT_USER"),
#   apiKey = Sys.getenv("CONNECT_API_KEY")
# )
# 
# vetiver_deploy_rsconnect(
#   board = board,
#   name = "garrett@posit.co/lending_club_model",
#   predict_args = list(debug = TRUE)
# )
# 
# 
# # Give your API a path on Connect so we can use it in future steps, e.g.
# # endpoint <- 
# #   vetiver_endpoint(
# #     "https://pub.demo.posit.team/public/lending-club-model-vetiver-api/predict"
# #   )
