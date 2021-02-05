# Clear Memory
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)


# Loading and preparing data ----------------------------------------------

# Use R format so it keeps factor definitions
# data <- read_csv("data/fast_growth_clean.csv")
data <- read_rds(paste(data_out,"bisnode_firms_clean.rds", sep = "/"))

#summary
datasummary_skim(data, type='numeric', histogram = TRUE)
# datasummary_skim(data, type="categorical")

glimpse(data)

# Notes:
#   Drop x1 column - this was an index but no longer complete
#   Maybe drop begin and end if we have age?
#   Drop D since it is NA for all observations
#   Difference between birth year and founded year?
#   

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap") # Have all of these
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear") # Have all of these
balancesheet <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs")
profitandloss <- c("extra_exp_pl", "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl",
                   "inventories_pl", "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")

# Description table

#| Groups          | N vara      | Description |
#| ----------------| ----------- | ----------- |
#| Raw variables | 16 | Current Assets, Current liabilities, Extra expenses, Extra income, Extra profit or loss, Fixed assets, Income before tax, Intangable assets, Inventories, Liquid assets, Material expenses, Personnel expenses, Profit and loss, Sales, Equity in shares, Subscribed capital       |
#| Quality variables | 3 | Balance sheet flag, Length of balance sheet, Flag for incomplete balance sheets |
#| Balance sheet ratios | 7 | Total assets ratio, Fixed assets ratio, Liquid assets ratio,  Current assets ratio, Equity in shares ratio, Subscribed capital ratio, Intangable assets ratio |
#| Profit and loss ratios | 8 | Extra expenses ratio, Extra income ratio, Extra profit or loss ratio, Income before tax ratio, Inventory ratio, Material expenses ratio, Profit or loss ratio, Personnel expenses ratio |
#| Quadratir variables | 4 | Extra profit or loss squared, Income before tax squared, Profit or loss squared, Equity in shares squared |
#| Flags for values |  | Flags showing high, low, zeros, and errors for the raw variables when applicable |
#| Sales growth | 4 | 
