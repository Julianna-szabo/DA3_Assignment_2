# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
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

# import data

data <- read_csv(file = "Data/Clean/cs_bisnode_panel.csv")

data_original <- data

# drop variables with many NAs
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) %>%
  filter(year !=2016)

options(digits = 3)
###########################################################
# label engineering
###########################################################
# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
# data <- data %>%
#   complete(year, comp_id)

# Filter 2010 - 2015

data <- data %>% 
  filter(year >= 2010 & year <=2015)

# Sort by company id

data <- data[order(data$comp_id),]


############################
# Find fast growing firms  #
############################

# Filter for only years 2012 and 2013

data <- data %>% 
  filter(year == 2012 | year == 2013)

# Check for missing values

count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}
count_missing_values(data)

# Drop 

data<- filter(data, !is.na(profit_loss_year)) 
data<- filter(data, !is.na(sales)) 
data<- filter(data, !is.na(inc_bef_tax)) 

comp_count_check <- data %>% 
  group_by(comp_id) %>% 
  count()

missing <- comp_count_check %>% 
  filter(n<2)

data <- data[!(data$comp_id %in% (missing$comp_id)),]

rm(missing, comp_count_check)

# Create year on year column

# Columns to do year on year for
y_on_y <- c("profit_loss_year", "sales", "inc_bef_tax")

# Look at all unique values for com_id
for (i in unique(data$comp_id)) {
  # Look at all the columns to use for year on year
  for (j in y_on_y) {
    column_name <- (paste0(j, "_y_on_y"))
    # Find the row numbers
    row_2013 <- which(data$comp_id == i & data$year == 2013)
    row_2012 <- which(data$comp_id == i & data$year == 2012)
    # Create the year on year columns
    data[row_2013, column_name] <- (data[row_2013, j] - data[row_2012, j]) / data[row_2012, j] 
  }
}

write.csv(data, "Data/Clean/fast_growth_workingfile_1.csv")

#data <- read_csv(file = "Data/Clean/fast_growth_workingfile_1.csv")

##########################################################################
# Check the three different possible variables: sales, profit and EBIDTA

# Filter out NaNs and infinates
data<- filter(data, !is.infinite(profit_loss_year_y_on_y)) 
data<- filter(data, !is.infinite(sales_y_on_y)) 
data<- filter(data, !is.infinite(inc_bef_tax_y_on_y)) 
data<- filter(data, !is.na(profit_loss_year_y_on_y)) 
data<- filter(data, !is.na(sales_y_on_y)) 
data<- filter(data, !is.na(inc_bef_tax_y_on_y))

glimpse(data)

##############################
# 1). Growth on sales_y_on_y:
##############################
# check growth on sales statistics
data %>% 
  filter(year == 2013) %>% 
  summarise(
    Obs = 'sales_y_on_y',
    n = sum( !is.na( sales_y_on_y ) ),
    mean = mean(sales_y_on_y),
    first_quartile = quantile( !is.na(sales_y_on_y), 0.25),
    median   = median(sales_y_on_y),
    third_quartile = quantile(!is.na(sales_y_on_y), 0.75),
    min = min(sales_y_on_y),
    max = max(sales_y_on_y),
    sd = sd(sales_y_on_y)
  )
# from the summary statistics, we should filter out extreme variables. Let's restrict max growth on sales at 200%, and min growth
# (max loss) at -200%

# check summary statistics again:
summary_sales_growth <- data %>% 
  filter(year == 2013) %>% 
  filter(sales_y_on_y < 2 & sales_y_on_y > -2) %>% 
  summarise(
    Obs = 'sales_y_on_y',
    n = sum( !is.na(sales_y_on_y )),
    mean = mean(sales_y_on_y),
    first_quartile = quantile(sales_y_on_y, 0.25),
    median   = median(sales_y_on_y),
    third_quartile = quantile(sales_y_on_y, 0.75),
    min = min(sales_y_on_y),
    max = max(sales_y_on_y),
    sd = sd(sales_y_on_y)
  )

count(data, sales_y_on_y > 0)

# check distribution
data %>% 
  filter(year == 2013) %>% 
  filter(sales_y_on_y < 2 & sales_y_on_y > -2) %>% 
  ggplot(aes(x = sales_y_on_y)) +
  geom_histogram(bins = 100)


###########################################
# 2). Growth on profit_loss_year_y_on_y:
###########################################
# Distribution of profit year on year
data %>% 
  filter(year == 2013) %>% 
  summarise(
    Obs = 'profit_loss_year_y_on_y',
    n = sum( !is.na( profit_loss_year_y_on_y ) ),
    mean = mean(profit_loss_year_y_on_y),
    first_quartile = quantile(profit_loss_year_y_on_y, 0.25),
    median   = median(profit_loss_year_y_on_y),
    third_quartile = quantile(profit_loss_year_y_on_y, 0.75),
    min = min(profit_loss_year_y_on_y),
    max = max(profit_loss_year_y_on_y),
    sd = sd(profit_loss_year_y_on_y)
  )
# from the summary statistics, we should filter out extreme variables. Let's restrict max growth on profit at 200%, and min growth
# (max loss) at -200%

# check summary statistics again:
summary_profit_growth <- data %>% 
  filter(year == 2013) %>% 
  filter(profit_loss_year_y_on_y < 2 & profit_loss_year_y_on_y > -2) %>% 
  summarise(
    Obs = 'profit_loss_year_y_on_y',
    n = sum( !is.na( profit_loss_year_y_on_y ) ),
    mean = mean(profit_loss_year_y_on_y),
    first_quartile = quantile(profit_loss_year_y_on_y, 0.25),
    median   = median(profit_loss_year_y_on_y),
    third_quartile = quantile(profit_loss_year_y_on_y, 0.75),
    min = min(profit_loss_year_y_on_y),
    max = max(profit_loss_year_y_on_y),
    sd = sd(profit_loss_year_y_on_y)
  )

count(data, profit_loss_year_y_on_y > 0)

# check distribution
data %>% 
  filter(year == 2013) %>% 
  filter(profit_loss_year_y_on_y < 2 & profit_loss_year_y_on_y > -2) %>% 
  ggplot(aes(x = profit_loss_year_y_on_y)) +
  geom_histogram(bins = 100)


#####################################
# 3). Growth on inc_bef_tax_y_on_y:
#####################################
# Distribution of EBIDTA year on year
data %>% 
  filter(year == 2013) %>% 
  summarise(
    Obs = 'inc_bef_tax_y_on_y',
    n = sum( !is.na( inc_bef_tax_y_on_y ) ),
    mean = mean(inc_bef_tax_y_on_y),
    first_quartile = quantile(inc_bef_tax_y_on_y, 0.25),
    median   = median(inc_bef_tax_y_on_y),
    third_quartile = quantile(inc_bef_tax_y_on_y, 0.75),
    min = min(inc_bef_tax_y_on_y),
    max = max(inc_bef_tax_y_on_y),
    sd = sd(inc_bef_tax_y_on_y)
  )

# from the summary statistics, we should filter out extreme variables. Let's restrict max growth on profit at 200%, and min growth
# (max loss) at -200%

# check summary statistics again:
summary_EBIDTA_growth <- data %>% 
  filter(year == 2013) %>% 
  filter(inc_bef_tax_y_on_y < 2 & inc_bef_tax_y_on_y > -2) %>% 
  summarise(
    Obs = 'inc_bef_tax_y_on_y',
    n = sum( !is.na( inc_bef_tax_y_on_y ) ),
    mean = mean(inc_bef_tax_y_on_y),
    first_quartile = quantile(inc_bef_tax_y_on_y, 0.25),
    median   = median(inc_bef_tax_y_on_y),
    third_quartile = quantile(inc_bef_tax_y_on_y, 0.75),
    min = min(inc_bef_tax_y_on_y),
    max = max(inc_bef_tax_y_on_y),
    sd = sd(inc_bef_tax_y_on_y)
  )

count(data, inc_bef_tax_y_on_y > 0)

# check distribution
data %>% 
  filter(year == 2013) %>% 
  filter(inc_bef_tax_y_on_y < 2 & inc_bef_tax_y_on_y > -2) %>% 
  ggplot(aes(x = inc_bef_tax_y_on_y)) +
  geom_histogram(bins = 100)

# combine the 3 summary statistics together
growth_variables_summary <- summary_sales_growth %>% add_row( summary_profit_growth ) %>% add_row(summary_EBIDTA_growth)
growth_variables_summary

# based on the combined summary statistics, I suggest we use third quartile as our threshold, everything above third quartile
# we should classify them as fast-growth firms. So that's like first 25% of the data. 
# 1). for growth on sales: threshold is 0.213
# 2). for profit on sales: threshold is 0.132
# 3). for ebidta on sales: threshold is 0.158

# We choose to use growth on sales as our growth vairbale since it gives the largest data sample and more consistant
data <- filter(data, sales_y_on_y < 2 & sales_y_on_y > -2)

# Binary variables for fast growth on SALES - target variable
data <- data %>% 
  mutate(
    fast_growth_sales = ifelse((sales_y_on_y >= 0.213), 1, 0))

# save file
write.csv(data, "Data/Clean/fast_growth_workingfile_targetchoosen.csv")


#####################
### Sample design ##
####################
# data <- read_csv(file = "Data/Clean/fast_growth_workingfile_targetchoosen.csv")

data <- data %>% 
  filter(data$year == 2013)

###########################################################
# Feature engineering
###########################################################

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inventories",
              "material_exp", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# Create rations
# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(default_f = factor(default, levels = c(0,1)) %>%
           recode(., `0` = 'no_default', `1` = "default"))

########################################################################
# sales CHECK
########################################################################

data <- data %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)


ggplot(data = data, aes(x=sales_mil_log, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "default") +
  theme_bg()


ols_s <- lm(default~sales_mil_log+sales_mil_log_sq,
            data = data)
summary(ols_s)

########################################################################
# sales change CHECK
########################################################################
# Note: graphs not in book

# lowess
Hmisc::describe(data$d1_sales_mil_log) # no missing

d1sale_1<-ggplot(data = data, aes(x=d1_sales_mil_log, y=as.numeric(default))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default") +
  theme_bg() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))
d1sale_1
save_fig("ch17-extra-1", output, "small")

# generate variables ---------------------------------------------------

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

d1sale_2<-ggplot(data = data, aes(x=d1_sales_mil_log_mod, y=as.numeric(default))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default") +
  theme_bg() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))
d1sale_2
save_fig("ch17-extra-2", output, "small")

d1sale_3<-ggplot(data = data, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  theme_bg() +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))
d1sale_3
save_fig("ch17-extra-3", output, "small")


# Write into a CSV

write_csv(data,paste0("data/firms_fast growth_clean.csv"))