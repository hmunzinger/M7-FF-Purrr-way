## Loading package pacman and use its function p_load for loading all required packages
library(pacman)
p_load(timetk, tidyquant, highcharter, tidyverse, PerformanceAnalytics, quantmod, tibbletime, PortfolioAnalytics, 
  tibbletime, ROI, tidyr, estimatr, broom, ROI.plugin.glpk, ROI.plugin.quadprog, lubridate, modelr)

## Set working directory to folder where the csv file is saved
setwd()

## Create a vector with ticker symbols
symbols <- c("GOOG", "MSFT", "AMZN", "AAPL", "META", "NVDA", "TSLA")

## Import stock market prices for ticker symbols
prices <-
  getSymbols(symbols, src = 'yahoo', from = "2012-08-01", to = "2024-12-01",
    auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

head(prices)

## Convert daily stock prices to monthly ones
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)

head(prices_monthly)

## Calculate log returns for each stock
m7asset_returns_xts <- Return.calculate(prices_monthly, method = "log") %>% 
  na.omit()

head(m7asset_returns_xts)

m7asset_returns_tbl <- as_tibble(m7asset_returns_xts) 

head(m7asset_returns_tbl)

m7asset_returns_tbl <- m7asset_returns_tbl %>% 
  add_column(date = index(m7asset_returns_xts), .before = 1)

head(m7asset_returns_tbl)

m7asset_returns_long_tbl <- m7asset_returns_tbl %>% 
  gather(stock, return, -date)
  
head(m7asset_returns_long_tbl)

## Download Fama French Global 3 Factors csv file
## https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip

## Import Global 3 Factors csv file
Global_3_Factors <- read.csv("F-F_Research_Data_Factors.csv", skip = 3) %>%
  rename (date = X1) %>% 
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>%
  mutate_at(vars(-date), as.numeric)

## Convert values to decimales
Global_3_Factors <- Global_3_Factors %>%   
  mutate(Mkt.RF = Global_3_Factors$Mkt.RF/100,
    SMB = Global_3_Factors$SMB/100,
    HML = Global_3_Factors$HML/100,
    RF =  Global_3_Factors$RF/100) %>% 
    na.omit()

## Convert Global 3 Factors object to a tibble
Global_3_Factors <- as_tibble(Global_3_Factors)

## Check on object created
head(Global_3_Factors)
map_chr(Global_3_Factors, class)

## Add one month to date for matching data sets
Global_3_Factors <- Global_3_Factors %>% 
  mutate(date = lubridate::rollback(date + months(1)))
  
head(Global_3_Factors)

## Change data set from wide to long format
m7asset_returns_tbl_long <- m7asset_returns_tbl %>%  
  gather(key = "asset", value = "return", -date)

head(m7asset_returns_tbl_long)

## Merge the two data sets into one
joined_data_set <- m7asset_returns_tbl_long %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  na.omit()

## Calculate excess returns for each asset
joined_data_set <- joined_data_set %>% 
mutate(excess_return = round(return - RF, 4))

head(joined_data_set)

## Nest data set
nested_data_set <- joined_data_set %>%
  group_by(asset) %>%
  nest()

head(nested_data_set)

## Set up function for regressions
lm_robust_model <- function(df) {
  lm_robust(excess_return ~ Mkt.RF + SMB + HML, data = df)
}

## Run lm robust regression
nested_data_set <- nested_data_set %>%
  mutate(regression_models = map(data, lm_robust_model))

head(nested_data_set)    

## Create column of lm regression residuals
nested_data_set <- nested_data_set %>% 
  mutate(resids = map2(data, regression_models, add_residuals))

## Unnest resids column for being able to do graphics
resids <- unnest(nested_data_set, resids)

head(resids)

## Create a graph of the residuals
resids %>%
  ggplot(aes(date, resid)) +
  geom_line(aes(group = asset), alpha = 1/3) +
  geom_smooth(se = FALSE)

## Create a graph of the residuals for each asset
resids %>%
  ggplot(aes(date, resid, group = asset)) +
  geom_line(alpha = 1/3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~asset)

## Unnest lm robust regression results
model_output <- nested_data_set %>%
  mutate(model_output = map(regression_models, broom::glance)) %>%
  unnest(model_output)

## Arrange lm robust regression results by ascending adjusted r.squared 
model_output %>% arrange(adj.r.squared)

## Create a graph showing adjusted r.squared for each asset
model_output %>%
  ggplot(aes(asset, adj.r.squared)) +
  geom_jitter(width = 0.5)



