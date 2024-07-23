##############################################################
##################### Data ###################################
##############################################################

rm(list=ls())

# libraries 
library("tidyverse")  
library("DescTools")  # for winsorizing 
library("readxl")     # self explanatory
library("moments")    # self explanatory
library("envalysis")  # publishable graphs 
library("glmnet")     # lasso 
library("pls")        # pca regression and plots
library("gridExtra")  # for multiplots
library("stargazer")  # nice regression tables
library("caret")      # machine learning algos


############## Data ###################################### 

# load on data
crypto <- as_tibble(read_excel("weekly_cryptos.xlsx")) 


# change commas to points: 
comma_to_point <- function(x){
  gsub(",", ".",x)
}

crypto[] <-  lapply(crypto,comma_to_point)


# change to numeric:
crypto[,3:9] <- lapply(crypto[,3:9],as.numeric)


# check types: 
lapply(crypto[], typeof)


# compute variables 
crypto <- 
  crypto %>% 
  arrange(date) %>% 
  group_by(coin) %>% 
  mutate(lnret = log(close)-log(lag(close)),     # log returns 
         ret = (close - lag(close))/lag(close),  # weekly returns 
         ret_sqr = ret^2,                        # volatility proxy return squared 
         ret_abs = abs(ret),                     # volatility proxy |return| 
         max_ret = (high - low)/low,             # daily max ret  
         prc  = log(close),                      # logarithm of closing price
         prcvol = log(close * volume),           # logarithm of closing price * volume 
         lag_ret = lag(ret),                     # ret last week
         lag_ret2 = lag(lag(ret)),               # ret 2 weeks ago
         lag_ret3 = lag(lag(lag(ret))),          # ret 3 weeks ago
         lag_prc  = lag(prc)     ,               # log(prc) last week 
         lag_size = lag(marketCap),              # size last week
         lag_volume = lag(volume),               # volume last week
         lag_retsq = lag(ret_sqr),               # return squared last week
         lag_retabs = lag(ret_abs),              # absolute value of return last week
         lag_max_ret = lag(max_ret),             # max return last week 
         lag_age = lag(age),                     # age of coin last week 
         lag_prcvol = lag(prcvol))%>%            # price * volume last week
  filter(volume > 0)       %>%                 # remove rows with very low volume because bad price making
  filter(marketCap > 10000) %>%                # filter out coins lower than 100k market cap 
  na.omit()                                      # delete Na  




#number of coins in given years 
crypto %>%
  filter(date == "2015-12-28") %>%
  nrow()

crypto %>% 
  filter(date == "2017-12-25") %>% 
  nrow()

crypto %>% 
  filter(date == "2019-12-30") %>% 
  nrow()

crypto %>% 
  filter(date == "2021-12-27") %>% 
  nrow()

crypto %>% 
  filter(date == "2023-12-25") %>% 
  nrow()


# compute mean/median for 2015 
mktcap15 <- 
  crypto %>% 
  filter(date =="2015-12-28") 

mean(mktcap15$marketCap)
median(mktcap15$marketCap)
mean(mktcap15$volume)
median(mktcap15$volume) 

# compute mean/median for 2017
mktcap17 <- 
  crypto %>% 
  filter(date =="2017-12-25") 

mean(mktcap17$marketCap)
median(mktcap17$marketCap)
mean(mktcap17$volume)
median(mktcap17$volume)


# compute mean/median for 2019
mktcap19 <- 
  crypto %>% 
  filter(date =="2019-12-30") 

mean(mktcap19$marketCap)
median(mktcap19$marketCap)
mean(mktcap19$volume)
median(mktcap19$volume)


# compute mean/median for 2021
mktcap21 <- 
  crypto %>% 
  filter(date =="2021-12-27") 

mean(mktcap21$marketCap)
median(mktcap21$marketCap)
mean(mktcap21$volume)
median(mktcap21$volume)

# compute mean/median for 2023
mktcap23 <- 
  crypto %>% 
  filter(date =="2023-12-25") 

mean(mktcap23$marketCap)
median(mktcap23$marketCap)
mean(mktcap23$volume)
median(mktcap23$volume)


# compute market returns per date 
crypto <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(market = sum(marketCap),     # proxy since dont have all coins, but close enough 
         weight = marketCap/market,   # weight of each coin
         mkt_ret = sum(weight*ret),   # weekly market return
         ln_mkt_ret = sum(weight*lnret))


#### join macroeconomic variables for later tasks 


# sp500 data for comparison  
spx <- as_tibble(read_csv("SPX.csv",col_names = TRUE))
spx <- 
  spx %>% 
  mutate(spx_lnret = log(Close) - log(lag(Close)),
         spx_ret = (Close - lag(Close))/lag(Close),
         lag_spx = lag(spx_ret)) %>% 
  na.omit() %>% 
  mutate(spx_cumret = cumsum(spx_lnret)) %>% 
  rename(date = Date)

spx$date <- as.character(spx$date)

crypto <- left_join(crypto,spx)

######## join some macroeconomic variables ################
# vix 
vix <-  as_tibble(read_excel("vix.xls",col_names = TRUE))

vix <- 
  vix %>% 
  fill(VIXCLS) %>% 
  rename(vix = VIXCLS) %>% 
  mutate(vix_lag = lag(vix))
vix$date = as.character(vix$date)

crypto <- left_join(crypto,vix)




# us bonds high yield index 
usbond <- as.tibble(read_excel("usbond_highyield_index.xls",col_names = T))

usbond <- 
  usbond %>% 
  rename(usbond_id = BAMLHYH0A0HYM2TRIV) %>% 
  fill(usbond_id) %>% 
  mutate(usbond_lag = lag(usbond_id))

usbond$date <- as.character(usbond$date)

crypto <- left_join(crypto,usbond)

# 5 year breakeven inflation rate
infl <- as.tibble(read_excel("5year_breakeven_inflationrate.xls",
                             col_names = T))
infl <- 
  infl %>% 
  fill(T5YIE) %>% 
  mutate(infl_lag = lag(T5YIE))
infl$date <- as.character(infl$date)

crypto <- left_join(crypto,infl)


# 10Y vs 2Y yield 
yielddiff <- as.tibble(read_excel("10Y_vs_2Y.xls",col_names = T))

yielddiff <- 
  yielddiff %>% 
  fill(T10Y2Y) %>% 
  mutate(yielddiff_lag = lag(T10Y2Y))
yielddiff$date = as.character(yielddiff$date)

crypto <- left_join(crypto,yielddiff)



# oil price 

oil <- as.tibble(read_excel("oil.xls",col_names = T))

oil <- 
  oil %>% 
  fill(DCOILBRENTEU) %>% 
  mutate(oil_lag = lag(DCOILBRENTEU))
oil$date <- as.character(oil$date)

crypto <- left_join(crypto,oil)


# risk free rate (proxy)
# load in risk free rate (t-bill used as proxy freom FRED)
rf <- as_tibble(read_csv("DTB3.csv"))
rf$DTB3 <- as.numeric(rf$DTB3) 

rf <- 
  rf %>% 
  fill(DTB3) %>% 
  rename(rf_rate = DTB3) %>% 
  rename(date = DATE) %>% 
  mutate(rf_rate = rf_rate/100)  # because was given in percent
rf$date <- as.character(rf$date)

crypto <- left_join(crypto,rf) %>% 
  mutate(ret_ex = ret -rf_rate )





# compute cumulative returns 
# use the summability of log-returns
btc <- crypto[crypto$coin=="BTC",]
btc$cumret <- cumsum(btc$lnret)

eth <- crypto[crypto$coin=="ETH",]
eth$cumret <- cumsum(eth$lnret)


# create market variable
mkt <- crypto[crypto$coin == "BTC",] # to get mkt once for all dates
mkt <- mkt[,c(1,28,30,31)]
mkt$mkt_cumret <- cumsum(mkt$ln_mkt_ret)
mkt$mkt_ret_sqr = mkt$mkt_ret^2




## moments of crypto market 
mean_crypto <- mean(mkt$mkt_ret)
median_crypto <- median(mkt$mkt_ret)
sd_crypto   <- sd(mkt$mkt_ret)
skew_crypto <- skewness(mkt$mkt_ret)
kurt_crypto <- kurtosis(mkt$mkt_ret)


## moments sp500
mean_sp <- mean(spx$spx_ret)
median_sp <- median(spx$spx_ret)
sd_sp   <- sd(spx$spx_ret)
skew_sp <- skewness(spx$spx_ret)
kurt_sp <- kurtosis(spx$spx_ret)

## moments bitcoin 
mean_btc <- mean(btc$ret)
median_btc <- median(btc$ret)
sd_btc   <- sd(btc$ret)
skew_btc <- skewness(btc$ret)
kurt_btc <- kurtosis(btc$ret)

## moments eth
mean_eth <- mean(eth$ret)
median_eth <- median(eth$ret)
sd_eth   <- sd(eth$ret)
skew_eth <- skewness(eth$ret)
kurt_eth <- kurtosis(eth$ret)


## agostino test for skewness 
agostino.test(mkt$mkt_ret)
agostino.test(btc$ret)
agostino.test(eth$ret)
agostino.test(spx$spx_ret)

# anscombe test for kurtosis
anscombe.test(mkt$mkt_ret)
anscombe.test(btc$ret)
anscombe.test(eth$ret)
anscombe.test(spx$spx_ret)

# jarque bera test normality 
jarque.test(mkt$mkt_ret)
jarque.test(btc$ret)
jarque.test(eth$ret)
jarque.test(spx$spx_ret)

summary(crypto)


### --> now we have dataframe crypto with that we will work 
save(crypto,file = "data.Rda")