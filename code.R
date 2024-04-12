########################################################
########## Asset Pricing of Crypto Currencies ##########
########################################################


rm(list=ls())

# libraries 
library("tidyverse")
library("DescTools")  # for winsorizing 
library("readxl")
library("moments")
 


############## Data ###################################### 

# load on data
crypto <- as_tibble(read_excel("weekly_cryptos.xlsx")) 

# save as date format
crypto$date <- as.Date(crypto$date)


# change commas to points: 
comma_to_point <- function(x){
  gsub(",", ".",x)
}

crypto[] <-  lapply(crypto,comma_to_point)

# change to numeric:
crypto[,3:8] <- lapply(crypto[,3:8],as.numeric)


# check types: 
lapply(crypto[], typeof)

#test

# compute variables 

crypto <- 
  crypto %>% 
  arrange(date) %>% 
  group_by(coin) %>% 
  mutate(ret = log(close)-log(lag(close)),
         ret_sqr = ret^2,
         ret_abs = abs(ret),
         hilo = high - low) %>% 
  ungroup()
        
         
  
 



  

# load in risk free rate 
risk_free <- as_tibble(read_csv("us_t_bill.csv",col_names = TRUE))


# rate is in percent, therefore divide by 100 
risk_free <- 
  risk_free %>% 
  mutate(yield = rate/100)

# join risk free with crypto data 
data <- left_join(crypto, risk_free)

# compute excess return               
data <- 
  data %>% 
  mutate(excess = ret-yield)

# compute market returns per date 
data <- 
  data %>% 
  group_by(date) %>% 
  mutate(market = sum(marketCap)) %>% 
  mutate(weight = marketCap/market) %>% 
  mutate(market_return = sum(weight*ret)) 

# plot market returns 
plot(data$date,data$market_return)

# compute log cumulative product returns 
test <- 
  data %>% 
  group_by(date) %>% 
  summarise(mean = mean(market_return)) %>% 
  mutate(mean = 1 + mean) %>% 
  mutate(cumret = cumprod(mean)) %>% 
  mutate(lncumprod_crypto = log(cumret))

sp500 <- as_tibble(read_csv("SPX.csv",col_names = TRUE))

#sp500 <- 
#  sp500 %>% 
#  mutate(yield = 1 + ret) %>% 
#  mutate(cumprod = cumprod(yield)) %>% 
#  mutate(lncumprod_sp = log(cumprod)) %>% 
#  rename(date = observation_date)

test2 <- left_join(test,sp500)




# compute moments of crypto market returns 
marketreturn <- 
  data %>% 
  group_by(date) %>% 
  summarise(mean = mean(market_return))

# filter out btc values
btc <-
  crypto %>% 
  filter(coin == "BTC")

# filter out eth   
eth <- 
  crypto %>% 
  filter(coin == "ETH")

# line plot to compare returns 
plot(x = data$date, y = data$market_return,type="l")
lines(x = sp500$date, y = sp500$ret,col="red")
lines(x = btc$date, y = btc$ret, col = "blue")

# moments of crypto market
mean_crypto <- mean(marketreturn$mean)
sd_crypto   <- sd(marketreturn$mean)
skew_crypto <- skewness(marketreturn$mean)
kurt_crypto <- kurtosis(marketreturn$mean)

# moments sp500
mean_sp <- mean(sp500$ret)
sd_sp   <- sd(sp500$ret)
skew_sp <- skewness(sp500$ret)
kurt_sp <- kurtosis(sp500$ret)

# moments bitcoin 
mean_btc <- mean(btc$ret)
sd_btc   <- sd(btc$ret)
skew_btc <- skewness(btc$ret)
kurt_btc <- kurtosis(btc$ret)








# agostino test for skewness 
agostino.test(marketreturn$mean)

# anscombe test for kurtosis
anscombe.test(marketreturn$mean)

# jarque bera test normality 
jarque.test(marketreturn$mean)




test = Winsorize(data$ret,maxval = 5)




# market factor 










