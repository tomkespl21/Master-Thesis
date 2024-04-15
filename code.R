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



# compute variables 
crypto <- 
  crypto %>% 
  arrange(date) %>% 
  group_by(coin) %>% 
  mutate(ret = log(close)-log(lag(close)),
         ret2 = (close - lag(close))/lag(close),
         ret_sqr = ret^2,
         ret_abs = abs(ret),
         hilo = high - low,
         lag_ret = lag(ret),
         lag_size = lag(marketCap))%>% 
  ungroup() %>% 
  na.omit()

         
  
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




  
# compute market returns per date 
crypto <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(market = sum(marketCap)) %>% 
  mutate(weight = marketCap/market) %>% 
  mutate(market_return = sum(weight*ret))  
  
  # compute cumulative product returns 
btc <- 
  crypto %>% 
  filter(coin == "BTC" ) #%>% 
#  mutate(yield = 1 + ret) %>% 
#  mutate(cumret = cumprod(yield))



eth <- 
  crypto %>% 
  filter(coin == "ETH") # %>%
#  mutate(yield = 1 + ret) %>% 
#  mutate(cumret = cumprod(yield))



# creat market variable
mkt <- 
  crypto %>% 
  filter(coin == "ETH") %>%
  mutate(yield = 1 + market_return) %>% 
  mutate(cumret = cumprod(yield)) %>% 
  select(date,ret,yield,cumret)




 

# sp500 data for comparison  
spx <- as_tibble(read_csv("SPX.csv",col_names = TRUE))
spx <- 
  spx %>% 
  mutate(ret = log(Close) - log(lag(Close)),
         yield = 1 + ret) %>% 
  na.omit() %>% 
  mutate(cumret = cumprod(yield))




#test2 <- left_join(test,sp500)




# moments of crypto market
mean_crypto <- mean(mkt$ret)
sd_crypto   <- sd(mkt$ret)
skew_crypto <- skewness(mkt$ret)
kurt_crypto <- kurtosis(mkt$ret)

# moments sp500
mean_sp <- mean(spx$ret)
sd_sp   <- sd(spx$ret)
skew_sp <- skewness(spx$ret)
kurt_sp <- kurtosis(spx$ret)

# moments bitcoin 
mean_btc <- mean(btc$ret)
sd_btc   <- sd(btc$ret)
skew_btc <- skewness(btc$ret)
kurt_btc <- kurtosis(btc$ret)


# agostino test for skewness 
agostino.test(mkt$ret)
agostino.test(btc$ret)
agostino.test(eth$ret)
agostino.test(spx$ret)

# anscombe test for kurtosis
anscombe.test(mkt$ret)
anscombe.test(btc$ret)
anscombe.test(eth$ret)
anscombe.test(spx$ret)

# jarque bera test normality 
jarque.test(mkt$ret)
jarque.test(btc$ret)
jarque.test(eth$ret)
jarque.test(spx$ret)



################# PLOTS ################################


# histogram with normal distribution 
h <- hist(crypto$ret,breaks = 60)
xfit <- seq(min(crypto$ret), max(crypto$ret), length = 50) 
yfit <- dnorm(xfit, mean = mean(crypto$ret), sd = sd(crypto$ret)) 
yfit <- yfit * diff(h$mids[1:2]) * length(crypto$ret) 

lines(xfit, yfit, col = "blue", lwd = 1)


# time series returns 
# make one plot out of these ! 
plot(btc$ret,type = "l")
plot(eth$ret,type="l")
plot(mkt$ret,type = "l")
plot(spx$ret,type="l")



# cumret plot eth seems wrong !  
ggplot(data = btc, aes(x = date,y=cumret)) +
  geom_line() 

ggplot(data = eth, aes(x = date,y=cumret)) +
  geom_line()

ggplot(data = mkt, aes(x = date,y=cumret)) +
  geom_line()



# load in risk free rate 
#rf <- as_tibble(read_csv("DTB3.csv"))
#rf$DTB3 <- as.numeric(rf$DTB3)

# rate is in percent, therefore divide by 100 
#rf <- 
#  rf %>% 
#  mutate(yield = DTB3/100) %>% 
#  rename(date = DATE)

# join risk free with crypto data 
#data <- left_join(crypto, rf)




############ portfolio sorting ######################


# size and volume and volatility

crypto <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quant_size = ntile(lag_size,5))


size_portfolio <- 
  crypto %>% 
  group_by(date, quant_size) %>% 
  mutate(mean_size = mean(ret)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(mean_size)) 

tstat <- 
  crypto %>% 
  group_by(date, quant_size) %>% 
  mutate(mean_size = mean(ret))  

tstat <- tstat[tstat$quant_size ==5,]
  

t.test(tstat$mean_size)

  





















