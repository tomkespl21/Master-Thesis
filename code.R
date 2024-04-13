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
         hilo = high - low) %>% 
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

# histogram with normal distribution 
h <- hist(crypto$ret,breaks = 60)
xfit <- seq(min(crypto$ret), max(crypto$ret), length = 50) 
yfit <- dnorm(xfit, mean = mean(crypto$ret), sd = sd(crypto$ret)) 
yfit <- yfit * diff(h$mids[1:2]) * length(crypto$ret) 

lines(xfit, yfit, col = "blue", lwd = 1)


  
  
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
  filter(coin == "BTC" ) %>%
  mutate(yield = 1 + ret) %>% 
  mutate(cumret = cumprod(yield))

# returns plot 
plot(btc$ret,type = "l")

eth <- 
  crypto %>% 
  filter(coin == "ETH") %>%
  mutate(yield = 1 + ret) %>% 
  mutate(cumret = cumprod(yield))

plot(eth$ret,type="l")

mkt <- 
  crypto %>% 
  filter(coin == "ETH") %>%
  mutate(yield = 1 + market_return) %>% 
  mutate(cumret = cumprod(yield)) %>% 
  select(date,ret,yield,cumret)

plot(mkt$ret,type = "l")
 
# plots --> eth seems wrong  
ggplot(data = btc, aes(x = date,y=cumret)) +
  geom_line() 

ggplot(data = eth, aes(x = date,y=cumret)) +
  geom_line()

ggplot(data = mkt, aes(x = date,y=cumret)) +
  geom_line()


# load in risk free rate 
rf <- as_tibble(read_csv("DTB3.csv"))
rf$DTB3 <- as.numeric(rf$DTB3)


# rate is in percent, therefore divide by 100 
rf <- 
  rf %>% 
  mutate(yield = DTB3/100) %>% 
  rename(date = DATE)

# join risk free with crypto data 
data <- left_join(crypto, rf)

# compute excess return               
data <- 
  data %>% 
  mutate(excess = ret-yield)

 


# sp500 data 
spx <- as_tibble(read_csv("SPX.csv",col_names = TRUE))
spx <- 
  spx %>% 
  mutate(ret = log(Close) - log(lag(Close)),
         yield = 1 + ret,
         cumret = cumprod(yield))



test2 <- left_join(test,sp500)




# compute moments of crypto market returns 
marketreturn <- 
  data %>% 
  group_by(date) %>% 
  summarise(mean = mean(market_return))


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












