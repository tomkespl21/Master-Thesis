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
         lag_size = lag(marketCap),
         lag_volume = lag(volume),
         lag_vol = lag(ret_sqr))%>% 
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


## size long short portfolio

# sort coins into quantiles by lagged size 
size_data <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_size,5)) %>% 
  select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_size <- 
  size_data %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


size_data2 <- 
  size_data %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles)

# t-tests
for(i in 1:5){
  print(t.test(size_data2$ret_portf[size_data2$quantiles == i]))
}

  
## momentum long short portfolio

# sort coins into quantiles by lagged size 
mom_data <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret,5)) %>% 
  select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_mom <- 
  mom_data %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


mom_data2 <- 
  mom_data %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles)

# t-tests
for(i in 1:5){
  print(t.test(mom_data2$ret_portf[mom_data2$quantiles == i]))
}



## volume long short portfolio

# sort coins into quantiles by lagged size 
volume_data <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_volume,5)) %>% 
  select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_volume <- 
  volume_data %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


volume_data2 <- 
  volume_data %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles)

# t-tests
for(i in 1:5){
  print(t.test(volume_data2$ret_portf[volume_data2$quantiles == i]))
}


## volatility long short portfolio

# sort coins into quantiles by lagged size 
vol_data <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_vol,5)) %>% 
  select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_vol <- 
  vol_data %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


vol_data2 <- 
  vol_data %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles)

# t-tests
for(i in 1:5){
  print(t.test(vol_data2$ret_portf[vol_data2$quantiles == i]))
}


########## Fama Macbeth #########################


# Routine computes FMB estimators with Shanken covariance matrix
# R asset returns
# X Factors
# Choose c=0 to drop constant in second stage
FMB <- function(R,X, c){
  
  X <- as.matrix(X)
  R <- as.matrix(R)
  
  N <- dim(R)[2] #Number of assets
  K <- dim(X)[2]
  T <- dim(R)[1]
  
  # TimeSeries Regression
  TSmodel <- lm(R~X)
  residvar <- colMeans(TSmodel$residuals^2)
  
  if(c ==0){
    
    Betas <- t(as.matrix(TSmodel$coefficients[2:(K+1),]))
    Means <- as.matrix(colMeans(R))
    
    if(K==1){Betas <- t(Betas)}
    colnames(Betas) <- colnames(X)
    
    # CrossSection Regression
    CSmodel <- lm(Means ~ Betas-1)
    
    # Shanken Errors
    Sigma <- cov(TSmodel$residuals)
    Sigmaf <- cov(X)
    gamma <- as.matrix(CSmodel$coefficients)
    c <- t(gamma)%*%solve(Sigmaf)%*%gamma
    B <- solve(t(Betas)%*%Betas)%*%t(Betas)
    Omega <- B%*%Sigma%*%t(B)
    
    CM <- (as.double(1+c)*Omega+Sigmaf)/T
    
    # Compute Pricing Error
    #MAPE <- t(1/residvar)%*%abs(CSmodel$residuals)/N
    
  }
  
  else{
    
    Betas <- t(as.matrix(TSmodel$coefficients[2:(K+1),]))
    Means <- as.matrix(colMeans(R))
    
    if(K==1){Betas <- t(Betas)}
    
    
    # CrossSection Regression
    CSmodel <- lm(Means ~ Betas)
    
    # Shanken Errors
    Sigma <- cov(TSmodel$residuals)
    Sigmaf <- cov(X)
    gamma <- as.matrix(CSmodel$coefficients[2:(K+1)])
    c <- t(gamma)%*%solve(Sigmaf)%*%gamma
    Betas <- cbind(1,Betas)
    B <- solve(t(Betas)%*%Betas)%*%t(Betas)
    Omega <- B%*%Sigma%*%t(B)
    
    # Create bordered matrix
    Sigmaf <- rbind(matrix(0,1,K),Sigmaf)
    Sigmaf <- cbind(matrix(0,(K+1),1),Sigmaf)
    
    CM <- (as.double(1+c)*Omega+Sigmaf)/T
    
    # Compute Pricing Error
    #MAPE <- abs(CSmodel$coefficients[1]) +  t(1/residvar)%*%abs(CSmodel$residuals)/N
  }
  
  
  
  results <- list(TS = TSmodel, CS = CSmodel, Shanken = CM)
  return(results)
  
}

# create Return matrix 
R <- 
  crypto %>% 
  group_by(coin,date) %>% 
  select(date,coin,ret) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = coin,values_from = ret) 

# drop row 313 
R <- R[-313,]

R <- as.matrix(R[,3:54])



# compute size factor 
small <- 
  size_data2 %>% 
  filter(quantiles == 1)  
  

big <- 
  size_data2 %>% 
  filter(quantiles == 5)

SMB = as.vector(small$ret_portf - big$ret_portf)

# momentum factor 
bull <- 
  mom_data2 %>% 
  filter(quantiles == 5)

bear <- 
  mom_data2 %>% 
  filter(quantiles == 1 )

MOM <- bull$ret_portf - bear$ret_portf

# Volume factor 
alot <- 
  volume_data2 %>% 
  filter(quantiles == 5)

 few <- 
  volume_data2 %>% 
  filter(quantiles == 1)

AMF <- alot$ret_portf - few$ret_portf

# volatility factor 
high <- 
  vol_data2 %>% 
  filter(quantiles == 5)

low <- 
  vol_data2 %>% 
  filter(quantiles == 1)

HML <- high$ret_portf - low$ret_portf

X = cbind(SMB,MOM,AMF,HML)


FMB(R,X,0)



########## Freyberger et. al. ############################





















############### Factor models ##################################


# 1 Factor model (CAPM)

reg1 <- lm(crypto$ret ~ crypto$market_return)

summary(reg1)


# 3 Factor model: RET = MKT + SMB + MOM 


# 5 Factor model: RET = MKT + SMB + MOM + VOLATILITY + VOLUME 





# Principal Component factor model : 













