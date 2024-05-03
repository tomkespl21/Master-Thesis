########################################################
########## Asset Pricing of Crypto Currencies ##########
########################################################


rm(list=ls())

# libraries 
library("tidyverse")  
library("DescTools")  # for winsorizing 
library("readxl")     # self explanatory
library("moments")    # self explanatory
library("envalysis") # publishable graphs 
 


############## Data ###################################### 

# load on data
crypto <- as_tibble(read_excel("weekly_cryptos.xlsx")) 

# change to date format
# doesnt work for some reason (outputs a character)
crypto$date <- ymd(crypto$date)


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
  mutate(lnret = log(close)-log(lag(close)),
         ret = (close - lag(close))/lag(close),
         ret_sqr = ret^2,
         ret_abs = abs(ret),
         hilo = high - low,
         lag_ret = lag(ret),
         lag_ret2 = lag(lag_ret),
         lag_size = lag(marketCap),
         lag_volume = lag(volume),
         lag_vola = lag(ret_sqr))%>% 
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
  mutate(market = sum(marketCap),
         weight = marketCap/market,  
         mkt_ret = sum(weight*ret),
         ln_mkt_ret = sum(weight*lnret))
  

# compute cumulative returns 
# use the summability of log-returns
btc <- crypto[crypto$coin=="BTC",]
btc$cumret <- cumsum(btc$lnret)


eth <- crypto[crypto$coin=="ETH",]
eth$cumret <- cumsum(eth$lnret)




# creat market variable
mkt <- crypto[crypto$coin == "BTC",] # to get mkt once for all dates
mkt$mkt_cumret <- cumsum(mkt$ln_mkt_ret)
mkt$mkt_ret_sqr = mkt$mkt_ret^2
mkt <- mkt[,c(1,19,21:24)]



 

# sp500 data for comparison  
spx <- as_tibble(read_csv("SPX.csv",col_names = TRUE))
spx <- 
  spx %>% 
  mutate(spx_lnret = log(Close) - log(lag(Close)),
         spx_ret = (Close - lag(Close))/lag(Close)) %>% 
  na.omit() %>% 
  mutate(spx_cumret = cumsum(spx_lnret)) %>% 
  rename(date = Date)

spx$date <- as.character(spx$date)

test <- left_join(crypto,spx)




# moments of crypto market
mean_crypto <- mean(mkt$mkt_ret)
sd_crypto   <- sd(mkt$mkt_ret)
skew_crypto <- skewness(mkt$mkt_ret)
kurt_crypto <- kurtosis(mkt$mkt_ret)

# moments sp500
mean_sp <- mean(spx$spx_ret)
sd_sp   <- sd(spx$spx_ret)
skew_sp <- skewness(spx$spx_ret)
kurt_sp <- kurtosis(spx$spx_ret)

# moments bitcoin 
mean_btc <- mean(btc$ret)
sd_btc   <- sd(btc$ret)
skew_btc <- skewness(btc$ret)
kurt_btc <- kurtosis(btc$ret)


# agostino test for skewness 
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



################# PLOTS ################################


# histogram with normal distribution 
h <- hist(crypto$ret,breaks = 60)
xfit <- seq(min(crypto$ret), max(crypto$ret), length = 50) 
yfit <- dnorm(xfit, mean = mean(crypto$ret), sd = sd(crypto$ret)) 
yfit <- yfit * diff(h$mids[1:2]) * length(crypto$ret) 

lines(xfit, yfit, col = "blue", lwd = 1)





btc$date <- ymd(btc$date)
eth$date <- ymd(eth$date)
mkt$date <- ymd(mkt$date)
spx$date <- ymd(spx$date)

spx <- spx[-4,]
spx$btcret <- btc$cumret
spx$mktret <- mkt$mkt_cumret

ggplot(spx,aes(x=date))+
  geom_line(aes(y=btcret),color="darkred")+
  geom_line(aes(y=mktret),color="steelblue")+
  geom_line(aes(y=spx_cumret),color="chartreuse4")+
  labs(
    title = "Comparison Cumulative Returns",
    #subtitle = 
    caption = "Sum of logarithmic returns of BTC,SP500 and crypto-market portfolio",
    x = "Date",
    y = "Return") +
  theme_publish()

# time series returns 
# make one plot out of these ! 

# make same ret axis ! 

p1 <- ggplot(data=btc,aes(x=date,y=ret))+
             geom_line()+
             theme_publish()

p2 <- ggplot(data=eth,aes(x=date,y=ret))+
             geom_line()+
             theme_publish()

p3 <- ggplot(data=mkt,aes(x=date,y=mkt_ret))+
             geom_line()+
             theme_publish()

p4 <- ggplot(data=spx,aes(x=date,y=spx_ret))+
             geom_line()+
             theme_publish()


grid.arrange(p1,p2,p3,p4,ncol=2)






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
# sort coins into quantiles by lagged size 
size_t <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(marketCap,5)) %>% 
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


SMB <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(marketCap,5))
  summarise(SMB = ret_portf[quantiles == 1] - ret_portf[quantiles == 5]) # long: small size ; short: large size

crypto <- left_join(crypto,SMB)

# momentum factor 
MOM <- 
  mom_data2 %>% 
  group_by(date) %>% 
  summarise(MOM = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: high ret ; short: low ret

crypto <- left_join(crypto,MOM)




# volatility factor 
high <- 
  vol_data2 %>% 
  filter(quantiles == 5)

low <- 
  vol_data2 %>% 
  filter(quantiles == 1)

HML <- high$ret_portf - low$ret_portf

X = cbind(SMB[,2],MOM,AMF,HML)


FMB(R,X,0)



########## Freyberger et. al. ############################





















############### Factor models ##################################


# 1 Factor model (CAPM)

reg1 <- lm(crypto$ret ~ crypto$market_return)

summary(reg1)


# 3 Factor model: RET = MKT + SMB + MOM 


reg2 <- lm(ret ~ market_return + SMB + MOM  ,data = crypto)

summary(reg2)
  


# 5 Factor model: RET = MKT + SMB + MOM + VOLATILITY + VOLUME 





# Principal Component factor model : 









