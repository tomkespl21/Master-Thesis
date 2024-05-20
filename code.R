########################################################
########## Asset Pricing of Crypto Currencies ##########
########################################################


rm(list=ls())

# libraries 
library("tidyverse")  
library("DescTools")  # for winsorizing 
library("readxl")     # self explanatory
library("moments")    # self explanatory
library("envalysis")  # publishable graphs 
library("glmnet")     # lasso 
library("pls")
library("gridExtra")  # for multiplots
library("stargazer")  # nice regression tables
 


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
         lag_ret2 = lag(lag(ret)),                # ret 2 weeks ago
         lag_ret3 = lag(lag(lag(ret))),               # ret 3 weeks ago
         lag_prc  = lag(prc)     ,               # log(prc) last week 
         lag_size = lag(marketCap),              # size last week
         lag_volume = lag(volume),               # volume last week
         lag_retsq = lag(ret_sqr),               # return squared last week
         lag_retabs = lag(ret_abs),              # absolute value of return last week
         lag_max_ret = lag(max_ret),             # max return last week 
         lag_age = lag(age),                     # age of coin last week 
         lag_prcvol = lag(prcvol))%>%            # price * volume last week     
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
  rename(vix = VIXCLS)
vix$date = as.character(vix$date)

crypto <- left_join(crypto,vix)


# gold etf vola index 
gold_vola <- as.tibble(read_excel("gold_etf_vola_index.xls",col_names = T))

gold_vola <- 
  gold_vola %>% 
  fill(GVZCLS)
gold_vola$date <- as.character(gold_vola$date)

crypto <- left_join(crypto,gold_vola)


# us bonds high yield index 
usbond <- as.tibble(read_excel("usbond_highyield_index.xls",col_names = T))

usbond <- 
  usbond %>% 
  rename(usbond_id = BAMLHYH0A0HYM2TRIV) %>% 
  fill(usbond_id)
usbond$date <- as.character(usbond$date)

crypto <- left_join(crypto,usbond)

# 5 year breakeven inflation rate
infl <- as.tibble(read_excel("5year_breakeven_inflationrate.xls",
                             col_names = T))
infl <- 
  infl %>% 
  fill(T5YIE)
infl$date <- as.character(infl$date)

crypto <- left_join(crypto,infl)


# 10Y vs 2Y yield 
yielddiff <- as.tibble(read_excel("10Y_vs_2Y.xls",col_names = T))

yielddiff <- 
  yielddiff %>% 
  fill(T10Y2Y)
yielddiff$date = as.character(yielddiff$date)

crypto <- left_join(crypto,yielddiff)


# oil price 

oil <- as.tibble(read_excel("oil.xls",col_names = T))

oil <- 
  oil %>% 
  fill(DCOILBRENTEU)
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

crypto <- left_join(crypto,rf)

crypto$ret_ex <- crypto$ret - crypto$rf_rate  


### --> now we have dataframe crypto with that we will work 

# compute cumulative returns 
# use the summability of log-returns
btc <- crypto[crypto$coin=="BTC",]
btc$cumret <- cumsum(btc$lnret)

# compute btc share of the market

### make plot about bitcoin marketcap dominance if time allows 

eth <- crypto[crypto$coin=="ETH",]
eth$cumret <- cumsum(eth$lnret)




# create market variable
mkt <- crypto[crypto$coin == "BTC",] # to get mkt once for all dates
mkt$mkt_cumret <- cumsum(mkt$ln_mkt_ret)
mkt$mkt_ret_sqr = mkt$mkt_ret^2
mkt <- mkt[,c(1,28,30,31,50,51)]



## moments of crypto market 
mean_crypto <- mean(mkt$mkt_ret)
sd_crypto   <- sd(mkt$mkt_ret)
skew_crypto <- skewness(mkt$mkt_ret)
kurt_crypto <- kurtosis(mkt$mkt_ret)

## moments sp500
mean_sp <- mean(spx$spx_ret)
sd_sp   <- sd(spx$spx_ret)
skew_sp <- skewness(spx$spx_ret)
kurt_sp <- kurtosis(spx$spx_ret)

## moments bitcoin 
mean_btc <- mean(btc$ret)
sd_btc   <- sd(btc$ret)
skew_btc <- skewness(btc$ret)
kurt_btc <- kurtosis(btc$ret)


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

spx <- spx[-c(1,2),]
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


# plot to show market dominance if bitcoin



############### Factor models ##################################

# create Return matrix 
R <- 
  crypto %>% 
  group_by(coin,date) %>% 
  select(date,coin,ret_ex) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = coin,values_from = ret_ex) %>% 
  filter(id == 1 ) 





R <- as.matrix(R[,3:142])


#### Building Factors 

# Size Factor
# sort coins into quantiles by size at time t (not lagged)

SMB <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(marketCap,5)) %>% 
  select(date,coin,ret,ret_ex,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  group_by(date) %>% 
  summarise(SMB = ret_portf[quantiles == 1] - ret_portf[quantiles == 5]) # long: small size ; short: large size
  

SMB$yield <- 1+SMB$SMB


crypto <- left_join(crypto,SMB)



# momentum factor lag 1 
MOM <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret,5)) %>% 
  select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(MOM = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: high ret ; short: low ret
  

crypto <- left_join(crypto,MOM)




# Volume High minus low factor
HML <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(volume,5)) %>% 
  select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(HML = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: high volume ; short: low volume
  

crypto <- left_join(crypto,HML)


# AGE Factor Young_Minus_OLD
YMO <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(age,5)) %>% 
  select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(YMO = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: young coins ; short: old coins
  


crypto <- left_join(crypto,YMO)


# 1 Factor model (CAPM)

reg1 <- lm(ret ~ mkt_ret,data = crypto)

summary(reg1)


# 3 Factor model: RET = MKT + SMB + MOM 


reg2 <- lm(SMB~mkt_ret, data= crypto)
summary(reg2)

reg3 <- lm(MOM~mkt_ret , data = crypto)
summary(reg3)

reg4 <- lm(ret ~ mkt_ret + SMB + MOM  ,data = crypto)
summary(reg4)




# 5 Factor model: RET = MKT + SMB + MOM + AGE + VOLUME 

reg5 <- lm(YMO ~ mkt_ret + SMB + MOM  ,data = crypto)
summary(reg5)

reg6 <- lm(HML ~ mkt_ret + SMB + MOM  ,data = crypto)
summary(reg6)


reg7 <- lm(ret ~ mkt_ret + SMB + MOM + YMO   ,data = crypto)
summary(reg7)



####################### Fama Macbeth #########################################################



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
    MAPE <- abs(CSmodel$coefficients[1]) +  t(1/residvar)%*%abs(CSmodel$residuals)/N
  }
  
  
  
  results <- list(TS = TSmodel, CS = CSmodel, Shanken = CM)
  return(results)
  
}

# factor matrix 
X = as.matrix(cbind(SMB[,2],MOM1[,2],MOM2[,2],HML[,2],YMO[,2],mkt[,3]))

# run Fama Macbeth regressions
FMB(R[,1:33],X,0)




################################# Portfolio Sorting ################################################


## size long short portfolio

# sort coins into quantiles by lagged size 
size_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_size,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_size <- 
  size_sort %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret_ex)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


# compute mean returns per portfolio per date
size_sort2 <- 
  size_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

# compute long short portfolio 
size_longshort <- 
  size_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
                        ret_portf[quantiles == 1])

# mean of long-short portfolio
mean_size_longshort <- mean(size_longshort$longshort)
  

# t-tests
for(i in 1:5){
  print(t.test(size_sort2$ret_portf[size_sort2$quantiles == i]))
}

t.test(size_longshort$longshort)

#Price sorting 
prc_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_prc,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_prc <- 
  prc_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


prc_sort2 <- 
  prc_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

prc_longshort <- 
  prc_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean_prc_longshort <- mean(prc_longshort$longshort)


# t-tests
for(i in 1:5){
  print(t.test(prc_sort2$ret_portf[prc_sort2$quantiles == i]))
}
t.test(prc_longshort$longshort)

  
## momentum long short portfolio
mom_sort1 <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret,5)) %>% 
  select(date,coin,ret_ex,quantiles)


# summarize mean returns of quantile portfolios
mean_ret_mom1 <- 
  mom_sort1 %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret_ex)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 




mom_sort12 <- 
  mom_sort1 %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

mom1_longshort <- 
  mom_sort12 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean_mom1_longshort <- mean(mom1_longshort$longshort)
# t-tests
for(i in 1:5){
  print(t.test(mom_sort12$ret_portf[mom_sort12$quantiles == i]))
}

t.test(mom1_longshort$longshort)

# momentum 2 weeks
mom_sort2 <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret2,5)) %>% 
  select(date,coin,ret_ex,quantiles)


# summarize mean returns of quantile portfolios
mean_ret_mom2 <- 
  mom_sort2 %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret_ex)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


mom_sort22 <- 
  mom_sort2 %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

mom2_longshort <- 
  mom_sort22 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean(mom2_longshort$longshort)

# t-tests
for(i in 1:5){
  print(t.test(mom_sort22$ret_portf[mom_sort22$quantiles == i]))
}

t.test(mom2_longshort$longshort)


## volume factors
# sort coins into quantiles by lagged size 
volume_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_volume,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_volume <- 
  volume_sort %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret_ex)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


volume_sort2 <- 
  volume_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

volume_longshort <- 
  volume_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean_vol_longshort <- mean(volume_longshort$longshort)

# t-tests
for(i in 1:5){
  print(t.test(volume_sort2$ret_portf[volume_sort2$quantiles == i]))
}
t.test(volume_longshort$longshort)

#Price*volume  sorting 
prcvol_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_prcvol,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_prcvol <- 
  prcvol_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


prcvol_sort2 <- 
  prcvol_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

prcvol_longshort <- 
  prcvol_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean_prcvol_longshort <- mean(prc_longshort$longshort)
# t-tests
for(i in 1:5){
  print(t.test(prc_sort2$ret_portf[prc_sort2$quantiles == i]))
}
t.test(prcvol_longshort$longshort)



## volatility long short portfolio

# sort coins into quantiles by lagged size 
retsq_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_retsq,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_retsq <- 
  retsq_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


retsq_sort2 <- 
  retsq_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

retsq_longshort <- 
  retsq_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])
mean_retsq_longshort <- mean(retsq_longshort$longshort)

# t-tests
for(i in 1:5){
  print(t.test(retsq_sort2$ret_portf[retsq_sort2$quantiles == i]))
}
t.test(retsq_longshort$longshort)


# hilo sorting 
hilo_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_hilo,5)) %>% 
  select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_hilo <- 
  hilo_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


hilo_sort2 <- 
  hilo_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  select(date,ret_portf,quantiles)

hilo_longshort <- 
  hilo_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

# t-tests
for(i in 1:5){
  print(t.test(hilo_sort2$ret_portf[hilo_sort2$quantiles == i]))
}
t.test(hilo_longshort$longshort)

# age sorting 
age_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_age,5)) %>% 
  select(date,coin,ret_ex,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_age <- 
  age_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


age_sort2 <- 
  age_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  select(date,ret_portf,quantiles)

age_longshort <- 
  age_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])
mean_age_longshort <- mean(age_longshort$longshort)

# t-tests
for(i in 1:5){
  print(t.test(age_sort2$ret_portf[age_sort2$quantiles == i]))
}
t.test(age_longshort$longshort)








##########  Lasso ############################


X = as.matrix(cbind(crypto$volume,crypto$hilo,crypto$lag_ret,
          crypto$lag_ret2,crypto$lag_vol,crypto$mkt_ret,crypto$SMB,
          crypto$MOM1, crypto$MOM2, crypto$YMO,crypto$prcvol,crypto$lag_ret3,
          crypto$spx_ret,crypto$vix,crypto$GVZCLS,crypto$usbond_id,
          crypto$T10Y2Y,crypto$DCOILBRENTEU,crypto$HML))

lasso1 <- glmnet(x = X , y=Y , alpha = 1)
lasso_cv <- cv.glmnet(x = X, y = Y,
          ## type.measure: loss to use for cross-validation.
          type.measure = "mse",
          ## K = 10 is the default.
          nfold = 10,
          ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
          alpha = 1)
lasso_cv$lambda.min

coef(lasso_cv, s = lasso_cv$lambda.min)

# adaptive lasso 
best_ridge_coef <- as.numeric(coef(lasso_cv, s = lasso_cv$lambda.min))[-1]
lasso2 <-  glmnet(x = X, y = Y,
                  ## type.measure: loss to use for cross-validation.
                  ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                  alpha = 1,
                  ##
                  ## penalty.factor: Separate penalty factors can be applied to each
                  ##           coefficient. This is a number that multiplies 'lambda' to
                  ##           allow differential shrinkage. Can be 0 for some variables,
                  ##           which implies no shrinkage, and that variable is always
                  ##           included in the model. Default is 1 for all variables (and
                  ##           implicitly infinity for variables listed in 'exclude'). Note:
                  ##           the penalty factors are internally rescaled to sum to nvars,
                  ##           and the lambda sequence will reflect this change.
                  penalty.factor = 1 / abs(best_ridge_coef))
plot(lasso2, xvar = "lambda")

lasso2_cv <- cv.glmnet(x = X, y = Y,
                        ## type.measure: loss to use for cross-validation.
                        type.measure = "mse",
                        ## K = 10 is the default.
                        nfold = 10,
                        ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                        alpha = 1,
                        ##
                        ## penalty.factor: Separate penalty factors can be applied to each
                        ##           coefficient. This is a number that multiplies 'lambda' to
                        ##           allow differential shrinkage. Can be 0 for some variables,
                        ##           which implies no shrinkage, and that variable is always
                        ##           included in the model. Default is 1 for all variables (and
                        ##           implicitly infinity for variables listed in 'exclude'). Note:
                        ##           the penalty factors are internally rescaled to sum to nvars,
                        ##           and the lambda sequence will reflect this change.
                        penalty.factor = 1 / abs(best_ridge_coef),
                        ## prevalidated array is returned
                        keep = TRUE)

lasso2_cv$lambda.min

coef(lasso2_cv, s = lasso2_cv$lambda.min)


ezlasso=function(df,yvar,folds=10,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar,"~.")),data=df)
  x=x[,-1] ##remove intercept
  
  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)
  
  co<-coef(glmnet1,s = "lambda.1se")
  inds<-which(co!=0)
  variables<-row.names(co)[inds]
  variables<-variables[!(variables %in% '(Intercept)')];
  return( c(yvar,variables));
}









############## Principal Component Regression ################


returns.pca <- prcomp(Y, center = TRUE, scale. = TRUE)
summary(returns.pca)
pcafactors <- as.matrix(returns.pca$x)

#print(cor(DOL,pcafactors[,1]))
#print(cor(HML,pcafactors[,2]))

pcr_model <- pcr(Y~X,scale=T, validation = "CV")

lm(X~pcafactors[1:5])


summary(pcr_model)


prcomp(R,center = T, scale. = T)




############ Machine Learning Prediction ###################























