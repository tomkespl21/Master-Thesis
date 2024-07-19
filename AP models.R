
# load in crypto data
load("data.Rda")

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



############### Factor models ##################################

# create Return matrix 
R <- 
  crypto %>% 
  group_by(coin,date) %>% 
  dplyr::select(date,coin,ret_ex) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = coin,values_from = ret_ex) %>% 
  filter(id == 1 ) %>% 
  replace(is.na(.), 0)           # replace NA values with zeros 




R = R[-(1:31),3:174]
R = as.matrix(R)


#### Building Factors 

## Size Factor
# sort coins into quantiles by size at time t (not lagged)

SMB <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(marketCap,5)) %>% 
  dplyr::select(date,coin,ret,ret_ex,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  group_by(date) %>% 
  summarise(SMB = ret_portf[quantiles == 1] - ret_portf[quantiles == 5]) # long: small size ; short: large size

# join SMB factor to data set
crypto <- left_join(crypto,SMB)


# compute 
SMB$yield_ex <- 1+SMB$SMB 

length(SMB$SMB[SMB$SMB >0])/length(SMB$SMB)

# Compute cumulative returns of SMB strategy
SMB_cumret = c(1)  
for(i in 2:466){
  SMB_cumret[i] = SMB_cumret[i-1] * SMB$yield_ex[i-1] 
}




# momentum  
MOM <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret,5)) %>% 
  dplyr::select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  dplyr::select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(MOM = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: high ret ; short: low ret


crypto <- left_join(crypto,MOM)

# compute 
MOM$yield_ex <- 1+MOM$MOM 

length(MOM$MOM[MOM$MOM >0])/length(MOM$MOM)

# Compute cumulative returns of SMB strategy
MOM_cumret = c(1) 
for(i in 2:466){
  MOM_cumret[i] = MOM_cumret[i-1] * MOM$yield_ex[i-1] 
}



# Volume High minus low factor
HML <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(volume,5)) %>% 
  dplyr::select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  dplyr::select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(HML = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: high volume ; short: low volume


crypto <- left_join(crypto,HML)

# compute 
HML$yield_ex <- 1+HML$HML 

length(HML$HML[HML$HML >0])/length(HML$HML)

# Compute cumulative returns of SMB strategy
HML_cumret = c(1) 
for(i in 2:466){
  HML_cumret[i] = HML_cumret[i-1] * HML$yield_ex[i-1] 
}



# AGE Factor Young_Minus_OLD
YMO <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(age,5)) %>% 
  dplyr::select(date,coin,ret,quantiles) %>% 
  group_by(date,quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  dplyr::select(date,ret_portf,quantiles) %>% 
  group_by(date) %>% 
  summarise(YMO = ret_portf[quantiles == 5] - ret_portf[quantiles == 1]) # long: young coins ; short: old coins

# create market variable
mkt <- crypto[crypto$coin == "BTC",] # to get mkt once for all dates
mkt <- mkt[,c(1,28,30,31)]
mkt$mkt_cumret <- cumsum(mkt$ln_mkt_ret)
mkt$mkt_ret_sqr = mkt$mkt_ret^2



crypto <- left_join(crypto,YMO)


# compute 
YMO$yield_ex <- 1+YMO$YMO 

length(YMO$YMO[YMO$YMO >0])/length(YMO$YMO)

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
FMB <- function(R2,X, c){
  
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
X <-  as.matrix(cbind(SMB[,2],MOM[,2],HML[,2],YMO[,2],mkt[,3]))
X = X[-(1:31),]

R <- R[,c(1:100)]

# run Fama Macbeth regressions
FMB(R,X,0)

############## Principal Component Regression ################


returns.pca <- prcomp(X, center = TRUE, scale. = TRUE)
summary(returns.pca)
pcafactors <- as.matrix(returns.pca$x)
pcafactors <- pcafactors[,1:5]

reg8 <- lm(R ~ pcafactors)
summary(reg8)

#print(cor(SMB,pcafactors[,1]))
#print(cor(HML,pcafactors[,2]))

pcr_model <- pcr(R~X,scale=T, validation = "CV")

lm(X~pcafactors[,1:5])

summary(pcr_model)


prcomp(X,center = T, scale. = T)




################################# Portfolio Sorting ################################################


## size long short portfolio

# sort coins into quantiles by lagged size 
size_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_size,5)) %>% 
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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
  dplyr::select(date,coin,ret_ex,quantiles)


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
  dplyr::select(date,ret_portf,quantiles)

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
  dplyr::select(date,coin,ret_ex,quantiles)


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
  dplyr::select(date,ret_portf,quantiles)

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

# momentum 3 weeks
mom_sort3 <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_ret3,5)) %>% 
  dplyr::select(date,coin,ret_ex,quantiles)


# summarize mean returns of quantile portfolios
mean_ret_mom3 <- 
  mom_sort2 %>% 
  group_by(date, quantiles) %>% 
  mutate(ret_portf = mean(ret_ex)) %>%
  ungroup(date) %>% 
  summarise(mean= mean(ret_portf)) 


mom_sort33 <- 
  mom_sort3 %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret_ex)) %>% 
  dplyr::select(date,ret_portf,quantiles)

mom3_longshort <- 
  mom_sort33 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

mean(mom3_longshort$longshort)

# t-tests
for(i in 1:5){
  print(t.test(mom_sort33$ret_portf[mom_sort33$quantiles == i]))
}

t.test(mom3_longshort$longshort)



## volume factors
# sort coins into quantiles by lagged size 
volume_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_volume,5)) %>% 
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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


# max_ret sorting 
maxret_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_max_ret,5)) %>% 
  dplyr::select(date,coin,ret,quantiles)

# summarize mean returns of quantile portfolios
mean_ret_maxret <- 
  maxret_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>%
  group_by(quantiles) %>% 
  summarise(mean= mean(ret_portf)) 


maxret_sort2 <- 
  maxret_sort %>% 
  group_by(date, quantiles) %>% 
  summarise(ret_portf = mean(ret)) %>% 
  dplyr::select(date,ret_portf,quantiles)

maxret_longshort <- 
  maxret_sort2 %>% 
  group_by(date) %>% 
  summarise(longshort = ret_portf[quantiles == 5]-
              ret_portf[quantiles == 1])

# t-tests
for(i in 1:5){
  print(t.test(maxret_sort2$ret_portf[maxret_sort2$quantiles == i]))
}
t.test(maxret_longshort$longshort)

# age sorting 
age_sort <- 
  crypto %>% 
  group_by(date) %>% 
  mutate(quantiles = ntile(lag_age,5)) %>% 
  dplyr::select(date,coin,ret_ex,quantiles)

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
  dplyr::select(date,ret_portf,quantiles)

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

