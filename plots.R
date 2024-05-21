############################################################## 
################ PLOTS Master Thesis #########################
##############################################################

load("data.Rda")


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
