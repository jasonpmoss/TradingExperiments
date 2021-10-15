library(tidyquant)
library(lubridate)
library(fOptions)

BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}

xntk <- getSymbols("xntk", from = "2003-01-01", to = "2021-03-14", auto.assign = FALSE)
sd(xntk$XNTK.Adjusted)/mean(xntk$XNTK.Adjusted)

xntk_f <- getSymbols("xntk", from = "2017-01-01", to = "2018-01-01", auto.assign = FALSE)

sphd <- getSymbols("sphd", from = "2020-01-01", to = "2021-03-14", auto.assign = FALSE)

sd(xntk_f$XNTK.Close)/mean(xntk_f$XNTK.Close)
sd(sphd$SPHD.Close)/mean(sphd$SPHD.Close)
sd(sphd$SPHD.Adjusted)/mean(sphd$SPHD.Adjusted)

start_date<- today(tzone = "") - 1000
end_date<- today(tzone = "") 

stxdiv <- na.omit(getSymbols("stxdiv.jo", from = start_date, to = end_date, auto.assign = FALSE))
etfbnd <- na.omit(getSymbols("etfbnd.jo", from = start_date, to = end_date, auto.assign = FALSE))
stxrafi <- na.omit(getSymbols("stxraf.jo", from = start_date, to = end_date, auto.assign = FALSE))
stx40 <- na.omit(getSymbols("stx40.jo", from = start_date, to = end_date, auto.assign = FALSE))
J203 <- na.omit(getSymbols("^j203.jo", from = start_date, to = end_date, auto.assign = FALSE))

sig=sd(stx40$STX40.JO.Close*10)
mean(stxdiv$STXDIV.JO.Close)
sd(etfbnd$ETFBND.JO.Close)/mean(etfbnd$ETFBND.JO.Close)
sd(stxrafi$STXRAF.JO.Close)/mean(stxrafi$STXRAF.JO.Close)
sd(stx40$STX40.JO.Close)/mean(stx40$STX40.JO.Close)

option_type<- "c"
expiry_date<- date("2021-11-02")
delta <- 0.4912
gearing <- 12.52
option_market_price <- 0.39
K= 63000/gearing
cover_ratio <- 6000
b<-as.numeric(CoVariance(stx40$STX40.JO.Close,J203$J203.JO.Close)/var(J203$J203.JO.Close))

rfr = 0.16 #this is my "hurdle" rate

S=as.numeric(last(stx40$STX40.JO.Close))*10/gearing
T = as.numeric(expiry_date - today(tzone = ""))/365 #ratio of time to expiry of option
price<-(1/(delta/cover_ratio) *option_market_price) # price for exposure to one unit
number_of_units<- round(1/(delta/cover_ratio)) #of warrants to buy to get exposure to 1 unit of instrument; can use this to calculate "real" option price

my_price<-GBSOption(TypeFlag = option_type, S, X = K, Time = T, r = rfr*T, 
          b = 1, sigma = sig)
# 
# GBSOption(TypeFlag = "c", S, X = 0, Time = T, r = rfr*T, 
#           b , sigma = sig)

fees_UST <- 250
fees_STRATE <- 10.6
fees_brokerage <- 0.8/100 * price
fees_VAT <- 0.15 * fees_brokerage
fees_insidertradinglevy <- 0.7
fees<- sum(fees_UST,fees_STRATE, fees_brokerage, fees_VAT, fees_insidertradinglevy)

#paste("I'm willing to pay ",my_price@price)
#paste("Standard bank is charging: ",round(price)," with fees of ", round(fees), "at a total cost of ", round(price + fees),sep=" ")
#ifelse((price+fees) > my_price@price,"Too expensive!",paste("Price difference: ", my_price@price - price + fees))


cbind(c(paste("I'm willing to pay ",round(my_price@price)),
  paste("Standard bank is charging: ",round(price)," with fees of ", round(fees), "at a total cost of ", round(price + fees),sep=" "), ifelse((price+fees) > round(my_price@price),"Too expensive!",paste("Price difference: ", round(my_price@price - price - fees)))))
       

#2000/number_of_units*(S-K)*gearing



delta <- function(TypeFlag, S, X, Time, r, b, sigma) {
  d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
  if (TypeFlag == "c")
    delta = exp((b - r) * Time) * pnorm(d1)
  else if (TypeFlag == "p")
    delta = exp((b - r) * Time) * (pnorm(d1) - 1)
  delta
}

delta(TypeFlag = option_type, S, X = K, Time = T, r = rfr*T, b, sigma = sig)

