library(tidyquant)
library(lubridate)
library(fOptions)

cat("\014") 

start_date<- today(tzone = "") - 100
end_date<- today(tzone = "") 

stx40 <- na.omit(getSymbols("stx40.jo", from = start_date, to = end_date, auto.assign = FALSE))
J203 <- na.omit(getSymbols("^j203.jo", from = start_date, to = end_date, auto.assign = FALSE))

sig=sd(stx40$STX40.JO.Close*10)

option_type<- "c"
expiry_date<- date("2021-11-02")
delta <- 0.5481
gearing <- 12.35
option_market_price <- 0.41
K<- 63000/gearing
cover_ratio <- 6000
b<- 0.99 #as.numeric(CoVariance(stx40$STX40.JO.Close,J203$J203.JO.Close)/var(J203$J203.JO.Close))

rfr = 0.16 #this is my "hurdle" rate

S=as.numeric(last(stx40$STX40.JO.Close))*10/gearing
T = as.numeric(expiry_date - today(tzone = ""))/365 #ratio of time to expiry of option
price<-(1/(delta/cover_ratio) *option_market_price) # price for exposure to one unit
number_of_units<- round(1/(delta/cover_ratio)) #of warrants to buy to get exposure to 1 unit of instrument; can use this to calculate "real" option price

my_price<-GBSOption(TypeFlag = option_type, S, X = K, Time = T, r = rfr*T, 
                    b, sigma = sig)
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

print(cbind(c(paste("I'm willing to pay ",round(my_price@price)),
        paste("Standard bank is charging: ",round(price)," with fees of ", round(fees), "at a total cost of ", round(price + fees),sep=" "), ifelse((price+fees) > round(my_price@price),"Too expensive!",paste("Price difference: ", round(my_price@price - price - fees))))))


#2000/number_of_units*(S-K)*gearing