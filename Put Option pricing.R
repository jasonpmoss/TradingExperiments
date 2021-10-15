#--------------------------------Config-------------------------
library(tidyquant)
library(lubridate)
library(fOptions)
library(esquisse)
library(tidyverse)
library(magrittr)
library(DT)

cat("\014") 
options(scipen = 999) 

#--------------------------------API calls-------------------------


start_date<- today(tzone = "") - 100
end_date<- today(tzone = "") 

stx40 <- na.omit(getSymbols("stx40.jo", from = start_date, to = end_date, auto.assign = FALSE))
J203 <- na.omit(getSymbols("^j203.jo", from = start_date, to = end_date, auto.assign = FALSE))

stx40_df<-as.data.frame(stx40)
j203_df<-as.data.frame(J203)

sig = sd(stx40$STX40.JO.Close*10)

#--------------------------------User Inputs-------------------------

auto_close_out_ratio <- 1/3
dividend_witholding_tax <- 0.2
income_tax<-0.4
cash_savings <- 50000

option_type<- "p"
expiry_date<- date("2021-11-02")
delta <- 0.2584
gearing <- 7.04
option_market_price <- 0.42
K= 54000/gearing
cover_ratio <- 6000
b<- 0.99 #as.numeric(CoVariance(stx40$STX40.JO.Close,J203$J203.JO.Close)/var(J203$J203.JO.Close))

rfr = 0.16 #this is my "hurdle" rate

S=as.numeric(last(stx40$STX40.JO.Close))*10/gearing
T = as.numeric(expiry_date - today(tzone = ""))/365 #ratio of time to expiry of option
price<-(1/(delta/cover_ratio) *option_market_price) # price for exposure to one unit
number_of_units<- round(1/(delta/cover_ratio)) #of warrants to buy to get exposure to 1 unit of instrument; can use this to calculate "real" option price

times_cover<- 0.5 #4545/27385

instrument <-  c('STXCFD','STDCFD'  ,'STFCFD',"Cash"   ,"Dummy")
quantity <-    c(860     ,272083    ,1680    ,102992.39,1)
init_margin <- c(9.88     ,0.46      ,3.02    ,1        ,0)
leverage <-    c(6.08    ,6.06      ,6.00    ,0        ,1)

portfolio_summary <- data.frame(instrument, quantity, init_margin, leverage)
portfolio_summary$margin <- portfolio_summary$quantity * portfolio_summary$init_margin
portfolio_summary$exposure <- portfolio_summary$margin * portfolio_summary$leverage

portfolio_gearing<- sum(portfolio_summary$exposure)/sum(portfolio_summary$margin)
portfolio_val<- sum(portfolio_summary$margin)
exposure <- sum(portfolio_summary$exposure)
exposure_coverage <- 1/portfolio_gearing

close_out<- ((sum(portfolio_summary$margin)-portfolio_summary$margin[4])*auto_close_out_ratio) + portfolio_summary$margin[4]
close_out_margin_value <- ((sum(portfolio_summary$margin)-portfolio_summary$margin[4])*(1-auto_close_out_ratio)) 

close_out_max_drawdown<- 1-(exposure - close_out)/exposure
close_out_max_drawdown

#view(portfolio_summary)


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

#paste("I'm willing to pay ",my_price@price
#paste("Standard bank is charging: ",round(price)," with fees of ", round(fees), "at a total cost of ", round(price + fees),sep=" ")
#ifelse((price+fees) > my_price@price,"Too expensive!",paste("Price difference: ", my_price@price - price + fees))

print(cbind(c(paste("I'm willing to pay ",round(my_price@price)),
              paste("Standard bank is charging: ",round(price)," with fees of ", round(fees), "at a total cost of ", round(price + fees),sep=" "), ifelse((price+fees) > round(my_price@price),"Too expensive!",paste("Price difference: ", round(my_price@price - price - fees))))))

#2000/number_of_units*(S-K)*gearing

portfolio_put_option_premium<- round(exposure/(S*gearing) * number_of_units * option_market_price,0) #per quarter
#portfolio_expected_return<-exposure*0.8

#(portfolio_expected_return-portfolio_put_option_premium)/exposure

hypothetical_drawdown<- seq(0,0.5, by=0.01)

option_payouts <- ((K*gearing)-(S*gearing)*(1-hypothetical_drawdown)) * exposure/(S*gearing) * times_cover

option_payouts <- ifelse(option_payouts<0,0,option_payouts)

strik_price_scenarios <- (S*gearing)*(1-hypothetical_drawdown)
exposure_scenarios <- exposure*(1-hypothetical_drawdown)

exposure_scenarios <- if_else(exposure_scenarios <= (exposure - close_out), close_out_margin_value, exposure_scenarios)

portfolio_val_scenarios <- portfolio_val - (exposure-exposure_scenarios)
portfolio_val_scenarios <- if_else(portfolio_val_scenarios<= close_out_margin_value, close_out_margin_value, portfolio_val_scenarios)

portfolio_val_scenarios_protected<- portfolio_val_scenarios + option_payouts
portfolio_val_scenarios_protected_with_cash<- portfolio_val_scenarios_protected + cash_savings
losses<- if_else((exposure-exposure_scenarios) <= close_out, exposure-exposure_scenarios, close_out)
hypothetical_gearing_with_cash <- exposure_scenarios/portfolio_val_scenarios_protected_with_cash
hypothetical_gearing_ex_cash <- exposure_scenarios/portfolio_val_scenarios_protected
dframe<-cbind(hypothetical_drawdown, strik_price_scenarios, option_payouts,losses, exposure_scenarios,portfolio_val_scenarios,portfolio_val_scenarios_protected,portfolio_val_scenarios_protected_with_cash,hypothetical_gearing_with_cash,hypothetical_gearing_ex_cash)
dframe %<>% as.data.frame()
dframe$close_out<- if_else((dframe$losses-dframe$option_payouts)<= close_out,FALSE,TRUE)
library(data.table)
j203_df<-setDT(j203_df, keep.rownames = TRUE)[]

#close_out_max_drawdown<-last(subset(dframe,dframe$close_out==FALSE))[1,1]

g<- dframe  %>%
  ggplot() +
  aes(
    x = hypothetical_drawdown,
    y = hypothetical_gearing_with_cash
  ) + 
  ylim(0, 6) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") + 
  geom_line(aes(x = close_out_max_drawdown), color = "red", linetype = "dotted", labels = close_out_max_drawdown)+
  theme_minimal()

show(g)

graph_label <- paste("Auto Close Out = R",round(close_out_margin_value))

g2<- dframe  %>%
  ggplot() +
  aes(
    x = hypothetical_drawdown,
    y = hypothetical_gearing_ex_cash
  ) + 
  ylim(0, 6) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_line(aes(x = close_out_max_drawdown), color = "red", linetype = "dotted", labels = close_out_max_drawdown)+
  geom_text(aes(close_out_max_drawdown, min(hypothetical_gearing_ex_cash) , label = graph_label, vjust= -0.3, hjust = 0, angle = 90))
  theme_minimal()

show(g2)

hypothetical_gearing <- portfolio_gearing

fees <- 57 * 12
expected_return <- 0.10

dividend_yield<- 0.05

iterations = 20
variables = 8
annual_deposit <- 200000
portfolio_dframe <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  portfolio_dframe[i,1] <- i
  portfolio_dframe[i,2] <- ifelse(i==1, portfolio_val, portfolio_dframe[i-1,8])
  portfolio_dframe[i,3] <- ifelse(i==1, exposure, portfolio_dframe[i,2] * hypothetical_gearing)
  portfolio_dframe[i,4] <- fees
  portfolio_dframe[i,5] <- portfolio_dframe[i,3] * expected_return
  portfolio_dframe[i,6] <- (1-dividend_witholding_tax) * portfolio_dframe[i,3] * dividend_yield
  portfolio_dframe[i,7] <- ifelse(i==1,annual_deposit,portfolio_dframe[i-1,7]*1.1)
  portfolio_dframe[i,8] <- sum(portfolio_dframe[i,2], portfolio_dframe[i,5], portfolio_dframe[i,6], portfolio_dframe[i,7])
}

portfolio_dframe %<>% as.data.frame()

colnames(portfolio_dframe)<- c("Year", "Portfolio Value", "Exposure", "Fees","Capital Gains", "Dividends","Deposit","Closing Balance")
portfolio_dframe$Return<- (portfolio_dframe$`Closing Balance`-portfolio_dframe$Deposit)/portfolio_dframe$`Portfolio Value`-1
portfolio_dframe$ReturnValue <- portfolio_dframe$`Capital Gains` + portfolio_dframe$Dividends - portfolio_dframe$Fees
portfolio_dframe$portfolio_put_option_premium<- round(portfolio_dframe$`Portfolio Value`/(S*gearing) * number_of_units * option_market_price,0)*4*times_cover
portfolio_dframe$adjusted_return<- (portfolio_dframe$`Closing Balance`-portfolio_dframe$Deposit-portfolio_dframe$portfolio_put_option_premium)/portfolio_dframe$`Portfolio Value`-1
portfolio_dframe

plot(dframe$hypothetical_drawdown,dframe$portfolio_val_scenarios_protected)

datatable(dframe)
datatable(portfolio_dframe)

#view(portfolio_dframe)

#esquisse::esquisser()

# ggplot(j203_df, aes(x = rn, y = J203.JO.Close)) + geom_point() +
#   theme_minimal()
# 
# plot(toString(j203_df$rn), j203_df$J203.JO.Close)
# 
# 
# toString(j203_df$rn)
# j203_df$J203.JO.Close
