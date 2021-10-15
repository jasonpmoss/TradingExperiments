library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(plotly)
library(htmlwidgets)
library(data.table)

source('~/Documents/ggplot_template.R')

Divi <- read_excel("Downloads/StatisticsHistory-SATRIXDIV-2020-08-22.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "text", "numeric", "numeric", "numeric", 
                                         "numeric"))

divi2<-Divi
divi2[1,]

last(divi2)[2]/first(divi2)[2] -1


divi2<- arrange(divi2,Date)
divi2$delta <- divi2$`Closing (c)`- lag(divi2$`Closing (c)`,default=first(divi2$`Closing (c)`))
divi2$margin <- 0.39/2.42 * divi2$`Closing (c)` #could also make it the high or midpoint values


#gearing = 6.05 and margin = 0.39/2.42 = 0.161157

Top40 <- read_excel("Downloads/StatisticsHistory-SATRIX_40-2020-08-22.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "text", "numeric", "numeric", "numeric", 
                                  "numeric"))

RAFI <- read_excel("Downloads/StatisticsHistory-SATRIXRAF-2020-08-22.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric"))

ETFBNDcfd <- read_excel("/Users/jasonpmoss/Downloads/StatisticsHistory-ETFBNDcfd-2021-03-15.xls", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric"))

Divi$`Closing (c)` <- scale(Divi$`Closing (c)`)
RAFI$`Closing (c)` <- scale(RAFI$`Closing (c)`)
Top40$`Closing (c)` <- scale(Top40$`Closing (c)`)

Divi$instrument <- "divi"
RAFI$instrument <- "rafi"
Top40$instrument <- "top40"




Divi %<>% subset(., Date>"2015-01-01")
Top40 %<>% subset(., Date>"2015-01-01")
RAFI %<>% subset(., Date>"2015-01-01")

portfolio<-rbind(Divi, Top40, RAFI)

portfolio_melted <- melt(portfolio,id.vars=c("Date","instrument"))
g<-subset(portfolio_melted, variable == "Closing (c)")
g$variable<-NULL

summary(Divi)
cor(Top40$`Closing (c)`,RAFI$`Closing (c)`)
cor(Divi$`Closing (c)`,RAFI$`Closing (c)`)
cor(Divi$`Closing (c)`,Top40$`Closing (c)`)

var(Top40$`Closing (c)`)
var(Divi$`Closing (c)`)
var(RAFI$`Closing (c)`)

sd(Top40$`Closing (c)`)/mean(Top40$`Closing (c)`)*100
sd(Divi$`Closing (c)`)/mean(Divi$`Closing (c)`)*100
sd(RAFI$`Closing (c)`)/mean(RAFI$`Closing (c)`)*100

(sd(Top40$`Closing (c)`) + sd(Divi$`Closing (c)`) + sd(RAFI$`Closing (c)`))

(sd(Top40$`Closing (c)`) + sd(Divi$`Closing (c)`) + sd(RAFI$`Closing (c)`)) > sd(Top40$`Closing (c)` + Divi$`Closing (c)` + RAFI$`Closing (c)`) # True = diversification benefit

sd(Top40$`Closing (c)` + Divi$`Closing (c)` + RAFI$`Closing (c)`)
mean(Top40$`Closing (c)` + Divi$`Closing (c)` + RAFI$`Closing (c)`)
min(Top40$`Closing (c)` + Divi$`Closing (c)` + RAFI$`Closing (c)`)
max(Top40$`Closing (c)` + Divi$`Closing (c)` + RAFI$`Closing (c)`)

sd(Top40$`Closing (c)`)
sd(Divi$`Closing (c)`)
sd(RAFI$`Closing (c)`)


sd(Top40$`Closing (c)`)/mean(Top40$`Closing (c)`)
sd(Divi$`Closing (c)`)/mean(Divi$`Closing (c)`)
sd(RAFI$`Closing (c)`)/mean(RAFI$`Closing (c)`)

mean(Top40$`Closing (c)`)
mean(Divi$`Closing (c)`)
mean(RAFI$`Closing (c)`)


first(Top40$`Closing (c)`)/mean(Top40$`Closing (c)`)
first(Divi$`Closing (c)`)/mean(Divi$`Closing (c)`)
first(RAFI$`Closing (c)`)/mean(RAFI$`Closing (c)`)
# 
# ggplot(slide7_melted, aes( x=Date, y=value, colour=variable, group=variable )) + 
#   geom_line() +
#   #scale_color_manual(values=c("S"="black","I"="red","R"="orange")) +
#   scale_linetype_manual(values=c("S"="solid","I"="solid","R"="dashed")) +
#   scale_x_date(date_labels = "%b/%d",date_breaks  ="1 week") +
#   chart_attributes


esquisse::esquisser() #create plot with interactive interface
