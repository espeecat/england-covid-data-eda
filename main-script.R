# main-script.R
# Coronavirus Data -England only by local authority
# Main file to run code

#
# jason bailey
# 20200602 first commit

library('jsonlite')
library('ggplot2')
library('tidyverse')
library('lubridate')
library('xts')


# Open Rproj to setwd() nicely
source("includes.R")

coronavirusdata <-'https://c19downloads.azureedge.net/downloads/json/coronavirus-cases_latest.json'
jsonData<- fromJSON(coronavirusdata)

# Local Authority (LA) data
data<-jsonData$ltlas
data$date<-as.Date(data$specimenDate, "%Y-%m-%d")

# stores images here
imagesFolder <-'media'
inputDataFolder<-'indata'

localLAs <-levels(as.factor(data$areaName))
# plot charts per LA
plots <- lapply(localLAs,FNplotting)

##################################################
# R Value
# Attempt to predict R value using England only data.
# simple estimate of R
# https://en.wikipedia.org/wiki/Basic_reproduction_number#Estimation_methods
# K = d ln(N) / dt
# For exponential growth, N can be interpreted as the cumulative number of diagnoses 

#
# Total Daily England 
#

#############################################
# Estimate and plot R for whole of England
aggregateByDate<-aggregate(data$dailyLabConfirmedCases, by=list(Category=data$specimenDate), FUN=sum)
# Category= date e.g. 2020-04-20
# x is sum across all LAs
# was hoping to see something in autocorrelation at about lag =14 (2 weeks)
# acf(aggregateByDate$x,max.lag=40)

# Remove N/A and set to zero
aggregateByDate<-aggregateByDate %>%
  fill(x)

# change daily totals to Weekly for R value by week estimation. 
# Too much variation in daily rates
aggregatesAsXTS <- as.xts(aggregateByDate$x,order.by=as.Date(aggregateByDate$Category))
weeklySum<-apply.weekly(aggregatesAsXTS ,sum)
# length of x for finite difference
xlen=length(weeklySum[,1])
h<-seq(1,xlen)

weeklyCumSum = cumsum(weeklySum[,1])
weeklyLNCumSum = log(cumsum(weeklySum[,1]))
r<-finite.differences(h, coredata(weeklyLNCumSum))

area="England"
path<-paste("./",imagesFolder,"/R-value-for-",area,".png", sep="")

df <-data.frame(index(weeklyLNCumSum), r)
colnames(df)<-c('date','R value')
title <-paste('Estimated R value for',area)
p <- ggplot(data=df, aes(x = date, y = `R value`))+geom_point() +ggtitle(title)+theme(title =element_text(size=7 ),   axis.text.x = element_text(color = "grey20", size = 8, angle = 45, hjust = .5, vjust = .5, face = "plain"),
                                                                                      panel.grid.minor = element_blank())
# save England R plot
ggsave(
  path,
  plot = p,
  device = "png",
  
  scale = 1,
  width = 8,
  height = 4,
  units = c("in"),
  dpi = 150,
  limitsize = TRUE,
  
)

#############################################
# Estimate and plot R by week for each local authority (LA)
plotsForRVals <- lapply(localLAs,FNplotR)
