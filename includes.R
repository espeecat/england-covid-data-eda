# include file for functions

#
# jason bailey
# 20200602 first commit


FNplotting <- function(la) {
  # data is global var
  # grab data by each LA
  subset_df <- data[data$areaName == la,];
  # image filepath
  path<-paste("./",imagesFolder,"/local-authority-total-cases-",la,".png", sep="")
  # image title
  title <-paste('Positive test cases over time for',la)
  #p <- ggplot(data=subset_df, aes(x = date, y = dailyLabConfirmedCases)) +
  #  geom_line()+ggtitle(title) # for the main title
  #p<-p + scale_x_date(date_breaks = "days", date_labels = "%d-%b")+xlab("Date") + ylab("Number of positive tests")+theme(title =element_text(size=10, face='bold'),axis.text.x = element_text(color = "grey20", size = 5, angle = 45, hjust = .5, vjust = .5, face = "plain"),
  #                                                                                                      panel.grid.minor = element_blank())
  p <- ggplot(data=subset_df, aes(date, dailyLabConfirmedCases)) +
  geom_col()+ggtitle(title) # for the main title
  p<-p + scale_x_date(date_breaks = "weeks", date_labels = "%d-%b")+xlab("Date") + ylab("Number of positive tests")+theme(title =element_text(size=7),axis.text.x = element_text(color = "grey20", size = 5, angle = 45, hjust = .5, vjust = .5, face = "plain"), panel.grid.minor = element_blank())
  # save LA plot
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
}

# PLOT R value
FNplotR <- function(la) {
  # R value plot by LA 
  dfbyLA <- data[data$areaName == la,]
  aggregateByDate<-aggregate(dfbyLA$dailyLabConfirmedCases, by=list(Category=dfbyLA$specimenDate), FUN=sum)
  # Remove N/A and set to zero
  aggregateByDate<-aggregateByDate %>%
    fill(x)
  aggregatesAsXTS <- as.xts(aggregateByDate$x,order.by=as.Date(aggregateByDate$Category))
  weeklySum<-apply.weekly(aggregatesAsXTS ,sum)
  
  # length of x for finite difference
  xlen=length(weeklySum[,1])
  h<-seq(1,xlen)
  
  weeklyCumSum = cumsum(weeklySum[,1])
  weeklyLNCumSum = log(cumsum(weeklySum[,1]))
  r<-finite.differences(h, coredata(weeklyLNCumSum))
  
  #area="Brighton and Hove"
  path<-paste("./",imagesFolder,"/R-value-for-",la,".png", sep="")
  
  df <-data.frame(index(weeklyLNCumSum), r)
  colnames(df)<-c('date','R value')
  title <-paste('Estimated R value for',la)
  p <- ggplot(data=df, aes(x = date, y = `R value`))+geom_point() +ggtitle(title)+theme(title =element_text(size=7 ),   axis.text.x = element_text(color = "grey20", size = 8, angle = 45, hjust = .5, vjust = .5, face = "plain"),
                                                                                        panel.grid.minor = element_blank())
  
  # save LA plot
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
  

}



# finite.differences
# Finite difference taken from:
# https://www.r-bloggers.com/numerical-differentiation-with-finite-differences-in-r/
# Numerical Differentiation with Finite Differences in R
# August 3, 2017
# By Aaron Schlegel
# 
finite.differences <- function(x, y) {
  if (length(x) != length(y)) {
    stop('x and y vectors must have equal length')
  }
  
  n <- length(x)
  
  # Initialize a vector of length n to enter the derivative approximations
  fdx <- vector(length = n)
  
  # Iterate through the values using the forward differencing method
  for (i in 2:n) {
    fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }
  
  # For the last value, since we are unable to perform the forward differencing method 
  # as only the first n values are known, we use the backward differencing approach
  # instead. Note this will essentially give the same value as the last iteration 
  # in the forward differencing method, but it is used as an approximation as we 
  # don't have any more information
  fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  
  return(fdx)
}