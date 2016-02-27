repo1 <- function() {
  
  

  data <- read.csv("activity.csv")
  
 meanStepsPerInterval = aggregate(steps ~ interval, data, sum) 
 totalStepsPerDay =  aggregate(steps ~ date, data, sum)
 medianStepsPerDay = aggregate(steps ~ date, data, median)
meanStepsPerDay = aggregate(steps ~ date, data, mean)
  
  #Plot total steps per day
  #with(totalStepsPerDay, plot(date, steps, main="Hello", type="l", pch=12))

  #Plot means steps per inerval
  #with(meanStepsPerInterval, plot(interval, steps, main="Hello", type="l", pch=12))
  
  
  #Number of missing steps is 2304
  #sum(is.na(data$steps))
  
  #Replace NA data with the average of all Steps
  data2 <- data
  data2$steps[which(is.na(data2$steps))] <- mean(data2$steps, na.rm=TRUE)
  
  meanStepsPerInterval2 = aggregate(steps ~ interval, data2, sum) 
  totalStepsPerDay2 =  aggregate(steps ~ date, data2, sum)
  medianStepsPerDay2 = aggregate(steps ~ date, data2, median)
  meanStepsPerDay2 = aggregate(steps ~ date, data2, mean)  
 # with(totalStepsPerDay2, plot(date, steps, main="Hello", type="h", pch=12))
  
  
  day = weekdays(as.Date(data2$date))
  
  data2$day = weekdays(as.Date(data2$date))
  data2["daytype"] <- NA 
  #data2$daytype[data2$day == "Saturday" ,] <- "weekend"
  data2$daytype[which(data2$day=="Saturday" | data2$day=="Sunday")] <- "weekend"
  data2$daytype[which(data2$day=="Monday" | data2$day=="Tuesday" | data2$day=="Wednesday" | data2$day=="Thursday" | data2$day=="Friday")] <- "weekday"
  
  meanStepsPerDay3 = aggregate(steps ~ interval, data2, mean)  
  #par(mfrow = c(2,1))
  with(meanStepsPerDay3, plot(interval, steps, main="Ass 4", type="l"))
  with(subset(meanStepsPerDay3, data2$daytype=="weekday"), points(interval, steps, col="blue"))
  with(subset(meanStepsPerDay3, data2$daytype=="weekend"), points(interval,steps, col="red"))
  legend("topright", pch=1, col = c("blue", "red"), legend=c("Weekday", "Weekend"))  

  data2
  
  

 
}