temp <- activity.manipulated[activity.manipulated.weekdays=="Weekday",]
meansteps.weekday <- tapply(temp$steps, temp$interval, mean, simplify=TRUE)
meansteps.weekday <- data.frame(meansteps.weekday, "Weekday")
colnames(meansteps.weekday) <- c("Interval", "MeanSteps")

temp <- activity.manipulated[activity.manipulated.weekdays=="Weekend",]
meansteps.weekend <- tapply(temp$steps, temp$interval, mean, simplify=TRUE)
meansteps.weekend <- data.frame(meansteps.weekend, "Weekend")
colnames(meansteps.weekend) <- c("Interval", "MeanSteps")





splitmarks <- interaction(activity.manipulated$interval, activity.manipulated.weekdays)
splitdata <- split(activity.manipulated, splitmarks)
plot.data <- sapply(splitdata, function(x) mean(x$steps))

