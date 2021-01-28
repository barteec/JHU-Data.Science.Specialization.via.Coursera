# Plot.3  
library(lubridate)
library(data.table)

# read data and format data
da <- read.table("household_power_consumption.txt",skip=1,sep=";")
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(power) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

#convert to data.table
power <- data.table(power)

#filter for dates with in data.table
subData <- power[power$Date %in% c("1/2/2007","2/2/2007") ,]

#use base R for date format this time
datetime <- strptime(paste(subData$Date, subData$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
#change to numeric
globalActivePower <- as.numeric(subData$Global_active_power)

#make vectors for sub-Metering 
subMetering1 <- as.numeric(subData$Sub_metering_1)
subMetering2 <- as.numeric(subData$Sub_metering_2)
subMetering3 <- as.numeric(subData$Sub_metering_3)

#make plot for plot3
png("plot3.png", width=480, height=480)
plot(datetime, subMetering1, type="l", ylab="Energy Submetering", xlab="")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))
dev.off()