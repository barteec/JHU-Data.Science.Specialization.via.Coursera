# Plot.1   
library(lubridate)

# read data and format data
da <- read.table("household_power_consumption.txt",skip=1,sep=";")
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(power) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")


# this will be my sub set filterd for dates
subpower <- subset(power,power$Date=="1/2/2007" | power$Date =="2/2/2007")
#set to as.numeric
subpower$Global_active_power <- as.numeric(subpower$Global_active_power)

#calling the basic plot function
png("plot1.png", width=480, height=480)
hist(subpower$Global_active_power,
     col="red",
     main="Global Active Power",
     xlab="Global Active Power(kilowatts)")
#turn off
dev.off()
