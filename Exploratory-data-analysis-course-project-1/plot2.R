# Plot.2   
library(data.table)

# read data and format data
da <- read.table("household_power_consumption.txt",skip=1,sep=";")
power <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(power) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

#convert to data.table
power <- data.table(power)

#filter for dates 
power <- power[power$Date %in% c("1/2/2007","2/2/2007") ,]

# lets make plot number 2
#use base R for date format this time
datetime <- strptime(paste(power$Date, power$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
#change to numeric
globalActivePower <- as.numeric(power$Global_active_power)

#Make plot2 
png("plot2.png", width=480, height=480)
plot(datetime, globalActivePower, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()