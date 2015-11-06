library(dplyr)
library(lubridate)

#read the power data
power_data <- read.csv2("household_power_consumption.txt", stringsAsFactor=FALSE)

#process the data 
power_data <- power_data %>%
    #Transform Date to Date Class and then
    mutate(Date=as.Date(Date,"%d/%m/%Y")) %>%
    #Filter out the required dates
    filter(Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02")) %>%
    #Coerce Global_active power to numeric values
    mutate(Global_active_power=as.numeric(Global_active_power)) %>%
    #Coerce Global_reactive power to numeric values
    mutate(Global_reactive_power=as.numeric(Global_reactive_power)) %>%
    #coerce sub_metering data to numeric
    mutate(Sub_metering_1=as.numeric(Sub_metering_1)) %>%
    mutate(Sub_metering_2=as.numeric(Sub_metering_2)) %>%
    mutate(Sub_metering_3=as.numeric(Sub_metering_3)) %>%
    #add weekday
    mutate(weekday=wday(Date))


#set par
par(mfcol=c(2,2))
par(mex=0.85)
#par(mfg=c(1,1))
#First one is Global active plot
with(power_data, {
    #initiate a plot 
    plot(Global_active_power, xaxt="n", type="n", xlab="", ylab="Global Active Power (kilowatts)", bg="white")
    #add line to it
    lines(Global_active_power)
    #Add custom x-axis to it. add 3 ticks at 10,1440,2870 and label 
    #them as "Thu", "Fri" and "Sat"
    axis(1, at=c(10,1440,2870), labels=c("Thu","Fri","Sat"))
})


#Second one is sub_metering plot
with(power_data, {
    #initiate a plot 
    plot(Sub_metering_1, xaxt="n", type="n", xlab="", ylab="Energy sub metering")
    lines(Sub_metering_1, col="black")
    lines(Sub_metering_2, col="red")
    lines(Sub_metering_3, col="blue")
    #Add custom x-axis to it add 3 ticks at 10,1440,2870 and label 
    #them as "Thu", "Fri" and "Sat"
    axis(1, at=c(10,1440,2870), labels=c("Thu","Fri","Sat"))
    ##add a legend
    legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           lty=1, col=c("black","red","blue"), cex=0.5, bty="n")
})

##Third one
with(power_data, {
    #initiate a plot
    plot(Voltage, xaxt="n", xlab="datetime", ylab="Voltage", type="n")
    lines(Voltage)
    axis(1, at=c(10,1440,2870), labels=c("Thu","Fri","Sat"))
})

#Fourth one
with(power_data,{
    #initiate a plot
    plot(Global_reactive_power, xaxt="n", xlab="datetime", type="n")
    lines(Global_reactive_power)
    axis(1, at=c(10,1440,2870), labels=c("Thu","Fri","Sat"))
})

##copy as a png file##
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()