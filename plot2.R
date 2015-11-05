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
    #Coerce Global_active power to a numeric values
    mutate(Global_active_power=as.numeric(Global_active_power)) %>%
    #add weekday
    mutate(weekday=wday(Date))

#plot the data
with(power_data, {
     #initiate a plot 
     plot(Global_active_power, xaxt="n", type="n", xlab="", ylab="Global Active Power (kilowatts)", bg="white")
     #add line to it
     lines(Global_active_power)
     #Add custom x-axis to it
     #add 3 ticks at 10,1440,2870 and label 
     #them as "Thu", "Fri" and "Sat"
     axis(1, at=c(10,1440,2870), labels=c("Thu","Fri","Sat"))
     })

##copy as a png file##
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()


