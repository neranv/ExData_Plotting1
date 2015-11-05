library(dplyr)
# Read the power consumption data
power_data <- read.csv2("household_power_consumption.txt", stringsAsFactor=FALSE)

 
#process the data 
power_data <- power_data %>%
                #Transform Date to Date Class and then
                mutate(Date=as.Date(Date,"%d/%m/%Y")) %>%
                #Filter out the required dates
                filter(Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02")) %>%
                #Coerce Global_active power to a numeric values
                mutate(Global_active_power=as.numeric(Global_active_power))

#plot the histogram 
with(power_data, 
     hist(Global_active_power, 
          col  = "red", 
          main = "Global Active Power",
          xlab = "Global Active Power (kilowatts)")
     )

##copy as a png file##
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()