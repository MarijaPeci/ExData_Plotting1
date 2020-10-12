# load the necessary libraries to read and manage external data files

package_list <- c("tidyverse", "chron")
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(chron)

# upload the dataset to R

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
data_dest <- "household_power.zip"

if(!file.exists(data_dest)){
download.file(url, destfile = data_dest)}

if(!exists("power_data", where = parent.frame())) {
power_data <- read.csv2(unzip(zipfile = data_dest),
                      header = TRUE,
                      nrows = 2075259,
                      na.strings = "?")}

#pick only the rows between 01/02/2007 and 02/02/2007

power_df <- filter(power_data, Date == "1/2/2007" | Date == "2/2/2007")
power_df <- power_df[complete.cases(power_df),]

#add datetime element 

power_df <- mutate(power_df, DateTime = paste(power_df$Date, power_df$Time, sep = " "))

#change the classes of columns

power_df$DateTime <- as.POSIXct(power_df$DateTime, format = "%d/%m/%Y %H:%M:%S")
power_df$Date <- as.Date(power_df$Date, format = "%d/%m/%Y")
power_df$Time <- chron(time = power_df$Time)
nums <- 3:9
power_df[, nums] <- lapply(power_df[,nums], 
                           FUN = function(x) as.double(x)) 
#create the 1st plot

png(filename = "Plot1.png", width = 480, height = 480)
par(mfrow = c(1,1), mar = c(4,4,2,1), oma = c(0, 0, 2, 0))
hist(power_df$Global_active_power, 
     col = "red",
     xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power")

dev.off()
