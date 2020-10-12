# load the necessary libraries to read and manage external data files

package_list <- c("tidyverse", "chron", "reshape2", "ggplot2", "grid", "gtable")
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(chron)
library(reshape2)
library(ggplot2)
library(gtable)
library(grid)

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

#add datetime object 

power_df <- mutate(power_df, DateTime = paste(power_df$Date, power_df$Time, sep = " "))

#change the classes of columns

power_df$DateTime <- as.POSIXct(power_df$DateTime, format = "%d/%m/%Y %H:%M:%S")
power_df$Date <- as.Date(power_df$Date, format = "%d/%m/%Y")
power_df$Time <- chron(time = power_df$Time)
nums <- 3:9
power_df[, nums] <- lapply(power_df[,nums], 
                           FUN = function(x) as.double(x)) 
#create the 4th plot

## 1st plot

Sys.setlocale("LC_TIME","English")
p_1 <- ggplot(power_df, aes(x = DateTime, y = Global_active_power)) +
        geom_line() +
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
        labs(x = "", y = "Global Active Power (kilowatts)") +
        theme_classic() + 
        theme(panel.border = element_rect(colour = "black", 
                                          fill = NA, 
                                          size = 1),
              axis.text.y = element_text(angle = 90))

## 2nd plot 
p_2 <- ggplot(power_df, aes(x = DateTime, y = Voltage)) +
        geom_line() +
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
        labs(x = "datetime") +
        theme_classic() + 
        theme(panel.border = element_rect(colour = "black", 
                                          fill = NA, 
                                          size = 1),
              axis.text.y = element_text(angle = 90))

## 3rd plot

sub_power <- power_df[, (7:10)]
sub_pw <- melt(sub_power, id = c("DateTime"))

p_3 <- ggplot(sub_pw) +
        geom_line(aes(x = DateTime, y = value, colour = variable)) +
        scale_colour_manual(values = c("black", "red", "blue")) +
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
        theme_classic() + 
        theme(panel.border = element_rect(colour = "black", 
                                          fill = NA, 
                                          size = 1),
              legend.position = c(1,1),
              legend.justification = c("right", "top"),
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"),
              axis.text.y = element_text(angle = 90)) +
        labs(x = "", y = "Energy sub metering")

## 4th plot

p_4 <- ggplot(power_df, aes(x = DateTime, y = Global_reactive_power)) +
        geom_line() +
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
        labs(x = "datetime") +
        theme_classic() + 
        theme(panel.border = element_rect(colour = "black", 
                                          fill = NA, 
                                          size = 1),
              axis.text.y = element_text(angle = 90))


## Joining graphs

plot1 <- ggplotGrob(p_1)
plot2 <- ggplotGrob(p_2)
plot3 <- ggplotGrob(p_3)
plot4 <- ggplotGrob(p_4)

graph_final <- rbind(cbind(plot1, plot2), cbind(plot3, plot4), size = "first")

## Draw the graph into a file

png(filename = "Plot4.png", width = 480, height = 480)
grid.draw(graph_final)
dev.off()
