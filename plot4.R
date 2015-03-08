##
# Plot4 - Plots the graphics to Global Active Power and SubMetering 1, 2 & 3 from 01 FEB 2007 to 02 FEB 2007
##
plot4 <- function() {
    
    library(data.table)
    library(graphics)
    
    ##
    # Loads and prepares tidy data.
    # Requires household_power_consumption.txt file unzipped in working directory
    ##
    loadData <- function() {
        
        filename <- "household_power_consumption.txt"
        
        readData <- function() {
            
            message("Reading data from file")
            cc <- c("Date", "Character", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric")
            fread(filename, sep = ";", na.strings = c("?"), colClasses = cc, stringsAsFactors = FALSE, data.table = FALSE, showProgress = TRUE)
        }
        
        filterData <- function(data) {

            message("Filtering data")
            data <- data[(data$Date %in% c("1/2/2007", "2/2/2007")),]
            data
        }
        
        adjustData <- function(data) {
            
            message("Adjusting data types")
            data$DateTime <- strptime(paste(data$Date, data$Time, sep = " "), "%d/%m/%Y %H:%M:%s")
            data[,1] <- as.Date(data[,1], "%d/%m/%Y")
            for (i in 3:9) {
                data[, i] <- as.numeric(data[,i], na.rm = TRUE)
            }
            data
        }
        
        message("Loading data")
        d <- readData()
        d <- filterData(d)
        d <- adjustData(d)
        d
    }

    plotGlobalActivePower <- function(data) {
        
        ylabs <- "Global Active Power (kilowatts)"
        plot(x = data$DateTime, y = data$Global_active_power, lwd = 1, pch = NA, xlab = "", ylab = ylabs)
        lines(x = data$DateTime, y = data$Global_active_power)
    }
    
    plotVoltage <- function(data) {
        
        ylabs <- "Voltage"
        xlabs <- "datetime"
        plot(x = data$DateTime, y = data$Voltage, lwd = 1, pch = NA, xlab = xlabs, ylab = ylabs)
        lines(x = data$DateTime, y = data$Voltage)
    }
    
    plotSubMetering <- function(data) {
        
        ylabs <- "Energy sub metering"
        plot(x = data$DateTime, y = data$Sub_metering_1, lwd = 1, pch = NA, xlab = "", ylab = ylabs)
        lines(x = data$DateTime, y = data$Sub_metering_1)
        lines(x = data$DateTime, y = data$Sub_metering_2, col = "red")
        lines(x = data$DateTime, y = data$Sub_metering_3, col = "blue")
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 3, col=c("black", "red", "blue"), box.lwd = 0)
    }
    
    plotGlobalReactivePower <- function(data) {
        
        ylabs <- "Global_reactive_power"
        xlabs <- "datetime"
        plot(x = data$DateTime, y = data$Global_reactive_power, lwd = 1, pch = NA, xlab = xlabs, ylab = ylabs)
        lines(x = data$DateTime, y = data$Global_reactive_power)
    }
    
    data <- loadData()

    message("Plotting data")
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")

    par(mfrow = c(2, 2))
    plotGlobalActivePower(data)
    plotVoltage(data)
    plotSubMetering(data)
    plotGlobalReactivePower(data)
    dev.off()
    
    message("done")
}