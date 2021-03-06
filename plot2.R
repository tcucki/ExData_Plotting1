##
# Plot2 - Global Active Power from 01 FEB 2007 to 02 FEB 2007
##
plot2 <- function() {
    
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
    
    data <- loadData()

    message("Plotting data")
    ylabs <- "Global Active Power (kilowatts)"
    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    plot(x = data$DateTime, y = data$Global_active_power, lwd = 1, pch = NA, xlab = "", ylab = ylabs)
    lines(x = data$DateTime, y = data$Global_active_power)
    dev.off()
    message("done")
}