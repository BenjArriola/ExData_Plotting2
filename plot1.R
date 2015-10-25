# Initial Download, and unzipping of data files.
# Calls this function only when the data file is missing.
# Downloads and unzips
initialdownloadunzip <- function(){
  #local git
  # mywd <- "C:/Users/Benj Arriola/Dropbox/Personal/Data Science/ExData_Plotting2"
  # my external drive
  # mywd <- "F:/Coursera/Data Science Specialization/04 Exploratory Data Analysis/Project 2"
  # setwd(mywd)
  
  dataDownloadURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(dataDownloadURL,"PM25EmissionData.zip")
  unzip("PM25EmissionData.zip")
}

# Load the data. This functions takes paramter values NEI or SCC and loads
# their correponsing dataframe files and returns the data.
loadData <- function(dataFile) {
  # If the NEI Data is request, it loads the dataframe, sets factors
  # cleans up NA's
  if(dataFile == "NEI"){
    dataReturn <- readRDS("summarySCC_PM25.rds")
    columnFactors <- c("year", "type", "Pollutant","SCC","fips")
    dataReturn[,columnFactors] <- lapply(dataReturn[,columnFactors], factor)
    levels(dataReturn$fips)[1] = NA
    dataReturn<-dataReturn[complete.cases(dataReturn),]
  }
  if(dataFile == "SCC"){
    dataReturn <- readRDS("Source_Classification_Code.rds")
  }
  return(dataReturn)
}

loadLibraries <- function(){
  library(ggplot2)
  library(plyr)
}

# All my plot1.R, plot2.r, plot3.R and plot4.R have the same content
# All my functions are together and only the last function call changes.
# I just found it easier to work this way. Work with 1 file until I was done.

# plot 1 
plot1 <- function(NEIData=NULL) {
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  totalEmission <- aggregate(Emissions ~ year, NEIData, sum)
  png("plot1.png", width=500, height=500)
  barplot((totalEmission$Emissions)/10^6, names.arg=totalEmission$year, xlab="Year", ylab=expression(PM2.5~Emissions~10^{6}~Tons), main="Total PM2.5 Emissions From All US Sources")
  dev.off()
}

# plot 2
plot2 <- function(NEIData=NULL) {
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  NEIDataBaltimore<-subset(NEIData, fips == "24510")
  totalEmissionBaltimore <- aggregate(Emissions ~ year, NEIDataBaltimore, sum)
  totalEmissionBaltimore
  png("plot2.png", width=500, height=500)
  barplot((totalEmissionBaltimore$Emissions)/10^6, names.arg=totalEmissionBaltimore$year, xlab="Year", ylab=expression(PM2.5~Emissions~10^{6}~Tons), main="Total PM2.5 Emissions From All Baltimore City Sources")
  dev.off()
}

# plot 3 line plot
plot3 <- function(NEIData=NULL) {
  loadLibraries()
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  NEIDataBaltimore<-subset(NEIData, fips == "24510")
  # png("plot31.png", width=500, height=500)
  BaltimoreGPlot<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIDataBaltimore)
  BaltimoreGPlot+geom_bar(stat="identity")+
    facet_grid(.~type)+
    labs(x="year", y="Total PM2.5 Emission in Tons") + 
    labs(title="PM2.5 Emissions in Baltimore City")+
    guides(fill=FALSE)
  # dev.off()
}

# plot 4 combo
plot4 <- function(powerConsumptionData=NULL) {
  if(file.exists("household_power_consumption.txt")==FALSE) initialdownloadunzip()
  if(is.null(powerConsumptionData)) powerConsumptionData <- loadData()
  png("plot4.png", width=400, height=400)
  par(mfrow=c(2,2))
  
  # 1st quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  
  # 2nd quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  # 3rd quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Sub_metering_1, type="l", col="black", xlab="", ylab="Energy sub metering")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_2, col="red")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_3, col="blue")
  legend("topright", col=c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, box.lwd=0)
  
  # 4th quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Global_reactive_power, type="n", xlab="datetime", ylab="Global_reactive_power")
  lines(powerConsumptionData$Time, powerConsumptionData$Global_reactive_power)
  dev.off()
}

# Plot all, 1 to 4 for faster testing
plotall <- function(powerConsumptionData=NULL){
  plot1()
  plot2()
  plot3()
  plot4()
}

plot1()