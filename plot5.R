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
  # library(plyr)
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

# plot 3
plot3 <- function(NEIData=NULL) {
  loadLibraries()
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  NEIDataBaltimore<-subset(NEIData, fips == "24510")
  png("plot3.png", width=500, height=500)
  ggplot3 <- ggplot(NEIDataBaltimore,aes(factor(year),Emissions,fill=type)) + theme_bw() + guides(fill=FALSE) + geom_bar(stat="identity") + facet_grid(.~type,scales = "free",space="free") + labs(title=expression("PM"[2.5]*" Emissions in Baltimore City")) + labs(x="year", y=expression("Total PM"[2.5]*" Emission in Tons"))
  print(ggplot3)
  dev.off()
}

# plot 4
plot4 <- function(NEIData=NULL,SCCData=NULL) {
  loadLibraries()
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  if(is.null(SCCData)) SCCData <- loadData("SCC")
  combustionSCCData <- grepl("comb", SCCData$SCC.Level.One, ignore.case=TRUE)
  coalRelatedSCCData <- grepl("coal", SCCData$SCC.Level.Four, ignore.case=TRUE) 
  coalCombustionSCCData <- (combustionSCCData & coalRelatedSCCData)
  combustionSCC <- SCCData[coalCombustionSCCData,]$SCC
  combustionNEI <- NEIData[NEIData$SCC %in% combustionSCC,]
  png("plot4.png", width=500, height=500)
  ggplot4 <- ggplot(combustionNEI,aes(factor(year),Emissions/10^5)) + geom_bar(stat="identity",fill="grey",width=0.75) + theme_bw() +  guides(fill=FALSE) + labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across U.S.")) + labs(x="year", y=expression(Total~PM[2.5]*~Emission~(10^{5}~Tons)))
  print(ggplot4)
  dev.off()
}

# plot 5
plot5 <- function(NEIData=NULL,SCCData=NULL) {
  loadLibraries()
  if(file.exists("summarySCC_PM25.rds")==FALSE) initialdownloadunzip()
  if(is.null(NEIData)) NEIData <- loadData("NEI")
  if(is.null(SCCData)) SCCData <- loadData("SCC")
  vehicles <- grepl("vehicle", SCCData$SCC.Level.Two, ignore.case=TRUE)
  vehiclesSCCData <- SCCData[vehicles,]$SCC
  vehiclesNEIData <- NEIData[NEIData$SCC %in% vehiclesSCCData,]
  vehiclesNEIDataBaltimore <- vehiclesNEIData[vehiclesNEIData$fips==24510,]
  png("plot5.png", width=500, height=500)
  ggplot5 <- ggplot(vehiclesNEIDataBaltimore,aes(factor(year),Emissions)) + theme_bw() +  guides(fill=FALSE) + geom_bar(stat="identity",fill="grey",width=0.75) + labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore")) + labs(x="year", y=expression(Total~PM[2.5]*~Emission~(10^{5}~Tons)))
  print(ggplot5)
  dev.off()
}

# Plot all for faster testing
plotall <- function(NEIData=NULL,SCCData=NULL){
  plot1()
  plot2()
  plot3()
  plot4()
  plot5()
}

plot5()