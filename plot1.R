filename <- "exdata-data-NEI_data.zip"


if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(fileURL, filename, method="libcurl")
}  
if (!file.exists("exdata-data-NEI_data")) { 
        
        # Extract to current environment
        unzip(filename) 
}

## Reading input files as suggested 
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
}


# set the working directory
setwd("")
# read the downloaded two files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
#derive a new dataset which sum up the emissions for each year using 'dplyr' package.
newDF<-NEI%>%group_by(year)%>%
          summarise(totalEmissions=sum(Emissions, na.rm=TRUE))

#assign variable "year" as a factor
newDF$year<-as.factor(newDF$year)

png(filename = "Plot1.png",
        width = 480, height = 480, units = "px", pointsize = 12,bg="white")
Plot1<-barplot(newDF$totalEmissions,names=newDF$year,space=0.6,
              main="total Emissions of 4 years in the US", 
              xlab="year",ylab=" ylab=expression('Total PM'[2.5]*' Emissions')",col="red")
           
dev.off()
