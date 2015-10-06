# set the working directory
setwd("")
# read the downloaded two files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
#subset Baltimore Emission data and then group them by year, finally sum up the total emissions of each year
baltEmission<-NEI%>%filter(fips=="24510")%>%
                    group_by(year)%>%
                    summarise(totalEmissions=sum(Emissions,na.rm=TRUE))

# make the plot to compare the PM25 emissions for 1999,2002,2005 and 2008 in Baltimore
png(filename = "Plot2.png",
    width = 480, height = 480, units = "px", pointsize = 12,bg="white")
barplot(baltEmission$totalEmissions,names=baltEmission$year,space=0.6,
               main="Baltimore total Emissions for each year", 
               xlab="year",ylab="Total Emission (ton)",col="red")

dev.off()        
