# set the working directory
setwd("")
# read the downloaded two files 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

#subset Baltimore and Los Angeles Emission data,  then group them by year and their city name, 
# and finally summarize the data for the total emissions of each year for each city
motorDF<-NEI%>% filter(fips=="24510"|fips=="06037")%>%filter(type=="ON-ROAD")%>%
        group_by(year,fips)%>%
        summarise(totalMotorEmission=sum(Emissions))

motorDF[,1:2]<-lapply(motorDF[,1:2],as.factor)
#check the classes for each variable
lapply(motorDF,class)

#change the names of the factor levels
levels(motorDF$fips)[levels(motorDF$fips)=="06037"]<-"Los Angeles"
levels(motorDF$fips)[levels(motorDF$fips)=="24510"]<-"Baltiimore"

#make a graph
png(filename="Plot6.png",width = 480, height = 480, units = "px", pointsize = 12,bg="white")
ggplot(motorDF,aes(year,totalMotorEmission))+
        geom_point(size=4,color="black")+
        facet_grid(.~fips)+
        geom_smooth(method="lm", se=FALSE, col="red",aes(group=1))+
        labs(x="Year",y="Total Emission (ton)",title="Motor Vehicle Emissions")+
        ggtitle("Motor Vehicle Emission trend")+
        theme_bw()+
        theme(axis.title=element_text(size=12.5))+
        theme(plot.title = element_text(size = rel(1.2),face="bold"))+
        theme(axis.text.x=element_text(size=rel(1.1)))+
        theme(strip.background=element_rect("pink"), strip.text.x=element_text(size=14,face="bold"))

dev.off()
----------------------------------------------------------------------------------------
# Code to select data omitted to comply with the requirement to not post a full 
# solution in the forums.

# sum by year by fips
byYear <- aggregate(Emissions ~ Year + fips, data = vehicle, sum)
# replace fips with label and rename it
byYear = transform(byYear, fips = as.character(factor(
        fips, levels = c("24510", "06037"), labels = c("Baltimore City", "Los Angeles County")
)))
names(byYear)[2] <- "County"
byYear <- transform(byYear, Emissions = round(Emissions)) # to be able to show rounded values in graph

# create a column of 1999 reference values
byYear$Reference <- byYear$Emissions
byYear$Reference[byYear$County == "Los Angeles County"] <-
        byYear[byYear$Year == 1999 & byYear$County == "Los Angeles County", "Emissions"]
byYear$Reference[byYear$County == "Baltimore City"] <-
        byYear[byYear$Year == 1999 & byYear$County == "Baltimore City", "Emissions"]

# I answer the question by comparing the length of the vertical line from the 
# 1999 reference to the 2008 value. I also compare the area of the respective 
# polygons bounded by the reference line and the emission values.
# Baltimore has seen more change in absolute amounts of PM2.5 emissions between 
# 1999 and 2008, but the values for Los Angeles county have been more variable 
# as shown by the larger area bounded by its polygon.
require(ggplot2)
png(filename = "plot6.png", width=1200, height=800, res=120)
print(
        ggplot(data = byYear, aes(x = Year, y = Emissions, group = County, color = County))  +
                ylab("Tons of PM2.5")  +
                ggtitle("PM2.5 Emissions From Motor Vehicle Related Sources\nBaltimore City vs Los Angeles County")  +
                geom_point(size = 3)  +
                geom_line(linetype = "solid")  +
                geom_line(aes(y = Reference), linetype = "dashed")  +
                geom_text(aes(y = Reference, label = Emissions - Reference, color = County), 
                          hjust = -0.2, vjust = -0.5, angle = 0, size = 3)  +
                geom_linerange(aes(ymin = pmin(Emissions, Reference), ymax = pmax(Emissions, Reference)),
                               linetype = "dashed", stat = "identity")  +
                scale_x_continuous(breaks = seq(1999, 2008, 3), "Year")
)
dev.off()