# set the working directory
setwd("")
# read the downloaded two files 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(reshape2)
library(ggplot2)

#subset Baltimore Emission data and select "year","type" and "Emission" columns only
baltDF<-NEI%>% 
        filter(fips=="24510")%>%
        select(year,type,Emissions)

# assign year and type columns to be factor variable
baltDF[,1:2]<-lapply(baltDF[,1:2], as.factor)
#chcke the class for each variable
lapply(baltDF, class)

#derive a data frame with calcualted the total emissions for each type and each year
newdf<-baltDF%>%
        group_by(year,type)%>%
        summarise(totalEmissions=sum(Emissions))

#make a wide format of the subset data from a long data frame format using dcast()
#newdf.wide<-dcast(newdf, year~type,value.var="totalEmissions")

library(ggplot2)
png(filename = "Plot3.png",
    width = 480, height = 320, units = "px", pointsize = 12,bg="white")
ggplot(data=newdf,aes(x=year,y=totalEmissions))+
        geom_point(color="black",size=3)+
        facet_grid(.~type)+
        geom_smooth(method="lm", se=FALSE, col="red",aes(group=1))+
        labs(x="Year",y="Total Emission (ton)",title="Baltimore Emissions Trend")+
        theme_bw()+
        theme(axis.title=element_text(size=11.5))
        
dev.off()


#alternative approch using qplot()
qplot(year,totalEmissions, data = newdf, 
      facets = . ~ type,geom=c("point","smooth"),method="lm",se=FALSE,group=1,color="red",
      xlab="Year",ylab="Total Emission (ton)", main="Baltimore Emissions Trend")
