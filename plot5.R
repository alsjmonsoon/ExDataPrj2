# set the working directory
setwd("")
# read the downloaded two files 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

#subset Baltimore Emission data and then group them by year, and finally summarize the data for the total emissions of each year
baltDFmotor<-NEI%>% filter(fips=="24510")%>%filter(type=="ON-ROAD")%>%
                    group_by(year)%>%
                    summarise(totalMotorEmission=sum(Emissions))

baltDFmotor$year<-as.factor(baltDFmotor$year)

png(filename="Plot5.png",width = 480, height = 480, units = "px", pointsize = 12,bg="white")
ggplot(baltDFmotor,aes(year,totalMotorEmission))+
        geom_bar(stat="identity",width=0.6,fill="lightblue")+
        scale_x_discrete("Year")+
        scale_y_continuous("Total Emission from Motor Vehicle (ton)")+
        stat_smooth(aes(group=1),method="lm", se=FALSE, color="red", size=1.5)+
        theme_bw()+
        ggtitle("Motor Vehicle Emission trend in Baltimore")+
        theme(plot.title = element_text(size = rel(1.2),face="bold"))+
        theme(axis.text.x=element_text(size=rel(1.1),face="bold"))
dev.off()
