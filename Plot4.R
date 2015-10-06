# set the working directory
setwd("")
# read the downloaded two files 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#load necessary packages
library(dplyr)
library(ggplot2)

#subset the data, which has "Coal" combustion from data frame SCC. Please note, 
#we can also subset the data from EI.Sector, or SCC.Level.Four etc, however, the resulting trend of 
#the plots from different subset methods are very similar. Therefore I choose SCC.Level.Three from SCC data frame 
#because it covers the largest number of "Coal" strings

coalDF<- SCC[grepl('(?i)(?=.*\\bcoal\\b)', SCC$SCC.Level.Three, perl=T), ]
coalDFmerged<-merge(NEI,coalDF,by.x="SCC",by.y="SCC")

# calculate the total PM2.5 emissions from coal combustion for each year and make a summary data
coalDFsubset<-select(coalDFmerged,c(SCC,Emissions,type,year))%>%group_by(year)%>%
        summarise(totalCoalPM2.5=sum(Emissions))

png(filename="Plot4.png",width = 480, height = 480, units = "px", pointsize = 12,bg="white")
ggplot(coalDFsubset,aes(factor(year),totalCoalPM2.5))+
        geom_bar(stat="identity",width=0.8,fill="lightblue")+
        scale_x_discrete("Year")+
        scale_y_continuous("Total Emission from Coal (ton)")+
        labs(title="Coal combustion trend in the U.S.")+
        stat_smooth(aes(group=1),method="lm", se=FALSE, color="orange", size=1.5)+
        theme_bw()+
        theme(axis.text=element_text(size=12),axis.text.y=element_text(angle=90,hjust=1,vjust=0.5))
        
dev.off()
---------------------------------------------------------------------------------------
#calcualte the total number of 'coal'related combustion
length(grep("Coal",SCC$EI.Sector))
length(grep("Coal",SCC$SCC.Level.Three))
length(grep("Coal",SCC$SCC.Level.Four))

#make exact matches to "coal"string
length(grep('(?i)(?=.*\\bcoal\\b)', SCC$EI.Sector, perl=T, value=T))
length(grep('(?i)(?=.*\\bcoal\\b)', SCC$SCC.Level.Three, perl=T, value=T))
length(grep('(?i)(?=.*\\bcoal\\b)', SCC$SCC.Level.Four, perl=T, value=T))

#investigate differnet ways of including the "coal"
coalDF<- SCC[grepl('(?i)(?=.*\\bcoal\\b)', SCC$Short.Name, perl=T), ]

coalDFmerged<-merge(NEI,coalDF,by.x="SCC",by.y="SCC")

coalDFsubset<-select(coalDFmerged,c(SCC,Emissions,type,year))%>%group_by(year)%>%
        summarise(totalCoalPM25=sum(Emissions))

coalDF2<- SCC[grepl('(?i)(?=.*\\bcoal\\b)', SCC$EI.Sector, perl=T), ]
coalDFmerged2<-merge(NEI,coalDF2,by.x="SCC",by.y="SCC")
coalDFsubset2<-select(coalDFmerged2,c(SCC,Emissions,type,year))%>%group_by(year)%>%
        summarise(totalCoalPM25=sum(Emissions))



coalDF4<- SCC[grepl('(?i)(?=.*\\bcoal\\b)', SCC$SCC.Level.Four, perl=T), ]
coalDFmerged4<-merge(NEI,coalDF4,by.x="SCC",by.y="SCC")
coalDFsubset4<-select(coalDFmerged4,c(SCC,Emissions,type,year))%>%group_by(year)%>%
        summarise(totalCoalPM25=sum(Emissions))