library("ggplot2")
library("lattice")
library("tidyverse")
library("reshape2")
library("gridExtra")
library(dplyr)


Developing<- read.csv("Data/DevelopingNations.CSV",check.names = FALSE)
Developing[is.na(Developing)]="NA" #Namibia's country code is 'NA', R reads NA as NA value 
Developed<- read.csv("Data/DevelopedNations.CSV",check.names = FALSE)
#Spread of data


plot1<-ggplot(Developed,aes(x=`Country`,y=`Lifeexpectancy`))+
  geom_boxplot()+
  labs(title="Life Expectancy Developed Countries",y="Average Life Expectency")+
  theme_bw()
plot2<-ggplot(Developed,aes(x=`Country`,y=`NeonatalRate`))+
  geom_boxplot()+
  labs(title="Neonatal Rate Developed Countries",y="Neonatal Rate (per 1,000 births)")+
  theme_bw()
plot3<-ggplot(Developed,aes(x=`Country`,y=`InfantMortality`))+
  geom_boxplot()+
  labs(title="Infant Mortality Developed Countries",y="Infant Mortality (per 1,000 births")+
  theme_bw()
library(gridExtra)
grid.arrange(plot1,plot2,plot3)

plot1<-ggplot(Developing,aes(x=`Country`,y=`Lifeexpectancy`))+
  geom_boxplot()+
  labs(title="Life Expectancy Developing Countries",y="Average Life Expectency")+
  theme_bw()
plot2<-ggplot(Developing,aes(x=`Country`,y=`NeonatalRate`))+
  geom_boxplot()+
  labs(title="Neonatal Rate Developing Countries",y="Neonatal Rate (per 1,000 births)")+
  theme_bw()
plot3<-ggplot(Developing,aes(x=`Country`,y=`InfantMortality`))+
  geom_boxplot()+
  labs(title="Infant Mortality Developing Countries",y="Infant Mortality (per 1,000 births")+
  theme_bw()
grid.arrange(plot1,plot2,plot3)




#Begin
par(mfrow=c(1,2))
plot(Developed$InfantMortality,Developed$Lifeexpectancy,xlab="Infant Mortality (per 1,000 births)",ylab="Average Life Expectancy",font.main=1,main="Developed Countries")
plot(Developing$InfantMortality,Developing$Lifeexpectancy,xlab="Infant Mortality (per 1,000 births)",ylab="Average Life Expectancy",font.main=1,main="Developing Countries")
mtext("Infant Mortality as a predictor of Life Expectancy" ,side=3,line=-1,outer = TRUE, font=2)

#as infantmortality rate increases, life expectancy increases

par(mfrow=c(1,2))
plot(Developed$NeonatalRate,Developed$Lifeexpectancy,xlab="Neonatal Mortality (per 1,000 births)",ylab="Average Life Expectancy",font.main=1,main="Developed Countries")
plot(Developing$NeonatalRate,Developing$Lifeexpectancy,xlab="Neonatal Mortality (per 1,000 births)",ylab="Average Life Expectancy",font.main=1,main="Developing Countries")
mtext("Neonatal Mortality as a predictor of Life Expectancy",side=3,line=-1,outer=TRUE,font=2)
#as neonatal rate increases, life expectancy decreases


bwplot(`Patents`~`Country`,data=Developed,main="Boxplot of Developed Countries with yearly Patent Count",xlab="Country")
bwplot(`Patents`~Country,data=Developing,main="Boxplot of Developing Countries with yearly Patent Count",xlab="Country")

xyplot(Patents~Year,data=subset(Developed,Country=="US"),main="Yearly Patents for US")
xyplot(Patents~Year,data=subset(Developing,Country=="CN"),main="Yearly Patents for China")
#Developed Countries
par(mar=c(5,4,3,2))
par(mfrow=c(1,3))
plot(Developed$Patents,Developed$Lifeexpectancy,xlab="Yearly number of Patents",ylab="Average Life Expectancy")
plot(Developed$Patents,Developed$InfantMortality,xlab="Yearly number of Patents",ylab="Infant Mortality (per 1,000 births)")
plot(Developed$Patents,Developed$NeonatalRate,xlab="Yearly number of Patents",ylab="Neonatal Mortality (per 1,000 births)")
mtext("Relationship between Yearly Number of Patents and Dependent Variables for Developed Countries",side=3,line=-2,outer=TRUE,font=2)
#No outward trend can be seen
#Developing Countries
par(mar=c(5,4,3,2))
par(mfrow=c(1,3))
plot(Developing$Patents,Developing$Lifeexpectancy,xlab="Yearly number of Patents",ylab="Average Life Expectancy")
plot(Developing$Patents,Developing$InfantMortality,xlab="Yearly number of Patents",ylab="Infant Mortality (per 1,000 births)")
plot(Developing$Patents,Developing$NeonatalRate,xlab="Yearly number of Patents",ylab="Neonatal Mortality (per 1,000 births)")
mtext("Relationship between Yearly Number of Patents and Dependent Variables for Developing Countries",side=3,line=-2,outer=TRUE,font=2)
#No outward trend can be seen

#Now to look at each plot individually
#Developed Countries
ggplot(Developed, aes(Patents,Lifeexpectancy))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Life Expectancy Rates Developed Countries", x="Yearly Number of Patents", y="Average Life Expectancy" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(Developed, aes(Patents,InfantMortality))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Infant Mortality Developed Countries", x="Yearly Number of Patents", y="Infant Mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(Developed, aes(Patents,NeonatalRate))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Neonatal Mortality Developed Countries", x="Yearly Number of Patents", y="Neonatal mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(subset(Developed,`Country`!="US"), aes(Patents,Lifeexpectancy))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Life Expectancy Rates Developed Countries", x="Yearly Number of Patents", y="Average Life Expectancy" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))


ggplot(subset(Developed,`Country`!="US"), aes(Patents,InfantMortality))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Infant Mortality Developed Countries", x="Yearly Number of Patents", y="Infant Mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(subset(Developed,`Country`!="US"), aes(Patents,NeonatalRate))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Neonatal Mortality Developed Countries", x="Yearly Number of Patents", y="Neonatal mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))




ggplot(Developing, aes(Patents,Lifeexpectancy))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Life Expectancy Rates Developing Countries", x="Yearly Number of Patents", y="Average Life Expectancy" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(Developing, aes(Patents,InfantMortality))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Infant Mortality Developing Countries", x="Yearly Number of Patents", y="Infant Mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))

ggplot(Developing, aes(Patents,NeonatalRate))+
  geom_point(aes(colour = `Country Name`))+
  labs(title= "Neonatal Mortality Developing Countries", x="Yearly Number of Patents", y="Neonatal mortality (per 1,000 births)" )+
  theme(plot.title=element_text(size=9),
        axis.title=element_text(size=8),
        axis.text.x = element_text(angle=90))



library(lattice)
xyplot(Lifeexpectancy~Patents|`Country Name`,data=Developed,ylab="Average Life Expectancy",xlab="Yearly Patents",col="black",main="Average Life Expectency against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(NeonatalRate~Patents|`Country Name`,data=Developed,ylab="Neonatal Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Neonatal Mortality against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(InfantMortality~Patents|`Country Name`,data=Developed,ylab="Infant Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Infant Mortality against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(Lifeexpectancy~Patents|`Country Name`,data=subset(Developed,`Country`!="US"),ylab="Average Life Expectancy",xlab="Yearly Patents",col="black",main="Average Life Expectency against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(NeonatalRate~Patents|`Country Name`,data=subset(Developed,`Country`!="US"),ylab="Neonatal Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Neonatal Mortality against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(InfantMortality~Patents|`Country Name`,data=subset(Developed,`Country`!="US"),ylab="Infant Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Infant Mortality against Patents for Developed Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})


xyplot(Lifeexpectancy~Patents|`Country Name`,data=Developing,ylab="Average Life Expectancy",xlab="Yearly Patents",col="black",main="Average Life Expectency against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(NeonatalRate~Patents|`Country Name`,data=Developing,ylab="Neonatal Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Neonatal Mortality against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(InfantMortality~Patents|`Country Name`,data=Developing,ylab="Infant Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Infant Mortality against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})

xyplot(Lifeexpectancy~Patents|`Country Name`,data=subset(Developing,Country!="IN" & Country != "CN"),ylab="Average Life Expectancy",xlab="Yearly Patents",col="black",main="Average Life Expectency against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(NeonatalRate~Patents|`Country Name`,data=subset(Developing,Country!="IN" & Country != "CN"),ylab="Neonatal Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Neonatal Mortality against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})
xyplot(InfantMortality~Patents|`Country Name`,data=subset(Developing,Country!="IN" & Country != "CN"),ylab="Infant Mortality (Per 1,000 births)",xlab="Yearly Patents",col="black",main="Infant Mortality against Patents for Developing Countries",pch=1,cex=0.5,panel = function(x, y) {
  panel.xyplot(x, y, grid = TRUE,
               type = c("p", "smooth"),
               col.line = "blue")
})



#India and United States
DF<-subset(Developed,`Country`=="US")
DF2<-subset(Developing,`Country`=="IN")
Final<-merge(DF,DF2,all=TRUE)

#Descriptive
summary(DF)
summary(DF2)

#Developed
l1<-lm(Lifeexpectancy~Patents,data=Developed)
summary(l1)
#Not Significant
l2<-lm(NeonatalRate~Patents,data=Developed)
summary(l2)
#Significant
l3<-lm(InfantMortality~Patents,data=Developed)
summary(l3)
#Significant

#Developing
l1<-lm(Lifeexpectancy~Patents,data=Developing)
summary(l1)
#Significant
l2<-lm(NeonatalRate~Patents,data=Developing)
summary(l2)
#Significant
l3<-lm(InfantMortality~Patents,data=Developing)
summary(l3)
#Significant

#Developed without united states
l1<-lm(Lifeexpectancy~Patents,data=subset(Developed,`Country`!="US"))
summary(l1)

l2<-lm(NeonatalRate~Patents,data=subset(Developed,`Country`!="US"))
summary(l2)

l3<-lm(InfantMortality~Patents,data=subset(Developed,`Country`!="US"))
summary(l3)


#Developing Wihout China and India
l1<-lm(Lifeexpectancy~Patents,data=subset(Developing,Country!="IN" & Country != "CN"))
summary(l1)

l2<-lm(NeonatalRate~Patents,data=subset(Developing,Country!="IN" & Country != "CN"))
summary(l2)

l3<-lm(InfantMortality~Patents,data=subset(Developing,Country!="IN" & Country != "CN"))
summary(l3)


par(mfrow=c(1,3))
plot(Developing$Year,Developing$Lifeexpectancy,xlab="Year",ylab="Life Expectancy")
plot(Developing$Year,Developing$NeonatalRate,xlab="Year",ylab="Neonatal Rate")
plot(Developing$Year,Developing$InfantMortality,xlab="Year",ylab="Infant Mortality Rate")
