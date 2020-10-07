setwd("C:/Users/ACER/Desktop/Thesis/Writepart")
library(ggplot2)

#Maximum and minimum temperatures at selected stations
bocamin<-c(-8.7,-7.6,-5.2,-3.0,0.3,3.2,6.4,5.9,2.0,-1.3,-5.1,-8.3);
bocamax<-c(5.3,6.9,9.6,13.0,18.1,23.3,28.2,27.9,24.2,18,10,5.1);

brawleymax<-c(21.4,23.3,26.5,30.1,34.7,39.3,41.6,41.3,38.7,32.8,25.8,20.7)
brawleymin<-c(5.6,7.3,9.9,12.7,16.6,20.4,24.7,25.33,21.9,15.6,9.2,5.1)

#All data put in a dataframe
data<-data.frame(month.abb,bocamax,bocamin,brawleymax,brawleymin)

#Plot of Boca Station
ggplot(data,aes(x=1:12))+geom_smooth(aes(y=bocamax), colour="red",method="gam", se=FALSE) + geom_smooth(aes(y=bocamin), colour="blue",method="gam", se=FALSE)+
                    scale_x_continuous(name = "Month", breaks = 1:12, labels = month.abb)+
                    scale_y_continuous(name = "Max Temperature (°C)",breaks = seq(-10,30,5))
#Plot of Brawley Station
ggplot(data,aes(x=1:12))+geom_smooth(aes(y=brawleymax), colour="red",method="gam", se=FALSE) + geom_smooth(aes(y=brawleymin), colour="blue",method="gam", se=FALSE)+
  scale_x_continuous(name = "Month", breaks = 1:12, labels = month.abb)+
  scale_y_continuous(name = "Max Temperature (°C)",breaks = seq(0,45,5))