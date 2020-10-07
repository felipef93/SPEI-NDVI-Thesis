setwd("C:/Users/ACER/Desktop/Thesis/Writepart")
library(ggplot2)
data<-read.csv("Figure5.csv") #Average precipitation and max temperature in California

t<-matrix(data=data[,2],nrow=12,ncol=5) #Indicates column to be read
colnames(t)<-c("pmean","pmin","pmax","tmax","tmin") #Naming the column in the matrix
rownames(t)<-month.abb 
t<-as.data.frame(t)
n<-c(month.abb)

#Plot of temperature and precipitation
ggplot(t,aes(x=1:12))+geom_col(aes(y=pmean*200),fill="blue",colour="black")+
                      geom_line(aes(y=tmax),colour="red",size=1.5)+
                      scale_x_continuous(name = "Month", breaks = 1:12, labels = n)+
                      scale_y_continuous(name = "Max Temperature (°C)", 
                            sec.axis = sec_axis(~./200, name = "% of yearly precipitation", 
                                       labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  theme(
    axis.title.y = element_text(color = "red"),
    axis.text.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"))




