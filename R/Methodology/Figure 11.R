library(sf)
library(rgdal)
#library(raster)
#library(ggplot2)
library(tmap)


setwd("C:/Users/ACER/Desktop/Thesis")
#Polygon of the California region
california <-st_read("ArcGIS/California.shp")
california<-st_transform(california, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 

#California boundaries
xrange<-c(-13849820,-12705030) 
yrange<-c(3810852,5133800)

#Creating the grid
grd <- expand.grid(x=seq(from=xrange[1], to=xrange[2], by=80000), y=seq(from=yrange[1], to=yrange[2], by=80000)) 
grd <- SpatialPoints(coords=grd,proj4string = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#Plot of the grid of the shape of california along with the grid
tm_shape(california)+tm_borders()+tm_shape(grd)+tm_dots()

library(ggplot2)

set.seed(5)

SPEI <-rnorm(38, mean=0,sd=1)
NDVI<- SPEI*0.2+rnorm(38,0,sd=0.1)
date<- 1982:2019
data<-data.frame(date,NDVI,SPEI);


lm_eqn <- function(df){
  m <- lm(SPEI ~ NDVI, df);
  eq <- substitute(italic(r)~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

ggplot(data,aes(x=date,y=SPEI))+geom_line(color='red')
ggplot(data,aes(x=date,y=NDVI))+geom_line(color='blue')+ylab('NDVI anomalies')

ggplot(data,aes(x=NDVI,y=SPEI))+geom_point()+geom_smooth(method='lm',se=FALSE)+
  annotate("text",x = -0.25, y = 2, label="r = x   ,   p > 0.05")+
  theme(axis.title.x = element_text(colour = "blue"),
        axis.title.y = element_text(colour = "red"))+
  xlab('NDVI anomalies')