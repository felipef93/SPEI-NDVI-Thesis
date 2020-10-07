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

