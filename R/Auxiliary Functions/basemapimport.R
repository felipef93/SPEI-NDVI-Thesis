#Because ggmap and tmap are not compatible, a basemap was imported from ggmap and coverted to a tmap object
#The result of the conversion is an RGB raster for the region

basemap<- function(boundingbox, maptype, zoom =6){
  base<-get_stamenmap(boundingbox, zoom,maptype)
  mgmap <- as.matrix(base)
  vgmap <- as.vector(mgmap)
  vgmaprgb <- col2rgb(vgmap)
  gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
  gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
  gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
  rgmaprgb <- brick(raster(gmapr), raster(gmapg), raster(gmapb))
  projection(rgmaprgb) <- CRS("+init=epsg:4326")
  extenttest <- unlist(attr(base, which = "bb"))[c(2, 4, 1, 3)]
  extenttest[3:4]<-extenttest[3:4]+0.17 #For some reason data has to be adfjusted by 0.17 in california
  extent(rgmaprgb) <- extenttest
  return(rgmaprgb)
}

