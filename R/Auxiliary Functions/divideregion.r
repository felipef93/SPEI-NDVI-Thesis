#Function to divide the raster of California in ecoregions
divideregion<-function(listofraster){
  econames<-ecoregions$NA_L3NAME
  na_to_y <- function(x){
    x[is.na(x)] <- -9 #Locations whithin ecoregions that do not have values are set to -9  
    return(x) 
  }
  newlist<-list()
  for (i in econames){
  filterimg<-filter(ecoregions,ecoregions$NA_L3NAME==i)
  newlist[[i]]<-listofraster%>%map(crop,filterimg)%>%map(na_to_y)%>%map(mask,filterimg,updateNA=TRUE, 
                                                                        updatevalue=-99) #Locations outside 
                                                                        #ecoregions set to -99
  }
  return(newlist)
}
