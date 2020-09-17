library(sf) #Reads shapefile
library(raster) #Reads raster data
library(purrr) # Working with lists
library(ggplot2) # tidyverse data visualization package
library(stringr) #String manipulations
library(reshape2) #Melt the data
library(tmap) #making maps
library(bannerCommenter) #Organizing my code
library(dplyr) #Pipes

#############################################################################################################
##                          Import shapefiles and rasters for the analysis                                 ##   
#############################################################################################################
banner("Data inputs")
setwd("C:/Users/ACER/Desktop/Thesis/")
california <-st_read("ArcGIS/California.shp") #California shapefile
ecoregions<-st_read("ArcGIS/Ecoregions Simplified/Ecocal_project.shp") #Ecoregions of California Shapefile

#Import map of highest negative and positive correlations 
setwd("C:/Users/ACER/Desktop/Thesis/GEE_Exports/Maxmin - All")
Maxminglob<-sapply(list.files(),raster)
Maxminglob<-map(Maxminglob,crop,california)
Maxminglob<-map(Maxminglob,mask,california)


setwd("C:/Users/ACER/Desktop/Thesis/GEE_exports") #location where my maps are
directories<-grep("NDVI|SPEI",list.dirs(),value= TRUE) #Selection folders with archives consisting of NDVI
                                                       #and SPEI different scenarios studied
directories<-gsub("./","",directories)

#For plotting maps, this is the framing of the region
bbox_cal<-st_bbox(california)
xrange<-bbox_cal$xmax-bbox_cal$xmin
yrange<-bbox_cal$ymax-bbox_cal$ymin
bbox_cal[1] <- bbox_cal[1] - (0.05 * xrange) # xmin - left
bbox_cal[3] <- bbox_cal[3] + (0.05 * xrange) # xmax - right
bbox_cal[2] <- bbox_cal[2] - (0.05 * yrange) # ymin - bottom
bbox_cal[4] <- bbox_cal[4] + (0.05 * yrange) # ymax - top

#Import raster of correlation, separated by positive and negative correlations
myrasters<-locatearchives(directories,california) #Import function
names(myrasters)<-c("positive","negative") #Naming components of list
namesof<-mapply(names,myrasters[[2]])%>%map(rev)%>%str_extract_all('[0-9]+') #Each possible scenario, 
                                                                             #represented in months
#############################################################################################################
##                                Plot of best R values of correlations                                    ##   
#############################################################################################################

#############################################################################################################
##                            Plot of best scenarios for best correlations                                 ##   
#############################################################################################################

#############################################################################################################
##                              Plot unique locations and common locations                                 ##   
#############################################################################################################
banner("Unique and Common Locations")

rasterplot<-getuniqueandcommon(myrasters[[2]]) #HEREIN the raster for the plot are defined, note that:
                                               #1 = Positive raster plots; 2=Negative raster plots

Nodata<-calc(stack(rasterplot[[1]]),sum,na.rm=T)  #No data is a quick fix for representing a layer of no val.
Nodata<-mask(Nodata,Nodata,inverse=TRUE,maskvalue=0, updatevalue=NA) #Its an inverse of cumulative plots
Nodata<-mask(Nodata,california)

paletteunique<-rev(c('#feebe2','#fa9fb5','#f768a1','#c51b8a','#7a0177','#54278f')) #Shades of purple
palettecommon<-c('#a1d99b','#ffffb2')         #Yellow and green

plots<-list()  #Initialization of plots
for (n in 1:4){
  breaking<-values(rasterplot[[1]][[n]])%>%table%>%names%>%as.numeric #Fixed breaks according to month
  palettetot<-c(paletteunique[1:(length(breaking)-2)],palettecommon) #complete palette
  
  plots[[n]]<-tm_shape(rasterplot[[1]][[n]],bbox=bbox_cal)+ 
              tm_raster(style = "fixed", breaks=c(-10,breaking+1),labels = as.character(rasterplot[[2]][[n]]$Var1),
              palette = palettetot, title=names(rasterplot[[1]][n]))+ #Raster plot
              tm_shape(california)+tm_borders()+tm_shape(Nodata)+tm_raster(palette='#969696',legend.show = FALSE)+
              tm_layout(legend.outside = TRUE, legend.title.color='black')+ #Auxiliary shapes
              if(n==3){tm_compass(type = "arrow", position = c(0.0, 0.10),size=1)+ #Legend and adjustments
                       tm_scale_bar(breaks = c(0, 100,200,300), text.size = 1, position=c(0.0,-0.02))}
}

tmap_arrange(plots[[2]],plots[[1]],plots[[4]],plots[[3]],ncol=2,nrow=2) #Map arranged to correct order
#############################################################################################################
##                                         Ecoregions: summary                                             ##   
#############################################################################################################
banner("Ecoregions: summary")

maxminecodiv<-divideregion(Maxminglob) 

maxminpercentage<-list()
maxminpercentage<-maxminecodiv%>%map(map,~length(.x[.x>-1])/length(.x[.x>-10])) #See divide region function
                                                                                #for understanding
maxminecofiltered<-list()
maxminecofiltered<-map2(maxminecodiv, maxminpercentage,filterifinsignificant,0.55) #Getting only the 
                                                          #ecoregions that make the cut of more than 50% area

#Significant rasters values are transformed to vectors, and values of no correlation are removed
maxminvector<-map(maxminecofiltered, map,map, as.vector)%>%map(map,map,~.x[.x>-1])%>%melt
maxminvector<-maxminvector[maxminvector!=0,]

#Vector is split into positives and negatives correlations
vectorbyvariable<-split(maxminvector, maxminvector$L2)

#Organizing values according as they appear in my thesis, not in alphabetical order
econames<-c("Sonoran Desert","Mojave Basin and Range","Central Basin and Range", "Northern Basin and Range",
            "California Coastal Sage, Chaparral, and Oak Woodlands","Central California Valley",
            "Coast Range","Southern and Baja California Pine-Oak Mountains", "Cascades",
            "Eastern Cascades Slopes and Foothills","Klamath Mountains","Sierra Nevada")
for (n in 1:2) vectorbyvariable[[n]]$L1<-factor(vectorbyvariable[[n]]$L1, levels=econames) 

#HEREIN the raster for the plot are defined, note that: 2 = Positive raster plots; 1=Negative raster plots
#plots
p<-ggplot(vectorbyvariable[[1]], aes(x=L3, y=abs(value), fill=L1)) + geom_boxplot(outlier.size=0,outlier.alpha = 0)+
  scale_fill_manual(values=paletteeco)+xlab("")+ylab('Correlation')+ guides(fill=guide_legend(title="Ecoregions"))+scale_y_continuous(n.breaks=6, limits=c(0.3,1))+ theme_light()
#summary by quantiles or mean
aggregate(vectorbyvariable[[1]]$value, list(vectorbyvariable[[1]]$L1), mean)

#############################################################################################################
##                                  Ecoregions: Different scenarios                                        ##   
#############################################################################################################
banner("Ecoregions: Different scenarios")

ecoregdivided<-map(myrasters[[2]],divideregion) #HEREIN the raster for the plot are defined, note that:
                                                #1 = Positive raster plots; 2=Negative raster plots

percentagefillecoregion<-list()# % of area that each scenario covers initialized
percentagefillecoregion<-ecoregdivided%>%map(map,map,~length(.x[.x>-1])/length(.x[.x>-10])) #See divideregion
                                                                        #function for understanding

ecofiltered<-list()
ecofiltered<-map2(ecoregdivided, percentagefillecoregion,map2,filterifinsignificant) #Getting only the 
                                                          #ecoregions that make the cut of more than 50% area
#Renaming the variables in each scenario
for (n in 1:4){
  for (m in 1:12){
    names(ecofiltered[[n]][[m]])<-rev(namesof[[n]])
  }
}
#Significant rasters values are transformed to vectors, and values of no correlation are removed
ecofilteredrvector<-map(ecofiltered, map,map,map, as.vector)%>%map(map,map,map,~.x[.x>-1])%>%melt
ecofilteredrvector<-ecofilteredrvector[ecofilteredrvector$value!=0,]

#Pallete according to my ecoregions figure in the Background section of my work
paletteeco<- c("#006d2c","#515151","#fb0000","#00ff83","#b6ffb5","#80342d","#714320","#fff682",
               "#ff8282","#48600c","#fbe901","#b5ffd8")
names(paletteeco)<-names(percentagefillecoregion[[1]])

#Organizing values according as they appear in my thesis, not in alphabetical order
ecofilteredrvector$L1<-factor(ecofilteredrvector$L1, levels=c("NDVI_Start","NDVI_Range","SPEI_Range",
                                                              "SPEI_lag"))
ecofilteredrvector$L2<-factor(ecofilteredrvector$L2, levels=econames)

#Simplification of labels, as the whole ecoregion would be too big if printed with the figure
loclab<-c("Sonoran","Mojave","Central", "Northern","C.Sage","C.Valley","C.Range","SB Pine-Oak", "Casc.",
          "E.Casc.","Klamath", "Nevada")
names(loclab)<-econames

#Plot of everything in a facet 
plot <- ggplot(ecofilteredrvector, aes(x=L3, y=abs(value), fill=L2)) + 
  geom_boxplot(show.legend=F, outlier.size=0,outlier.alpha = 0)+scale_fill_manual(values=paletteeco)+
  xlab("")+ylab('')+
  facet_grid(L2~L1,scale='free_x',space='free_x', labeller=labeller(L2=loclab)) + scale_y_continuous(n.breaks=3, limits=c(0.4,0.8))+ theme_light()

#Getting a csv file with % occupied by each scenario for each ecoregion
ecopercentage<-melt(percentagefillecoregion)
ecopercentage$L3<-as.numeric(str_extract_all(ecopercentage$L3,'[0-9]+'))
#write.csv(ecopercentage, file = "C:/Users/ACER/Desktop/Thesis/Excel Data/Export R/Percentage_eco_neg.csv")

#############################################################################################################
##                                   Statistical testing: Different Scenarios                              ##   
#############################################################################################################
