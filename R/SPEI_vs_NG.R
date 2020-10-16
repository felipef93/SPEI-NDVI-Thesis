library(sf) #Reads shapefile
library(raster) #Reads raster data
library(purrr) # Working with lists
library(tmap) #making maps
library(ggplot2)
library(RColorBrewer)
library(reshape2)

#############################################################################################################
##                          Import shapefiles and rasters for the analysis                                 ##   
#############################################################################################################
#Import of Shapefiles used in the analysis
setwd("C:/Users/ACER/Desktop/Thesis/")
california <-st_read("ArcGIS/California.shp") #California shapefile
ecoregions<-st_read("ArcGIS/Ecoregions Simplified/Ecocal_project.shp") #Ecoregions of California Shapefile

#Functions for analysis
setwd("C:/Users/ACER/Documents/GitHub/Thesis/R/Auxiliary Functions")
sapply(list.files(),source)

#Import map of highest negative and positive correlations -Section 4.1 of Thesis
setwd("C:/Users/ACER/Desktop/Thesis/NET Growth - GEE exports")
Maxminglob<-sapply(list.files()[list.files()!="All Images"],raster)
Maxminglob<-map(Maxminglob,crop,california)
Maxminglob<-map(Maxminglob,mask,california)

#Import all images of pearson correlation between Net growth and SPEI - Section 4.3 of the thesis
Maxmin<-sapply(paste0("All Images/",list.files("All Images")),raster) #Reads data From wd
Maxmin<-map(Maxmin,crop,california) #Crops all data
metadataimg<-read.csv("C:/Users/ACER/Desktop/Thesis/Excel Data/NGmeta2Y.csv", sep = ';')
names(Maxmin)<-metadataimg$Sim.num

#For plotting maps, this is the framing of the region
bbox_cal<-st_bbox(california)
xrange<-bbox_cal$xmax-bbox_cal$xmin
yrange<-bbox_cal$ymax-bbox_cal$ymin
bbox_cal[1] <- bbox_cal[1] - (0.05 * xrange) # xmin - left
bbox_cal[3] <- bbox_cal[3] + (0.05 * xrange) # xmax - right
bbox_cal[2] <- bbox_cal[2] - (0.05 * yrange) # ymin - bottom
bbox_cal[4] <- bbox_cal[4] + (0.05 * yrange) # ymax - top

#Basemap import: It imports the basemap in as an rgb raster
basmapcal<-basemap(c(left=-125,bottom=32,right=-113,top=43), "watercolor")
#############################################################################################################
##                                Plot of best R values of correlations - Net Growth                                   ##   
#############################################################################################################
#Creates a shape that is the inverse of the locations where  there is a correlation
#Positive
locationspos<-Maxminglob[[1]]
locationspos[is.na(locationspos)]<-0
#Negative
locationsneg<-Maxminglob[[2]]
locationsneg[is.na(locationsneg)]<-0

unusedeco<-ecoregions[c(1,4,8,11),] # Ecoregions that were not taken into account for analysis
breaksreg<-c(0,0.2,0.39,0.6,0.75,1) # Categories of correlations
label<-c('None','Weak','Moderate','Strong','Very Strong') #Label for both graphs

#Plots of best positive and negative correlations (Fig. 15)
globalnegative<-tm_shape(basmapcal)+tm_rgb()+
  tm_shape(Maxminglob[[2]],bbox=bbox_cal)+tm_raster(style = "fixed",breaks=rev(-breaksreg),labels=rev(label),palette='YlOrRd')+
  tm_shape(california)+tm_borders()+
  tm_scale_bar(breaks = c(0, 100,200,300), text.size = 1, position=c(0.0,-0.02))+
  tm_layout(legend.outside = TRUE, legend.title.color='white')
globalpositive<-tm_shape(basmapcal)+tm_rgb()+
  tm_shape(Maxminglob[[1]],bbox=bbox_cal)+tm_raster(style = "fixed",breaks=breaksreg,labels=label,palette='YlOrRd')+
  tm_shape(california)+tm_borders()+
  tm_compass(type = "arrow", position = c(0.0, 0.10),size=1)+
  tm_layout(legend.show = FALSE)

#Get the mean values of the correlations for each ecoregion (Table 3)
meanvalues<-map(Maxminglob,function(image){
  meany<-c()
    for (n in 1:12) {
      t<-image%>%mask(ecoregions[n,]) %>%crop(ecoregions[n,])
      u<-getValues(t)%>%na.omit%>%as.vector
      meany[n]<-mean(u)
    }
  names(meany)<-ecoregions$NA_L3NAME
  return(meany)
})

#Histogram of postive and negative values
valu<-map(Maxminglob,getValues)%>%as.vector
for (n in 1:2) {valu[[n]]<-as.data.frame(cbind(valu[[n]][complete.cases(valu[[n]])]))
names(valu)<-c('data','data')}

pal<-brewer.pal(n = 9, name = 'YlOrRd')
ggplot(data=valu[[1]],aes(x=V1))+geom_histogram(bins=9,aes(y=stat(count)/sum(count)),fill=pal,colour = "black")+xlab("Values")+ylab('Frequency')+theme_minimal()
ggplot(data=valu[[2]],aes(x=V1))+geom_histogram(bins=9,aes(y=stat(count)/sum(count)),fill=rev(pal),colour = "black")+xlab("Values")+ylab('Frequency')+theme_minimal()

categpos<-table(cut(valu[[1]][,1], c(0.2,0.4,0.6,0.75,1)))/length(valu[[1]][,1])
categneg<-table(cut(valu[[2]][,1], c(-0.2,-0.4,-0.6,-0.75,-1)))/length(valu[[2]][,1])
#############################################################################################################
##                                         Ecoregions: summary                                             ##   
#############################################################################################################
byseason<-filterseason(Maxmin,metadataimg,max) #Filter images by season

ecointerest<-ecoregions[c(2,3,5,6,7,9,10,12),] #ecoregions where analysis was done
names1<-ecointerest$NA_L3NAME #names of ecoregions

byecoseason<-list() #Crops the raster by season and ecoregions
for (n in 1:8){
  byecoseason[[n]]<- map(byseason,map,crop,ecointerest[n,])%>%map(map,mask,ecointerest[n,])
}
names(byecoseason)<-names1 #Rename the list with ecoregions names
valuesecoseason<-map(byecoseason,map,map, getValues)%>%map(map,as.vector)%>%map(map,map,na.omit)%>%
                        map(map,map,sort) #Get the values of each season and ecoregion
cumultivearea<-map(valuesecoseason,map,map,function(x){ #Function to compute cumulative area from 0 to 1 
                                  g<-0                  #From highest to lowest values
                                  for (n in 1:length(x)){
                                  g[n]<-(length(x)-n+1)/length(x)
                                  }
                                  return (g)
})
df<-map2(valuesecoseason,cumultivearea,map2,map2,data.frame) #Dataframe with both values and cum area

d<-melt(df, id.vars=c('.x..i..','.y..i..')) #Melt dataframe
names(d)<-c("Correlation","CumSum","SPEI Range","SPEI Season","Ecoregion" ) #Rename each column of data
d$Correlation[d$Correlation<0]<-0 # If looking at minimum >0 and maximum <0
                                  # Equates areas of inverse values to having no correlation
d$Correlation<-abs(d$Correlation) #Computes absolute values for nagative correlations
d$CumSum<-d$CumSum #For negative subtract 1.01-d$CumSum
d$`SPEI Range`<-factor(d$`SPEI Range`, levels=c(1,3,6,9,12,24)) #SPEI range converte to factor
d$`SPEI Season`<-as.factor(d$`SPEI Season`) #SPEI season converted to factor
names2<-names1[c(2,6,3,8,1,4,5,7)] #Reordering ecoregions names
d$Ecoregion<-factor(d$Ecoregion,levels=names2) #Factoring ecoregions
d$CumSum<-cut(d$CumSum,seq(0,1,0.05),labels=seq(0.05,1,0.05)) #Factoring cumulativesum to steps of 0.05
d$Correlation<-cut(d$Correlation,c(-1,0.2,0.4,0.6,0.75,1),
                   labels=c("none","weak","moderate","strong","v.strong"))

#Names of the plots
loclab<-c("i-Central", "ii-Northern","iii-C.Range","iv-SB P-O", "v-Casc.","vi-E.Casc.","vii-Klamath", 
          "viii-Nevada") #ecoregions "y grid axe"
names(loclab)<-names2 #Indexing y grid axe
loclab2<-c("a-Spring0","b-Winter0","c-Autumn+1","d-Summer+1","e-Spring+1","f-Winter+1","g-Autumn+2",
           "h-Summer+2","i-Spring+2") #season "x grid axe"
season<-0  #initializing vector with season names
for (n in 1:9) season[n]<-paste0("S0",n) #Giving the names in vector
names(loclab2)<-season #Indexing the x grid axe

palettecorr<- c("#bdbdbd","#ffffb2","#fecc5c","#fd8d3c","#e31a1c") #Palette
names(palettecorr)<-c("none","weak","moderate","strong","v.strong")

#plot
p1<- ggplot(d, aes(`CumSum`,`SPEI Range`, fill= Correlation)) + 
  geom_tile()+scale_x_discrete(breaks=c(0.2,0.4,0.6,0.8))+
  scale_fill_manual(values=palettecorr)+
  facet_grid(`Ecoregion`~`SPEI Season`,labeller=labeller(Ecoregion=loclab, `SPEI Season`=loclab2))+
  xlab("Fraction of Area")+
  theme_light()

