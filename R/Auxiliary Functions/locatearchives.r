# Function that will read raster data and crop according to a pre defined geometry
# it takes as input a vector with names of the directories to get the values (dir)
# and a geometry of coordinates (crop_img) the funtion returns the raster files in 
# respective directories split in a list divided in two: odd vectors and even vectors

locatearchives<-function(dir, crop_img=NULL){
  imagespos<-list() #List odd is initialized
  imagesneg<-list() #list even is initialized
  for (n in dir){
    m<-list.files(n,full.names = TRUE)  
    readdata<-sapply(m,raster) #Reads data From wd
    readdata<-map(readdata,crop, crop_img) #Crops all data
    vectorlength<-1:length(readdata)
    evenvec<-vectorlength[vectorlength%%2==0]
    oddvec<-vectorlength[vectorlength%%2!=0]
    min<-readdata[evenvec] #Divsion of odds and evens
    max<-readdata[oddvec]
    imagespos[[n]]<-max
    imagesneg[[n]]<-min
  }
  return(test<-list(imagespos,imagesneg)) #Returns 2 lists: one with all pos rasters and the other with all negatives
										  #obs: list within list for each scenario studied