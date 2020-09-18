#This function computes the % of unique area for each of the variables in scenarios studied. It's input
#Must be a list(Scenarios) of a List (Variables on each scenario) the function will return 3 elements:
#in it's first the raster of unique and common areas is computed, in the second the % of unique area for each
#scenario and in the third the % of common area for each scenario
getuniqueandcommon<-function(listofscenarios){
  uniquearea<-vector("list",4)  #Initialized: The unique area of one variable
  stackunique<-vector("list",4)  #Initialized:The unique area of all variables (same plot)
  commonarea<-vector("list",4)  #Initialized: The common area to all but one variable
  stackcommon<-vector("list",4) #Initialized: The common area among all variables
  commonunique<-vector("list",4) #Initialized: Common and uniques areas
#Note that the calc(stack(x)) function basically merges all scenarios together when na.rm true,
#It will basically merge two images independent of their coordinates. Ex: if I wanted to merge a raster of
#Brazil and Sweden, with na.rm=T the merge would be Brazil+Sweden with na.rm=F merge would be NULL
  for (n in 1:4){
    l<-length(listofscenarios[[n]]) 
    stackcommon[[n]]<- calc(stack(listofscenarios[[n]]),sum)
    stackcommon[[n]][abs(stackcommon[[n]])>0]<-1 #Scenarios that are common to all variables have value of 1
    for (m in 1:l){
      notm<-discard(listofscenarios[[n]],names(listofscenarios[[n]])==names(listofscenarios[[n]][m]))
      stacked<-stack(notm)
      masksum<-calc(stacked,sum,na.rm=T)
      uniquearea[[n]][[m]]<-mask(x=listofscenarios[[n]][[m]],mask=masksum,maskvalue=0,updatevalue=-m)
                            #variables will have the value of -m
      commonarea[[n]][[m]]<-mask(x=listofscenarios[[n]][[m]],mask=stackcommon[[n]])
    }
    stackunique[[n]]<-calc(stack(uniquearea[[n]]),mean,na.rm=T) #Unique areas grouped together
    stackunique[[n]][stackunique[[n]]>-1]<-NA  #Non unique values are set to NA
  }
  wholearea<-calc(stack(listofscenarios[[2]]),sum,na.rm=T) #fix to get whole area of significant correlations
  dfwholea<-as.vector(wholearea) 
  dfwholea<-dfwholea[dfwholea!=0] #number of pixels with correlations
  for (n in 1:4){
    commonunique[[n]]<-calc(stack(stackunique[[n]],stackcommon[[n]]),sum,na.rm=T) #Common and unique stacked
    commonunique[[n]]<-mask(commonunique[[n]],wholearea, maskvalue=0) # Mask applied for locations with
                                  #significant correlation and location where there is more than 
                                  #one scenario correlation are set to have values of 0
  }
  namesof<-mapply(names,listofscenarios)%>%map(rev)%>%str_extract_all('[0-9]+')
  totarea<-listofscenarios%>%map(map,getValues)%>%map(map,na.omit)%>%map(map,length)%>%
            map(map,~.x/length(dfwholea)) #Getting the percentage of area that each variable in each scenario
                                          #covers
  totarea<-map(totarea, melt)
  for (n in 1:4){ #Adding two elements to the list of variables in each scenario
    m<-length(namesof[[n]])
    namesof[[n]][[m+1]]<-"+ 1 scenario"
    namesof[[n]][[m+2]]<-"All scenarios"
  }
  
  
  perunique<-map(commonunique,getValues)%>%map(table)%>%
    map(~.x/length(dfwholea))%>%map2(.y=namesof,set_names) # In a similar fashion percentage of unique
                                                           #areas are calculated
  perunique<-map(perunique, as.data.frame) 
  
  exit<-list()
  
  exit[[1]]<-commonunique
  names(exit[[1]])<-names(listofscenarios)
  exit[[2]]<-perunique
  names(exit[[2]])<-names(listofscenarios)
  exit[[3]]<-totarea
  names(exit[[3]])<-names(listofscenarios)
  names(exit)<-c('Raster of common and unique areas','% of unique area in each scenario', '% of total area in each scenario')
  return(exit)
}
