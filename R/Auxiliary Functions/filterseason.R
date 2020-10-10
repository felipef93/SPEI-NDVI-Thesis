#Function to filter the correlation collection according to the season it represents, uses as input the 
#collection of images of correlation, the metadata, in csv format and a reduction that is what value should
#be the season should be reduced (i.e. max, min, mean)

filterseason<-function(imagecollection,metadata,reduction){
  testlist<-list() #Initialize two lists as I want results in a list of lists (Values by season and
  testlist2<-list() #SPEI Range
  p<-c(1,3,6,9,12,24) #SPEI ranges considered
    for (n in 1:9){ #First loop list of seasons represented
      test0<-keep(imagecollection,metadata[4]==n) 
      for(m in 1:6){ # Second loop SPEI ranges
        testlist2[[m]]<-keep(test0,metadata[metadata[4]==1,][2]==p[m])%>%stack%>%calc(function(x){reduction(x)})
      }
     names(testlist2)<-p
      testlist[[n]]<-testlist2 #List inside list
    }
  a<-0
  for (n in 1:9) a[n]<-paste0("S0",n)
  names(testlist)<-a
  return (testlist)
}