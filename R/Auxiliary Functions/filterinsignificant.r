#Function to filter x according to values of Y
#X and y are lists
filterifinsignificant<- function(x,y,filter=0.5){n<-length(y) 
      newlist<-list()
      for(i in 1:n)   
        {
          if(y[[i]]>filter) {
          newlist[[i]]<-x[i]
          }else{
          newlist[[i]]<-0
          }
    }
return(newlist)
}
