complete<-function(directory,id=1:332){ #function definition
  a<-data.frame() #initialize an empty data frame
  for (i in id){   #loop to go over all the files passed in the id argument
    if (i<10){     #creating loop to generate variable to pass to directory
      y<-paste("00",i,sep="") 
    }
    else if(i>9 & i<100){
      y<-paste("0",i,sep="")
    }
    else {
      y<-i
    } 
    path<-paste(directory,"/",y,".csv",sep="") #generating the path variable for reading the file from
    readfile<-read.csv(path,header = TRUE) #read the files
    z<-eval(parse(text=paste("readfile","$","sulfate",sep=""))) #reading the pollutant column in 
    "an eval statement assuming if one variable is missing, both are and its incomplete."
    w<-eval(parse(text=paste("readfile","$","nitrate",sep="")))
    u<-sum(!is.na(w))
    k<-sum(!is.na(z))
    if(u==k){
      a<-rbind(a,c(i,k))
    }
    else if(u>k){
      a<-rbind(a,c(i,k))
    }
    else{
      a<-rbind(a,c(i,u))
    }
  }
  names(a)<-c("id","nobs")
  print(a)
}
