corr<-function(directory,threshold=0){ #function definition
  t<-c() #initialize an empty data frame
  for (i in 1:332){   #loop to go over all the files passed in the id argument
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
      a<-k
    }
    else if(u>k){
      a<-k
    }
    else{
      a<-u
    }
    if(a>threshold){
    v<-cor(z,w,use="pairwise.complete.obs")
    t<-c(t,v)
    }
  }
  return(t)
}
