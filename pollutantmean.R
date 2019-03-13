pollutantmean<-function(directory,pollutant,id=1:332){ #function definition
  a<-c() #initialize an empty vector
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
    z<-eval(parse(text=paste("readfile","$",pollutant,sep=""))) #reading the pollutant column in an eval statement
    a<-c(a,z) #aggregating all values of pollutants from all files
  }
  print(mean(a,na.rm=TRUE)) #calculating mean after removing NA
}
