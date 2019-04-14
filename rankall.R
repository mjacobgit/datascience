rankall<-function(outcome,num="best"){
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){ #checking if outcome provided is valid
    stop("invalid outcome")
  }
  statelist<-sort(unique(data$State)) #gathering
  #print(statelist)
  m<-data.frame(hospital=character(),state=character())
  #print(names(m))
  for (i in statelist){
    k<-rankhospital(i,outcome,num)
    l<-data.frame(hospital=k,state=i)
    m<-rbind(m,l)
    #print(k)
    #print(m)
    #print(l)
  }
  
  return(m)
}
  
  