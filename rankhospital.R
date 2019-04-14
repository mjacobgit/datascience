rankhospital<-function(state,outcome,num="best"){
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!state %in% data$State){ #checking if state provided is valid
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){ #checking if outcome provided is valid
    stop("invalid outcome")
  }
  m<-match(data$State,state) #gathering
  t<-which(!is.na(m))        #state
  statedata<-data[t,]        #data
  if(outcome =="heart attack"){
    ordereddata<-statedata[order(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),statedata$Hospital.Name,na.last = NA),] ##ordering the state data in ascending rates of heart attack mortality and State
    if(as.character(num)=="best")return(ordereddata$Hospital.Name[1])
    if(as.character(num)=="worst") return(tail(ordereddata$Hospital.Name,1))
    if(num>length(ordereddata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) return(NA)
    else return(ordereddata$Hospital.Name[num])
  }
  if(outcome =="heart failure"){
    ordereddata<-statedata[order(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),statedata$Hospital.Name,na.last = NA),] #ordering the state data in ascending rates of heart failure mortality and State
    if(as.character(num)=="best")return(ordereddata$Hospital.Name[1])
    if(as.character(num)=="worst") return(tail(ordereddata$Hospital.Name,1))
    if(num>length(ordereddata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) return(NA)
    else return(ordereddata$Hospital.Name[num])
  }
  if(outcome =="pneumonia"){
    ordereddata<-statedata[order(as.numeric(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),statedata$Hospital.Name,na.last = NA),] #ordering the state data in ascending rates of pneumonia mortality and State
    if(as.character(num)=="best")return(ordereddata$Hospital.Name[1])
    if(as.character(num)=="worst") return(tail(ordereddata$Hospital.Name,1))
    if(num>length(ordereddata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) return(NA)
    else return(ordereddata$Hospital.Name[num])
  }
  
}