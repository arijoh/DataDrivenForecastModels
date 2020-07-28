## Function based on evaluation Luca Vezzaro has sent me

### The data is trained based on a rain events based on precipitation and untill the rain event is finished + 12hours
### With this, the data will be trained on datapoints prior to the wet weather activation
### However, we will evaluate each model base on predictive preformance based on wet weather activation prediction.
### The treshold will be 500 m3/hr and here we will aquire the rain index and all that to evaluate the model in meta_optim

## Data is the evaluation data.
ATS_eval <- function(data){
  
  startTime=3600*2
  tailTime=3600*1
  
  measData <- as.numeric(levels(data$Meas))[data$Meas]
  data_pred <- as.numeric(levels(data$pred))[data$pred]
  
  tMeas <- as.POSIXct(data$Timestamp, format="%Y-%m-%d %H:%M", tz = "GMT")
  tPred <- as.POSIXct(data$Timestamp, format="%Y-%m-%d %H:%M", tz = "GMT")
  
  measData <- cbind.data.frame(tMeas, measData)
  data_pred <- cbind.data.frame(tPred, data_pred)
  
  colnames(data_pred)<-c("Time","pred")
  colnames(measData)<-c("Time","meas")
  
  ATSthr=5000 # threshold for wet weather [m3/hr]
  
  ATS=data.frame(matrix(NA, nrow = length(tPred), ncol=3))
  colnames(ATS)<-c("Time","Meas","pred")
  ATS$Time=tPred
  ATS$Meas=measData$meas>ATSthr
  ATS$pred=data_pred$pred>ATSthr
  
  idxON=sort(unique(c(which(ATS$Meas),which(ATS$pred)))) 
  if (length(idxON)>0) {
    # Get the end index of rain event with using diff
    endATS=which(diff(idxON,1)>1) # If the difference of idxOn > 1, the rain events are separated.
    if (length(endATS)==0) { # case: only one event
      startATS=1
      endATS=length(idxON)
    } else { # case: two or more events
      {  
        startATS=c(1,endATS[1:(length(endATS))]+1)
        endATS=c(endATS,length(idxON))
      }
    }
    startTime=startTime # [second] time before wet threshold was exceeded, but can be considered as wet (to be removed from estimation)
    tailTime=tailTime # [second] length of tail of wet event (to be removed from estimation)
    idxDummy=NULL
    longATS=which(startATS!=endATS) # remove events of only one time step
    
    # Indexes of idxOn where activation treshold is triggerred
    startATS=startATS[longATS]  
    endATS=endATS[longATS] 
    
    for (ev in 1:length(startATS)) {
      ### Add tail and start time to idxOn
      newStartATS=which(tPred>(tPred[idxON[startATS[ev]]]-startTime))[1]
      newEndATS=which(tPred>(tPred[idxON[endATS[ev]]]+tailTime))[1] 
      if (is.na(newEndATS)) {
        newEndATS=length(tPred)
      }
      idxDummy=c(idxDummy,seq(newStartATS,newEndATS))
    }
    idxATS=unique(idxDummy) #remove dublette (if events now overlap)
  } 
  
  
  ## start event based analysis
  ## identify start-stop point for event
  if (length(idxATS)>0) {
    endATS=which(diff(idxATS,1)>1)
    if (length(endATS)==0) { # case: only one event
      startATS=1
      endATS=length(idxATS)
    } else { # case: two or more events
      {
        startATS=c(1,endATS[1:(length(endATS))]+1)
        endATS=c(endATS,length(idxATS))
      }
    }
    noEv=length(startATS)
    evList=rep(list(as.POSIXct(rep(NA,noEv))),2)
    
    for (ev in 1:noEv) {
      evList[[1]][ev]=tPred[idxATS[startATS[ev]]]
      evList[[2]][ev]=tPred[idxATS[endATS[ev]]]
    }
  }
 
  ATSstartTimes_m = vector(noEv, mode="list")
  ATSperformance=data.frame(matrix(0,nrow = noEv,ncol = 3))
  VOLperformance=data.frame(matrix(0,nrow = noEv,ncol = 2))
  colnames(ATSperformance)=c("pred","noEv","predfalse")
  colnames(VOLperformance)=c("meas","pred")
  
  
  for (ev in 1:noEv) {
    # make plot
    xl<-as.POSIXct(c(evList[[1]][ev],evList[[2]][ev]))

    ## analyse performance
    # when ATS was activated by meas flow?
    idxATS_m=which(diff(ATS$Meas[idxATS[startATS[ev]:endATS[ev]]])>0)[1]  # Find when FALSE -> TRUE
    evLength=length(idxATS[startATS[ev]:endATS[ev]])
    
    downTime=sum(is.na(ATS$pred[idxATS[startATS[ev]:endATS[ev]]]))
    if (downTime==evLength) { # forecast was down all the time
      ATSperformance$pred[ev]="down"
    } else {
      
      if (is.na(idxATS_m)) # there was no activation of ATS from measurements
      {
        if (sum(match(ATS$pred[idxATS[startATS[ev]:endATS[ev]]],TRUE),na.rm = TRUE)>0) # count numbers of activations
        {
          ATSperformance$pred[ev]="false alarm" 
        } else #the model did not predict an event
        { ATSperformance$pred[ev]= "correct negative"} 
      }
      else { #there was an event in the measurements    
        # when ATS was activated by pred
       idxATS_pred=which(diff(ATS$pred[idxATS[startATS[ev]:endATS[ev]]])>0)[1] #FALSE -> TRUE
        
        #volume - simulated
        flowEv = data_pred$pred[idxATS[startATS[ev]:endATS[ev]]]
        #VOLperformance$pred[ev]=sum(flowEv[flowEv>ATSthr]*deltaTimeStep)
        
        #volume - measured
        flowEv=measData$meas[idxATS[startATS[ev]:endATS[ev]]]
        #VOLperformance$meas[ev]=sum(flowEv[flowEv>ATSthr]*deltaTimeStep)
        
        
        if (sum(idxATS_pred,na.rm=TRUE)==0) # ATS was never activated
        { ATSperformance$pred[ev]="missed"} else
        {
          deltaTime=difftime(ATS$Time[idxATS_m],ATS$Time[idxATS_pred],units = "mins")
          if(deltaTime< -15) # the model was late compared to measurements
            {ATSperformance$pred[ev]=">15 min late"} else
          if(deltaTime<0) # the model was late compared to measurements       
            { ATSperformance$pred[ev]="<15 min late"} else     
          if(deltaTime<=60) # the model activated ATS within 60 min before meas
            { ATSperformance$pred[ev]="correct"} else
          { ATSperformance$pred[ev]="early"}
        }}
    }  
  }
  
  ##Still dont understand well what predfalse and noEv is
  # noEv: Number of ATS switches within wet activation?

  
  statesNames=c("g down","f false alarm","e missed","d >15 min late","c early","b <15 min late","a correct negative","a correct")
  noStates=length(statesNames)
  summaryATS=data.frame(matrix(0,ncol = 2,nrow=noStates))  
  
  
  colnames(summaryATS)=c("state","value")

  for (j in 1:length(statesNames)) {
    summaryATS[j,1]=statesNames[j] 
    summaryATS[j,2]=length(which(ATSperformance[,1]==substring(statesNames[j],3)))
  }
  
  
  idxEarly=which(summaryATS[,1]=="b <15 min late")
  idxCorrect=which(summaryATS[,1]=="a correct")
  summaryATS$value[idxCorrect]=summaryATS$value[idxCorrect]+summaryATS$value[idxEarly]
  summaryATS <- summaryATS[-idxEarly,]
  idxLate=which(summaryATS[,1]=="d >15 min late")
  idxMissed=which(summaryATS[,1]=="e missed")
  summaryATS$value[idxMissed]=summaryATS$value[idxMissed]+summaryATS$value[idxLate]
  summaryATS <- summaryATS[-idxLate,]
  
  summaryATS <- list(down = summaryATS[1,2], 
                     false_alarm = summaryATS[2,2], 
                     missed = summaryATS[3,2], 
                     early = summaryATS[4,2], 
                     correct_negative = summaryATS[5,2], 
                     correct = summaryATS[6,2])
  
  return(summaryATS)
}




