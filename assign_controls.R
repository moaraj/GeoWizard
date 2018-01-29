AssignContainsControls <- function(GsmDesignDf, UsefulDesignFactorCols){
  QueryColumnsDf <- GsmDesignDf[ ,UsefulDesignFactorCols]
  
  SampleKeyClassList <- lapply(QueryColumnsDf, function(FactorColumn){
    isContCol <- sapply(FactorColumn, FUN = isControlGsm)
    isPertCol <- sapply(FactorColumn, FUN = isPerturbationGsm)
    res <- cbind.data.frame(FactorColumn, isContCol, isPertCol)
    colnames(res) <- c("Text", "ContClass", "PertClass")
    res
  })
  
  ContColKeyClass <- lapply(SampleClassList, function(ClassList){
    ContDetected <- sum(ClassList[,2], na.rm=TRUE)})
  PertColKeyClass <- lapply(SampleClassList, function(ClassList){
    PertDetected <- sum(ClassList[,3], na.rm=TRUE)})
  
  return(list("SampleKeyClass" = SampleClassList,
              "PertKeyCol" = PertColKeyClass,
              "ContKeyCol" = ContColKeyClass))
}

NoContDetectedContAsign <- function(GsmDesignDf, UsefulDesignFactorCols,ControlInformation){
  SampleKeyClassList <- ControlInformation[["SampleKeyClass"]]
  NoContDefaultIndex <- names(ControlInformation[["ContKeyCol"]])
  
  SampleDefContClassList <- 
  sapply(NoContDefaultIndex, function(ExpVarCol){
    if(ControlInformation[["ContKeyCol"]][[ExpVarCol]] == 0){
      QueryCol <- GsmDesignDf[[ExpVarCol]]
      DefControlSet <- unique(QueryCol)[1]
      message(sprintf("No control for this factor detected. %s used as default control for contrast",
                      DefControlSet))
      NewDefault <- grepl(pattern = DefControlSet, x = QueryCol)
      DefaultContList <- SampleKeyClassList[[ExpVarCol]]
      DefaultContList <- NewDefault}
  })
  return(SampleDefContClassList)
}


AssignBlockCatagories <- function(GsmDesignDf, UsefulDesignFactorCols, PertInformation){
  
  ControlInformation <- AssignContainsControls(GsmDesignDf, UsefulDesignFactorCols)
  NoContDetectedContAsign(GsmDesignDf, UsefulDesignFactorCols,ControlInformation)
  
  #Classificaiton of Control and Perterbatuion through Sample Text
  if(GsmDesignDf[1,"TimeSeries"] == FALSE){
    
    ControlSamples <- sapply(GsmDesignTxt, FUN = isControlGsm)
    PertSamples <- sapply(GsmDesignTxt, FUN = isPerturbationGsm)
    
    if(identical(ControlSamples, !PertSamples)){
      message("Control and Perterbation Samples Classification do not clash")
      GsmDesignDf[ControlSamples ,"SampleType"] <- "Control"
      GsmDesignDf[PertSamples ,"SampleType"] <- "Perturbation"
      
    } else {
      message("Control and Perterbation Sample Classifications clash \n 
                 Samples labelled Perturbations")
      GsmDesignDf[PertSamples ,"SampleType"] <- "Perturbation"
      
    }
  }
  return(GsmDesignDf)
}



PertInformation <- AssignContainsPerturbations(GsmDesignDf, UsefulDesignFactorCols)
