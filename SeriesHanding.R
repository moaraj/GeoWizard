#'
#'
#'
#'

TimeConversion <- function(text){
     TimeRegex <- paste(ExpKeywords$TimeUnits, collapse = "|")
     ExtractTxt <- grep(pattern = paste("\\s",TimeRegex,"\\s"), x = text, ignore.case = T)
     TimeTextNum <- '[[^[[:punct:a-z\\s]]'
}


#' Convert Time Units
#'
#'
#'
#'
TimeConversion <- function(Input, InputUnit, FinalUnit){
     TimeInSeconds = list(second = 1,
                          minute = 60,
                          hour = 3600,
                          day = 8640,
                          week = 604800,
                          month = 2419200,
                          year = 31449600)
     Output <- Input * TimeInSeconds[[InputUnit]] / TimeInSeconds[[FinalUnit]]
     return(Output)
}


#AddDilutionSeriesFactors <- function(ClassDFList){
#     ListIndex <- names(ClassDFList)
     
isSeriesFactor <- function(ClassDF, mode) {
     if (is.null(ClassDF) | is.null(mode)) {
          stop("Dude man, I need some options")
     } else if (mode == "time") {
          res <- ClassDF$TimeSeries
     } else if (mode == "titration") {
          res <- ClassDF$DilutionSeries
     }
     res <-
          sum(as.numeric(res)) >= (length(res) * 0.8) # Classification Error
     return(res)
}

AddSeriesDFs <- function(ClassDFList, mode) {
     if (is.null(mode)) {
          stop("Supply Time or Dilution")
     } else if (class(mode) != "character") { stop("Submit mode as Character")
     } else if (class(ClassDFList) != "list") { stop("Input is not List") 
     } else {
          SeriesDF <- unlist(lapply(ClassDFList, isSeriesFactor, mode))
          SeriesDFList <- ClassDFList[which(SeriesDF)]
     } 
     return(SeriesDFList)
}





