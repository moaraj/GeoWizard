Keywordfile <- source(file = "GeoTrainingSets/KeyWords.r")

#' The Following three functions
#' classify the GSMs in each GSEwhether of not
#'

MatchExpKeys <- function(GsmDesignTxt, KeyList, perl = F) {
     keymatch <-
          grep(
               pattern = paste(KeyList, collapse = "|"),
               x = GsmDesignTxt,
               ignore.case = TRUE,
               value = FALSE,
               perl = perl
          )
     if (length(keymatch) == 0) {
          keymatch <- FALSE
     }
     return(as.logical(keymatch))
}


#'
#'
#' @Column takes a character vector and assign wheter it might be contorl pert or time
#'
#'
#'

AssignColContainsFactor <- function(FactorColumn) {
     FactorColumn <- as.character(FactorColumn)
     isContCol <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$ControlKeywords)
     isPertCol <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TreatmentKeywords)
     isPertCol <- as.logical(!isContCol + isPertCol)
     isTimeCol <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TimeUnits)
     isDilSCol <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TitrationKeywords)
     
     # Leverage the Ontologies to add additoinaly Classifications
     #isDisease <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TitrationKeywords)
     #isCellLine <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TitrationKeywords)
     #isTissue <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TitrationKeywords)
     #isTissue <- sapply(FactorColumn, FUN = MatchExpKeys, ExpKeywords$TitrationKeywords)
     res <- cbind.data.frame(FactorColumn,
                             isContCol,
                             isPertCol,
                             isTimeCol,
                             isDilSCol)
     colnames(res) <-
          c("Text",
            "ContClass",
            "PertClass",
            "TimeSeries",
            "DilutionSeries")
     return(res)
}

#' Classify GSM text and Return List of Data Frames
#'
#' Function takes in a Dataframe contains the factor level text only
#' and returns the time, perturnation and control classificution as a list
#' with each element of the list contains a dataframe with the level text and
#' other columns showing whether it was classificed a control or a perturnation or a time series
#'
#'

ClassGsmText <- function(GsmDesignDF, returnList = T) {
     message("executing ClassGSMText")
     if (ncol(GsmDesignDF) == 1) {
          message("Stemming Factor Text for Text Classification")
          # english work better than porter
          ProcGsmDesignDF <- data.frame(stringsAsFactors = F, apply(GsmDesignDF, MARGIN = 1, stemString))
          colnames(ProcGsmDesignDF) <- "ExpVar1"
     } else {
          message("Stemming Concatonated Multi ColumnFactor Text for Text Classification")
          ProcGsmDesignDF <- data.frame(stringsAsFactors = F, sapply(GsmDesignDF, stemString))
     }
     
     ExpClassPrior <-
          lapply(ProcGsmDesignDF, AssignColContainsFactor)
     if (returnList == F) {
          ExpClassPrior <- do.call(cbind, ExpClassPrior)
     }
     return(ExpClassPrior)
}

#' This function takes the output from the ClassOntologies function
#' and returns a shortened list containing only the title and ExpVar columns
#'
#' @param ClassOntList output from ClassOntologies()
#' @param ExpDescLocation where useful experimental dessign information is written in the GSM Data
#'

ExtractExpVarList <- function(GsmList) {
     message("executing ExtractExpVarList")
     IndexRes <-
          grep(pattern = "ExpVar[[:digit:]]", x = names(GsmList))
     OntologyDFs <- lapply(IndexRes, function(x) {
          GsmList[[x]]
     })
     names(OntologyDFs) <- names(GsmList)[IndexRes]
     return(OntologyDFs)
}


ClassSummary <- function(GsmDesignDF) {
     message("executing ClassSummary")
     characteristics_ch1 <- GsmDesignDF %>% dplyr::select(characteristics_ch1)
     GsmDesignDF <- GsmDesignDF %>% dplyr::select(-characteristics_ch1)
     OverallSummary <- unique(GsmDesignDF$OverallSummary)
     GsmDesignDF <- AssignTimeSeriesInformation(GsmDesignDF, OverallSummary)
     GsmDesignDF <- bind_cols(GsmDesignDF, characteristics_ch1)
     return(GsmDesignDF)
}

#' Coverts Control Classificaiton to text
#'
#' The control classificaiton is 1 for control and 0 for not control
#' this function is used in the design labs function to generate human readable design and contrasts
#' 

NumtoText <- function(strvec) {
     strvec <- gsub(pattern = 1,
                    replacement = "cont",
                    x = strvec)
     strvec <- gsub(pattern = 0,
                    replacement = "pert",
                    x = strvec)
     return(strvec)
}



#' Extract Useful Classificaitons
#' 
#' Takes in list of Experimental Factors and returns a dataframe of only those classificaiton 
#' 
#'
#'
#'

DesignLabs <- function(FullClassDF) {
     GsmTextDF <- FullClassDF %>% dplyr::select(matches("Text"))
     GsmContDF <- FullClassDF %>% dplyr::select(matches("Cont"))
     GsmPertDF <- FullClassDF %>% dplyr::select(matches("Pert"))
     
     UsefulCols <- 
          c(DescerningFactors(GsmContDF),
            DescerningFactors(GsmPertDF))
     
     if (length(UsefulCols) != 0) {
          GsmContDF <- FullClassDF %>% 
            dplyr::select(matches(paste(UsefulCols, collapse = "|")))
          
          DesignLabsDF <-
               sapply(UsefulCols, function(FactorCol) {
                    FactorColName <- str_split(FactorCol, "\\.")[[1]][1]
                    isControlText <-
                         NumtoText(as.numeric(GsmContDF[, FactorCol]))
                    TreatLabs <-
                         paste(FactorColName, ".", isControlText, sep = "")
               })
          
          UsefulTxtCols <-
               unlist(sapply(UsefulCols, function(FactorCol) {
                    str_split(FactorCol, "\\.")[[1]][1]
               }))
          
          GsmTextDF <-
               GsmTextDF %>% dplyr::select(matches(paste(UsefulTxtCols, collapse = "|")))
          GsmTextDF <- cbind.data.frame(GsmTextDF, DesignLabsDF)
          #GsmTextDF <- GsmTextDF[, order(names(GsmTextDF))]
          
     } else {
          colnames(FullClassDF) <- c(
               "ExpVar1.Text",
               "ExpVar1.ContClass",
               "ExpVar1.PerClass",
               "ExpVar1.TimeClass",
               "ExpVar1.DilSClass")
          GsmTextDF <- FullClassDF[, 1:5]
     }
     return(GsmTextDF)
}


#'
#'
#'
#'
#'
#'
#'





ContMultiTreat <- function(GsmDesignDf) {
     DesignMatrix <-
          model.matrix(object = ~ 0 + DesignMatrixLab, data = GsmTextDF)
     colnames(DesignMatrix) <- gsub(
          pattern = "DesignMatrixLab",
          replacement = "",
          x = colnames(DesignMatrix)
     )
     return(DesignMatrix)
}


#'
#'
#'
#'
#'
#'
NoContDetectedContAsign <- function(GsmDesignDf) {
     ControlInformation <- AssignContainsControls(GsmDesignDf)
     SampleKeyClassList <- ControlInformation[["SampleKeyClass"]]
     NoContDefaultIndex <- names(ControlInformation[["ContKeyCol"]])
     
     SampleDefContClassList <-
          sapply(NoContDefaultIndex, function(ExpVarCol) {
               if (ControlInformation[["ContKeyCol"]][[ExpVarCol]] == 0) {
                    QueryCol <- GsmDesignDf[[ExpVarCol]]
                    DefControlSet <- unique(QueryCol)[1]
                    message(
                         sprintf(
                              "No control for this factor detected. %s used as default control for contrast",
                              DefControlSet
                         )
                    )
                    NewDefault <- grepl(pattern = DefControlSet, x = QueryCol)
                    DefaultContList <- SampleKeyClassList[[ExpVarCol]]
                    DefaultContList <- NewDefault
               }
          })
     return(SampleDefContClassList)
}

#'
#'
#'
AssignBlockCatagories <- function(GsmDesignDf, PertInformation) {
     GsmDesignDfKey <-
          NoContDetectedContAsign(GsmDesignDf, UsefulDesignFactorCols)
     #Classificaiton of Control and Perterbatuion through Sample Text
     if (GsmDesignDf[1, "TimeSeries"] == FALSE) {
          if (identical(ControlSamples,!PertSamples)) {
               message("Control and Perterbation Samples Classification do not clash")
               GsmDesignDf[ControlSamples , "SampleType"] <- "Control"
               GsmDesignDf[PertSamples , "SampleType"] <- "Perturbation"
               
          } else {
               message(
                    "Control and Perterbation Sample Classifications clash \n
                    Samples labelled Perturbations"
               )
               GsmDesignDf[PertSamples , "SampleType"] <- "Perturbation"
               
          }
     }
     return(GsmDesignDf)
}




#' Determine if Time Series Experiment
#'
#'
AssignTimeSeriesInformation <-
     function(GsmDesignDF, OverallSummary) {
          message("Determining if study has a time series")
          ClassTimeSeries <-
               MatchExpKeys(OverallSummary, ExpKeywords$is_time_study)
          if (ClassTimeSeries == FALSE) {
               message("This gene series does not contain a time series experiment")
          } else {
               message("This Gene Series contains a time series experiment")
          }
          GsmDesignDF["TimeSeries"] <- ClassTimeSeries
          return(GsmDesignDF)
     }


#' Contrast Matrix with One Factor Level
#'
#'
ContOneTreat <-
     function(GsmDesignDf,
              UsefulDesignFactorCols,
              geo_accession,
              ControlInformation) {
          # If its a one Factor Design Then There can Only be Control and Perterbation
          # or Many perterbations being compared
          ContainsControls <- ControlInformation[1]
          ControlSamples <- ControlInformation[2]
          
          
          # IF there are no Controls First Perturbtaion is the Control
          TreatmentCol <- UsefulDesignFactorCols[1]
          TreatmentLabs <-
               paste("T1.Pert", as.numeric(GsmDesignDf[, TreatmentCol]), sep = "")
          
          # Test TreatmentLabs <- paste("T1.Pert",rep(c(1:7),4), sep = "")
          GsmDesignDf["DesignMatrixLab"] <- TreatmentLabs
          
          DesignMatrix <-
               model.matrix(object = ~ 0 + DesignMatrixLab, data = GsmDesignDf)
          colnames(DesignMatrix) <- gsub(
               pattern = "DesignMatrixLab",
               replacement = "",
               x = colnames(DesignMatrix)
          )
          
          DesignMatrixFileName <-
               paste(geo_accession, "DesMat.tsv", sep = "_")
          write.table(
               file = file.path(GeoRepo, DesignMatrixFileName),
               x = DesignMatrix,
               sep = "\t",
               quote = FALSE,
               row.names = FALSE
          )
          
          
          ContrastMat <- DiffContrasts(unique(TreatmentLabs))
          ContrastMat <-
               makeContrasts(contrasts = ContrastMat, levels = DesignMatrix)
          
          ContrastMatrixFileName <-
               paste(geo_accession, "ConMat.tsv", sep = "_")
          write.table(
               file = file.path(GeoRepo, ContrastMatrixFileName),
               x = ContrastMat,
               sep = "\t",
               quote = FALSE,
               row.names = FALSE
          )
          
     }

#' Contrast Matrix for Mulit Factor Level
#'
#'
ContMultiTreat <-
     function(GsmDesignDf,
              UsefulDesignFactorCols,
              geo_accession) {
          # Write a function that always makes control columns first
          # If there is a multifactor design and no obvious controls are present the first
          # treatment gets labelled a control by defaul
          TempDf <- GsmDesignDf %>% select(one_of(UsefulDesignFactorCols))
          
          TreatmentCol <- UsefulDesignFactorCols[1]
          TreatLabs <-
               paste("T", as.numeric(GsmDesignDf[, TreatmentCol]), ".", sep = "")
          
          ConditionCol <- UsefulDesignFactorCols[2]
          ConditionNums <- as.numeric(GsmDesignDf[, ConditionCol])
          ConditionLabs <- paste("Pert", ConditionNums, sep = "")
          DesignLabs <- paste(TreatLabs, ConditionLabs, sep = "")
          
          GsmDesignDf["DesignMatrixLab"] <- DesignLabs
          UsefulDesignFactorCols <-
               c(UsefulDesignFactorCols, "DesignMatrixLab")
          
          DesignMatrix <-
               model.matrix(object = ~ 0 + DesignMatrixLab,
                            data = GsmDesignDf %>% select(one_of(UsefulDesignFactorCols)))
          colnames(DesignMatrix) <- gsub(
               pattern = "DesignMatrixLab",
               replacement = "",
               x = colnames(DesignMatrix)
          )
          
          DesignMatixFileName <-
               paste(geo_accession, "DesMat.tsv", sep = "_")
          write.table(
               file = file.path(GeoRepo, DesignMatixFileName),
               x = DesignMatrix,
               sep = "\t",
               quote = FALSE,
               row.names = FALSE
          )
          
          GenerateContrast(TreatLabs, ConditionLabs, geo_accession, DesignMatrix)
     }

#' From a GSE and GSE Experimental Data
#' This function produced a design and contrast matrix
#'
#'
#' @example
#' GSE <- (FetchThisGse("GSE72355"))
#' ExpDesignDescription <- GetGsmCharText(GSE)
#' ExpGroupAsign(ExpDesignDescription, GSE)

ExpGroupAsign <- function(ExpDesignDescription, GSE) {
     ExpKeywords <- readRDS("ExpKeywords.rds")
     OverallDesign <-
          paste(ExpDesignDescription[["OverallDesign"]], collapse = "")
     OverallSummary <-
          paste(ExpDesignDescription[["OverallSummary"]], collapse = "")
     GsmDesignDf <- ExpDesignDescription[["GsmDesign"]]
     geo_accession <- ExpDesignDescription$GSE
     
     UsefulDesignFactorCols <-
          ExpDesignDescription[["UsefulDesignFactorCols"]]
     message(sprintf(
          "There are %d useful factors found in this study",
          length(UsefulDesignFactorCols)
     ))
     GsmDesignDf <-
          AssignTimeSeriesInformation(GsmDesignDf, OverallSummary, OverallDesign)
     
     ControlInformation <-
          AssignContainsControls(GsmDesignDf, UsefulDesignFactorCols)
     PertInformation <-
          AssignContainsPerturbations(GsmDesignDf, UsefulDesignFactorCols)
     
     GsmDesignDf <- AssignBlockCatagories(GsmDesignDf)
     
     if (length(UsefulDesignFactorCols) < 1) {
          message(
               "One factor and one level detected using the characterictic and titles of the GSM \n
               differential expresssion analysis can only be done with more factor levels"
          )
          NA
     } else if (length(UsefulDesignFactorCols) == 1) {
          message("Detected Experimental Design: No Controls, One Factor w. Multiple Levels")
          ContOneTreat(GsmDesignDf, UsefulDesignFactorCols, geo_accession)
          
     } else if (length(UsefulDesignFactorCols) > 1) {
          message("Detected Experimental Design: No Controls, Multi Factors w. Multiple Levels")
          ContMultiTreat(GsmDesignDf, UsefulDesignFactorCols, geo_accession)
     }
}
