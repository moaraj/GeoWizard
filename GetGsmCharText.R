#' Get the Meta Data from a GSE Objects
#'
#' Fetch all the sample specific text in the characteristics for each sample 
#'
#'
#' @example 
#' GSE <- (FetchThisGse("GSE46909"))
#' ExpDesignDescription <- GetGsmCharText(GSE)

GetGsmCharText <- function(GSE){

  
  # Genrate the GSM Design DF
  GsmMetaDataDF <- t(GSM_MetaData)
  GsmAccessions <- row.names(GsmMetaDataDF)
  GsmMetaDataDF <- cbind.data.frame(GsmAccessions, GsmMetaDataDF)
  rownames(GsmMetaDataDF) <- NULL
  
  # Name the Columns
  nExpVars <- ncol(GsmMetaDataDF) - 1
  colnames(GsmMetaDataDF) <-  c("GSM", paste("ExpVar", c(1:nExpVars), sep = ""))
  
  # Arrange the GSM Design Df according to factors with more than one level
  UsefulDesignFactorCols <- DescerningFactors(GsmMetaDataDF)
  
  GsmMetaDataDF <- GsmMetaDataDF %>% arrange_(.dots=UsefulDesignFactorCols)
  
  return(list("OverallDesign" = OverallDesign, 
              "OverallSummary" = OverallSummary,
              "GsmDesign" = GsmMetaDataDF,
              "UsefulDesignFactorCols" = UsefulDesignFactorCols,
              "GSE" = GSE@header$geo_accession))
}

#' Factors that Change in the Study
#' 
#' Functions that Returns a character vector of Factor Columns that will be helpul 
#' For Determining Experimental Design
#' 
#' @example DescerningFactors(GsmMetaDataFin)
#' FactorDf <- GsmMetaDataFin
#' 
DescerningFactors <- function(FactorDf){
  FactorDf <- FactorDf %>% select(starts_with("ExpVar"))
  descerning_factors <- sapply(FactorDf, function(x){nlevels(factor(x))})
  descerning_factors[1] <- 1
  descerning_factors <- descerning_factors > 1
  descerning_factors <- colnames(FactorDf[descerning_factors])
  return(descerning_factors)
}



