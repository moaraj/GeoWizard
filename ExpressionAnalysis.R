# Limma Pipeline

#'
#'
#'
LimmaRes <- function(ArrayData, DesignMatrix){
     fit <- lmFit(ArrayData, DesignMatrix)
     fit <- eBayes(fit)
     fit <- eBayes(fit,trend=TRUE)
     return(fit)
}

#'
#'
#'
LimmaTopTable <- function(fit, DesignMatrix) {
     DesignMatrix <- fit$design 
     
     ExpVar <- colnames(DesignMatrix)
     ExpVarIndex <- 1:length(ExpVar)
     if (grep(pattern = "Intercept", x = ExpVar[1])) {
          ExpVarIndex <- ExpVarIndex[-1]
     }
     
     TopTableList <- lapply(ExpVarIndex, function(Index){
          LimmaTable <- topTable(fit, coef = Index, n=1000, adjust="BH")
          LimmaTable$ExpVar <- ExpVar[Index]
          LimmaTable
     })
     
     names(TopTableList) <- ExpVar[ExpVarIndex]
     LimmaTable <- bind_rows(TopTableList)
     
     return(LimmaTable)
}

#'
#'
#'
LimmaOutput <- function(ArrayData, DesignMatrix){
     fit <- LimmaRes(ArrayData, DesignMatrix)
     TopTable <- LimmaTopTable(fit, DesignMatrix)
     return(TopTable)
}

#Deseq Pipline

#EdgeR Pipline