# Limma Pipeline

#'
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
#'
LimmaTopTable <- function(fit) {
     LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")
     return(LimmaTable)
}

#'
#'
#'
#'
LimmaOutput <- function(ArrayData, DesignMatrix){
     fit <- LimmaRes(ArrayData, DesignMatrix)
     TopTable <- LimmaTopTable(fit)
     return(TopTable)
}



#Deseq Pipline

#EdgeR Pipline