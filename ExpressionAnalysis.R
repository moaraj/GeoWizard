GenMatrixWithFactors <- 


LimmaRes <- function(ArrayData, DesignMatrix){
     fit <- lmFit(ArrayData, DesignMatrix)
     fit <- eBayes(fit)
     fit <- eBayes(fit,trend=TRUE)
     return(fit)
}


LimmaTopTable <- function(fit) {
     LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")
     return(LimmaTable)
}




