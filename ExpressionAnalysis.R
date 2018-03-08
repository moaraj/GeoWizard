# Limma Pipeline

#'
#'
#'
LimmaOutput <- function(ArrayData, DesignMatrix, ContrastString){
    ContrastMatrix <- makeContrasts(ContrastMatrix, levels = DesignMatrix)    
    ContrastMatrix <- PairWiseContrast(colnames(DesignMatrix))
    fit <- lmFit(GeneSymbolArrayData, DesignMatrix)
    fit <- contrasts.fit(fit, ContrastMatrix)
    fit <- eBayes(fit)
    
    ContrastNames <- colnames(fit$coefficients)
    TopTableList <- lapply(1:length(ContrastNames), function(Index){
        LimmaTable <- topTable(fit, coef = Index, n=1000, adjust="BH")
        LimmaTable$Contrast <- ContrastNames[Index]
        LimmaTable
        })
     
    names(TopTableList) <- colnames(fit$coefficients)
    LimmaTable <- bind_rows(TopTableList)
     
     return(TopTable)
}

PairWiseContrast <-
    function(levels) {
    n <- length(levels)
    Contrast <- matrix(0,n,choose(n,2))
    rownames(Contrast) <- levels
    colnames(Contrast) <- 1:choose(n,2)
    k <- 0
    for (i in 1:(n-1))
    for (j in (i+1):n) {
    k <- k+1
    Contrast[i,k] <- 1
    Contrast[j,k] <- -1
    colnames(Contrast)[k] <- paste(levels[i],"-",levels[j],sep="")
    }
    Contrast
    }

#Deseq Pipline

#EdgeR Pipline