#' @param DesignMatrix Model Matrix input whose column names do not contain "."s as it can cause javascript error
#' @param ControlFactorLevels character vector defining control/baseline level in all factor columns
#' 
# ContrastInput1 <- ContMultiTreat(DesignMatrix, ControlFactorLevels = c("Cont.Cont", "Cont.Pert"))
# ContrastInput2 <- ContMultiTreat(DesignMatrix, ControlFactorLevels = c("Cont.Cont","none", "Cont.Pert"))
# ContrastInput3 <- ContMultiTreat(DesignMatrix, ControlFactorLevels = "none")
# ContrastInput4 <- ContMultiTreat(DesignMatrix)
# makeContrasts(contrasts = ContrastInput4, levels = DesignMatrix)

ConTextInput <- function(DesignMatrix, ControlFactorLevels){
    message("Generating makeContrasts input")
    if(missing(ControlFactorLevels)){ message("no control factor levels supplied, generating all combinatorial contrasts")}
    ExpLevels <- colnames(DesignMatrix)
    
    if (length(colnames(DesignMatrix)) < 2) {
    stop("Cannot make contrasts with only 1 factor level")
    }
    
    if(missing(ControlFactorLevels)){
        ExpContrasts <- combn(ExpLevels, 2)
        ContrastInput <- apply(ExpContrasts, MARGIN = 2, paste, collapse = "-")
        
    } else if (length(ControlFactorLevels) == 1 && ControlFactorLevels == "none") {
        ExpContrasts <- combn(ExpLevels, 2)
        ContrastInput <- apply(ExpContrasts, MARGIN = 2, paste, collapse = "-")
        
    } else {
    ControlFactorLevels <- unique(ControlFactorLevels)
    ContrastInput <- 
        lapply(ControlFactorLevels, function(ControlLevel) {
            if (ControlLevel == "none") { NA
            } else { paste(ControlFactorLevels, setdiff(ExpLevels, ControlLevel), sep = "-") }
        })
    ContrastInput <- unlist(ContrastInput)
    }
    return(ContrastInput)
}


GenContrastMatrix <- function(ContrastString){
    ContrastString <- paste(ContrastString, collapse = ",")
    prestr="makeContrasts("
    poststr=",levels=DesignMatrix)"
    commandstr=paste(prestr,ContrastString,poststr,sep="")
    ContrastMatrix <- eval(parse(text=commandstr))
    return(ContrastMatrix)
}


# Limma Pipeline

#'
#'
#'
LimmaOutput <- function(ExpressionMatrix, DesignMatrix, ContrastMatrix){
    fit <- lmFit(ExpressionMatrix, DesignMatrix)
    fit <- contrasts.fit(fit, ContrastMatrix)
    fit <- eBayes(fit); message("fit <- eBayes(fit)")
    
    ContrastNames <- colnames(fit$coefficients)
    TopTableList <- lapply(1:length(ContrastNames), function(Index){
        LimmaTable <- topTable(fit, coef = Index, n=1000, adjust="BH")
        LimmaTable$Contrast <- ContrastNames[Index]
        gene <- rownames(LimmaTable)
        LimmaTable <- cbind.data.frame(gene, LimmaTable)
        LimmaTable
        })
    
    names(TopTableList) <- colnames(fit$coefficients)
    LimmaTable <- bind_rows(TopTableList)
    return(LimmaTable)
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



