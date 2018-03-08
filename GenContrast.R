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

