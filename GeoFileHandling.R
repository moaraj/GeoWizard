#' Load Large Matrix Files
#'
#' @param GSE GEO accession number - str length [1]
#' @param GeoRepo File path to localGeoRepo - str length[1]
#' @param RdsCache Save Geo Matrix files as RDS inside GeoRepo for faster loading - boolean lenght[1]
#' 
#'  @example GeoRepo <- "~/GeoWizard/GEORepo"
#'  GSE <- "GSE69967"
#'  GPL <- NULL
#'  GSE <- "GSE60482"
#'  GPL <- "GPL11154"
#'  esetQuery <- LoadGEOFiles(GSE, GPL, GeoRepo)


#setInternet2(use=FALSE)

LoadGEOFiles <- function(GSE, GPL, GeoRepo){
     GeoRepoFiles <- dir(GeoRepo)

    if(missing(GPL)) {
        RegularExp <- paste(GSE, ".+matrix\\.txt\\.gz$", sep = "")
    } else {
        RegularExp <- paste(GSE,"-",GPL, ".+matrix\\.txt\\.gz$", sep = "")
    }
    
    MatrixFile <- grep(pattern = RegularExp, x = GeoRepoFiles, value = T)
    MatrixFilePath <- file.path(GeoRepo, MatrixFile)
    RDSFilePath <- paste(MatrixFilePath, ".rds", sep = "")
     
    if (isTRUE(file.exists(RDSFilePath)) &
        length(MatrixFile) != 0) {
        message("Loading Matrix File from RDS")
        GSEeset <- readRDS(RDSFilePath)
        
    } else if (isTRUE(file.exists(MatrixFilePath)) &
        length(MatrixFile) != 0) {
        message(paste("Matrix File for", GSE, "Found in GEORepo at", GeoRepo))
        GSEeset <- getGEO(filename = MatrixFilePath)
        message("Save ESET as RData for faster loading next time")
        saveRDS(object = GSEeset, file = RDSFilePath)
        
    } else if (length(MatrixFilePath) > 1) {
        GSElist <- lapply(MatrixFilePath, function(GseFile) {
        GSEeset <- getGEO(filename = MatrixFilePath)
        })
        names(GSElist) <- MatrixFile
        
    } else {
        GSEeset <- try(getGEO(GSE, destdir = GeoRepo))
        if (class(GSEeset) == "try-error") {
            Sys.sleep(0.5) # Retry GEO Servers, load adjustment changes acess server sometimes
            GSEeset <- getGEO(GSE)
        }
        if (class(GSEeset) == "try-error") {
        stop("Error Downloading Data Using GEO Query, download manually")
        }
        
        if(missing(GPL)) { return(GSEeset) 
        } else { GSEeset <- GetGPLFromList(GSEeset, GPL)}
        saveRDS(object = GSEeset, file = RDSFilePath)
        message("Save ESET as RData for faster loading next time")
    }
    
    if(class(GSEeset) == "list"){ GSEeset <- GSEeset[[1]] }
    return(GSEeset)
}

#' Retrieve the GSE matrix pertaining to selected GPL
#'
#' @param esetList list of expression set objects
#' @param GPL character string of GPL to extract from GSE

GetGPLFromList <- function(esetList, GPL){
    if (length(GPL) > 1) { warning("Multiple GPL supplied, using first"); GPL <- GPL[1] }
    
    if (class(esetList) == "list" | !missing(GPL) |!missing(esetList)) {
        GPLinList <- as.character(lapply(esetList, annotation))
        GPLSelectedIndex <- grep(pattern = GPL, x = GPLinList) # Replace Grep Funciton with index ==
        GSEeset <- esetList[[GPLSelectedIndex]]
        if (class(GSEeset)=="ExpressionSet") { return(GSEeset) } else { stop("GetGPLFromList - extracting datamatrix from GEO Query output")}
        
    } else {
        message("GSE List only contains one element or GPL not supplied")
        return(esetList)    
    }
    }

#' @param GSEeset eset of the GSE being processed
#' @param Annotation string containing one of gene annotation systems
#' @return ArrayData with Annotations
#'
#'

ConvertGSEAnnotations <- function(GSEeset, AnnotationType){
  message("Loading Expression Set Data")
  if(class(GSEeset) == "list"){ GSEeset <- GSEeset[[1]]
  } else if(class(GSEeset) != "ExpressionSet") { stop("Error in Loading Expression Data for Annotation")}
  
  ValidAnnotations <- c("Gene Title","ENTREZ_GENE_ID","Gene Symbol","RefSeq Transcript ID")
  if (grep(pattern = AnnotationType, x = ValidAnnotations)) {
    warning("Annotation type not recognized by ConvertGSEAnnotations funtion(), using 'Gene Symbol'")
    AnnotationType <- "Gene Symbol"
  }
  
  message("Extracting Feature annotations")
  NewAnnotations <- GSEeset@featureData@data[,AnnotationType]
  
  GeneSymbolEset <- exprs(GSEeset)
  rownames(GeneSymbolEset) <- NewAnnotations
  AnnotatedArrayData <- GeneSymbolEset
  
  return(AnnotatedArrayData)
}

#' Get GMT File from eset
#'
#'
#' @return a matrix in which the rows are genes and the columns are samples
#'
MakeGMTFile <- function(GSEeset){
     ArrayData <- exprs(GSEeset)
     GMT <- t(ArrayAndFactorDataDF)
     return(GMT)
}




