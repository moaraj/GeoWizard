#'
#'It functions basically the same as library(), but takes an extra version argument:
use <- function(package, version=0, ...) {
  package <- as.character(substitute(package))
  library(package, ..., character.only=TRUE)
  pver <- packageVersion(package)
  if (compareVersion(as.character(pver), as.character(version)) < 0)
    stop("Version ", version, " of '", package, 
         "' required, but only ", pver, " is available")
  invisible(pver)
}




#' Load Large Matrix Files
#'
#' @param GSE GEO accession number - str length [1]
#' @param GeoRepo File path to localGeoRepo MUST END WITH "/" - str length[1]
#' @param RdsCache Save Geo Matrix files as RDS inside GeoRepo for faster loading - boolean lenght[1]
#' 
#'  @example GeoRepo <- "~/GeoWizard/GEORepo/"
#'  GSE <- "GSE69967"
#'  GPL <- NULL
#'  GSE <- "GSE60482"
#'  GPL <- "GPL11154"
#'  esetQuery <- LoadGEOFiles(GSE, GPL, GeoRepo)


#setInternet2(use=FALSE)

LoadGEOFiles <- function(GSE, GPL, GeoRepo){
     GeoRepoFiles <- list.files(path = "~/GeoWizard/GEORepo/")

    if(missing(GPL)) { 
        stop("Must supply GPL even if there is only one in the GSE")
    } else {
        DataRDS <- paste(GSE,"-",GPL, ".rds", sep = "")
    }
    
    RDSFile <- grep(pattern = DataRDS, x = GeoRepoFiles, value = T)
    RDSFilePath <- file.path(GeoRepo, RDSFile)
    
    if (isTRUE(file.exists(RDSFilePath)) & length(RDSFile) != 0) {
        message("Loading Matrix File from RDS")
        eset <- readRDS(file = RDSFilePath)
    
    } else {
        
        eset <- try(getGEO(GSE, destdir = GeoRepo))
        if (class(eset) == "try-error") {
            Sys.sleep(0.5) # Retry GEO Servers, load adjustment changes acess server sometimes
            eset <- getGEO(GSE)
        }
        if (class(eset) == "try-error") {
        stop("Error Downloading Data Using GEO Query, download manually")
        }
        
        newRDSFilePath <- file.path(GeoRepo, DataRDS)
        saveRDS(object = eset, file = newRDSFilePath)
        message("Save ESET as RData for faster loading next time")
    }
    
    eset <- GetGPLFromList(eset, GPL)
    
    return(eset)
}

#' Retrieve the GSE matrix pertaining to selected GPL
#'
#' @param esetList list of expression set objects
#' @param GPL character string of GPL to extract from GSE

GetGPLFromList <- function(esetList, GPL){
    if (length(GPL) > 1) { warning("Multiple GPL supplied, using first"); GPL <- GPL[1] }
    
    if (class(esetList) == "list") {
        GPLinList <- as.character(lapply(esetList, annotation))
        GPLSelectedIndex <- grep(pattern = GPL, x = GPLinList) # Replace Grep Funciton with index ==
        eset <- esetList[[GPLSelectedIndex]]
        if (class(eset)=="ExpressionSet") { return(eset) } else { stop("GetGPLFromList - extracting datamatrix from GEO Query output")}
        
    } else { message("GSE List only contains one element or GPL not supplied")
        if (class(eset)=="ExpressionSet") { 
            return(esetList)}    
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
  
  message("Extracting Feature annotations")
  NewAnnotations <- GSEeset@featureData@data[,AnnotationType]
  
  GeneSymbolEset <- exprs(GSEeset)
  rownames(GeneSymbolEset) <- as.character(NewAnnotations)
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




