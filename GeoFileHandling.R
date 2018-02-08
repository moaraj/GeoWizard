#' Load Large Matrix Files
#'
#' @param GSE GEO accession number - str length [1]
#' @param GeoRepoPath File path to localGeoRepo - str length[1]
#' @param RdsCache Save Geo Matrix files as RDS inside GeoRepoPath for faster loading - boolean lenght[1]
#' 
#'  @example GeoRepoPath <- "~/GeoWizard/GEORepo"
#'  GSE <- "GSE69967"
#'  LoadGEOFiles(GSE, GeoRepoPath)


LoadGEOFiles <- function(GSE, GeoRepoPath){
     
     GeoRepoFiles <- dir(GeoRepoPath)

     RegularExp <- paste(GSE, ".+matrix\\.txt\\.gz$", sep = "")
     MatrixFileIndex <- grep(pattern = RegularExp, x = GeoRepoFiles)
     MatrixFilePath <- file.path(GeoRepoPath, GeoRepoFiles[MatrixFileIndex])
     RDSFilePath <- paste(MatrixFilePath, ".rds", sep = "")

     if(file.exists(RDSFilePath)){
          message("Loading Matrix File from RDS")
          GSEeset <- readRDS(RDSFilePath)
          
     } else if (file.exists(MatrixFilePath)) {
          message(paste("Matrix File for",GSE, "Found in GEORepo at", GeoRepoPath))
          GSEeset <- getGEO(filename = MatrixFilePath)
          message("Save ESET as RData for faster loading next time")
          saveRDS(object = GSEeset, file = RDSFilePath)
          
     } else {
          message(paste("Matrix File for", GSE, "Found in GEORepo at", GeoRepoPath))
          GSEeset <- getGEO(GEO = "GSE69967", destdir = "~/GeoWizard/GEORepo")
          saveRDS(object = GSEeset, file = RDSFilePath)
          message("Save ESET as RData for faster loading next time")
     }
     
     return(GSEeset)
}

#' @param GSEeset eset of the GSE being processed
#' @return ArrayData with GeneSymbol Annotations
#'
#'
ConvertGSEAnnotations <- function(GSEeset, Annotation = "GeneSymbol"){
  message("Loading Expression Set Data")
  if(class(GSEeset) == "list"){
    GSEeset <- GSEeset[[1]]
    }
  
  if (Annotation == "GeneSymbol") {
    message("Extracting Gene Symbol feature annotations")
    geneSymbolNames <- colsplit(
      string = GSEeset@featureData@data$`Gene Symbol`,
      pattern = " ",
      names = c("GeneSymbol", "SecondarySymbol"))
    
    GeneSymbolEset <- exprs(GSEeset)
    rownames(GeneSymbolEset) <- geneSymbolNames$GeneSymbol
    AnnotatedArrayData <- GeneSymbolEset
  }
  
  
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

