GeoWizard <- "~/GeoWizard/"
GeoRepo <- "~/GeoWizard/GeoRepo"
setwd(GeoWizard)
#'
#' @param keyword is the Key word used to query GEO eutils esearch
#' @param retmax is used to determine the max number of GSE returns returned, default 1000
#'
#' @example GDSeSearch(keyword = "Mycophenolate mofetil", species = c("Homo sapiens", "Mus musculus"))
#' should contain this string in the output URL +(Homo+sapiens+OR+Mus+musculus)+

GDSeSearch <- function(keyword,
                       retmax = 1000) {
     if (missing(keyword)) {
          stop("No keyword query supplied")
     }
     
     database <- "gds" #GEO Datasets
     keyword <- gsub(pattern = " ",
                     replacement = "+",
                     x = keyword)
     suppfiles <- "cel[suppFile]"
     datatype <- "GSE"
     search_terms <- paste(keyword,
                           datatype, suppfiles,
                           sep = "+AND+")
     
     geneQuery <-
          paste(
               "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
               "&db=",
               database,
               "&term=",
               search_terms,
               "&retmax=",
               retmax,
               "&usehistory=y",
               sep = ""
          )
     return(geneQuery)
}


#' @param eSearchResultUrl Result from the function eSerach Uid provides the url where WebEnv and query key token are stored
#' @return Returns the Global Env and QuuryKey required to retieve the pages identified in the search qoeury
#' eSearchResultUrl <- eSearch_UID("Tofacitinib")
#' WebEnvExtract(eSearchResultUrl)

library(xml2)
#library(RCurl)

WebEnvExtract <- function(eSearchResultUrl) {
     PageXML <- eSearchResultUrl
     PageXML <- read_xml(PageXML)
     
     QueryKey <- xml_find_all(PageXML, ".//QueryKey")
     QueryKey <- xml_text(QueryKey)
     
     WebEnv <- xml_find_all(PageXML, ".//WebEnv")
     WebEnv <- xml_text(WebEnv)
     EnvVars <- list("QueryKey" = QueryKey,
                     "WebEnv" = WebEnv)
     return(EnvVars)
}



#' Retrieve eSummary XML
#'
#' @param Keyword is the keyword, or keywork being serach against the GEO data base
#' @return  Returbns the URL for where the data from the query is stored in an XML format
#' @example GSEeSummary("Mycophenolate mofetil")

GSEeSummary <- function(EnvKeys) {
     if (class(EnvKeys) != "list") {
          stop("Something wrong in Web Query function")
     }
     eutils_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
     esummary_query <- "esummary.fcgi?db=gds&query_key=%s&WebEnv=%s"
     esummary_link <-
          sprintf(esummary_query, EnvKeys[["QueryKey"]], EnvKeys[["WebEnv"]])
     esummary_link <- paste(eutils_url, esummary_link, sep = "")
     return(esummary_link)
}

#' Retreive GSE records with Keyword
#' Grab all the GSE from the GEO data base that have the keyword as a keyword in the study
#' @param Keyword the key word being searched against the GEO database
#' @return a dataframe containing the GSE and Meta data that matched the keyword Query
#' @example View(FetchGSEwithKeyword("Mycophenolate mofetil"))

FetchGSEwithKeyword <- function(eSummaryUrl, keyword) {
     GseList <- eSummaryXMLParse(eSummaryUrl)
     
     if (length(GseList[, 1]) == 0) {
          message(sprintf("No GSEs for keyword %s, retrieved", keyword))
          GseList[1, ] <- rep("No Records in GEO found", 8)
     } else {
          message(sprintf(
               "All GSE accession numbers for keyword %s, retrieved",
               keyword
          ))
          GseList <- as.data.frame(GseList, stringsAsFactors = F)
          GseList$n_samples <- as.numeric(GseList$n_samples)
          GseList$gdsType <-
               unlist(strsplit(GseList$gdsType, ";"))[1]
          
          GseList$taxon <-
               gsub(pattern = ";",
                    replacement = " \n",
                    GseList$taxon)
          keyword <- as.character(keyword, rep(nrow(GseList)))
     }
     
     GseList <- cbind(keyword, GseList)
     return(GseList)
}


#' @param PageXML esummary page
#' @return Returns all the GSE acession numbers found in the XML input
#'
#' PageXML <- GSEeSummary("Mycophenolate mofetil")
#' eSummaryXMLParse(PageXML)
#' eSummaryXMLParse(GSEeSummary("Mycophenolate mofetil"))

library(xml2)
eSummaryXMLParse <- function(PageXML) {
     PageXML <- read_xml(PageXML)
     GseDataCol <- c(
          "Accession",
          "GPL",
          'title',
          'gdsType',
          'taxon',
          'n_samples',
          'suppFile',
          'summary'
     )
     
     GseDF <- lapply(GseDataCol, function(MetaData) {
          message(sprintf("Retrieving %s for the GSE files", MetaData))
          xmlQuery <-
               paste(".//DocSum/Item[@Name='", MetaData, "']", sep = "")
          xmlRes <- xml_find_all(PageXML, xmlQuery)
          xml_text(xmlRes)
     })
     
     GseDF <- do.call(cbind, GseDF)
     GseDF <- data.frame(GseDF, stringsAsFactors = F)
     colnames(GseDF) <- GseDataCol
     return(GseDF)
}

#' @param keyword keyword to be queried against GEO data base
#' @retmax how many results to return
#'
#' @example View(FetchGseDescDF("Mycophenolate mofetil") )

FetchGseDescDF <- function(keyword, retmax = 1000) {
     message(sprintf("Retrieving records for %s", keyword))
     eSearchUrl <- GDSeSearch(keyword)
     EnvVars <- WebEnvExtract(eSearchUrl)
     eSummaryUrl <- GSEeSummary(EnvVars)
     GSEDescDF <- FetchGSEwithKeyword(eSummaryUrl, keyword)
     return(GSEDescDF)
}

#'
#' @param text a chacter string
#' @return Captilaises the first letter of each word in the character string
TitleCase <- function(text) {
     gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", text, perl = TRUE)
}

#' Fetch Multi GEO Query Results
#'
#' @param file a csv file that contains
#' @param MolQuery character vector or list of molcules that are searched against the GEO Database
#'
#' @return a list with a chacacter vector of GSE acession number for each keyword query
#' @example
#' MolQuery <- c("Mycophenolate mofetil", "Tofacitinib")
#'
#' View(MultiGSEQuery(c("Mycophenolate mofetil", "Tofacitinib")))
#' GseTable <- MultiGSEQuery(c("Mycophenolate mofetil", "Corticosteroids"), species = c("Homo sapiens", "Mus musculus"))
#'

MultiGSEQuery <- function(MolQuery) {
     MolQuery <- as.character(unlist(MolQuery))
     GeoRes <- lapply(MolQuery, FetchGseDescDF)
     GeoRes <- do.call(rbind, GeoRes)
     
     GeoRes["keyword"] <- as.character(unlist(GeoRes["keyword"]))
     colnames(GeoRes)[grep(pattern = "title", x = colnames(GeoRes))] <- "gse.title" # change colnames to apply SQL join functions
     colnames(GeoRes)[grep(pattern = "Accession", x = colnames(GeoRes))] <- "series_id"
     
     return(GeoRes)
}

#' @param GseTable is GEO serach res dataframe contining keyword series_id GPL gse.title gdsType taxon n_samples suppFile summary
#' GseTable <- MultiGSEQuery(c("Mycophenolate mofetil", "Tofacitinib"), species = c("Homo sapiens", "Mus musculus"))
#' GseGsmTable <- SqlQueryMain(GseTable)
#' 

library(GEOmetadb)

SqlQueryMain <- function(GseTable) {
  
  if (is.data.frame(GseTable)) { GSEList <- as.character(unlist(GseTable['series_id']))
  } else if (class(gseSqlQuery) == "character") { GSEList <- gseSqlQuery }
  
  con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
  
  GsmTable <- FetchGSMMetaData(GSEList, con)
  colnames(GsmTable)[grep(pattern = "title", x = colnames(GsmTable))] <- "gsm.title"
     
  GseGsmTable <- dplyr::inner_join(GseTable, GsmTable)
  
  NotFoundVec <- lapply(GSEList, EmptySQLRes, GseGsmTable)
  NotFoundVec[sapply(NotFoundVec, is.null)] <- NULL
  if (length(NotFoundVec) > 0) {
    GseGsmTable <- bind_rows(GseGsmTable, NotFoundVec)  
  }
  
  return(GseGsmTable)
}

#' Get GSM level data for all GSMs in a GSE
#' 
#' @param character vector of GEO Acession IDs ie. "GSE69967"
#' @con SQLlite datebase connectection to MetaDB SQL database
#'
#' @return a dataframe with gse, gsm, title, description and characteristics
#' 
FetchGSMMetaData <- function(GSEList, con) {
  
  gseQuery <- paste("\'",GSEList, "\'", collapse = ", ", sep = "") 
  message(sprintf("Getting GSMs metadata for %s", gseQuery))
  SqlQuery <- paste(
      "select series_id, gsm, title, description, characteristics_ch1 ",
      "from gsm ", "where series_id in (" ,gseQuery, ")" ,
      sep = ""
    )
  
  res <- dbGetQuery(con, SqlQuery)
  
  NotFoundVec <- lapply(GSEList, EmptySQLRes, res) # if one of many GSE inputs not found
  NotFoundVec[sapply(NotFoundVec, is.null)] <- NULL
  if (length(NotFoundVec) > 0) {
  res <- bind_rows(res, NotFoundVec)  
  }
  
  if (length(res[, 1]) == 0) { # if all of many GSE inputs not found in metaDB
    message("GSE was not found in SQLite Server")
    res[1, 1] <- GSEList
    res[1, 2:5] <- "Metadata not in SQLite Server"
  }
  
  res <- as.data.frame(res)
  return(res)
}

#' Determine if GSE Meta data was found in the MetaDB 
#'
#' @param res DF result from the MetaDB sqlite serach
#' @param gse GSE Accession Number whose inclusion in search results is to be detemined
#'
#' @return a character vector indicating the GSE was not found in the SQL search

EmptySQLRes <- function(gse, res){
  FoundGSE <- unique(res$series_id)
  GseInRes <- grepl(pattern = gse, x = FoundGSE)
  GseInRes <- any(GseInRes, na.rm=TRUE)
  
  if(!GseInRes){
    NotFoundVec <- res[1,1:5]
    NotFoundVec [1, 1] <- gse
    NotFoundVec [1, 2:5] <- "Metadata not in SQLite Server"
    return(NotFoundVec)
  }
  return(NULL)
}



#'
#' @param GseGsmTable in GSE GSM joined table
#' @return The Characteristic column split into many columns
#'


SeparateCharacteristics <- function(GseGsmTable, CharInputs) {
     GseGsmDF <- data.frame(GseGsmTable, stringsAsFactors = F)
     GSEinSet <- unique(GseGsmDF['series_id'])
     GseFirstOccurance <- match(GSEinSet, GseGsmDF[, "series_id"])
     ExpVarColsDF <-
          data.frame(GseGsmDF[, CharInputs], stringsAsFactors = F)
     
     if (length(ExpVarColsDF) == 0) {
          stop(
               sprintf(
                    "The no multi level factors are found in %s, picks other columns",
                    CharInputs
               )
          )
     } else {
          message("Hello")
          ExpVarColsDF['CatVarText'] <-
               apply(X = ExpVarColsDF,
                     MARGIN = 1,
                     paste,
                     collapse = ";")
          ExpVarColsDF['CatVarText'] <-
               apply(X = ExpVarColsDF['CatVarText'], MARGIN = 1, function(text) {
                    gsub(
                         pattern = ",",
                         replacement = ";",
                         x = text
                    )
               })
          
     }
     nColsRequired <-
          max(str_count(string = ExpVarColsDF[GseFirstOccurance, 'CatVarText'] , pattern = ";")) + 1
     ExpColNames <- paste("ExpVar", 1:nColsRequired, sep = "")
     
     GseGsmDF <-
          cbind.data.frame(
               GseGsmDF %>% select(-one_of(CharInputs)),
               ExpVarColsDF %>%
                    tidyr::separate(
                         CatVarText,
                         into = ExpColNames,
                         sep = ",|;",
                         fill = "right"
                    )
          )
     return(GseGsmDF)
}



########################################################

#'
#'
#'
#' @example GseTable <- MultiGSEQuery(c("Tofacitinib"), species = c("Homo sapiens", "Mus musculus"))
#' GsmLabelMain(MolQuery =c("Tofacitinib"), speciesQuery = c("Homo sapiens", "Mus musculus"))

GsmLabelMain <- function(MolQuery) {
     GseTable <- MultiGSEQuery(MolQuery)
     GseGsmTable <- SqlQueryMain(GseTable)
     
     GseinTable <- unique(GseTable[, 'Accession'])
     GsmDesignDF <- GseGsmExpVarsTable(GseGsmTable, 'GSE69967')
     return(list("GseTable" = GseTable,
                 "SingleGseTable" = GsmDesignDF))
}

#'
#'
#' @param GseGsmTable GSE GSM joined table
#' @return Returns the GSE GSM table with the split characteristics columns which multiple factor levels
#'
GseGsmCharExpand <- function(GseGsmTable, CharInputs) {
     GseGsmDF <- GseGsmTable
     message("Expanding Columns with Factor Conataining Text")
     message(sprintf(
          "Columns containing Factors are %s",
          paste(CharInputs, collapse = " and ")
     ))
     
     if (is.null(CharInputs)) {
          stop(
               paste(
                    "You must pick atleast one of option",
                    "'gsm.title', 'description' or 'characteristics",
                    "to use has experimetanl Blocks in downstream analysis"
               )
          )
     }
     
     BuildRegEx <- paste(CharInputs, collapse = "|")
     GseGsmTableMeta <- GseGsmDF %>% select(-one_of(CharInputs))
     CharsDF <- SeparateCharacteristics(GseGsmDF, CharInputs)
     nExpVars <- sum(str_count(string = colnames(CharsDF), pattern = BuildRegEx))
     MultiLevelChars <- DescerningFactors(CharsDF)
     
     if (length(MultiLevelChars != 0)) {
          UsefulCharDF <- CharsDF [, MultiLevelChars]
          GsmDF <- cbind(GseGsmTableMeta, UsefulCharDF)
          nLast <- length(colnames(GsmDF))
          colnames(GsmDF)[(1 + nLast - length(MultiLevelChars)):nLast] <-
               MultiLevelChars
     } else {
          ExpVar1 <-
               paste("No Multi Level Factors Detected in ",
                     paste(CharInputs),
                     collapse = " ")
          GsmDF <- cbind.data.frame(GseGsmTableMeta, ExpVar1)
     }
     return(GsmDF)
}


#' Factors that Change in the Study
#'
#' Functions that Returns a character vector of Factor Columns that will be helpul
#' For Determining Experimental Design
#'
#' @example DescerningFactors(GsmMetaDataFin)
#' FactorDf <- GseGsmTable %>% filter(series_id == "GSE45551")
#' "GSE69967"  "GSE45551"  "GSE45514" "GSE104509"

DescerningFactors <- function(FactorDf) {
     FactorDf <- FactorDf %>% select(starts_with("ExpVar"))
     multiLevelFactors <-
          sapply(FactorDf, function(x) {
               nlevels(factor(x))
          })
     multiLevelFactors <- multiLevelFactors > 1
     message(sprintf("Number of Useful Factors is %d", multiLevelFactors))
     
     if (length(multiLevelFactors) == 0) {
          stop("No Factors with more than one level found using GSM select columns")
     } else {
          multiLevelFactors <- colnames(FactorDf[multiLevelFactors == 1])
     }
     
     return(multiLevelFactors)
}

#'
#' This Funciton takes in the names of a List of Classification Dataframes and returns the names
#' of which ones have both Control and Perterbation classifications, indicating they would be useful
#' for generating a design Matrix
#'
DescerningClassDF <- function(ClassListDF) {
     message("Finding Useful Default ExpVars")
     ListElementName <- names(ClassListDF)
     ClassDFRows <-
          lapply(ListElementName, DescerningClassifications, ClassListDF)
     ClassDF <-
          data.frame(do.call(rbind, ClassDFRows), stringsAsFactors = F)
     colnames(ClassDF) <- c("ExpVar", "isUsefulClassDF")
     
     if (sum(as.numeric(ClassDF['isUsefulClassDF'] == 'UsefulDF')) == 0) {
          message("No Useful Factor Classifications")
          res <- ClassListDF[1]
     } else {
          ListExtractNames <- ClassDF %>%
               filter(isUsefulClassDF == "UsefulDF") %>%
               select(ExpVar) %>%
               unlist
          
          ListExtractIndex <-
               grep(pattern = paste(ListExtractNames, collapse = "|"),
                    x = ListElementName)
          res <- ClassListDF[ListExtractIndex]
          names(res) <- ListExtractNames
     }
     return(res)
}

DescerningClassifications <-
     function(ListElementName, ClassListDF) {
          ClassDF <- ClassListDF[[ListElementName]]
          ClassDF <- ClassDF %>% select(matches("ContClass|PertClass"))
          multiClassCol <-
               sapply(ClassDF, function(x) {
                    nlevels(factor(x))
               })
          multiClassCol <- sum(as.numeric(multiClassCol)) > 2
          
          if (multiClassCol == FALSE) {
               multiClassVec <- c(ListElementName, "NotUsefulDF")
          } else {
               multiClassVec <- c(ListElementName, "UsefulDF")
          }
          return(multiClassVec)
     }

###################################################################################################################



#########################

#'
#'
#'
#' @param str character vector
#' str <- GsmDesignDF$ExpVar1

require(SnowballC)
require(parallel)

stemString <- function(str) {
     str <- tolower(str)
     str <-
          gsub(pattern = "[[:punct:]]\\s",
               replacement = " ",
               x = str)
     #     str <- gsub("([a-z])([1-9]+)", "\\1 \\2", str)
     #     str <- gsub("([1-9]+)([a-z])", "\\1 \\2", str)
     str <- gsub(pattern = "ment\\s",
                 replacement = " ",
                 x = str)
     str <- gsub(pattern = "er$",
                 replacement = "",
                 x = str)
     str <- sapply(str, gsub, pattern = "\t", replacement = "")
     
     nColsRequired <- sapply(str, strsplit, split = '\\s')
     nColsRequired <- max(unlist(sapply(nColsRequired, length)))
     
     textColNames <- paste("snippet", 1:nColsRequired, sep = "")
     strDF <- data.frame(str, stringsAsFactors = F)
     colnames(strDF) <- "StartString"
     strDF <-
          strDF %>% tidyr::separate(StartString,
                                    into = textColNames,
                                    sep = " ",
                                    fill = "right")
     strDF[is.na(strDF)] <- ""
     
     strDF <- data.frame(stringsAsFactors = F,
                         apply(strDF, MARGIN = c(1, 2), function(s) {
                              wordStem(s, language = "english")
                         }))
     
     str <-
          as.character(apply(
               X = strDF,
               MARGIN = 1,
               paste,
               collapse = " "
          ))
     return(str)
}
