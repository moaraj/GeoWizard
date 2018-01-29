library(jsonlite)
library(RCurl)
library(htmltidy)
library(xml2)

queryUrl <- "https://oneproduct-dev.roche.com/api/public/search/rogain"
tokenUrl <- "https://oneproduct-dev.roche.com/api/public/drug/58d4a6337349505e546f359a"

pageDoc <- readLines(tokenUrl)
