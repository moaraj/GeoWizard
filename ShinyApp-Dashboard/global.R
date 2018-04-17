# YOU HAVE TO CHANGE THIS THIS LINE TO YOUR GEOWIZARD CLONE
GeoWizard <- "D:/GitHub/GeoWizard"
GeoRepo <- paste(GeoWizard,"GeoRepo", sep = "/")
dir.create(GeoRepo, showWarnings = FALSE) #dir.create does not crash if dir doesn't exist, just gives warning

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)

library(reshape2)
library(plyr)
library(tidyr)
library(stringr)
library(dplyr)

library(ggplot2)
library(gplots)
library(ggridges)
library(vcd)
library(RColorBrewer)
library(DT)

#GeoParse
library(xml2)
library(RCurl)
library(XML)
library(GEOquery)
library(GEOmetadb)

library(limma)
library(Biobase)
#install.packages("https://bioarchive.galaxyproject.org/BioQC_1.4.0.tar.gz", repos = NULL, lib =  "~/GeoWizard/RequiredPackages/")
library(BioQC)
library(Rcpp)
library(BioQC)
library(vcd)

library(RColorBrewer)
library(beanplot)
library(vioplot)
library(beeswarm)
library(ggbeeswarm)
library(plotly)
library(heatmaply)

source(file = file.path(GeoWizard, "GeoParse.R")) # Eutils to find GSE sample information
source(file = file.path(GeoWizard, "GSMAnnotation.R")) 
source(file = file.path(GeoWizard, "GeoFileHandling.R")) # All funtions for downloading GEO files and turning them into a matrix
source(file = file.path(GeoWizard, "QCAnalysis.R")) # All functions for Downlaod and QC tab
source(file = file.path(GeoWizard, "ExpressionAnalysis.R")) 

source(file = file.path(GeoWizard, "Ontologies/MoleculeLibraries.R")) # Cell line and Disease matching
source(file = file.path(GeoWizard, "keywords.R")) # Training Sets/ Not used
source(file = file.path(GeoWizard, "helpers.R"))  # ShinyJS 
source(file = file.path(GeoWizard, "SeriesHanding.R"))
source(file = file.path(GeoWizard, "GenContrast.R"))
source(file = file.path(GeoWizard, "GeneBoxPlot.R"))




