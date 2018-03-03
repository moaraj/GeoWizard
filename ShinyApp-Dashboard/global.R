GeoWizard <- "~/GeoWizard/"
GeoRepo <- "~/GeoWizard/GeoRepo"

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
library(GEOquery)
library(GEOmetadb)



library(limma)
library(Biobase)
library(BioQC) #install.packages("https://bioarchive.galaxyproject.org/BioQC_1.4.0.tar.gz", repos = NULL)
library(vcd)


library(RColorBrewer)
library(beanplot)
library(vioplot)
library(beeswarm)
library(ggbeeswarm)
library(plotly)
library(heatmaply)

#library(FactoMineR)
#library(factoextra)

source(file = file.path(GeoWizard, "GeoParse.R"))
source(file = file.path(GeoWizard, "GSMAnnotation.R"))
source(file = file.path(GeoWizard, "GeoFileHandling.R"))
source(file = file.path(GeoWizard, "QCAnalysis.R"))
source(file = file.path(GeoWizard, "ExpressionAnalysis.R"))

source(file = file.path(GeoWizard, "Ontologies/MoleculeLibraries.R"))
source(file = file.path(GeoWizard, "GeoTrainingSets/keywords.R"))
source(file = file.path(GeoWizard, "helpers.R"))
source(file = file.path(GeoWizard, "SeriesHanding.R"))



