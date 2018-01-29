library(shiny)
library(shinydashboard)
library(shinyjs)


ui <- fluidPage(
     actionButton(inputId = "GoButton", label = "Run"),
     fluidRow(
          column(10,
                 uiOutput("FilterGSMbyFactor")
                 )
          ),
     fluidRow(column(6,
                     DT::dataTableOutput(outputId = "FinalTable")))
     )


server <- function(input, output) {
     
     ExampleData <- reactiveValues(DF = mtcars[,1:4])
     
     output$FilterGSMbyFactor <- renderUI({
          DF <- ExampleData$DF
          NamesIndex <- colnames(DF)
          
          FactorLevelInput <- 
               lapply(NamesIndex, function(ColName){
                    ColLevels <- DF[,ColName]
                    column(3,
                    selectInput(inputId = paste("Gsm_",ColName, sep = ""),
                                label = paste("Filter Level in", ColName),
                                choices = unique(ColLevels),
                                selected = unique(ColLevels),
                                multiple = T,
                                selectize = T)
                    )
               })

     })
     
     SampleFilterIndex <- reactiveValues()
     
          observe({
               input$GoButton
               DF <- ExampleData$DF
               NamesIndex <- colnames(DF)
               rowsToKeep <- lapply(NamesIndex, function(ColName){
                    message(ColName)
                    InputName<- paste("Gsm_", ColName, sep = "")
                    message(InputName)
                    FilterLevels <- input[[InputName]]
                    message(FilterLevels)
                    matches <- grep(paste(FilterLevels,collapse="|"), DF[,ColName], value=F)
                    })
               
               names(rowsToKeep) <- NamesIndex
               message(class(rowsToKeep))
               message(lapply(rowsToKeep, paste0))
               message("Get the combinations of names of list elements")
               
               nms <- combn( names(rowsToKeep) , 2 , FUN = paste0 , collapse = "" , simplify = FALSE )
               message(" Make the combinations of list elements")
               ll <- combn( rowsToKeep , 2 , simplify = FALSE )
               message("# Intersect the list elements")
               out <- lapply( ll , function(x) intersect( x[[1]] , x[[2]] ) )
               message("# Output with names")
               setNames( out , nms )
               message("# Find the length of all row name vectors")
               SmallestSet <- unlist(lapply(out, length))
               message("# Find the location of the smaller element")
               RowsToKeep <- out[which.min(SmallestSet)]
               SampleFilterIndex$RowFilterIndex <- unlist(RowsToKeep)
          })
     
     
     output$FinalTable <- renderDataTable({
          DF <- ExampleData$DF
          DF <- DF[SampleFilterIndex$RowFilterIndex,]
          datatable(data = DF)

     })
     
     
}







library(GEOquery)

GSEAccession <- "GSE69967"
GeoRepo <- "/pstore/home/hasanm7/GeoWizard/GeoRepo/"

RepoFiles <- dir(path = GeoRepo)
FileName <- RepoFiles[grep(pattern = GSEAccession, x = RepoFiles)]
GzFileName <- FileName[grep(pattern = "gz", x = FileName, fixed = T)]
FilePath <- file.path(GeoRepo, GzFileName)

if (file.exists(FilePath)) {
     message("GSE data found in GEO repo")
     GSEeset <- getGEO(filename = FilePath, GSEMatrix = T, destdir = GeoRepo)
} else {
     message("GSE data not found, downloading from GEO")
     GSEeset <- getGEO(GEO = GSEAccession, GSEMatrix = T, destdir = GeoRepo)
}




library(reshape2)
library(dplyr)

EsetAffy <- exprs(GSEeset)
GeneSymbolrnames <- colsplit(string = GSEeset@featureData@data$`Gene Symbol`,
                             pattern = " ", 
                             names = c("GeneSymbol", "SecondarySymbol"))
EsetGeneSymbol <- EsetAffy
rownames(EsetGeneSymbol) <- GeneSymbolrnames$GeneSymbol


library(Biobase)
library(BioQC)
library(RColorBrewer)
library(gplots)

myEset <- EsetGeneSymbol 

gmtFile <- system.file("extdata/exp.tissuemark.affy.roche.symbols.gmt", package="BioQC")
gmt <- readGmt(gmtFile)

genesets <- BioQC::readGmt(gmtFile)
testIndex <- BioQC::matchGenes(genesets, myEset)
testIndex <- matchGenes(genesets, myEset)

wmwResult.greater <- wmwTest(myEset, testIndex, valType="p.greater")
wmwResult.less <- wmwTest(myEset, testIndex, valType="p.less")
wmwResult.Q <- wmwTest(myEset, testIndex, valType="Q")

bioqcResFil <- filterPmat(wmwResult.greater, 1E-8)
bioqcAbsLogRes <- absLog10p(bioqcResFil)


heatmap.2(bioqcAbsLogRes, Colv=TRUE, Rowv=TRUE,
          cexRow=1,cexCol = 1, dendrogram = "both",
          col=rev(brewer.pal(11, "RdBu")),
          labCol=1:ncol(bioqcAbsLogRes),
          main = "BioQC results for GSE",
          xlab = "Sample Number",
          trace = 'row')


GeneNames <- rownames(EsetAffy)
EsetDF <- data.frame(EsetAffy)
EsetDF <- cbind(GeneNames, EsetDF)
EsetMelt <- melt(EsetDF)
colnames(EsetMelt) <- c("GeneNames", "GSM", "Expression")


library(limma)

DesignDF <- cbind.data.frame(
     GSEeset@phenoData@data$characteristics_ch1.2,
     GSEeset@phenoData@data$characteristics_ch1.3)

colnames(DesignDF) <- c("Treatment", "Tissue")
DesignMatrix <- model.matrix(~Treatment + Tissue, DesignDF)

ArrayData <- exprs(GSEeset)
ArrayData <- sapply(ArrayData, function(x){x^2})

fit <- lmFit(ArrayData, DesignMatrix)
fit <- eBayes(fit)
fit <- eBayes(fit,trend=TRUE)
LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")


plotSA(fit, main="Gene-level")
plotMD(fit)
plotMA(fit)


options(digits=2)

pValueThresHold <- 10
logFCThresHold <- 1.5
LimmaTable <- LimmaTable %>% 
     mutate(Threshold = logFC > 1.5 | logFC < -1.5) %>%
     mutate(Threshold = as.numeric(Threshold)) %>%
     mutate(Threshold = Threshold + as.numeric(-log(LimmaTable$adj.P.Val) >= 10))

ggplot(LimmaTable, 
       aes(x = logFC, 
           y = -log(adj.P.Val),
           color = factor(Threshold > 1))) + 
     geom_point() + theme_grey()

     


LimmaPlotDF <- LimmaTable
colnames(LimmaPlotDF) <- c("log2FoldChange", "baseMean", "t", "P.Value", "padj", "B", "Threshold")
ggmaplot(LimmaPlotDF, main = expression("Group 1" %->% "Group 2"),
         fdr = 0.05, fc = 2, size = 0.4,
         palette = c("#B31B21", "#1465AC", "darkgray"),
         genenames = as.vector(rownames(LimmaPlotDF)),
         legend = "top", top = 20,
         font.label = c("bold", 11), label.rectangle = TRUE,
         font.legend = "bold",
         font.main = "bold",
         ggtheme = ggplot2::theme_minimal())






fit <- lmFit(EsetGeneSymbol, DesignMatrix)
fit <- eBayes(fit)
fit <- eBayes(fit,trend=TRUE)
LimmaTableSymbol <- topTable(fit, coef=2, n=4000, adjust="BH")











shinyApp(ui, server)
