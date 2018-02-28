GeoWizard <- "~/GeoWizard"
setwd(GeoWizard)
source("ShinyApp-Dashboard/global.R")


if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(paste('\nConnected Database Tables:', dbListTables(con)))

MolQuery = c("Mycophenolate mofetil")
MolQuery = c("Tofacitinib")
#MolQuery = "GSE57800"


GseTable <- MultiGSEQuery(MolQuery)
GsmTable <- SqlQueryMain(GseTable)
GseGsmTable <- GseTable %>% dplyr::select(-one_of("GPL")) %>% 
  dplyr::inner_join(GsmTable, "series_id")
  


# Select Single GSE from GSE-GSM Table
gseList <- unique(GseGsmTable[,'series_id'])
selectedGse <- 1
selectedGse <- gseList[selectedGse]

# Filter Selected GSE
Step_1 <- GseGsmTable %>% filter(series_id %in% selectedGse) 
# Classify the Summary and Return the FIltered GSE GSM DF
Step_2 <- ClassSummary(GsmDesignDF = Step_1)

# Expands Character Column 
CharInputs <- c("characteristics_ch1", "gsm.title","description")
Step_3 <- GseGsmCharExpand(GseGsmTable = Step_2, CharInputs)

Step_3_colnames <- grep(pattern = "ExpVar[[:digit:]]", x = colnames(Step_3), value = T)
Step_3B <- data.frame(Step_3[,Step_3_colnames])
colnames(Step_3B) <- Step_3_colnames

# Classify All the Column in the DF
Step_4 <- ClassGsmText(Step_3B)

# Find the Useful Factors
Step_5A <- DescerningClassDF(ClassListDF = Step_4) # waring this funciton passes 1st factor DF if non found
Step_5B <- AddSeriesDFs(ClassDFList = Step_4, "time")
Step_5C <- AddSeriesDFs(ClassDFList = Step_4, "titration")

Step_5 <- c(Step_5A, Step_5B, Step_5C)
Step_6 <- DesignLabs(data.frame(Step_5))


GSM <- Step_1$gsm
GraphDF <- cbind(GSM, Step_6)
DesignMatrix <- model.matrix( ~ ExpVar4.Text, GraphDF )

######### Function to Download File
GeoRepoPath <- "~/GeoWizard/GEORepo"
GSE <- selectedGse
GSEeset <- LoadGEOFiles(GSE = GSE, GeoRepo)
ArrayData <- exprs(GSEeset)
FactorDF <- Step_6[1:2]

FeatureData <- fData(GSEeset)
######### Function to Make GMT Ggplotable
GSM <- colnames(ArrayData)
FactorGMT <-  GenFactorGMT(exprs(GSEeset), FactorDF)
FactorGMTMelt <- melt(FactorGMT)

GSEgmtDF <- GenGMTggplotDF(GSEeset = GSEeset,FactorDF = FactorDF)

######## GMT Boxplots and Histograms
GMTBoxplot(GSEgmtDF = GSEgmtDF, BoxPlotType = "Sample", PlotBy = "Overall", PlotFactor = "ExpVar3.Text" )
GMTBoxplot(GSEgmtDF = GSEgmtDF, BoxPlotType = "Sample", PlotBy = "Factor", PlotFactor = "ExpVar3.Text" )
GMTBoxplot(GSEgmtDF = GSEgmtDF,BoxPlotType = "Gene" , PlotBy = "Overall",PlotFactor = "ExpVar3.Text")
GMTBoxplot(GSEgmtDF = GSEgmtDF,BoxPlotType = "Gene" , PlotBy = "Factor",PlotFactor = "ExpVar3.Text")

GMTHistPlot(GSEgmtDF, "Gene", "ExpVar3.Text", 10)
GMTHistPlot(GSEgmtDF, "Sample", "ExpVar3.Text", 10)
GMTHistPlot(GSEgmtDF, "Factor", "ExpVar3.Text", 10)

########## Convert to Gene Symbol
source(file = "GeoFileHandling.R")
GeneSymbolArrayData <- ConvertGSEAnnotations(GSEeset = GSEeset, Annotation = "RefSeq")

######### QC
source(file = "QCAnalysis.R")
RunBioQC(GMT = GeneSymbolArrayData)

PCAPlots <- PlotPCA(ArrayData)

######### Limma
res <- LimmaOutput(GeneSymbolArrayData,DesignMatrix)


TopTableFilter <- "logFC"
     LimmaTable <- res %>% arrange_(TopTableFilter)
     nGenes <- 10
     
     TopGenes <- LimmaTable[1:nGenes,1]
     TopGenes[TopGenes == ""] <- NA
     TopGenes <- na.omit(TopGenes)
     
     FactorGMT <- GenFactorGMT(GSEeset, FactorDF)
     colnames(FactorGMT) <- make.names(colnames(FactorGMT), unique=TRUE)
     
     ColumnsToKeep <- colnames(FactorGMT)
     ColumnsToKeep <- grep(pattern = "GSM|ExpVar",x = ColumnsToKeep, value = T)
     ColumnsToKeep <- c(ColumnsToKeep, TopGenes)
     
     FactorGMT <- FactorGMT %>% select(one_of(ColumnsToKeep))





View(head(res))
limma::plotMA(GSEeset)
limma::plotMA(fit, 3)

View(model.matrix(~ExpVar3.Text+ExpVar4.Text, Step_6))



ConvertGSEAnnotations()
message("Loading Expression Set Data")
GSEeset <- GSEdata$Eset

message("Extracting Gene Symbol feature annotations")
geneSymbolNames <- colsplit(
  string = GSEeset@featureData@data$`Gene Symbol`,
  pattern = " ",
  names = c("GeneSymbol", "SecondarySymbol"))

GeneSymbolEset <- exprs(GSEeset)
rownames(GeneSymbolEset) <- geneSymbolNames$GeneSymbol
GeneSymbolEset

