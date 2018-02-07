GeoRepoPath <- "~/GeoWizard/"
setwd(GeoRepoPath)

source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")
source(file = "GeoFileHandling.R")
source(file = "SeriesHanding.R")
source(file = "QCAnalysis.R")
source(file = "ExpressionAnalysis.R")

if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(paste('Connected Database Tables:', dbListTables(con)))

MolQuery = c("Mycophenolate mofetil")
MolQuery = c("Tofacitinib")

GseTable <- MultiGSEQuery(MolQuery)
GseGsmTable <- SqlQueryMain(GseTable)

# Select Single GSE from GSE-GSM Table
gseList <- unique(GseGsmTable[,'series_id'])
selectedGse <- 2
selectedGse <- gseList[selectedGse]

# Filter Selected GSE
Step_1 <- GseGsmTable %>% filter(series_id %in% selectedGse) 
# Classify the Summary and Return the FIltered GSE GSM DF
Step_2 <- ClassSummary(Step_1)

# Expands Character Column 
CharInputs <- c("characteristics_ch1", "gsm.title","description")
CharInputs <- "characteristics_ch1"
Step_3 <- GseGsmCharExpand(GseGsmTable = Step_2, CharInputs = CharInputs)

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
DesignMatrix <- model.matrix( ~ ExpVar3.Text + ExpVar4.Text, GraphDF )

######### Function to Download File
GeoRepoPath <- "~/GeoWizard/GEORepo"
GSE <- "GSE69967"
GSEeset <- LoadGEOFiles(GSE = "GSE69967", GeoRepoPath = GeoRepoPath)
ArrayData <- exprs(GSEeset)

######### Function to Make GMT Ggplotable
FactorDF <- Step_6[1:2]
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
GeneSymbolArrayData <- ConvertGSEAnnotations(GSEeset = GSEeset)

######### QC
source(file = "QCAnalysis.R")
RunBioQC(GSEeset = GeneSymbolGSEeset)

PCAPlots <- PlotPCA(ArrayData)

######### Limma
LimmaOutput(GeneSymbolArrayData,DesignMatrix)


View(model.matrix(~ExpVar3.Text+ExpVar4.Text, Step_6))





