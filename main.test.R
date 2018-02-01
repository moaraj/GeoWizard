GeoRepoPath <- "~/GeoWizard/"
setwd(GeoRepoPath)

source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")

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
GeoRepoFiles <- dir(GeoRepoPath)
GSEMatrix <- "GSE69967"
RegularExp <- paste(GSEMatrix, ".+matrix\\.txt\\.gz", sep = "")
MatrixFile <- grep(pattern = RegularExp, x = GeoRepoFiles)
MatrixFilePath <- file.path(GeoRepoPath, GeoRepoFiles[MatrixFile])

if (file.exists(MatrixFilePath)) {
     message(paste("Matrix File for",GSEMatrix, "Found in GEORepo at", GeoRepoPath))
     GSEeset <- getGEO(filename = MatrixFilePath, GSEMatrix = T )
} else {
     message(paste("Matrix File for", GSEMatrix, "Found in GEORepo at", GeoRepoPath))
     GSEeset <- getGEO(GEO = "GSE69967", GSEMatrix = T, destdir = "~/GeoWizard/GEORepo")
}


ArrayData <- exprs(GSEeset)
ArrayDataT <- t(ArrayData)

FactorDF <- Step_6[1:2]
GSM <- rownames(ArrayDataT)
GSMFactorDF <- cbind.data.frame(GSM, FactorDF)
ArrayAndFactorDataDF <- cbind.data.frame(GSMFactorDF, ArrayDataT)
ArrayDataMeltDF <- melt(ArrayAndFactorDataDF)

SampleArrayDataMeltDF <- sample_frac(ArrayDataMeltDF, .001)
colnames(SampleArrayDataMeltDF)


library(ggridges)
ggplot(SampleArrayDataMeltDF, aes(x = value, group = ExpVar3.Text, fill =ExpVar3.Text, position="identity" )) 

FactorOptions <- colnames(SampleArrayDataMeltDF)

ggplot(SampleArrayDataMeltDF, aes(x = value, y = SampleArrayDataMeltDF[,2], height = ..density..)) + 
     geom_density_ridges(stat = "binline", bins = 20, scale = 4, draw_baseline = T) + 
     labs(title="Expression Counts Per Group", x="Number of Counts", y = "Proportion of Genes") +
     theme(legend.position="none") +  theme_classic()

ggplot(SampleArrayDataMeltDF, aes(y = value, x = ExpVar3.Text, fill = ExpVar3.Text)) + geom_boxplot() +
     theme(legend.position = "bottom")

ggplot(SampleArrayDataMeltDF, aes(y = value, x = GSM, fill = ExpVar3.Text)) + geom_boxplot() +
     theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 90))



##########

message("Loading Expression Set Data")
GSEeset <- GSEeset

message("Extracting Gene Symbol feature annotations")
geneSymbolNames <- colsplit(
     string = GSEeset@featureData@data$`Gene Symbol`,
     pattern = " ",
     names = c("GeneSymbol", "SecondarySymbol"))

GeneSymbolEset <- exprs(GSEeset)
rownames(GeneSymbolEset) <- geneSymbolNames$GeneSymbol
myEset <- GeneSymbolEset


message("Loading BioQC Panels")
gmtFile <- system.file("extdata/exp.tissuemark.affy.roche.symbols.gmt", package="BioQC")
gmt <- readGmt(gmtFile)

genesets <- BioQC::readGmt(gmtFile)
testIndex <- BioQC::matchGenes(genesets, myEset)

wmwResult.greater <- wmwTest(myEset, testIndex, valType="p.greater")
wmwResult.less <- wmwTest(myEset, testIndex, valType="p.less")
wmwResult.Q <- wmwTest(myEset, testIndex, valType="Q")

bioqcResFil <- filterPmat(wmwResult.greater, 1E-3)
bioqcAbsLogRes <- absLog10p(bioqcResFil)
bioqcAbsLogRes <- tail(bioqcAbsLogRes, n = 10)

message("Generating BioQC HeatMap")
heatmap.2(bioqcAbsLogRes, Colv=TRUE, Rowv=TRUE,
          cexRow=1, cexCol = 1, dendrogram = "both",
          col=rev(brewer.pal(11, "RdBu")),
          labCol=1:ncol(bioqcAbsLogRes),
          main = "BioQC results for GSE",
          xlab = "Sample Number",
          key = T,
          lmat = rbind(c(4,3,0),c(2,1,0),c(0,0,0)),
          lwid = c(1.5,4,1),
          lhei = c(1.5,4,1),
          trace = 'none')







#########

fit <- lmFit(ArrayData, DesignMatrix)
fit <- eBayes(fit)
fit <- eBayes(fit,trend=TRUE)

LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")
LimmaTable


View(model.matrix(~ExpVar3.Text+ExpVar4.Text, Step_6))





