GeoWizard <- "~/GeoWizard"
setwd(GeoWizard)
source("ShinyApp-Dashboard/global.R")


if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(paste('\nConnected Database Tables:', dbListTables(con)))

MolQuery = c("Mycophenolate mofetil")
MolQuery = c("GSE69967")



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
# Function to Reccomend which of the three to use

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


######### Function to Download File
<<<<<<< HEAD
GeoRepoPath <- "~/GeoWizard/GEORepo"
GSE <- "GSE45551"
GSEeset <- LoadGEOFiles(GSE = GSE, GeoRepoPath = GeoRepoPath)
=======
GSE <- selectedGse
GSEeset <- LoadGEOFiles(GSE = GSE, GeoRepo = GeoRepo)
GSEeset <- GSEeset[[1]]

>>>>>>> clustering
ArrayData <- exprs(GSEeset)
FactorDF <- Step_6[1:2]
saveRDS(FactorDF,file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.rds")
FeatureData <- fData(GSEeset)

########## Convert to Gene Symbol
source(file = "GeoFileHandling.R")
GeneSymbolArrayData <- ConvertGSEAnnotations(GSEeset = GSEeset, Annotation = "Gene Symbol")

<<<<<<< HEAD
######### QC
source(file = "QCAnalysis.R")
RunBioQC(GMT = GeneSymbolArrayData)

######### PC
Prcomp_res <- prcomp()
Prcomp_res$sdev^2
yvar <- switch(type, pev = ScreeData / sum(ScreeData), cev = cumsum(ScreeData) / sum(ScreeData))
yvar.lab <- switch(type, pev = 'proportion of explained variance', cev = 'cumulative proportion of explained variance')
=======
GSM <- Step_1$gsm
GraphDF <- cbind(GSM, Step_6)
DesignMatrix <- model.matrix( ~ 0 + ExpVar3.ContClass:ExpVar4.ContClass, GraphDF )
colnames(DesignMatrix) <- c("Cont.Cont","Pert.Cont","Cont.Pert","Pert.Pert")
ContrastString <- ConTextInput(DesignMatrix)
ContrastMatrix <- makeContrasts(ContrastString, levels = DesignMatrix)
>>>>>>> clustering

######### Limma
res <- LimmaOutput(GeneSymbolArrayData, DesignMatrix, ContrastMatrix)
