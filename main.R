source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")

MolQuery =c("Tofacitinib")
speciesQuery = c("Homo sapiens", "Mus musculus")

GseTable <- MultiGSEQuery(MolQuery, speciesQuery)
GseGsmTable <- SqlQueryMain(GseTable)

# Processing Single GSE
gseList <- unique(GseGsmTable[,'series_id'])
selectedGse <- 4
selectedGse <- gseList[selectedGse]

x1 <- GseGsmTable %>% filter(series_id %in% selectedGse)
x5 <- ClassSummary(x1)
x2 <- GseGsmExpVarsTable(x5)
x6 <- x2 %>% select(starts_with("Exp"))
x3 <- ClassOntologies(x6)
x4 <- DesignLabs(x3)

