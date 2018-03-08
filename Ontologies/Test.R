ontpath <- "~/GeoWizard/Ontologies"
setwd("~/GeoWizard/Ontologies")


CellLines <- read.csv(file = file.path(ontpath, "Cell Line Ontology Mahadevan.csv"), stringsAsFactors = F, sep = ";")

CellTypes <- read.csv(file = file.path(ontpath, "Cell Ontology.csv"), sep = ";", stringsAsFactors = F)
CellTypes$Preferred.Label <- gsub("cell", replacement = "", CellTypes$Preferred.Label)
CellTypes$Synonyms <- gsub("\\|", replacement = ",", CellTypes$Synonyms)
CellTypes <- c(CellTypes$Preferred.Label, CellTypes$Synonyms)
CellTypes <- unlist(str_split(string = CellTypes, pattern = ","))
CellTypes <- unique(CellTypes)
CellTypes <- CellTypes[CellTypes != ""]

HumanDisease <- read.csv(file = "Human Disease Ontology.csv", sep = ";", stringsAsFactors = F)
HumanDisease <- c(HumanDisease$Preferred.Label, HumanDisease$Synonyms)
HumanDisease <- unique(HumanDisease)
HumanDisease <- unique(unlist(str_split(string = HumanDisease, pattern = " ")))
HumanDisease <- HumanDisease[HumanDisease != ""]

SmallMolecule <- as.matrix(read.csv(file = "Drug_all.csv", sep = ";", header = T, stringsAsFactors = F))
SmallMolecule <- unique(unlist(lapply(SmallMolecule, unique)))
SmallMolecule <- unique(unlist(str_split(string = SmallMolecule, pattern = " ")))
SmallMolecule <- SmallMolecule[SmallMolecule != ""]

FactorSVM <- data.frame("key" = as.character(c(CellTypes, HumanDisease, SmallMolecule)),
                        "class" = c(
                            rep("cell", length(CellTypes)),
                            rep("disease", length(HumanDisease)),
                            rep("drug", length(SmallMolecule))
                            )
                        )
FactorSVMsample <- sample_n(FactorSVM, size = 100)

svm=svm(class~key,data=FactorSVMsample, kernel = "linear")

coef_imp=as.data.frame(t(svm$coefs)%*%svm$SV)
coef_imp1=data.frame(words=names(coef_imp),Importance=t(coef_imp))
coef_imp1=coef_imp1[order(coef_imp1$Importance),]
head(coef_imp1)
tail(coef_imp1)