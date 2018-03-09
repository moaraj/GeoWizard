ontPath <- "~/GeoWizard/Ontologies/"
setwd(ontPath)

celltypes <- read.csv(file = "Cell Line Ontology Mahadevan.csv", sep = ";", stringsAsFactors = F)
celltypes <- unique(c(celltypes$Preferred.Label))
celltypes <- gsub(pattern = "Cell|-Cell|CellLine", replacement = "", celltypes)
celltypes <- data.frame("celltypes" = celltypes, stringsAsFactors = F)
write.csv(x = celltypes,file = "celltypes.csv", quote = F, row.names = F)

disease <- read.csv(file = "Human Disease Ontology.csv", sep = ";", stringsAsFactors = F)
disease <- c(disease$Preferred.Label, disease$Synonyms)
disease <- gsub(pattern = "\\|", replacement = ",", x = disease)
disease <- unique(disease)
disease <- tolower(trimws(disease, which = "b"))
disease <- data.frame("disease" = disease, stringsAsFactors = F)
write.csv(x = disease,file = "disease.csv", quote = F, row.names = F)

DiseaseWords <- unlist(str_split(string = disease$disease ,pattern = "\\s"))
DiseaseWords <- unique(DiseaseWords)
DiseaseWords <- tolower(trimws(DiseaseWords, which = "b"))
DiseaseWords <- gsub(pattern = "[[:punct:]]", replacement = "", x = DiseaseWords)
DiseaseWords <- data.frame("DiseaseWords" = DiseaseWords, stringsAsFactors = F)
write.csv(x = DiseaseWords,file = "DiseaseWords.csv", quote = F, row.names = F)

drugs <- read.csv(file = "Drug_all.csv", sep = ";", stringsAsFactors = F)
drugs <- unique(unlist(sapply(drugs, unique)))
drugs <- gsub(pattern = "UNT", replacement = "", x = drugs) #987
drugs <- gsub(pattern = "MEQ", replacement = "", x = drugs)
drugs <- gsub(pattern = "^-", replacement = "", x = drugs)
drugs <- gsub(pattern = "\\sAU", replacement = "", x = drugs)
drugs <- gsub(pattern = "\\sBAU", replacement = "", x = drugs) # 798
drugs <- gsub(pattern = "HR\\s|\\sHR", replacement = "", x = drugs)
drugs <- gsub(pattern = "\\(HN\\)\\s", replacement = "", x = drugs) #710
drugs <- gsub(pattern = "\\(USP\\)\\s", replacement = "", x = drugs) #835
drugs <- tolower(trimws(drugs, which = "b"))
drugs <- data.frame("drugs" = drugs, stringsAsFactors = F)

#drugs <- drugs[1:sample(x = 1:length(drugs),size = 1)]
drugsWords <- unique(unlist(str_split(string = drugs,pattern = "\\s")))
drugsWords <- gsub(pattern = "[[:punct:]]", replacement = "", x = drugsWords)
drugsWords <- data.frame("drugsWords" = drugsWords, stringsAsFactors = F)
write.csv(x = drugsWords, file = "drugsWords.csv", quote = F, row.names = F)



svmData <- data.frame("words" = c(celltypes$celltypes, 
                                  disease$disease, 
                                  drugs$drugs),
                      "class" = c(rep("cell", length(celltypes$celltypes)), #cell
                                  rep("disease", length(disease$disease)), #disease
                                  rep("drug", length(drugs$drugs))), #drug
                      stringsAsFactors = F)

library(tm)
library(e1071)
library(dplyr)

svmDataSample <- svmData %>% sample_n(10000) %>% na.omit
svmDataTest <- svmData %>% sample_n(2) %>% na.omit


dtm <- DocumentTermMatrix(Corpus(VectorSource(svmDataSample$words))) ## creating DTM for texts
categories <- svmDataSample$class
new <- svmDataTest$words

df <- data.frame(categories, as.data.frame(inspect(dtm))) ## making DTM a data.frame and adding variable categories
model <- svm(categories~., data=df, scale = F)

dtm_n <- DocumentTermMatrix(Corpus(VectorSource(new)), control=list(dictionary=names(df))) ## creating dtm for new
df_n <- as.data.frame(inspect(dtm_n)) ## creating data.frame for new

acc <- lapply(c(1:1000), function(i){
    svmDataTest <- svmData %>% sample_n(1) %>% na.omit
    Test <- svmDataTest
    Prediction <- as.character(predict(model, df_n))
    Iteration <- i
    res <- cbind(Test, Prediction)
    res <- cbind(Iteration, res)
})

accDF <- bind_rows(acc)

