library(reshape2)
library(plyr)
library(tidyverse)

approvedFDA <- read.csv(file = "MoleculeLibraries/DrugsFDA FDA Approved Drug Products.csv", sep = ";", stringsAsFactors = F)
approvedFDA <- approvedFDA %>% select(Active.Ingredients) %>% unlist

#approvedFDA <- approvedFDA %>% select(Drug.Name, Active.Ingredients, Company)
#approvedFDA$Company <- gsub(pattern = "[[:punct:]]", replacement = " ", x = approvedFDA$Company)
#approvedFDA <- data.frame(sapply(approvedFDA, tolower), stringsAsFactors = F)
#approvedFDA <- dlply(approvedFDA, .(Company))
#approvedFDA <- lapply(approvedFDA, function(x) x[["Active.Ingredients"]])
#approvedFDA <- sapply(approvedFDA, tolower)
#names(approvedFDA) <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", names(approvedFDA), perl=TRUE)



     
MoleculeLibrary <- c(
     "Mycophenolate mofetil",
     "Cyclosporin A",
     "Corticosteroids",
     "Tacrolimus",
     "IVIG",
     "infliximab",
     "ustekinumab",
     "secukinumab",
     "Methotrexate",
     "Piclamilast",
     "Tofacitinib"
)
