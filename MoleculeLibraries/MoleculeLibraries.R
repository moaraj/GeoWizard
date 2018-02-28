MolLibPath <- file.path(GeoWizard, "MoleculeLibraries")

LoadFDA <- function(MolLibPath){
  approvedFDA <- read.csv(file = file.path(MolLibPath,"DrugsFDA.csv"), sep = ";", stringsAsFactors = F)
  approvedFDA <- approvedFDA %>% select(Active.Ingredients) %>% unlist
  approvedFDA
}

approvedFDA <- LoadFDA(MolLibPath)

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
