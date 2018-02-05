server <- function(input, output, session) {
     
     GeoSearchResults <- reactiveValues()
     
     observe({
          
          if (input$MolSelectFilter != "TextInput") {
               
               shinyjs::hide("MolSelectTextDiv")
               shinyjs::show("MolSelectLibraryDiv")
               
          } else {
               
               shinyjs::show("MolSelectTextDiv")
               shinyjs::hide("MolSelectLibraryDiv")
          }
          
     })
     
     output$MolSelectFromLibrary <- renderUI({
          if (input$MolSelectFilter == "TextInput") {
               MolQueryText <- gsub(pattern = "\n|\t",replacement = "", x = input$MolSelectTextBox)
               MolQueryText <- unlist(str_split(MolQueryText, pattern = ","))
               DefaultText <- MolQueryText
          } else if (input$MolSelectFilter == "DAVID") {
               MolQueryText <- MoleculeLibrary
               DefaultText <- "Tofacitinib"
          } else if (input$MolSelectFilter == "FDA") {
               MolQueryText <- unlist(str_split(approvedFDA, pattern = ",|;"))
               MolQueryText <- tolower(unique(MolQueryText))
               
               DefaultText <- MolQueryText[sample(size = 4,x = length(MolQueryText))]
          } else {
               MolQueryText <- MoleculeLibrary
               DefaultText <- "Tofacitinib"
          }
          
          MolTextInputOptions <- list(inputId = "MolSelectInput",
                                      label = "Molecule to GeoSearch",
                                      choices =  MolQueryText,
                                      multiple = T,
                                      selected = DefaultText)
          
          do.call(selectizeInput, MolTextInputOptions)
          
     })
     
     output$TaxonSelection <- renderUI({
          
          CommonSpecies <- c(
               "Human" = "Homo sapiens" ,
               "Chimpanzee" = "Pan troglodytes",
               "Cynomogous" = "Macaca fascicularis",
               "Rabbit" = "Oryctolagus cuniculus",
               "Rat" = "Rattus norvegicus",
               "Mouse" = "Mus musculus",
               "C.elegans" = "Caenorhabditis elegans",
               "Yeast" = "Saccharomyces cerevisiae")
          

          TaxonGSE <- GeoSearchResults$ResultSpecies
          TaxonGSE <- grep(pattern = paste(TaxonGSE, collapse = "|"),
                           CommonSpecies, 
                           ignore.case = T, 
                           value = T)
          
          selectInputOptions <- list(
               inputId = "SpeciesInput",
               label = "Species",
               choices = CommonSpecies,
               selected = TaxonGSE,
               multiple = T,
               selectize = T)
               
               do.call(selectInput, selectInputOptions)  
     })
     
     
     observeEvent(input$MolSelectButton, {
          withBusyIndicatorServer("MolSelectButton", {
               shinyjs::show("nStudiesPlotDiv", time = 1, anim = TRUE, animType = "fade")
               MolQuery <- as.character(unlist(input$MolSelectInput))
               message("Called MultiGSEQuery function")
               GeoSearchResults$ResGSETable <- MultiGSEQuery(MolQuery = MolQuery)
          })
     })
     
     
     GeoSearchResults$GseSummaryTableData <- reactive({
          GseDescDF <- GeoSearchResults$ResGSETable
          GseDescDF <- data.frame(GseDescDF, stringsAsFactors = F)
          
          GeoSearchResults$nTotalStudies <- unique(GseDescDF$Accession)
          GeoSearchResults$ResultSpecies <- unique(GseDescDF$taxon)
          
          FullSummary <- GseDescDF$summary
          TruncSumm <- substr(GseDescDF$summary, start = 1, stop = 100)
          
          if (input$FullSummaryCheck == "Truncated") {
               message("Truncated Summary will be Displayed")
               GseDescDF$summary <- TruncSumm
          } else {
               message("Full Summary will be Displayed")
               GseDescDF$summary <- FullSummary
          }
          message(class(GseDescDF))
          
          message("Filtering Studies by sample size")
          GseDescDF$n_samples <- as.numeric(as.character(GseDescDF$n_samples))
          GseDescDF <- GseDescDF %>% filter(n_samples >= input$SampleSizeSlider) 
          
          message("Filtering Studies by species")
          speciesRegEx <- paste(input$SpeciesInput, collapse = "|")
          GseDescDF <- GseDescDF %>% filter(grepl(pattern = speciesRegEx , taxon))
          
          GseDescDF
     })
     
     
     observeEvent(input$MolSelectButton, {
          output$GseSummaryData <- DT::renderDataTable({
               
               GseDescDF <- GeoSearchResults$GseSummaryTableData
               GseDescDF <- GseDescDF()
               
               DT::datatable(as.data.frame(GseDescDF),
                             rownames = FALSE,
                             class = "compact",
                             options = list(
                                  autoWidth = FALSE,
                                  scrollY = '350px',
                                  paging = FALSE,
                                  order = list(list(6,'desc')) # Order by sample size
                                  )) %>%
                    
                    formatStyle('n_samples', 
                                background = styleColorBar(GseDescDF$n_samples, 'steelblue'),
                                backgroundSize = '100% 90%',
                                backgroundRepeat = 'no-repeat',backgroundPosition = 'center')
               
          })
     })
     
     output$nTotalStudiesIndicator <- renderValueBox({
          nTotalStudies <- GeoSearchResults$nTotalStudies
          valueBox(
               paste0("Total Datasets:", length(nTotalStudies)),
               "Add species to view all studies" , 
               icon = icon("book"),
               color = "blue"
          )
          
          
     })
     
     output$nStudiesSelectedIndicator <- renderValueBox({
          rowSelection <- input$GseSummaryData_rows_selected
          rowSelection <- length(rowSelection)
          
          valueBox(
               paste0("Studies selected:",rowSelection),"Click Table Rows to Select Studies" , 
               icon = icon("list"),
               color = "blue"
          )
     })
     

     
     ########################{ Plot GSE Summary Data
     
     output$nStudiesPlotTaxon <- renderPlot({
          
          if(!is.null(GeoSearchResults$ResGSETable)){
               
          input <- 'taxon'
          titleText <- TitleCase(input)
          plotdata <- GeoSearchResults$GseSummaryTableData
          plotdata <- plotdata()
          
          plotdata$n_samples <- as.numeric(plotdata$n_samples)
          plotdata <- plotdata[complete.cases(plotdata), ]
          
          p <- ggplot(plotdata,
                      aes_string( fill = input, x = "sum(n_samples)", group = input)) +
               geom_bar(position = position_dodge()) +
               
               labs(title = paste("GSE for Molecule per", titleText),
                    x = titleText,
                    y = "Number of Studies") +
               
               theme(plot.title = element_text(hjust = 0.5),
                    legend.title =  element_text(titleText),
                    legend.text = element_text(size = 12),
                    axis.text = element_text(size = 12),
                    legend.spacing.x = unit(2, "cm"),
                    legend.position = "bottom"
               ) +
               guides(colour = guide_legend(ncol = 1))
          p
          
          }
     })
     
     output$nStudiesPlotGdsType <- renderPlot({
          
          if(!is.null(GeoSearchResults$ResGSETable)){
               
               input <- 'gdsType'
               titleText <- TitleCase(input)
               plotdata <- GeoSearchResults$GseSummaryTableData
               plotdata <- plotdata()
               
               plotdata$n_samples <- as.numeric(plotdata$n_samples)
               plotdata <- plotdata[complete.cases(plotdata), ]
               
               p <- ggplot(plotdata,
                           aes_string( fill = input, x = "sum(n_samples)", group = input)) +
                    geom_bar(position = position_dodge()) +
                    
                    labs(title = paste("GSE for Molecule per", titleText),
                         x = titleText,
                         y = "Number of Studies") +
                    
                    theme(plot.title = element_text(hjust = 0.5),
                          legend.title =  element_text(titleText),
                          legend.text = element_text(size = 12),
                          axis.text = element_text(size = 12),
                          legend.spacing.x = unit(2, "cm"),
                          legend.position = "bottom"
                    ) +
                    guides(colour = guide_legend(ncol = 1))
               p
               
          }
     })
     
     ########################{ Advance to GSM Metadata Page
     
     observeEvent(input$switchtab, {
          newtab <- switch(input$AnalyzeSelectedDatasets,
                           "GSMMetadata" = "GSESummary",
                           "GSESummary" = "GSMMetadata"
          )
          updateTabItems(session, "TabSet", newtab)
     })
     
     
     
     
     ################################### GSM Metadata TabItem
     ################################### GSM Metadata TabItem
     ################################### GSM Metadata TabItem
     
     
     
     DataSelection <- reactiveValues()
     
     ########################{ Data Selection and Filter
     
     output$GsmTabletoKeep <- renderUI({
          message("Dataframe for Data filtering")
          GseDescDF <- GeoSearchResults$GseSummaryTableData
          GseDescDF <- GseDescDF()

          GseDescDF <- GseDescDF[input$GseSummaryData_rows_selected, ]
          DataSelection$SelectedGSETable <- GseDescDF
          
          SelectedGSENames <- GseDescDF[, 2] # Get the GSE names of the selected GSE's in the data table
          DataSelection$GSEAccessions <- SelectedGSENames
          
          # Data for Value Boxes
          DataSelection$KeyWord <- unique(GseDescDF[ ,c(1,2)])
          DataSelection$Taxon <- unique(GseDescDF[ ,c(2,6)])
          DataSelection$nSamples <- unique(GseDescDF[ ,c(2,7)])
          
          message("Initializing input for filtering GSE/GSM data to show")
          CheckBoxOptions <- list(inputId = "KeepForExpVarAsign",
                             label = "Keep Datasets for Further Analysis",
                             choices = SelectedGSENames,
                             selected = SelectedGSENames)
          
          do.call(checkboxGroupInput, CheckBoxOptions)
     })
     
     output$GsmTabletoShow <- renderUI({
          radioButtonsOptions <- list(
               inputId = "GsmTableSelect",
               label = "Analyze Selected Dataset",
               choices = as.character(input$KeepForExpVarAsign))
          
          do.call(radioButtons,radioButtonsOptions) 
     })
     
  ####################################{ Table Showing Metadata Tables Containing ExpVars}
  
     SQLSearchData <- reactiveValues()
     
     ########################{ SQL Search 
     SQLSearchData$ResultDF <- reactive({
          message("SQL Query of Selected Datasets")
          GseTableData <- DataSelection$SelectedGSETable
          Result <- SqlQueryMain(GseTableData)
          ResultDF <- data.frame(Result, stringsAsFactors = F)
          ResultDF
     })
     
     SQLSearchData$FilteredResultDF <- reactive({
          message("Loading SQL search Results")
          ResultDF <- SQLSearchData$ResultDF
          ResultDF <- ResultDF()
          
          message("Determing Selected Dataset")
          GsmMetaDatatoShow <- input$GsmTableSelect
          
          message("Filtering SQL Query Res for Selected GSE")
          FilteredResultDF <- ResultDF %>%
               select(series_id, gsm, keyword, taxon, gsm.title, description, characteristics_ch1) %>%
               filter(series_id %in% GsmMetaDatatoShow)
          FilteredResultDF
     })
     
     
     ##################{ Render GSM Meta data
     
     output$GseGsmTable <- DT::renderDataTable({
          message("Retreiving Datatable Data")
          FilteredResultDF <- SQLSearchData$FilteredResultDF()
          
          message("Making SQL Summary Table")
          SqlQueryResDf <- FilteredResultDF %>% select(c(-1,-3,-4))
          DT::datatable(data = SqlQueryResDf ,
                        rownames = FALSE,
                        class = 'row-border',
                        options = list(scrollY = '400px',
                                       dom = 't',
                                       paging = FALSE,
                                       autoWidth = TRUE)) %>%
                         formatStyle(names(SqlQueryResDf),
                                     color = 'black',
                                     backgroundColor = 'white',
                                     fontWeight = 'bold')
     })
     
     ##################{Classify ExpVars}
     
     
     ExperimentalDesign <- reactiveValues()
     
     ExperimentalDesign$ExpClassFactorDF <- reactive({
          message("Processing SQL Table Output")
          GsetoGenDesign <- input$GsmTableSelect
          
          ExpFactorDF <- SQLSearchData$FilteredResultDF()
          
          message("Classify the Summary and Return the FIltered GSE GSM DF")
          ExpFactorClassSummary <- ClassSummary(ExpFactorDF)
          
          message("Expands Character Columns")  
          FactorColumnNames <- input$WhereVarData
          ExpFactorClassSummary <- GseGsmCharExpand(ExpFactorClassSummary, FactorColumnNames)
          ExpFactorClassSummary
     })
     

     #######################{ Data frame of all factors with more than one level }
     
     ExperimentalDesign$ExpFactorDF <- reactive({
          ExpandedDF <- ExperimentalDesign$ExpClassFactorDF()
          
          UseFulExpVarsCols <- grep(pattern = "ExpVar[[:digit:]]", x = colnames(ExpandedDF), value = T)
          UseFulExpVarsDF <- data.frame(ExpandedDF[,UseFulExpVarsCols])
          colnames(UseFulExpVarsDF) <- UseFulExpVarsCols
          UseFulExpVarsDF
     })
     
     
     ########################{ Useful Factor Classification
     
     ExperimentalDesign$ClassGSMTextRV <- reactive({
          message("Fetching ClassDFList")
          ExpFactorDF <- ExperimentalDesign$ExpFactorDF()
          ClassGsmText(ExpFactorDF)
     })
     
     ExperimentalDesign$DefaultClassRV <- reactive({
          message("Determining Default ExpVar Selection")
          
          message("Importing ClassListDF")
          ClassResList <- ExperimentalDesign$ClassGSMTextRV()
          
          message("Executing DescerningClassDF")
          UsefulFactorList <- DescerningClassDF(ClassResList)
          message("Adding Time Factors")
          TimeFactorList <- AddSeriesDFs(ClassDFList = ClassResList, "time")
          message("Adding Titration Series")
          TitrationFactorList <- AddSeriesDFs(ClassDFList = ClassResList, "titration")
          message("Output Default ExpVarSelection")
          c(UsefulFactorList,TimeFactorList,TitrationFactorList)
     })
     
     output$PickFactorColumns <- renderUI({
          GsmDF <- ExperimentalDesign$ExpFactorDF()
          RecVars <- ExperimentalDesign$DefaultClassRV()
          checkboxOptions <- list(
               inputId = "UsefulColumnsCheckbox", 
               label = "Select Which Factors You would like to Keep ",
               choices = colnames(GsmDF),
               selected = names(RecVars),
               inline = T)
          
          do.call(checkboxGroupInput, checkboxOptions)
     })
     
     
     ########################{ View Current Factor Col Selection

     ExperimentalDesign$SelectedFactorDF <- reactive({
          message("filtering DataTable with Default ExpVarSelection")
          # Select Only the Exp Vars that are selected in the UsefulColumnsCheckbox
          ExperimentalDesign$ExpFactorDF() %>% select(one_of(input$UsefulColumnsCheckbox))
     })

     SampleFilterIndex <- reactiveValues()

     # Get the Row Numbers of the Factors then Filter the table being sent to the Design Matrix Page

     ########################{ Render Inputs to Filter Factors
     
     
          output$FilterGSMbyFactor <- renderUI({
               DF <- ExperimentalDesign$SelectedFactorDF()
               NamesIndex <- colnames(DF)

               FactorLevelInput <-
                    lapply(NamesIndex, function(ColName){
                         ColLevels <- DF[,ColName]
                         
                                selectInput(inputId = paste("Gsm_",ColName, sep = ""),
                                            label = paste("Filter Level in", ColName),
                                            choices = unique(ColLevels),
                                            selected = unique(ColLevels),
                                            multiple = T,
                                            selectize = T)
                         
                    })
          })
     


     ########################{ Take Levels from inputs and determine rows of DF


          ExperimentalDesign$FilteredFactorLevels <- reactive({
          DF <- ExperimentalDesign$SelectedFactorDF()
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
          DF <- DF[unlist(RowsToKeep),]
     
          })

     #######################{ Output Table with Factor Selection
     
     ExperimentalDesign$UniqueFactorLevels <- reactive({
          unique(ExperimentalDesign$FilteredFactorLevels())
     })
               
          
     output$ImportantFactorTable <- DT::renderDataTable({
          
          GsmDF <- ExperimentalDesign$UniqueFactorLevels()
          DT::datatable(data = GsmDF,
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             dom = 't',
                             autoWidth = TRUE,
                             scrollX = T,
                             scrollY = '500px',
                             paging = FALSE,
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             colReorder = list(realtime = FALSE))) %>%
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })
     
     output$FullFactorTable <- DT::renderDataTable({
          GsmDF <- ExperimentalDesign$ExpFactorDF()
          DT::datatable(data = GsmDF,
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             dom = 't',
                             autoWidth = TRUE,
                             scrollX = T,
                             scrollY = '500px',
                             paging = FALSE,
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             colReorder = list(realtime = FALSE))) %>%
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })
     
     

     
     ################################################## Design Matrix 
     ################################################## Design Matrix 
     ################################################## Design Matrix
     
     output$DesignMat_SummaryTable <- DT::renderDataTable({
          GsmDF <- ExperimentalDesign$UniqueFactorLevels()
          
          DT::datatable(data = GsmDF,
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             dom = 't',
                             autoWidth = TRUE,
                             scrollX = T,
                             scrollY = '500px',
                             paging = FALSE,
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             colReorder = list(realtime = FALSE))) %>%
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })

     
     ########################{ Annotate the data table
     
     output$RearrangeLevels <- renderUI({
          DF <- ExperimentalDesign$FilteredFactorLevels()
          NamesIndex <- colnames(DF)
          FactorLevelInput <- 
               lapply(NamesIndex, function(ColVect){
                    ColLevels <- DF[ColVect]
                    selectInput(inputId = ColVect,
                                label = paste("Selected control level for",ColVect),
                                choices = unique(ColLevels))
               })
     })
     
     
     controlLevelsDF <- reactive({
          DesignDF <- ExperimentalDesign$FilteredFactorLevels()
          NamesIndex <- colnames(DesignDF)
          message("Changing control factor level")
          
          ResDF <- lapply(NamesIndex, function(ColName){
               ResultVector <- DesignDF[,ColName]
               controlLevels <- input[[ColName]]
               otherLevels <- levels(factor(ResultVector))
               levels(ResultVector) <- unique(c(controlLevels, otherLevels))
               ResultVector
          })
          
          names(ResDF) <- NamesIndex
          ResDF <- data.frame(do.call(cbind, ResDF))
          ResDF
     })
     
     #####################
     

     observeEvent(input$SubmitFormula, {
          if(length(input$formulaInputDesign) != 0){
               
          Designformula <- input$formulaInputDesign
          DesignExpression <- as.formula(Designformula)
          DesignDF <- controlLevelsDF()
          ExperimentalDesign$DesignMatrix <-
               model.matrix(as.formula(DesignExpression), DesignDF)
          }
     })
     
     # output$TextAhead <- renderUI({
     #      localDF <- controlLevelsDF()
     #      DFname <- colnames(localDF)
     #      DFlevs <- as.character(
     #      lapply(localDF, function(x){ 
     #           FactorLevels <- levels(x)
     #           nLevels <- length(FactorLevels)
     #           
     #           paste(FactorLevels[1],
     #                 FactorLevels[nLevels], 
     #                 collapse = " ")
     #           }))
     # 
     #      textInput.typeaheadOptions <- list(
     #           id="thti",
     #           placeholder="~ ExpVa1 + ExpVar2",
     #           local=data.frame(name=paste("~ ", DFname), 
     #                            info= paste("levels:", DFlevs)),
     #           valueKey = "name",
     #           tokens=c(1:length(DFname))
     #      )
     #      
     #      do.call(textInput.typeahead, textInput.typeaheadOptions)
     #      
     # })
     

     
     observeEvent(input$SubmitFormula, {
          output$CustomExpressionTable <- DT::renderDataTable({
               DT::datatable(data = ExperimentalDesign$DesignMatrix,
                             rownames = TRUE,
                             class = 'compact',
                             extensions = 'Buttons', 
                             options = list(
                                  scrollY = '300px',
                                  paging = FALSE,
                                  dom = 'Bfrtip',
                                  buttons = c('copy', 'csv', 'excel'))
               )
          })
     })
     
     
     #########################
     
     #observeEvent(input$SubmitFormula, {
     isolate({
     output$ExperimentalBlocksPlot <- renderPlot({
          message("Rendering Block Plot")
          DesignExpression <- as.formula(input$formulaInputDesign)
          DesignDF <- controlLevelsDF()
          
          vcd::mosaic(DesignExpression, DesignDF)
     })
     
     })
     
     
     
     ################################################## Expression Analysis
     ################################################## Expression Analysis
     ################################################## Expression Analysis
     
     
     ####Download the Data
     GSEdata <- reactiveValues()
     
     observeEvent(input$DownloadData, {
          
          shiny::req(SampleFilterIndex$RowFilterIndex)
          shinyjs::show("GMTTable")
          message("Downloading Data from GEO")
          GSE <- input$GsmTableSelect
          
          GeoRepoPath <- "~/GeoWizard/GEORepo"
          GeoRepoFiles <- dir(GeoRepoPath)
          GSEMatrix <- GSE
          
          RegularExp <- paste(GSEMatrix, ".+matrix\\.txt\\.gz", sep = "")
          MatrixFileIndex <- grep(pattern = RegularExp, x = GeoRepoFiles)
          MatrixFilePath <- file.path(GeoRepoPath, GeoRepoFiles[MatrixFileIndex])
          
          if (file.exists(MatrixFilePath)) {
                message(paste( "Matrix File for", GSEMatrix, "found in GEORepo at", GeoRepoPath))
                GSEeset <- getGEO(filename = MatrixFilePath, GSEMatrix = TRUE)
                
           } else {
                # Put button or warning for this failed try again
                message(paste( "Matrix File for", GSEMatrix,"not found in GEORepo at", GeoRepoPath))
                GSEeset <- getGEO(GEO = GSE, GSEMatrix = T, destdir = "~/GeoWizard/GEORepo/")
                
          }
          
          if(class(GSEeset) == "list"){ GSEeset <- GSEeset[[1]] } #Get GEO prodces list of files sometimes
          
          GSEdata$Eset <- GSEeset
          ArrayData <- exprs(GSEeset)
          GSEdata$ArrayData <- ArrayData[, SampleFilterIndex$RowFilterIndex] # Only Keep Selected Rows
          message("eset Loaded")
          
          ArrayDataT <- t(ArrayData)
          GSM <- rownames(ArrayDataT)
          FactorDF <- ExperimentalDesign$SelectedFactorDF() # GSM x Factors
          GSMFactorDF <- cbind.data.frame(GSM, FactorDF)
          ArrayAndFactorDataDF <- cbind.data.frame(GSMFactorDF, ArrayDataT) # Matrix GSM x Genes
          FilteredArrayFactorDF <- ArrayAndFactorDataDF[SampleFilterIndex$RowFilterIndex, ] # Rows To Keep
          GSEdata$ArrayDataMeltDF <- melt(FilteredArrayFactorDF)
          
          ArrayData
          })
     
     
          GSEdata$GeneSymbol <- reactive({
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
          })

     
     ######### Column 1 - GMT File Tab
     
     output$GMTFileTable <- renderDataTable({
          DF <- GSEdata$ArrayData
          DT::datatable(data = DF,
                        rownames = TRUE,
                        class = 'compact',
                        extensions = 'Buttons', 
                        options = list(
                             scrollX = T,
                             scrollY = '300px',
                             paging = T,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel')))
          
          })
     
     
     ####### Hist Tab
     
     GSEdata$SampleArrayDataMeltDF <- reactive({
          MeltedDF <- GSEdata$ArrayDataMeltDF
          sample_frac(MeltedDF, 0.1)     
     })
     
     
     output$HistFactorSelect <- renderUI({
          MeltedDF <- GSEdata$ArrayDataMeltDF
          FactorOptions <- grep(pattern = "ExpVar[0-9]", x = colnames(MeltedDF), value = T)
          selectInput(inputId = "HistFactorSelectInput",label = "Fill by Factor",choices = FactorOptions,selected = FactorOptions[1])
     })

     output$HistPlotGMT <- renderPlot({
          SampleArrayDataMeltDF <- GSEdata$SampleArrayDataMeltDF()
          
          ggplot(SampleArrayDataMeltDF, aes(x = value, y = SampleArrayDataMeltDF[,input$HistFactorSelectInput] , height = ..density..)) +
               geom_density_ridges(stat = "binline", bins = 20, scale = 4, draw_baseline = F) + 
               labs(title="Expression Counts Per Group", 
               x = "Number of Counts", 
               y = input$HistFactorSelectInput) +
               theme(legend.position="NULL") +
               theme_ridges()
          })
     
     ######### Column 1 - Boxplot
     
     output$BoxFactorSelect <- renderUI({
          MeltedDF <- GSEdata$ArrayDataMeltDF
          FactorOptions <- grep(pattern = "ExpVar[0-9]", x = colnames(MeltedDF), value = T)
          selectInput(inputId = "BoxFactorSelectInput",label = "Fill by Factor", choices = FactorOptions,selected = FactorOptions[1])
          })
     
     output$BoxplotGMT <- renderPlot({
          ArrayDataMeltDF <- GSEdata$SampleArrayDataMeltDF()
          
          if (input$BoxPlotType == "Sample") {
               ggplot(ArrayDataMeltDF, 
                      aes(y = ArrayDataMeltDF$value, 
                          x = ArrayDataMeltDF$GSM,
                          fill = ArrayDataMeltDF[,input$BoxFactorSelectInput])) + 
               geom_boxplot() + 
               theme(legend.position = "bottom") + 
               theme(axis.text.x = element_text(angle = 90))
               
          } else if (input$BoxPlotType == "Factor"){
               ggplot(ArrayDataMeltDF, 
                      aes(y = ArrayDataMeltDF$value,
                          x = ArrayDataMeltDF[, input$BoxFactorSelectInput], 
                          fill = ArrayDataMeltDF[, input$BoxFactorSelectInput])) + 
               geom_boxplot() + 
               theme(legend.position = "bottom")
               } else { NULL }
          })
     
     
     ###################################################################### Expression Analysis
     
     AnalysisResults <- reactiveValues()
     
     AnalysisResults$LimmaResults <- reactive({
          
          DesignMatrix <- ExperimentalDesign$DesignMatrix
          ArrayData <- GSEdata$ArrayData
          
          fit <- lmFit(ArrayData, DesignMatrix)
          fit <- eBayes(fit)
          fit <- eBayes(fit,trend=TRUE)
               
          LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")
          AnalysisResults$LimmaTable <- LimmaTable
          LimmaTable
          
          })
     
     
     ############ Volcano Plot
     
     
     output$PValThres <- renderUI({
          shiny::req(AnalysisResults$LimmaTable)
          LimmaTable <- AnalysisResults$LimmaTable
          numericInput(inputId = "PValThresInput",
                       label = "pValue Threshold",
                       value = 2,
                       min = min(-log(LimmaTable$adj.P.Val)),
                       max = max(-log(LimmaTable$adj.P.Val)),
                       step = median(-log(LimmaTable$adj.P.Val))/10)
     })
     
     output$LogFCThres <- renderUI({
          numericInput(inputId = "LogFCThresInput",
                       label = "LogFC Threshold",
                       value = 1,
                       min = 0,
                       max = 5,
                       step = 0.5)
          })
     
     
     
     output$VolcanoPlot <- renderPlot({
          pValueThresHold <- input$PValThresInput
          logFCThresHold <- input$LogFCThresInput
          
          LimmaTable <- AnalysisResults$LimmaResults
          LimmaTable <- LimmaTable()
          
          LimmaTable <- LimmaTable %>% 
               mutate(Threshold = logFC > logFCThresHold | logFCThresHold < -1.5) %>%
               mutate(Threshold = as.numeric(Threshold)) %>%
               mutate(Threshold = Threshold + as.numeric(-log(LimmaTable$adj.P.Val) >= pValueThresHold))
          
          ggplot(LimmaTable, aes(x = logFC, y = -log(adj.P.Val), color = factor(Threshold > 1))) + 
               geom_point() + theme_grey()
          
     })
     
     
     ############ MA Plot
     
     
     
     ############ Clustering
     
     
     
     ############ BoxPlot
     
     
     ############ TopTable     
     
     
     ####################################################################### QC Analysis
     
     ############# BioQC Analysis
     
     observeEvent(input$PerformBioQCAnalysis, {
     output$BioQCPlot <- renderPlot({
               message("Loading Expression Set for BioQC")
               myEset <- GSEdata$GeneSymbol()
               
               message("Loading BioQC Panels")
               gmtFile <- system.file("extdata/exp.tissuemark.affy.roche.symbols.gmt", package="BioQC")
               gmt <- readGmt(gmtFile)
               
               genesets <- BioQC::readGmt(gmtFile)
               testIndex <- BioQC::matchGenes(genesets, myEset)
               
               wmwResult.greater <- wmwTest(myEset, testIndex, valType="p.greater")
               wmwResult.less <- wmwTest(myEset, testIndex, valType="p.less")
               wmwResult.Q <- wmwTest(myEset, testIndex, valType="Q")
               
               bioqcResFil <- filterPmat(wmwResult.greater, 1E-8)
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
                         lmat = rbind(c(0,3,4),c(2,1,0),c(0,0,0)),
                         lwid = c(1.5,4,1),
                         lhei = c(1.5,4,1),
                         trace = 'none')
          })
     
     })
     
     
     ################ PCA Plot
     
     PCAData <- reactiveValues()
     
     PCAData$Res <- reactive({
          dat <- GSEdata$GeneSymbol()
          rownames(dat) = make.names(rownames(dat), unique=TRUE)
          PCA(dat, graph = F, scale.unit = T)
     })
     
     output$PCA <- renderPlot({
          fviz_pca_ind(PCAData$Res(),
                       col.ind = "cos2",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE)
     })
     
     output$ElbowPlot <- renderPlot({
          fviz_eig(PCAData$Res(), ddlabels = TRUE, ylim = c(0, 50)) # ScreePlot
     })
     
     output$CorrelationCircle <- renderPlot({
          fviz_pca_var(PCAData$Res(),
                       col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
     })
     
     output$Contrib1 <- renderPlot({
          fviz_contrib(PCAData$Res(),
                       choice = "var", 
                       axes = 1, 
                       top = 10) # Contributions of variables to PC1
     })
     
     output$Contrib2 <- renderPlot({
          fviz_contrib(PCAData$Res(), choice = "var", axes = 2, top = 10) # Contributions of variables to PC1
     })
     
     
     
     
     ################ tSNE Plot
     
     
     
     
     
     
     ########################{ Disconnect from SQLite Server on Exit
     
     session$onSessionEnded(function() {
          message("Disconnecting from GEOmetadb.sqlite")
          con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
          dbDisconnect(con)
     })
     
     
     

     
     
     

     
     

     
     
     
     
}

