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
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     ########################{ Disconnect from SQLite Server on Exit
     
     session$onSessionEnded(function() {
          message("Disconnecting from GEOmetadb.sqlite")
          con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
          dbDisconnect(con)
     })
     
     
     

     
     
     

     
     

     
     
     
     
}

