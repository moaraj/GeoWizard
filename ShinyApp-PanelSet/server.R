cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

FancyTable <- function(tablename){
     br()
     wellPanel(
          h3(tablename),
          rHandsontableOutput(tablename)
     )
}


server <- function(input, output, session) {
     
     rv <- reactiveValues(
          Mols = NULL,
          SetSelectDF = NULL,
          FullSummary = NULL,
          TruncSumm = NULL,
          keeprows = NULL
     )
     
     observe({
          if (input$MolSelectFilter != "TextInput") {
               shinyjs::hide("MolSelectTextBox")
               shinyjs::show("MolSelectFromLibrary")
          } else {
               shinyjs::show("MolSelectTextBox")
               shinyjs::hide("MolSelectFromLibrary")
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
     
     
     observeEvent(input$MolSelectButton, {
          withBusyIndicatorServer("MolSelectButton", {
               shinyjs::show("nStudiesPlotDiv", time = 1, anim = TRUE, animType = "fade")
               MolQuery <- as.character(unlist(input$MolSelectInput))
               DF <- MultiGSEQuery(MolQuery = MolQuery)
               rv$SetSelectDF <- DF
          })
     })
     
     
     GseDescDataRV <- reactive({
          localdata <- rv$SetSelectDF
          FullSummary <- rv$SetSelectDF$summary
          TruncSumm <- substr(rv$SetSelectDF$summary, start = 1, stop = 100)
          if (input$FullSummaryCheck == "Truncated") { localdata$summary <- TruncSumm
          } else { localdata$summary <- FullSummary }
          localdata <- data.frame(localdata, stringsAsFactors = F)
          
          GseDescDF <- localdata
          GseDescDF$n_samples <- as.numeric(as.character(GseDescDF$n_samples))
          speciesRegEx <- paste(input$SpeciesInput, collapse = "|")
          GseDescDF <- GseDescDF %>% 
               filter(n_samples >= input$SampleSizeSlider) %>% 
               filter(grepl(pattern = speciesRegEx , taxon))
          GseDescDF
     })
     
     
     observeEvent(input$MolSelectButton, {
          output$GseSummaryData <- DT::renderDataTable({
               GseDescDF <- GseDescDataRV()
               DT::datatable(GseDescDF,
                             rownames = FALSE,
                             options = list(
                                  scrollY = '600px',
                                  paging = FALSE,order = list(list(6,'desc')), # Order by sample size
                                  pageLength = 10)) %>%
                    formatStyle(names(GseDescDF), 
                                color = 'black', 
                                backgroundColor = 'white', 
                                fontWeight = 'bold') %>%
                    formatStyle('n_samples', 
                                background = styleColorBar(GseDescDF$n_samples, 'steelblue'),
                                lineHeight='70%',
                                backgroundSize = '100% 90%',
                                backgroundRepeat = 'no-repeat',backgroundPosition = 'center')
               
          })
     })
     
     ########################{ Plot GSE Summary Data
     
     output$nStudiesPlot <- renderPlot({
          input <- input$GSEInfoPlotSelection
          titleText <- TitleCase(input)
          plotdata <- GseDescDataRV()
          plotdata$n_samples <- as.numeric(plotdata$n_samples)
          plotdata <- plotdata[complete.cases(plotdata), ]
          
          p <-
               ggplot(plotdata,
                      aes_string(
                           fill = input,
                           x = "sum(n_samples)",
                           group = input
                      )) +
               geom_bar(position = position_dodge()) +
               labs(
                    title = paste("GSE for Molecule per", titleText),
                    x = titleText,
                    y = "Number of Studies"
               ) +
               theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.title =  element_text(titleText),
                    legend.text = element_text(size = 12),
                    axis.text = element_text(size = 12),
                    legend.spacing.x = unit(2, "cm"),
                    legend.position = "bottom"
               ) +
               guides(colour = guide_legend(ncol = 1))
          p
     })
     
     ########################{ Side bar
     
     observeEvent(input$AnalyzeSelectedDatasets, {
          updateTabsetPanel(session = session, 
                            inputId = "GeoWizTabSetPanel", 
                            selected = "GSM Metadata")
     })
     
     observe({
          toggleClass(condition = input$AnalyzeSelectedDatasets,
                      class = "disabled",
                      selector = "#navbar li a[data-value=tab2]")
     })
     
     
     observe({
          rowSelection <- input$GseSummaryData_rows_selected
          if (length(rowSelection) != 0){
               shinyjs::enable("AnalyzeSelectedDatasets")
          } else {
               shinyjs::disable('AnalyzeSelectedDatasets')
          }
     })
     
     
     #################################################################
     ########################{ GSM Meta data }########################
     #################################################################
     
     
     ########################{ Side bar
     GseTableRV <- reactive({
          GseTable <- rv$SetSelectDF
          GseTable <- GseTable[input$GseSummaryData_rows_selected, ]
          GseTable
     })
     
     # Get the GSE names of the selected GSE's in the data table
     gseRowsSelected <- reactive({
          GseNames <- GseTableRV()
          message(GseNames)
          GseNames[, 2]
     })
     
     # Make the Side bar for Pick which GSE GSM data to show
     output$GsmTabletoShow <- renderUI({
          SelectedGSENames <- unlist(gseRowsSelected())
          newbuttonoption <- list(inputId = "GsmTableSelect",
                                  label = "Examine Design Matrix for GSE",
                                  choices =  SelectedGSENames)
          do.call(radioButtons, newbuttonoption)
     })
     
     ########################{ SQL Search 
     SqlQueryResRV <- reactive({
          GseTableData <- GseTableRV()
          SqlQueryResDF <- SqlQueryMain(GseTableData)
          SqlQueryResDF
     })
     
     
     output$LoadingGif <- renderImage({
          filename <- normalizePath(file.path('./www/loadingGif.gif'))
          list(src = filename)
     }, deleteFile = FALSE)
     
     
     ########################{ GSE - GSM joined Table Output
     
     SqlQueryResTableRV <- reactive({
          GsmMetaDatatoShow <- input$GsmTableSelect
          SqlQueryRes <- SqlQueryResRV()
          SqlQueryResDf <- SqlQueryRes %>% 
               select(series_id, gsm, keyword, taxon, gsm.title, description, characteristics_ch1) %>%
               filter(series_id %in% GsmMetaDatatoShow)
          SqlQueryResDf
     })
     
     
     observeEvent(input$AnalyzeSelectedDatasets, {
          output$GseGsmTable <- DT::renderDataTable({ 
               SqlResDF <- SqlQueryResTableRV()
               SqlResDF <- SqlResDF %>% select(c(-1,-3,-4))
               DT::datatable(data = SqlResDF ,
                             rownames = FALSE,
                             class = 'row-border',
                             options = list(scrollY = '400px',
                                            paging = FALSE,
                                            autoWidth = TRUE)) %>%
                    formatStyle(names(SqlResDF),  color = 'black', 
                                backgroundColor = 'white',
                                fontWeight = 'bold')
          })
     })
     
     ########################{ Table to Show GPL, Taxon and Keyword}
     
     output$TaxonKeywordInfo <- renderDataTable({
          datatable(data = SqlQueryResTableRV() %>% select(c(1,3,4)) %>% filter(row_number()==1), 
                    options = list(dom = 't'))
     })
     
     
     ########################{ Select Which Data Sets for Further analysis
     
     
     output$GsmTabletoShow <- renderUI({
          SelectedGSENames <- unlist(gseRowsSelected())
          newbuttonoption <- list(inputId = "GsmTableSelect",
                                  label = "Examine Design Matrix for GSE",
                                  choices =  SelectedGSENames)
          do.call(radioButtons, newbuttonoption)
     })
     
     output$GsmTabletoKeep <- renderUI({
          SelectedGSENames <- unlist(gseRowsSelected())
          NewCheckBoxOptions <- list(
               inputId = "KeepForExpVarAsign",
               label = "Keep GSE for futher Analysis",
               choices = SelectedGSENames,
               selected = SelectedGSENames,
               inline = FALSE)
          do.call(checkboxGroupInput, NewCheckBoxOptions) 
     })
     
     
     ########################{ Factor and Levels Tables
     
     # Filter Data with only the GSE's Selected on GSM page
     GSMDataFactorLevelsTableRV <- reactive({
          DF <- SqlQueryResTableRV()
          SelectedForExpVarAsign <- input$KeepForExpVarAsign
          DF %>% filter(series_id %in% SelectedForExpVarAsign)
     })
     
     GsmDesignRV <- reactiveValues()
     
     reactive({
          message("Processing SQL Table Output")
          GsetoGenDesign <- input$GsmTableSelect
          DesignInput <- GSMDataFactorLevelsTableRV() %>% 
               filter(series_id %in% GsetoGenDesign)
          message("Classify the Summary and Return the FIltered GSE GSM DF")
          DesignClassSumRes <- ClassSummary(DesignInput)
          message("Expands Character Column")  
          CharInputs <- input$WhereVarData
          message(CharInputs)
          GsmDesignRV$GsmDesignDF <- GseGsmCharExpand(DesignClassSumRes, CharInputs)
     })
     
     GsmDesignDF <- reactive({
          message("Processing SQL Table Output")
          GsetoGenDesign <- input$GsmTableSelect
          DesignInput <- GSMDataFactorLevelsTableRV() %>% 
               filter(series_id %in% GsetoGenDesign)
          message("Classify the Summary and Return the FIltered GSE GSM DF")
          DesignClassSumRes <- ClassSummary(DesignInput)
          message("Expands Character Column")  
          CharInputs <- input$WhereVarData
          message(CharInputs)
          ExpandedDF <- GseGsmCharExpand(DesignClassSumRes, CharInputs)
     })
     
     UseFulExpVarsRV <- reactive({
          ExpandedDF <- GsmDesignDF()
          UseFulExpVarsCols <- grep(pattern = "ExpVar[[:digit:]]", x = colnames(ExpandedDF), value = T)
          UseFulExpVarsDF <- data.frame(ExpandedDF[,UseFulExpVarsCols])
          colnames(UseFulExpVarsDF) <- UseFulExpVarsCols
          UseFulExpVarsDF
     })
     
     
     ########################{ Useful Factor Classification
     
     ClassGSMTextRV <- reactive({
          message("Fetching ClassDFList")
          ExpVarCols <- UseFulExpVarsRV()
          ClassGsmText(ExpVarCols)
     })
     
     DefaultClassRV <- reactive({
          message("Determining Default ExpVar Selection")
          message("Importing ClassListDF")
          ClassResList <- ClassGSMTextRV()
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
          GsmDF <- UseFulExpVarsRV()
          RecVars <- DefaultClassRV()
          checkboxOptions <- list(
               inputId = "UsefulColumnsCheckbox", 
               label = "Select Which Factors You would like to Keep ",
               choices = colnames(GsmDF),
               selected = names(RecVars),
               inline = T)
          do.call(checkboxGroupInput,checkboxOptions)
     })
     
     
     ########################{ View Current Factor Col Selection
     
     SelectedFactorLevels <- reactive({
          message("filtering DataTable with Default ExpVarSelection")
          # Select Only the Exp Vars that are selected in the UsefulColumnsCheckbox
          UseFulExpVarsRV() %>% 
               select(one_of(input$UsefulColumnsCheckbox))
     })
     
     SampleFilterIndex <- reactiveValues()
     
     # Get the Row Numbers of the Factors then Filter the table being sent to the Design Matrix Page
     
     ########################{ Render Inputs to Filter Factors
     observeEvent(input$FilterGSMLevels,{
          output$FilterGSMbyFactor <- renderUI({
               DF <- SelectedFactorLevels()
               NamesIndex <- colnames(DF)
               
               FactorLevelInput <- 
                    lapply(NamesIndex, function(ColName){
                         ColLevels <- DF[,ColName]
                         column(3,
                                selectInput(inputId = paste("Gsm_",ColName, sep = ""),
                                            label = paste("Filter Level in", ColName),
                                            choices = unique(ColLevels),
                                            selected = unique(ColLevels),
                                            multiple = T,
                                            selectize = T)
                         )
                    })
               
          })
          
          
     })
     
     
     ########################{ Take Levels from inputs and determine rows of DF
     
          
     FilteredFactorLevels <- reactive({
               DF <- SelectedFactorLevels()
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
     
     
     
     
     
     ########################{ Output Table with Factor Selection
     
     output$DesignDataTable <- DT::renderDataTable({
          if(input$ViewAllorFiltVar == "All"){ GsmDF <- UseFulExpVarsRV()     
          } else {  GsmDF <- FilteredFactorLevels()
          }
          
          
          DT::datatable(data = unique(GsmDF), 
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             dom = 't',
                             autoWidth = TRUE,
                             scrollX = T,
                             scrollY = '200px',
                             paging = FALSE,
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             colReorder = list(realtime = FALSE))) %>% 
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })
     
     
     
     ########################{ Disconnect from SQLite Server on Exit
     
     session$onSessionEnded(function() {
          message("Disconnecting from GEOmetadb.sqlite")
          con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
          dbDisconnect(con)
     })
     
     #Switch to the Next Tab
     observeEvent(input$SendTableToDesignMatTab, {
          message("Sending Table to Design Matrix Panel")
          updateTabsetPanel(session = session,
                            inputId = "GeoWizTabSetPanel",
                            selected = "DesignMatTab")
          
     })
     
     #################################################################
     ########################{ Design Matrix }########################
     #################################################################
     
     # Show Design Matrix
     
     output$SelectAnnoGseInfo <- renderUI({
          SelectedGSENames <- input$KeepForExpVarAsign
          newbuttonoption <- list(inputId = "GenerateDesignMatrix",
                                  label = "Examine Design Matrix for GSE",
                                  choices =  SelectedGSENames)
          do.call(radioButtons, newbuttonoption)
          
     })
     
     observe({
          if(input$HowToSpecifyDesignMatrix == 1){
               shinyjs::show("AutoGen")
               shinyjs::hide("ManGen")
               shinyjs::hide("RExpression")
          } else if(input$HowToSpecifyDesignMatrix == 2){
               shinyjs::hide("AutoGen")
               shinyjs::show("ManGen")
               shinyjs::hide("RExpression")
          } else {
               shinyjs::hide("AutoGen")
               shinyjs::hide("ManGen")
               shinyjs::show("RExpression")
          }
          
          if(!is.null(input$GenerateDesignMatrix)){
               outputOptions(output, "GMTTable", suspendWhenHidden = FALSE)
          }
     })
     
     
     output$DesignMatGsmTable <- DT::renderDataTable({
          if(input$ViewAllorFiltVar == "All"){ GsmDF <- UseFulExpVarsRV()     
          } else { GsmDF <- SelectedFactorLevels()
          }
          
          DT::datatable(data = unique(GsmDF), 
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             pageLength = 5, 
                             colReorder = list(realtime = FALSE))) %>% 
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })
     
     
     ########################{ Annotate the data table
     
     output$RearrangeLevels <- renderUI({
          DF <- SelectedFactorLevels()
          NamesIndex <- colnames(DF)
          FactorLevelInput <- 
               lapply(NamesIndex, function(ColVect){
                    ColLevels <- DF[ColVect]
                    selectInput(inputId = ColVect,
                                label = ColVect, 
                                choices = unique(ColLevels))
               })
     })
     
     
     controlLevelsDF <- reactive({
          DesignDF <- GsmDesignDF()
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
     
     
     
     ExperimentalDesign <- reactiveValues()
     
     observeEvent(input$SubmitFormula, {
          Designformula <- input$formulaInputDesign
          DesignExpression <- as.formula(Designformula)
          DesignDF <- controlLevelsDF()
          ExperimentalDesign$DesignMatrix <-
               model.matrix(as.formula(DesignExpression), DesignDF)
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
     
     
     ########################{ Once you have hte design matrix go to contrast matrix
     
     #Don't Allow next tab button till Design Matrix is made
     observe({
          DesignDF <- ExperimentalDesign$DesignMatrix
          if (length(DesignDF) > 0) { shinyjs::enable("GoToContMatTab")
          } else {shinyjs::disable('GoToContMatTab')}
     })
     
     
     
     #Switch to the Next Tab
     observeEvent(input$GoToContMatTab, {
          message("Sending Table to Contrast Matrix Panel")
          updateTabsetPanel(session = session,
                            inputId = "GeoWizTabSetPanel",
                            selected = "ContrastMatTab")
          
     })
     
     
     ########################{ Contrast Matrix }########################
     
     
     ########################{ Sidebar
     
     output$ContrastMatGsmTable <- DT::renderDataTable({
          if(input$ViewAllorFiltVar == "All"){ GsmDF <- UseFulExpVarsRV()     
          } else { GsmDF <- SelectedFactorLevels()
          }
          
          DT::datatable(data = unique(GsmDF), 
                        extensions = 'ColReorder',
                        class = 'compact',
                        options = list(
                             autoWidth = TRUE,
                             paging = F,
                             scrollY = '200px',
                             columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
                             pageLength = 5, 
                             dom = 't',
                             colReorder = list(realtime = FALSE))) %>% 
               formatStyle(names(GsmDF),
                           color = 'black',
                           fontWeight = 'bold')
     })
     
     
          
          observeEvent(input$SubmitFormula, {
               output$ContrastMatDesignDF <- DT::renderDataTable({
                    DT::datatable(data = ExperimentalDesign$DesignMatrix,
                                  rownames = TRUE,
                                  class = 'compact',
                                  extensions = 'Buttons', 
                                  options = list(
                                       scrollY = '200px',
                                       paging = FALSE,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel'))
                         )
               })
     })
     
          output$DesignMatrixSummary <- DT::renderDataTable({
               DT::datatable(ExperimentalDesign$DesignMatrix)
          })
          
     ########################{ Main Panel
     
     
          ColumnSelectInput <- function(colwidth, ID, LAB, CH,MUL, SEL){
               column(colwidth,
                      selectInput(inputId = ID,
                                  label = LAB,
                                  choices = CH,
                                  multiple = MUL,
                                  selected = SEL,
                                  selectize = T))
          }
          
          
          output$ContrastMatrixTable <- DT::renderDataTable({
               
                    message("Making a Contrast Matrix")
                    DesignMat <- ExperimentalDesign$DesignMatrix
                    DesignMat <- data.frame(DesignMat, stringsAsFactors = F)
                    DesignLevels <- colnames(DesignMat)
                    message(DesignLevels)
                    message(paste(length(DesignLevels), "Length"))
                    message(paste(length(DesignLevels)))
                    DesignLevels <- DesignLevels[-grep(pattern = "Intercept", x = DesignLevels)]
                    message(DesignLevels)
                    Contrastsets <- combn(x = DesignLevels, m = 2)
                    message(Contrastsets)
                    ContrastMatrix <- makeContrasts(contrasts = Contrastsets, levels = DesignLevels)
               
               ExperimentalDesign$ContrastMatrix <- ContrastMatrix
               DT::datatable(data = data.frame(ContrastMatrix),
                             rownames = TRUE,
                             class = 'compact',
                             extensions = 'Buttons', 
                             options = list( scrollY = '300px', 
                                             dom = 'Bfrtip', 
                                             buttons = c('copy', 'csv', 'excel')))
               
          })
               
          
          ###################################################################
          ################{ Differential Expression Analysis }###############
          ###################################################################
          
          ExperimentalDesign$ExpressionSet <- reactive({
               GSEAccession <- input$GenerateDesignMatrix
               GSEAccession <- "GSE69967"
               message(paste("Current GSE selection from GenerateDesignMatrix input is:", GSEAccession))
               
               GeoRepo <- "/pstore/home/hasanm7/GeoWizard/GeoRepo/"
               
               
               RepoFiles <- dir(path = GeoRepo)
               FileName <- RepoFiles[grep(pattern = GSEAccession, x = RepoFiles)]
               GzFileName <- FileName[grep(pattern = "matrix.txt.gz", x = FileName, fixed = T)]
               FilePath <- file.path(GeoRepo, GzFileName)
               

               if(!is_empty(FilePath)){
                    message("GSE data found in GEO repo")
                    message(paste("file loaded:", FilePath))
                    GSEeset <- getGEO(filename = FilePath, GSEMatrix = T)
               } else {
                    message("GSE data not found, downloading from GEO")
                    GSEeset <- getGEO(GEO = GSEAccession, GSEMatrix = T, destdir = GeoRepo)
               }
               
               GSEeset
          })
     
          
          ExperimentalDesign$GeneSymbol <- reactive({
               message("Loading Expression Set Data")
               GSEeset <- ExperimentalDesign$ExpressionSet
               GSEeset <- GSEeset()
               
               message("Extracting Gene Symbol feature annotations")
               geneSymbolNames <- colsplit(
                    string = GSEeset@featureData@data$`Gene Symbol`,
                    pattern = " ",
                    names = c("GeneSymbol", "SecondarySymbol"))
               
               GeneSymbolEset <- exprs(GSEeset)
               rownames(GeneSymbolEset) <- geneSymbolNames$GeneSymbol
               GeneSymbolEset
          })
          

          
          output$GMTTable <- renderDataTable({
               message("Rendering GMT Table")
               DF <- ExperimentalDesign$GeneSymbol
               datatable(data = data.frame(DF()),
                         class = 'cell-border',
                         extensions = 'Buttons',
                         options = list(scrollY = '300px', 
                                        scrollX = T,
                                        dom = 'Bfrtip', 
                                        buttons = c('csv', 'excel'))
                         )
          })
          
          
          
          output$BioQCPlot <- renderPlot({
               message("Loading Expression Set for BioQC")
               myEset <- ExperimentalDesign$GeneSymbol
               myEset <- myEset()
               
               message("Loading BioQC Panels")
               gmtFile <- system.file("extdata/exp.tissuemark.affy.roche.symbols.gmt", package="BioQC")
               gmt <- readGmt(gmtFile)
               
               genesets <- BioQC::readGmt(gmtFile)
               testIndex <- matchGenes(genesets, myEset)
               
               wmwResult.greater <- wmwTest(myEset, testIndex, valType="p.greater")
               wmwResult.less <- wmwTest(myEset, testIndex, valType="p.less")
               wmwResult.Q <- wmwTest(myEset, testIndex, valType="Q")
               
               bioqcResFil <- filterPmat(wmwResult.greater, 1E-8)
               bioqcAbsLogRes <- absLog10p(bioqcResFil)
               
               message("Generating BioQC HeatMap")
               heatmap.2(bioqcAbsLogRes, Colv=TRUE, Rowv=TRUE,
                         cexRow=1,cexCol = 1, dendrogram = "both",
                         col=rev(brewer.pal(11, "RdBu")),
                         labCol=1:ncol(bioqcAbsLogRes),
                         main = "BioQC results for GSE",
                         xlab = "Sample Number",
                         trace = 'row')
          })
          
          
          AnalysisResults <- reactiveValues()
          
          AnalysisResults$LimmaResults <- reactive({
               
               if (!is.null(ExperimentalDesign$GeneSymbol)) {
               
               myEset <- ExperimentalDesign$GeneSymbol
               myEset <- myEset()
               
               DesignMatrix <- ExperimentalDesign$DesignMatrix
               
               ArrayData <- exprs(GSEeset)
               #ArrayData <- sapply(ArrayData, function(x){x^2})
               
               fit <- lmFit(ArrayData, DesignMatrix)
               fit <- eBayes(fit)
               fit <- eBayes(fit,trend=TRUE)

               LimmaTable <- topTable(fit, coef=2, n=4000, adjust="BH")
               LimmaTable
               
               }
          })
          
          
          output$VolcanoPlot <- renderPlot({
               pValueThresHold <- input$PValThres
               logFCThresHold <- input$LogFCThres
               
               LimmaTable <- AnalysisResults$LimmaResults
               LimmaTable <- LimmaTable()
               
               LimmaTable <- LimmaTable %>% 
                    mutate(Threshold = logFC > logFCThresHold | logFCThresHold < -1.5) %>%
                    mutate(Threshold = as.numeric(Threshold)) %>%
                    mutate(Threshold = Threshold + as.numeric(-log(LimmaTable$adj.P.Val) >= pValueThresHold))
               
               ggplot(LimmaTable, aes(x = logFC, y = -log(adj.P.Val), color = factor(Threshold > 1))) + 
                    geom_point() + theme_grey()
               
          })
          
          
          
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
}