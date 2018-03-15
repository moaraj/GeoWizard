server <- function(input, output, session) {
    GeoSearchResults <- reactiveValues()
  
    #'
    #'
    #'
    #'
    output$MolSelectFromLibrary <- renderUI({
        if (input$MolSelectFilter == "Accession") {
            SearchInputUI <- 
                textInput(inputId = "GSESelectText", 
                label = "Comma separated GSE Number", 
                placeholder = "GSE60482, GSE57251")
            GeoSearchResults$InputType <- "GSE"
            
        } else if (input$MolSelectFilter == "TextInput") {
            SearchInputUI <- 
                textAreaInput(inputId = "MolSelectText", 
                label = "Comma Separated Keywords", 
                placeholder = "Mycophenolate mofetil, Tofacitinib",
                height = '100px')
            GeoSearchResults$InputType <- "textbox"
            
        } else {
            if (input$MolSelectFilter == "DAVID") {
            MolQueryText <- MoleculeLibrary
            DefaultText <- "Tofacitinib"
            } else if (input$MolSelectFilter == "FDA") {
            MolQueryText <- unlist(str_split(approvedFDA, pattern = ",|;"))
            MolQueryText <- tolower(unique(MolQueryText))
            DefaultText <- MolQueryText[sample(size = 4, x = length(MolQueryText))]
            } else {
            MolQueryText <- MoleculeLibrary
            DefaultText <- "Tofacitinib"
            }
            GeoSearchResults$InputType <- "select"
            SearchInputUI <- selectizeInput(
                inputId = "MolSelectInput", 
                label = "GEO Keyword Query", 
                choices =  MolQueryText, 
                multiple = T,
                selected = DefaultText)
        }
        SearchInputUI
        })
    #'
    #'
    #'
    #'
    #' 
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
        TaxonGSE <- grep(pattern = paste(TaxonGSE, collapse = "|"), CommonSpecies, ignore.case = T, value = T)
        selectInputOptions <- list( inputId = "SpeciesInput", label = "Species", choices = CommonSpecies, selected = TaxonGSE, multiple = T, selectize = T)
        do.call(selectInput, selectInputOptions)  
     })
    
    #'
    #'
    #'
    #'
    #' 
    GeoSearchResults$GseTable <- eventReactive(input$MolSelectButton, {
        
        if (isTruthy(input$MolSelectText) && input$MolSelectFilter == "TextInput"){
            # Text Box input Parse comma separated values
            message("Text Query Detected")
            MolQueryText <- gsub(pattern = "\n|\t", replacement = "", x = input$MolSelectText)
            MolQuery <- unlist(str_split(MolQueryText, pattern = ","))
            
        } else if (isTruthy(input$GSESelectText) && input$MolSelectFilter == "Accession") {
            # GSE Accesssion number iputs Parse comma separated values
            message("GSE Query Detected")
            MolQueryText <- gsub(pattern = "\n|\t", replacement = "", x = input$GSESelectText)
            MolQuery <- unlist(str_split(MolQueryText, pattern = ","))
            
        } else {
            message("Select Query Detected")
            MolQuery <- as.character(unlist(input$MolSelectInput))
        }
        
        message(paste("Called MultiGSEQuery function with query:", MolQuery))
        GseTable <- MultiGSEQuery(MolQuery)
        GseTable
    })
        

    #'
    #'
    #'
    #' 
    GeoSearchResults$GseSummaryTableData <- reactive({
        message("Loading GEO Query Results")
        GseDescDF <- GeoSearchResults$GseTable()
        if(length(GseDescDF)==0){ stop("No results with keyword")}
        
        GeoSearchResults$nTotalStudies <- unique(GseDescDF$Accession)
        GeoSearchResults$ResultSpecies <- unique(GseDescDF$taxon)
        
        FullSummary <- GseDescDF$summary
        TruncSumm <- substr(GseDescDF$summary, start = 1, stop = 150)
        
        if (input$FullSummaryCheck == "Truncated") {
            message("Truncated Summary will be Displayed")
            GseDescDF$summary <- TruncSumm
        } else {
            message("Full Summary will be Displayed")
            GseDescDF$summary <- FullSummary
        }
        
        message("Filtering Studies by sample size")
        GseDescDF$n_samples <- as.numeric(as.character(GseDescDF$n_samples))
        GseDescDF <- GseDescDF %>% filter(n_samples >= input$SampleSizeSlider)
        message("Filtering Studies by species")
        speciesRegEx <- paste(input$SpeciesInput, collapse = "|")
        GseDescDF <- GseDescDF %>% filter(grepl(pattern = speciesRegEx , taxon))
        GseDescDF
    })
    
    #' When Search 
    #'
    #'
    #'
    #'
    observeEvent(input$MolSelectButton, {
    output$GseSummaryData <- DT::renderDataTable({
        GseDescDF <- GeoSearchResults$GseSummaryTableData()
        GseDescDF$GPL <- gsub(pattern = ";",replacement = " ",x = GseDescDF$GPL) 
        DT::datatable(GseDescDF, rownames = FALSE, class = "compact",
            options = list( autoWidth = FALSE, scrollX = T, scrollY = '350px', paging = FALSE, order = list(list(6,'desc')))) %>%
            formatStyle('n_samples', background = styleColorBar(GseDescDF$n_samples, 'steelblue'), backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',backgroundPosition = 'center')
    })
    })
     
    #' Generate Reactive object that contains subset Row's selected by user on the GseSummaryData Table
    #' @req input$MolSelectInput - character vector of keywords to search against GEO database
    #' @input input$GseSummaryData_rows_selected
    #' @return GseDescDFSelected
    GeoSearchResults$SelectedRowsGSETable <- reactive({
        shiny::req(input$GseSummaryData_rows_all)
        message("Filtering GSE table by row selection")
        GseDescDF <- GeoSearchResults$GseSummaryTableData()
        GseDescDFSelected <- GseDescDF[input$GseSummaryData_rows_selected, ]
        GseDescDFSelected
    })
    
    #' Render a Value box showing the total number of studies matching keyword found
    #' @req input$MolSelectInput - character vector of keywords to search against GEO database 
    #' @input input$GseSummaryData_rows_all - total number of rows in GseSummaryData table
    #' @output nTotalStudiesIndicator Value Box
    output$nTotalStudiesIndicator <- renderValueBox({
        nTotalStudies <- input$GseSummaryData_rows_all
        valueBox(paste0("Total Datasets:", length(nTotalStudies)), "Add species to view all studies" , icon = icon("book"), color = "blue")
    })
     
    #' Render a Value box showing the number studies selected from GseSummaryData Table by user
    #' @req NA
    #' @input input$GseSummaryData_rows_all - number of rows of GseSummaryData table selected by user 
    #' @output nStudiesSelectedIndicator Value Box  
    output$nStudiesSelectedIndicator <- renderValueBox({
        if(is.null(input$GseSummaryData_rows_selected)){ rowSelection <- 0 
        } else {
                rowSelection <- input$GseSummaryData_rows_selected
                rowSelection <- length(rowSelection)
       }
        valueBox( paste0("Studies selected:",rowSelection),"Click Table Rows to Select Studies" , icon = icon("list"), color = "blue")
    })
    
    #' Render a barplot of the taxon of the quries GEO studies
    #' 
    #'
    #'
    #'
    
    observeEvent(input$MolSelectButton, {
        shinyjs::show("SearchResPlot1")
        shinyjs::show("SearchResPlot2")
    })
    
    observeEvent(input$MolSelectButton, {
    output$nStudiesPlotTaxon <- renderPlot({
        input <- 'taxon'
        titleText <- TitleCase(input)
        plotdata <- GeoSearchResults$GseSummaryTableData()
        if(length(as.numeric(plotdata$n_samples))==0){ stop("No results with keyword")
        } else {

        plotdata$n_samples <- as.numeric(plotdata$n_samples)
        plotdata <- plotdata[complete.cases(plotdata),]
        
        p <- ggplot(plotdata, aes_string( fill = input, x = "sum(n_samples)", group = input)) +geom_bar(position = position_dodge()) +
            labs(title = paste("GSE for Molecule per", titleText), x = titleText, y = "Number of Studies") +
            theme(plot.title = element_text(hjust = 0.5),
                legend.title =  element_text(titleText),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(size = 0),
                axis.text.y = element_text(size = 12),
                legend.spacing.x = unit(2, "cm"),
                legend.position = "none") +
                guides(colour = guide_legend(ncol = 1))

        if (length(unique(plotdata$taxon))<15) {
        p <- ggplot(plotdata, aes_string( fill = input, x = "sum(n_samples)", group = input)) +geom_bar(position = position_dodge()) +
            labs(title = paste("GSE for Molecule per", titleText), x = titleText, y = "Number of Studies") +
            theme(plot.title = element_text(hjust = 0.5),
                legend.title =  element_text(titleText),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(size = 0),
                axis.text.y = element_text(size = 12),
                legend.spacing.x = unit(2, "cm"),
                legend.position = "bottom") +
                guides(colour = guide_legend(ncol = 1))
        }
        p
        
        }
    })
    })
    
    observeEvent(input$MolSelectButton, {
    output$nStudiesPlotGdsType <- renderPlot({
        shiny::req(input$GseSummaryData_rows_all)
        input <- 'gdsType'
        titleText <- TitleCase(input)
        plotdata <- GeoSearchResults$GseSummaryTableData()
        if(length(plotdata)==0){ stop("No results with keyword")}
        plotdata$n_samples <- as.numeric(plotdata$n_samples)
        plotdata <- plotdata[complete.cases(plotdata), ]
               
        p = ggplot(plotdata, aes_string( fill = input, x = "sum(n_samples)", group = input)) + geom_bar(position = position_dodge()) +
            labs(title = paste("GSE for Molecule per", titleText), x = titleText, y = "Number of Studies") +
            theme(plot.title = element_text(hjust = 0.5),
                legend.title =  element_text(titleText),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(size = 0),
                axis.text.y = element_text(size = 12),
                legend.spacing.x = unit(2, "cm"),
                legend.position = "bottom") +
                guides(colour = guide_legend(ncol = 1))
        p
    })
    })
     
    ########################{ Advance to GSM Metadata Page
    observe({    
        SelectedRows <- input$GseSummaryData_rows_selected 
        if (length(SelectedRows) > 0 ) {
        shinyjs::enable("AnalyzeSelectedDatasets")
        shinyjs::hide("GSMMetadataWarning")
        shinyjs::hide("GSMMetadataWarning_Design")
        shinyjs::hide("GSMMetadataWarning_Down")
        shinyjs::hide("GSMMetadataWarning_Exp")
        shinyjs::show("GSMMetadataLoading")
        } else {
        shinyjs::disable("AnalyzeSelectedDatasets")
        shinyjs::disable("ExpressAnalyse")
        shinyjs::show("GSMMetadataWarning")
        shinyjs::show("GSMMetadataWarning_Design")
        shinyjs::show("GSMMetadataWarning_Down")
        shinyjs::show("GSMMetadataWarning_Exp")
        shinyjs::hide("GSMMetadataLoading")
        }
        shinyjs::hide("GSMMetadataLoading") 
        })
    
    
    observeEvent( input[["AnalyzeSelectedDatasets"]], { updateTabItems(session, "TabSet", "GSMMetadata")})
    ################################### GSM Metadata TabItem ###################################
    
    SQLSearchData <- reactiveValues()
    ########################{ SQL Search

    #' Render a Table with join GSE and GSM from SQL search
    #' 
    #'
    #'
    SQLSearchData$GseGsmTable <- eventReactive(input$AnalyzeSelectedDatasets, {
        shiny::req(input$GseSummaryData_rows_selected)
        message("Generating ResultDF for SQL Search")
        GseTable <- GeoSearchResults$SelectedRowsGSETable()
        message("SQL Query of Selected Datasets")
        shinyjs::show("GSMMetadataLoading")
        GsmTable <- SqlQueryMain(GseTable)
        shinyjs::hide("GSMMetadataLoading")
        GsmTable <- data.frame(GsmTable, stringsAsFactors = F)
        GseGsmTable <- GseTable %>% dplyr::select(-one_of("GPL")) %>% dplyr::inner_join(GsmTable, "series_id")
        GseGsmTable
       })
     
     
    ########################{ GSE Selection and Filter
    SQLSearchData$SelectedGSENames <- reactive({
        GseTable <- GeoSearchResults$SelectedRowsGSETable()
        SelectedGSENames <- GseTable[,"series_id"] # Get the GSE names of the selected GSE's in the data table
    })
    
    output$GseTabletoKeep_UI <- renderUI({
        #shiny::req(input$GseSummaryData_rows_selected)
        SelectedGSENames <- SQLSearchData$SelectedGSENames()
        message("Initializing input for filtering GSE/GSM data to show")
        CheckBoxOptions <- list(inputId = "KeepForExpVarAsign", 
            label = "Datasets", 
            choices = SelectedGSENames, 
            selected = SelectedGSENames, 
            inline = F)
        do.call(checkboxGroupInput, CheckBoxOptions)
    })
    
    output$GseTabletoAnalyze_UI <- renderUI({
        
        SelectedGSENames <- SQLSearchData$SelectedGSENames()
        # message("Initializing input for filtering GSE/GSM data to show")
        # CheckBoxOptions <- list(inputId = "KeepForExpVarAsign", 
        #     label = "Datasets", 
        #     choices = SelectedGSENames, 
        #     selected = SelectedGSENames, 
        #     inline = F)
        # do.call(checkboxGroupInput, CheckBoxOptions)
        
        #shiny::req(input$KeepForExpVarAsign)
        #GSEChoices <- input$KeepForExpVarAsign
        selectInput(inputId = "GsmTableSelect", label = "Select Dataset", choices = SelectedGSENames)
    })
    
    #' @input KeepForExpVarAsign - GSE datasets to keep selection
    #' @input input$GsmTableSelect - GSE dataset to analyze input
    #' @input GeoSearchResults$SelectedRowsGSETable - GSE Search result table
    #' 
    #' @output Render input for user to select GPL in GSE to analyze
    SQLSearchData$SelectedGSETable <- reactive({
        GseTable <- GeoSearchResults$SelectedRowsGSETable()
        SelectedGSE <- input$GsmTableSelect
        GPLChoices <-  GseTable %>% dplyr::filter(series_id %in% SelectedGSE)
    })
    
    output$GplTabletoAnalyze_UI <- renderUI({
        #shiny::req(input$KeepForExpVarAsign, input$GsmTableSelect)
        SelectedGSETable <- SQLSearchData$SelectedGSETable()
        GPLChoices <- SelectedGSETable %>% dplyr::select(GPL) %>% unique %>% as.character
        GPLChoices <- unlist(stringr::str_split(string = GPLChoices,pattern = ";"))
        GPLChoices <- paste("GPL", GPLChoices, sep = "")
        selectInput(inputId = "GplTableSelect", label = "Select GPL", choices = GPLChoices) 
    })
    
    #' @input input$GsmTableSelect - User selects which GSE they would like to work with
    #' @input input$GplTableSelect - User select which GPL within the Selected GSE they would like to work with
    #' @req input$GsmTableSelect - Clickling the button "Analyze selected datasets" triggers SQL query
    #' @output SQLSearchData$FilteredResultDF Dataframe containing GSE and GSM metadata tables filtered with user selected GSE and GPL
    SQLSearchData$FilteredResultDF <- reactive({
        #shiny::req(input$KeepForExpVarAsign, input$GsmTableSelect, input$GplTableSelect)
        ResultDF <- SQLSearchData$GseGsmTable()
        GsmtoShow <- input$GsmTableSelect
        GpltoShow <- input$GplTableSelect
        
        message("Filtering SQL Query Res for Selected GSE")
        FilteredResultDF <- ResultDF %>%
            dplyr::select(series_id, gsm, gpl, keyword, taxon, gsm.title, description, characteristics_ch1) %>%
            dplyr::filter(series_id %in% GsmtoShow) %>% 
            dplyr::filter(gpl %in% GpltoShow)
        FilteredResultDF
    })
     
    ####################################{ Table Showing Metadata Tables Containing ExpVars}

    
    #' Render information text with the Species, nSamples and experiment type
    #'
    #'
    #'
    output$infobox_selectedGSE <- renderUI({
        shiny::req(input$GsmTableSelect, input$GplTableSelect)
        FilteredTable <-SQLSearchData$SelectedGSETable()
        
        SpeciesInfo <- FilteredTable %>% select(taxon) %>% as.character
        SamplesInfo <- FilteredTable %>% select(n_samples) %>% as.character
        GdsInfo <- FilteredTable <- FilteredTable %>% select(gdsType) %>% as.character
        
        fluidRow(
        column(3, h4(strong("n samples:")), h5(SamplesInfo)),
        column(3, h4(strong("species:")), h5(SpeciesInfo)),
        column(6, h4(strong("gdsType:")), h5(GdsInfo))
        )
    })
    

    ##################{ Render GSM Meta data
    
    #' @input SQLSearchData$FilteredResultDF() Dataframe of SQL query filter to only contain GSE and GPL selected by user
    #' @req input$KeepForExpVarAsign wait unlist option to select GSE is renderd before displaying table,
    #' prevents empty table from being rendered
    #' @req input$GsmTableSelect wait until GSE to analyze is picked before rendering the assoviated GSE GSM table
    #' @output Render Datatable that shows Description, Title and Characteristics of the GSM withing a given GSE
    output$GseGsmTable <- DT::renderDataTable({
        message("Rendering GseGSMTable")
        #shiny::req(input$PickFactorColumns)
        
        SqlQueryResDF <- SQLSearchData$FilteredResultDF()
        SqlQueryResDF <- SqlQueryResDF %>% select(-one_of(c("series_id","taxon","keyword","gpl","gsm")))
        message("Making SQL Summary Table")
        DT::datatable(data = SqlQueryResDF , rownames = FALSE, class = 'row-border', 
            options = list(scrollY = '400px', dom = 't', paging = FALSE, autoWidth = TRUE,scrollX = T)) %>%
            formatStyle(names(SqlQueryResDF), color = 'black', backgroundColor = 'white', fontWeight = 'bold')
     })

    #################{ Classify ExpVars
    ExperimentalDesign <- reactiveValues()
    
    #' 
    #'
    #'
    #'
    #'
    ExperimentalDesign$ExpFactorDF <- reactive({
        message("Processing SQL Table Output")
        ExpFactorDF <- SQLSearchData$FilteredResultDF()
        message("Classify the Summary and Return the Filtered GSE GSM DF")
        ExpFactorClassSummary <- ClassSummary(ExpFactorDF) #error is that ExpFactorDF not loaded when this first starts
        message("Expands Characteristics Column")
        CharInputs <- input$WhereVarData
        ExpandedDF <- GseGsmCharExpand(ExpFactorClassSummary, CharInputs)
        
        UseFulExpVarsColNames <- grep(pattern = "ExpVar[[:digit:]]",x = colnames(ExpandedDF),value = T)
        UseFulExpVarsDF <- data.frame(ExpandedDF[, UseFulExpVarsColNames])
        colnames(UseFulExpVarsDF) <- UseFulExpVarsColNames
        UseFulExpVarsDF
     })

    ########################{ Useful Factor Classification
    
    
    output$PickFactorColumns <- renderUI({
    shiny::req(input$GseGsmTable_rows_all)
        ExpFactorDF <- ExperimentalDesign$ExpFactorDF()
        message("Importing ClassListDF")
        ClassResList <- ClassGsmText(ExpFactorDF)
        message("Executing DescerningClassDF")
        UsefulFactorList <- DescerningClassDF(ClassResList)
        message("Adding Time Factors")
        TimeFactorList <- AddSeriesDFs(ClassDFList = ClassResList, "time")
        message("Adding Titration Series")
        TitrationFactorList <- AddSeriesDFs(ClassDFList = ClassResList, "titration")
        message("Output Default ExpVarSelection")
        
        RecVars <- c(UsefulFactorList, TimeFactorList, TitrationFactorList)
        message("Rendering checkbox input for selecting which Factors to use")
        checkboxOutput <- checkboxGroupInput(
            inputId = "UsefulColumnsCheckbox",
            label = "Factors that describe detected experimental design cohorts",
            choices = colnames(ExpFactorDF),
            selected = names(RecVars),
            inline = T)
        
        fluidRow(column(12,checkboxOutput))
        })
    
    ########################{ Render Inputs to Filter Factors
    
    #'
    #'
    #' @req UsefulColumnsCheckbox need to know what variables the user picked to make a filter of the levels within
    #' @req GseGsmTable_rows_all using the table as a req valraible more an insurance that if 
    #' wherevars inputs changes it will effect the factor filters and not just UsefulColumnsCheckbox
    #'
    output$FilterGSMbyFactor <- renderUI({
        shiny::req(input$UsefulColumnsCheckbox, input$GseGsmTable_rows_all)
        message("Rendering Selectize Inputs for filtering factor levels")
        FactorDF <- ExperimentalDesign$ExpFactorDF()
        FactorDF <- FactorDF %>% select(one_of(input$UsefulColumnsCheckbox))
        
        NamesIndex <- colnames(FactorDF)
        FactorLevelInput <- lapply(NamesIndex, function(ColName) {
            ColLevels <- FactorDF[, ColName]
            selectInput(
                inputId = paste("Gsm_", ColName, sep = ""),
                label = paste("Filter levels in", ColName),
                choices = unique(ColLevels),
                selected = unique(ColLevels),
                multiple = T,
                selectize = T
            )
        })
    })
    
    # Render Input to remove Specific Rows
    #' @input
    #' @req GseGsmTable_rows_all, UsefulColumnsCheckbox preventing selection input from rendering till the Use factor columns are renderd and used to filter the data
    #' @output Render a numerical input that allows use to define specific rows to remove
    output$RemoveSpecificRows_UI <- renderUI({
        shiny::req(input$UsefulColumnsCheckbox, input$GseGsmTable_rows_all)
        FactorDF <- ExperimentalDesign$ExpFactorDF()
        selectInput(inputId = "RemoveSpecificRows", label = "Remove Specific Rows", choices = as.character(1:nrow(FactorDF)), multiple = T)
    })
    
    ########################{ Take Levels from inputs and determine rows of DF
    #' @input
    #' @req
    #' @output
    ExperimentalDesign$RowFilteredFactorDF <- reactive({
        #shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
        message("Running RowsToKeep for Factor Level Filtering")
        FactorDF <- ExperimentalDesign$ExpFactorDF()
        FactorDF <- FactorDF %>% dplyr::select(one_of(input$UsefulColumnsCheckbox))
        if (!is.data.frame(FactorDF)){ FactorDF <- as.data.frame(FactorDF) }
        
        NamesIndex <- colnames(FactorDF)
        RowsToKeep <- lapply(NamesIndex, function(ColName){
            InputName<- paste("Gsm_", ColName, sep = "")
            FilterLevels <- input[[InputName]]
            matches <- grep(paste(FilterLevels,collapse="|"), FactorDF[,ColName], value=F)
        })
        
        names(RowsToKeep) <- NamesIndex
        if (length(NamesIndex) > 1) {
            nms <- combn( names(RowsToKeep) , 2 , FUN = paste0 , collapse = "" , simplify = FALSE ) # get the combinations of names of list elements
            ll <- combn( RowsToKeep , 2 , simplify = FALSE ) # Make the combinations of list elements
            out <- lapply( ll , function(x) intersect( x[[1]] , x[[2]] ) ) # Intersect the list elements")
            setNames( out , nms ) # Output with names
            SmallestSet <- unlist(lapply(out, length)) # Find the length of all row name vectors
            RowsToKeep <- out[which.min(SmallestSet)] # Find the location of the smaller element
            RowsToKeep <- unlist(RowsToKeep)
        } else if (length(NamesIndex) == 1){
            InputName<- paste("Gsm_", NamesIndex, sep = "")
            FilterLevels <- input[[InputName]]
            RowsToKeep <- grep(paste(FilterLevels,collapse="|"), FactorDF[,1], value=F)
        
        } else { 
            stop("Error in FactorDF, restart app or select different factor columns") }
        
        
        if(isTruthy(input$RemoveSpecificRows)){
            message("Removing specific Use selected row")
            removeSpecificRows <- as.numeric(input$RemoveSpecificRows)
            RowsToKeep <- setdiff(RowsToKeep, removeSpecificRows)
        }
        
        ExperimentalDesign$RowsToKeep <- RowsToKeep
        FilteredFactorDF <- FactorDF[RowsToKeep,]
        FilteredFactorDF <- as.data.frame(FilteredFactorDF)
        if (ncol(FilteredFactorDF) < 2) { colnames(FilteredFactorDF) <- "ExpVar1"}
       FilteredFactorDF
       })
     
    #######################{ Output Table with Factor Selection
      
    ##### Unique Factor Table
    # output$ImportantFactorTable <- DT::renderDataTable({
    #     #shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
    #     ExpFactorDF <- ExperimentalDesign$RowFilteredFactorDF()
    #     
    #     DT::datatable(data = ExpFactorDF, extensions = 'ColReorder', class = 'compact',
    #         options = list( dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px',paging = FALSE,
    #         columnDefs = list(list(width = '150px', targets = c(1:ncol(ExpFactorDF)))),
    #         colReorder = list(realtime = FALSE))) %>%
    #         formatStyle(names(ExpFactorDF), color = 'black', fontWeight = 'bold')
    #   })
      
    ##### Full Factor Table
    
    #' @req input$UsefulColumnsCheckbox - Rendered Input that selected Useful Variables
    #' @input RefreshFullFactorTable Button under the table to refresh incase shiny hangs
    #'
    output$FullFactorTable <- DT::renderDataTable({
        message("Rendering Factor Data Table")
        shiny::req(input$UsefulColumnsCheckbox)
        
        input$RefreshFullFactorTable
        FactorDF <- ExperimentalDesign$RowFilteredFactorDF()
        
        DT::datatable(data = FactorDF, extensions = 'ColReorder',class = 'compact',
            options = list(dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px', paging = FALSE,
                columnDefs = list(list(width = '150px', targets = c(1:ncol(FactorDF)))),
                colReorder = list(realtime = FALSE))) %>%
            formatStyle(names(FactorDF),color = 'black',fontWeight = 'bold')
        })
    ##### Excluded Factor Table
        output$ExcludedFactorTable <- DT::renderDataTable({
        message("Rendering Excluded Factor Data Table")
        
        FactorDF <- ExperimentalDesign$RowFilteredFactorDF()
        ExcludedFactorDF <- FactorDF %>% select(-one_of(input$UsefulColumnsCheckbox))
        ExcludedFactorDF <- as.data.frame(ExcludedFactorDF)
     
        DT::datatable(data = ExcludedFactorDF, extensions = 'ColReorder',class = 'compact',
            options = list(dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px', paging = FALSE,
            columnDefs = list(list(width = '150px', targets = c(1:ncol(ExcludedFactorDF)))),
            colReorder = list(realtime = FALSE))) %>%
            formatStyle(names(ExcludedFactorDF),color = 'black',fontWeight = 'bold')
        })
     
     
     observeEvent( input[["GoToDesignPage"]], { updateTabItems(session, "TabSet", "DesignMatrix")})
    ################################### GSM Metadata TabItem ##############################################
     
    ################################### Design Matrix #####################################################

        output$RenameFactorDF_UI <- renderUI({
            FactorDF <- ExperimentalDesign$RowFilteredFactorDF()

            lapply(1:length(colnames(FactorDF)), function(FactorNameIndex){
                orginalName <- colnames(FactorDF)[FactorNameIndex]
                checkinputID <- paste("RenameFactorDFCheck", FactorNameIndex, sep = "")
                checkinputLabel <- paste("Rename", orginalName)
                textInputID <- paste("RenameFactorDFText", FactorNameIndex, sep = "")
                textInputLabel <- paste("Rename Experimental Variable", FactorNameIndex)
                
                leveltext <- as.character(FactorDF[1, FactorNameIndex])
                if (grep(pattern = ":", x = leveltext)) {
                    newNameRec <- str_split(pattern =  ":", leveltext)[[1]][[1]]
                    newNameRec <- gsub(pattern = "^\\s", replacement = "", x = newNameRec)
                    newNameRec <- make.names(newNameRec)    
                } else {newNameRec <- NULL}
                
                wellPanel(
                fluidRow(
                column(4, checkboxInput(checkinputID, checkinputLabel, value = T)),
                conditionalPanel(paste("input.",checkinputID,"==1", sep = ""),
                column(8, textInput(inputId = textInputID ,label = textInputLabel, value = newNameRec)))))
            })
        })
     
        ExperimentalDesign$RenamedFactorDF <- reactive({
            shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
            input$ApplyFactorColRename
            FactorDF <- ExperimentalDesign$RowFilteredFactorDF()
            colnamesIndex <- 1:length(colnames(FactorDF))
        
            NewColNames <- 
            lapply(colnamesIndex, function(FactorNameIndex){
                colnameText <- colnames(FactorDF)[FactorNameIndex]
                checkinputID <- paste("RenameFactorDFCheck", FactorNameIndex, sep = "")
                textInputID <- paste("RenameFactorDFText", FactorNameIndex, sep = "")
            
                if (input[[checkinputID]] == T) {
                InputName <- input[[textInputID]]
                InputName <- gsub("[[:space:]]", "", InputName)
                colnameText <- make.names(InputName)
                } else {colnameText}
            })
            colnames(FactorDF) <- unlist(NewColNames)
            FactorDF
        })
                
        output$DesignMat_SummaryTable <- DT::renderDataTable({
            shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
            ExpFactorDF <- ExperimentalDesign$RenamedFactorDF()
            ExpFactorDF <- data.frame(ExpFactorDF)
        
            DT::datatable(data = ExpFactorDF, extensions = 'ColReorder', class = 'compact',
                options = list( dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px',paging = FALSE,
                columnDefs = list(list(width = '150px', targets = c(1:ncol(ExpFactorDF)))),
                colReorder = list(realtime = FALSE))) %>%
                formatStyle(names(ExpFactorDF), color = 'black', fontWeight = 'bold')
        })
     
     
    ########################{ Set the Control Levels for each factor

     output$RearrangeLevels <- renderUI({
         FactorDF <- ExperimentalDesign$RenamedFactorDF()
         NamesIndex <- colnames(FactorDF)
         
         lapply(1:length(NamesIndex), function(ColNameIndex) {
             ColName <- NamesIndex[ColNameIndex]
             ColLevels <- factor(FactorDF[, ColName])
             inputName <- paste("FactorLevels_", ColNameIndex, sep = "")
             selectInput( inputId = inputName, label = paste("Selected control level for", ColName), choices = c(levels(ColLevels), "none"))
         })
     })
      
    ExperimentalDesign$ControlFactorDF <- reactive({
        shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
        FactorDF <- ExperimentalDesign$RenamedFactorDF()
        NamesIndex <- colnames(FactorDF)
         
        ResList <- lapply(1:length(NamesIndex), function(ColNameIndex) {
            ColName <- NamesIndex[ColNameIndex]
            ResultVector <- FactorDF[, ColName]
            inputName <- paste("FactorLevels_", ColNameIndex, sep = "")
            InputControlLevel <- input[[inputName]]
            res <- factor(ResultVector,
            levels=c(InputControlLevel, setdiff(as.character(ResultVector), InputControlLevel)))
            res
        })
        names(ResList) <- NamesIndex
        ResDF <- do.call(cbind.data.frame, ResList)
        ResDF
    })
         
    ##################### Formula Input
     
    
    #' Looks at the user formula input and parses out the factors that the user wants to include in the design matrix
    #' @input Design Formula
    #' @return string vectors with the factors that are in the design formula input
    ExperimentalDesign$DesignformulaFactors <- reactive({
        Designformula <- input$formulaInputDesign
        Designformula_trimmed <- gsub("\\s", "", Designformula)
        
        if (!grep(pattern = "^~",x = Designformula_trimmed)) {
        stop("Formula input must begin with ~")
        } else {
        Designformula_trimmed <- gsub("0", "", Designformula_trimmed)
        Designformula_trimmed <- gsub("~", "", Designformula_trimmed)
        Designformula_trimmed <- gsub("*", "\\+", Designformula_trimmed)
        Designformula_trimmed <- gsub(":", "\\+", Designformula_trimmed)
        DesignformulaFactors <- unlist(strsplit(Designformula_trimmed, split = "\\+"))
        }
        DesignformulaFactors <- unique(DesignformulaFactors)
        DesignformulaFactors
    })
    
    # From the factors found in the input formula, 
    # the 
    ExperimentalDesign$DesignMatrixInput <- eventReactive(input$SubmitFormula, {
        FactorDF <- ExperimentalDesign$ControlFactorDF()
        DFindex <- c(1:nrow(FactorDF))
        FactorDF <- cbind.data.frame(DFindex, FactorDF)
        
        Designformula <- input$formulaInputDesign
        DesignformulaFactors <- ExperimentalDesign$DesignformulaFactors()
        
        # use the design formula input to arrange the FactorDF, this will be reflected as changed in the design matrix
        message(paste("Control level supplied for:", DesignformulaFactors, "    "))
        FormulaFactors <- grep(pattern = paste(DesignformulaFactors, collapse = "|"), colnames(FactorDF), value = T)
        RearrangeDF <- FactorDF %>% dplyr::arrange_(FormulaFactors)
        
        # Genrate the MOdel matrix from the Data frame after it has been arranged according to the levels
        DesignExpression <- try(as.formula(Designformula))
        if (class(DesignExpression)[1] == "try-error") { stop("Caught an error trying to make Design Matrix")
        } else {DesignMatrix <- model.matrix(as.formula(DesignExpression), FactorDF)}
        DesignMatrix
    })
    
    #' @input SubmitForumla Any time a new formula is submitted there should be a few matrix event
    #' @input takes in the intial design matrix input 
    output$DesignMatrixRename_UI <- renderUI({
        input$SubmitFormula
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndexVector <- 1:length(colnames(DesignMatrix))
        
        # If there is a leading 0 in the formula input, just name the first column baseline
        # # Determine if there is an intercept in the formula input        
        InputFormula <- input$formulaInputDesign
        InputFormula <- gsub("\\s", "", InputFormula)
        InputFormula <- gsub("~", "", InputFormula)
        InputFormula <- unlist(strsplit(InputFormula, split = "\\*"))
        InputFormula <- unlist(strsplit(InputFormula, split = "\\+"))
        InputFormula <- unlist(strsplit(InputFormula, split = ":"))
        
        # Go through and gererate the text inpouts for design columns names
        lapply(colnamesIndexVector, function(colnamesIndex){
            colNameText <- colnames(DesignMatrix)[colnamesIndex]
            # Render Checkbox input to determine if use wants to renmae factor
            checkinputID <- paste("RenameDesign", colnamesIndex, sep = "")
            checkinputLabel <- paste("Rename Design Column",colnamesIndex, "-", colNameText)
            # Render Textinput for the new variable names
            textInputID <- paste("RenameDesignText", colnamesIndex, sep = "")
            textInputLabel <- paste("new column", colnamesIndex, "name:")
            
            # If there is no 0 in the formula input then the first column of the design
            # matrix is automatically renamed, this is done by giving the RenameDesign
            # checkbox input for the first level a 1 and the value of the rename level "baseline"
            if (colnamesIndex == 1 & InputFormula[1] != 0) {
                newNameCheckBox <- 1
                newNameRec <- "baseline" 
            } else if (colnamesIndex != 1 & grep(pattern = ":", x = colNameText)) {
            # a lot of GSM are label Factor: Level formation, so if a colon
            # is detected the string after the colon is reccomnded for the design matrix colname
                newNameCheckBox <- 1 
                newNameRec <- str_split(pattern =  ":", colNameText)
                if (length(newNameRec)>0) {newNameRec <- newNameRec[[1]][[2]]} else {newNameRec <- newNameRec}
                newNameRec <- gsub(pattern = "^\\s", replacement = "", x = newNameRec)
                #newNameRec <- make.names(newNameRec)   
            } else {
                newNameCheckBox <- NULL
                newNameRec <- NULL
            }
             
            
            wellPanel(fluidRow(
            column(12, checkboxInput(checkinputID, checkinputLabel, value = newNameCheckBox)),
            conditionalPanel(paste("input.",checkinputID,"==1", sep = ""),
            column(12, textInput(inputId = textInputID , label = textInputLabel, value = newNameRec)))))
        })
    })
    

    #' Loop through all the user inputs for design column names and genrate matrix and 
    #' reset the column names if the textinputID 
    #' for that specific column is checked
    ExperimentalDesign$DesignMatrix <- reactive({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        # Loop through col names inputs and see if any have been supplied by the user
        # by default all columns are renamed
        NewColNames <- 
        lapply(colnamesIndex, function(FactorNameIndex){
            colnameText <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            
            if (input[[checkinputID]] == T) {
            InputName <- input[[textInputID]]
            InputName <- gsub("[[:space:]]", "", InputName)
            colnameText <- make.names(InputName)
            } else {colnameText}
        })
        colnames(DesignMatrix) <- make.names(unlist(NewColNames))
        
        return(DesignMatrix)
    })
     
        observeEvent(input$SubmitFormula, {
            shiny::req(input$SubmitFormula, input$UsefulColumnsCheckbox, input$WhereVarData)
            output$CustomExpressionTable <- DT::renderDataTable({
            DesignMatrix <- ExperimentalDesign$DesignMatrix()
            DT::datatable( data = DesignMatrix, rownames = TRUE, class = 'compact',extensions = 'Buttons',
                options = list( scrollY = '300px', scrollX = TRUE, paging = FALSE, dom = 'Bt', buttons = c('copy', 'csv', 'excel'))
              )
            })
        })
      
      
        
        ################### Custom Contrast Input
        
        #' @reactiveValues nUserContrasts is a ractive value that counts how many custom contrasts the user wants
        #' @input addContrast when action button  addContrast is clicked it add one to the counter of custom contrasts requried
        #' @input removeContrast when action button removeContrasts is clicked it removes one fron the nUseContrasts count
        nUserContrasts <- reactiveValues(count = 1)
        observeEvent(input$addContrast, nUserContrasts$count <- nUserContrasts$count + 1)
        observeEvent(input$removeContrast, nUserContrasts$count <- nUserContrasts$count - 1)
  
        
        #' This function loops from 1 to nUserContrasts$count, that the user has set and generated a title and forumula input textinput box
        #' for the user
        output$UserContrasts <- renderUI({
            nInputs <- nUserContrasts$count
            if (nInputs <= 0) { stop("cannot remove anymore contrasts")}
            lapply(1:nInputs, function(i){
                ContrastTitleId = paste("ContrastTitle", i, sep = "_")
                ContrastTitlelabel = paste("Contrast", i, "title:")
                ContrastFormulaId = paste("ContrastFormula", i, sep = "_")
                ContrastFormulaLabel = paste("Contrast", i, "formula input:")
                # Try to change inputs to render the inputs without compeltly wiping them any time a new contast is added
                fluidRow(column(6, textInput(ContrastTitleId, ContrastTitlelabel)),
                column(6, textInput(ContrastFormulaId, ContrastFormulaLabel)))
            })
        })
        
        #' When the generate contrasts button is click
        #' generate user defined contrast matrix
        ExperimentalDesign$UserContrastMatrixData <- eventReactive(input$GenerateUserContrast,{
        nInputs <- nUserContrasts$count
            
        UserContrasts <- lapply(1:nInputs, function(i){
            DesignMatrix <- ExperimentalDesign$DesignMatrix()
            
            # Loop throught the contrast inputs and extract user inputs
            ContrastTitleId = paste("ContrastTitle", i, sep = "_")
            ContrastTitle <- input[[ContrastTitleId]]
            ContrastFormulaId = paste("ContrastFormula", i, sep = "_")
            ContrastFormula <- input[[ContrastFormulaId]]
            
            # See if both title and contrast formula as applied
            # if they are generate a string input for the make contrast funciton
            # if not return null
            if (isTruthy(ContrastFormula) & isTruthy(ContrastTitle)) {
                message(paste("Valid inputs for contrast", i, "found"))
                ContrastInputDF <- data.frame(ContrastTitle,ContrastFormula)
                MakeContrastInputString = apply(ContrastInputDF, MARGIN = 1, paste, collapse = "=")
                
                # Make the string command to call make contrast function
                astr=paste(MakeContrastInputString, collapse=",")
                prestr="makeContrasts("
                poststr=",levels=DesignMatrix)"
                commandstr=paste(prestr,astr,poststr,sep="")
                
                # Call Make contrast string
                MatrixColumn <- eval(parse(text=commandstr))
                return(MatrixColumn)
            } else { return(NULL) }
        })
        
        # Remove any inputs for which a valid title and contrast formula were not supplied
        UserContrasts <- UserContrasts[!vapply(UserContrasts, is.null, logical(1))]
        if (length(UserContrasts) == 0) { stop("Please enter valid contrast title(No Spaces) and formula") }
        UserContrasts <- do.call(cbind.data.frame, UserContrasts)
        UserContrasts
        })
    
        output$UserContrastMatrix <- DT::renderDataTable({
            datatable(data = ExperimentalDesign$UserContrastMatrixData(),
                      options = list(searching = FALSE, paging = F, scrollX = T))
            })
        
        #' Generate Contrast Matrix 
        #' and concatonate user design matrix
        #' @param DesignMatrix requrired to determine the availible contrasts
        #' @param FormulaInputDesign the formula requried to determine if there is an intercept 
        #' and ensure first row of contrast matrix is always 0 when there is an intercept
        ExperimentalDesign$ContrastMatrixInput <- reactive({
        DesignMatrix <- ExperimentalDesign$DesignMatrix()
        
        #ControlFactorLevels 
        ContrastInput <- ConTextInput(DesignMatrix)
        ContrastInput <- gsub(" ", "", ContrastInput)
        ContrastMatrix <- makeContrasts(contrasts = ContrastInput, levels = DesignMatrix)
        
        # Determine if there is an intercept in the formula input        
        InputFormula <- input$formulaInputDesign
        InputFormula <- gsub("\\s", "", InputFormula)
        InputFormula <- gsub("~", "", InputFormula)
        InputFormula <- unlist(strsplit(InputFormula, split = "\\*"))
        InputFormula <- unlist(strsplit(InputFormula, split = "\\+"))
        InputFormula <- unlist(strsplit(InputFormula, split = ":"))

        if (InputFormula[1] != 0) {
        message("Intercept Detected")
        ContrastMatrix <- apply(ContrastMatrix, 2, function(x){
            if (x[1] == 1) {
            x <- abs(x)  
            } else (x)
            })
        ContrastMatrix[1,] <- 0
        }
        ContrastMatrix
        })  
    
    
    #' 
    #'
    #'
    #'
    # ExperimentalDesign$ContrastMatrix <- reactive({
    #     shiny::req( input$SubmitFormula, input$UsefulColumnsCheckbox, input$WhereVarData, input$CustomExpressionTable_rows_all)
    #     ContrastMatrix <- ExperimentalDesign$ContrastMatrixInput()
    #     UserContrastMatrix <- ExperimentalDesign$UserContrastMatrixData()
    #     
    #     if (isTruthy(UserContrastMatrix)) {
    #     UserContrastMatrix <- UserContrastMatrixData()
    #     # the final contrast matrix will either be the concatination of the input and the use contrast input
    #     ContrastMatrix <- cbind(ContrastMatrix,UserContrastMatrix)
    #     }
    #     ContrastMatrix
    # })
    #     

    # Render the Contrast matrix
    
    ExperimentalDesign$ContrastMatrix <- reactive({
        ContrastMatrix <- ExperimentalDesign$ContrastMatrixInput()
        if(isTruthy(input$UserContrastMatrix_rows_all)){
             UserContrastMAtrix <- ExperimentalDesign$UserContrastMatrixData()
             ContrastMatrix <- cbind(ContrastMatrix, UserContrastMAtrix)
         } else { ContrastMatrix <- ContrastMatrix}
        return(ContrastMatrix)
    })
    
    output$ContrastMatrixTable <- renderDataTable({
        DT::datatable(
            data = ExperimentalDesign$ContrastMatrix(), rownames = TRUE, class = 'compact', 
            extensions = 'Buttons', options = list( scrollY = '300px', scrollX = T, paging = FALSE, dom = 'Bt', buttons = c('copy', 'csv', 'excel')))
    })
     

    # Render the experimental blocks
    output$ExperimentalBlocksPlot <- renderPlot({
        shiny::req(input$SubmitFormula, input$UsefulColumnsCheckbox, input$WhereVarData)
        DesignDF <- ExperimentalDesign$ControlFactorDF()
        
        DesignExpression <- input$formulaInputDesign
        DesignExpression <- gsub(x = DesignExpression, pattern = ":",replacement = "\\+")
        DesignExpression <- gsub(x = DesignExpression, pattern = "\\*",replacement = "\\+")
        DesignExpression <- try(as.formula(DesignExpression))
        
        RenderMosaicPlot <- try(vcd::mosaic(DesignExpression, DesignDF))
          
        if (class(RenderMosaicPlot)[1] == "try-error" |
        class(DesignExpression)[1] == "try-error") {
        stop(
            paste( "Caught an error trying to make design Mosaic Plot,\n",
                "trying changing formula input.\n",
                "maybe try ~", colnames(DesignDF)[1])
            )
        } else { RenderMosaicPlot }
      })
    
    observeEvent( input[["GoToQCPage"]], { updateTabItems(session, "TabSet", "DataQC")})
    
    
    ######################## Data QC and Download Analysis
    
        ##################### Data QC and Download Analysis
        ##### Download the Data
        #'
        #'
        GSEdata <- reactiveValues()
                        #'
        #'
        #'
        #'
        #'
        #'
        output$InputSourceGMT <- renderUI({
        radioButtons(inputId = "DataSourceSelection",
            label = paste("Retreive Gene Matrix file for", "GSE"),
            selected = 1,
            inline = F,
            choiceNames = c("Download from GEO", "Upload GMT as CSV or TSV"),
            choiceValues = c(1,2))
        })
                        #'
        #'
        #'
        #'
        #'
        #'
        GSEdata$GMTinput_GEO <- eventReactive(input$DownloadGEOData, {
            shinyjs::show("GMTTableGEO")
            GSE <- input$GsmTableSelect
            GPL <- input$GplTableSelect
            message(paste("Downloading", GSE, "Data from GEO"))
            GSEeset <- LoadGEOFiles(GSE, GPL, GeoRepo = "~/GeoWizard/GEORepo")
            return(GSEeset)
        })
                #'
        #'
        #'
        #'
        #'
        GSEdata$GMTinput_CSV <- reactive({
            req(input$GMTcsv)
            message("Reading in data from CSV")
            DF <- read.csv(input$GMTcsv$datapath, header = input$CSVheader, sep = input$CSVsep, quote = input$CSVquote, row.names = 1)
            DF
        })
                        #'
        #'
        #'
        #'
        #'
        output$GeneAnnotationTypeUI <- renderUI({
            shiny::req(input$GsmTableSelect, input$DownloadGEOData)
            GSEeset <- GSEdata$GMTinput_GEO()
            FeatureData <- fData(GSEeset)
            if (length(FeatureData) == 0) {
            wellPanel(
                h4(icon("exclamation-triangle"),"no feature data included in eset"),
                paste(
                "You can restart the application and try once more, but most likely the author",
                "did not include a data matrix in this gene series.",
                "Unfortunately, you will have to download the raw files and process them locally.",
                "Then input them as a CSV or TSV"))
            } else {
            selectInput(
                inputId = "GeneAnnotationType",
                label = "Gene Annotations",
                choices = colnames(FeatureData),
                selected = colnames(FeatureData)[1],
                multiple = F,
                selectize = T)
            }
        })
                #'
        #'
        #'
        #'
        #'
        #'
        GSEdata$MatrixAnnotated <- reactive({
            shiny::req(input$GeneAnnotationType)
            if(input$DataSourceSelection == 1) {
            message("Annotating ExpressionMatrix")
            GSEeset <- GSEdata$GMTinput_GEO()
            FeatureData <- try(fData(GSEeset))
            ExpressionMatrix <- exprs(GSEeset)
                    if (length(FeatureData) == 0 | class(FeatureData) == "try-error") { stop("No Feature Data included in Expression Set")
            } else {
            rownames(ExpressionMatrix) <- make.names(FeatureData[,input$GeneAnnotationType])
            return(ExpressionMatrix)
            }
            } else { return(NULL)}
        })
        
        GSEdata$ExpressionMatrix <- reactive({
            shiny::req(input$GeneAnnotationType)
            if(input$DataSourceSelection == 1) {
                message("Annotated GMT Matrix asigned to reactive value: GSEdata$ExpressionMatrix")
                ExpressionMatrix <- GSEdata$MatrixAnnotated()
            } else if(input$DataSourceSelection == 2) {
                message("GMTinput_CSV asigned to reactive value: GSEdata$ExpressionMatrix")
                if (isTruthy(input$GMTcsv)) {
                ExpressionMatrix <- GSEdata$GMTinput_CSV()
                }
            }
            return(ExpressionMatrix)
          })
                        #'
        #'
        #'
        #'
        
        GSEdata$FactorGMT <- reactive({
            ControlFactorDF <- ExperimentalDesign$ControlFactorDF()
            message("Loading Expression Matrix fro Factor GMT input")
            ExpressionMatrix <- GSEdata$ExpressionMatrix()
            message("line 657: Generating FactorGMT")
            FactorGMT <- GenFactorGMT(ExpressionMatrix = ExpressionMatrix, FactorDF = ControlFactorDF)
            return(FactorGMT)
          })
                #'
        #'
        #'
        #'
        output$RawDataQC <- renderDataTable({
            if (input$RawDataTableMelt == "GMT") {
                message("Expression Matrix loaded for RawDataQC Table ")
                TableData <- GSEdata$ExpressionMatrix()
            } else if (input$RawDataTableMelt == "FactorGMTMelt") {
                message("FactorGMTMelt loaded for RawDataQC Table ")
                TableData <- melt(GSEdata$FactorGMT())
            } else { stop("Data not loaded properly") }
                    DT::datatable(data = as.data.frame(TableData), rownames = TRUE, class = 'compact', extensions = 'Buttons',
                options = list( scrollX = F, scrollY = '300px', paging = T, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')))
          })
                        #################### BoxPlot Tab
                    output$BoxPlot_GeneSelect_UI <- renderUI({
                message("Rendering BoxPlot Gene Select Input")
                return(textInput( inputId = "BoxPlot_GeneSelect", label = "Plot Specific Gene"))
            })
                            GSEdata$FactorGMTMelt.Sampled <- reactive({
                message("Generating FactorGMTMelt reactive values for BoxPlot Input")
                message(paste("Adding custom input:", input$BoxPlot_GeneSelect))
                input$RefreshBoxPlotSample
                        GeneInput <- isolate(input$BoxPlot_GeneSelect)
                if(!nchar(GeneInput) > 3){ GeneInput <- ""}
                        FactorGMT.sampled <- sampleFactorGMT(FactorGMT = GSEdata$FactorGMT(),
                    nFactors = ncol(ExperimentalDesign$ControlFactorDF()),
                    SpecificGenes = GeneInput,
                    nGenes = input$BoxPlot_nGenes)
                FactorGMTMelt.sampled <- melt(FactorGMT.sampled)
                FactorGMTMelt.sampled
            })
                    output$BoxPlot_FactorSelect_UI <- renderUI({
                shiny::req(input$BoxPlot_PlotBy)
                message("Rendering BoxPlot Factor Select Input")
                        if(input$BoxPlot_PlotBy == "o"){ FactorOptions <- c("GSM",colnames(ExperimentalDesign$ControlFactorDF()))
                } else { FactorOptions <- c(colnames(ExperimentalDesign$ControlFactorDF())) }
                selectInput( inputId = "BoxPlot_FactorSelect", label = "Fill by Factor", choices = FactorOptions)
            })
            
            output$BoxPlot_ggplot <- renderPlot({
            shiny::req(input$BoxPlot_PlotBy, input$BoxPlot_FactorSelect)
            message("Rednering BoxPlot")
            FactorGMTMelt <- GSEdata$FactorGMTMelt.Sampled()
            BoxPlot_JitterFill <- "red"
            p <-BoxPlotGSE(
                    # Data
                    FactorGMTMelt = FactorGMTMelt,
                    BoxPlot_IndpVar = input$BoxPlot_IndpVar,
                    BoxPlot_PlotBy = input$BoxPlot_PlotBy,
                    BoxFactorSelectInput = input$BoxPlot_FactorSelect,
                            # Colors and Aesthetics
                    BoxPlot_Type =  input$BoxPlot_Type,
                    BoxPlot_showColor = input$BoxPlot_showColor,
                    BoxPlot_ThemeSelect = input$BoxPlot_ThemeSelect,
                    BoxPlot_ToggleLegend = input$BoxPlot_ToggleLegend,
                            # Jitter Settings
                    BoxPlot_showData = input$BoxPlot_showData,
                    BoxPlot_showDataOption = input$BoxPlot_showDataOption,
                    BoxPlot_JitterWidth = input$BoxPlot_JitterWidth,
                    BoxPlot_JitterAlpha = input$BoxPlot_JitterAlpha,
                    BoxPlot_JitterFill = BoxPlot_JitterFill,
                            # Margin Settings // Important for Plotly, ggplot doesnt really have trouble with Marigs
                    BoxPlot_showMargins = input$BoxPlot_showMargins,
                    BoxPlot_margin_top = input$BoxPlot_margin_top,
                    BoxPlot_margin_right = input$BoxPlot_margin_right,
                    BoxPlot_margin_bottom = input$BoxPlot_margin_bottom,
                    BoxPlot_margin_left = input$BoxPlot_margin_left,
                            # Axis Flip
                    BoxPlot_PlotAxisFlip = input$BoxPlot_PlotAxisFlip,
                            # Labels
                    BoxPlot_main = input$BoxPlot_main,
                    BoxPlot_xlab = input$BoxPlot_xlab,
                    BoxPlot_ylab = input$BoxPlot_ylab,
                    BoxPlot_xlab_angle = input$BoxPlot_xlab_angle)
                p
          })
                                ########## PCA
            FactorGMTCast <- reactive({
                FactorGMTCast <- GSEdata$FactorGMT()
                DataDF <- GSEdata$ExpressionMatrix()
                FactorDF <- ExperimentalDesign$ControlFactorDF()
                #FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
                
                
                return(list("FactorGMTCast" = FactorGMTCast, "DataDF" = DataDF,"FactorDF" = FactorDF))
                })
                        #' @param  DataDF gene expression matrix with samples in the columns and genes in the rows
                #' @return list of Prcomp_res and PCA_ResDF
                #' Prcomp_res principle components object genertated from perfoemd PCA on gene expression data
                #' PCA_ResDF Data frame with the Prcomp_res object and Prcomp_res input matrix (x) coloumn bound
                PCA_Data <- reactive({
                PCA_DataInput <- FactorGMTCast()$DataDF
                Prcomp_res <- prcomp(na.omit(t(PCA_DataInput)), center = as.logical(input$PCA_center), scale = as.logical(input$PCA_scale))
                #Prcomp_res <- prcomp(na.omit(t(ArrayData)),center = T, scale = T)
                PCA_ResDF <- cbind(t(PCA_DataInput), Prcomp_res$x)
                return(list("Prcomp_res" = Prcomp_res, "PCA_ResDF" = PCA_ResDF))
                })
                        #' Render Input that allows user to select PCA grouping
                #' @param FactorDF
                output$PCA_GroupUI <- renderUI({
                FactorDF <- FactorGMTCast()$FactorDF
                FactorGrouping <- c("None", colnames(FactorDF))
                selectInput(inputId = "PCA_Group", label = "Group by Factor", choices = FactorGrouping, selected = "None", multiple = F)
                })
                        #' Render Input that allows user to select PCA labeling factor
                output$PCA_LabelUI <- renderUI({
                FactorDF <- FactorGMTCast()$FactorDF
                FactorGrouping <- c("Sample Number", colnames(FactorDF))
                selectInput(inputId = "PCA_Label", label = "Label by Factor", choices = FactorGrouping, selected = "Sample Number", multiple = F)
                })
                        #' Render input that allows user to select X axis of PCA Biplot
                output$PCA_xcomp_UI <- renderUI({
                Prcomp_res <- PCA_Data()$Prcomp_res$x
                CompOptions <- colnames(Prcomp_res)
                selectInput(inputId = "PCA_xcomp", label = "X Axis component", choices = CompOptions, selected = CompOptions[1])
                })
                        #' Render input that allows user to select Y axis of PCA Biplot
                output$PCA_ycomp_UI <- renderUI({
                Prcomp_res <- PCA_Data()$Prcomp_res$x
                CompOptions <- colnames(Prcomp_res)
                selectInput(inputId = "PCA_ycomp", label = "Y Axis component", choices = CompOptions, selected = CompOptions[2])
                })
                        #' Render PCA Biplot
                output$PCA_BiPlot <- renderPlotly({
                shiny::req(input$PCA_Group, input$PCA_Label, input$PCA_xcomp, input$PCA_ycomp)
                        PCA_ResDF <- PCA_Data()$PCA_ResDF
                PCA_ResDF <- data.frame(PCA_ResDF)
                Prcomp_res <-  PCA_Data()$Prcomp_res
                FactorDF <- FactorGMTCast()$FactorDF
                        var_expl_x <- round(100 * Prcomp_res$sdev[as.numeric(gsub("[^0-9]", "", input$PCA_xcomp))]^2/sum(Prcomp_res$sdev^2), 1)
                var_expl_y <- round(100 * Prcomp_res$sdev[as.numeric(gsub("[^0-9]", "", input$PCA_ycomp))]^2/sum(Prcomp_res$sdev^2), 1)
                        labeltype <- input$PCA_Label
                if (labeltype != "Sample Number") {labels <- FactorDF[, labeltype]
                 } else { labels <- c(1:nrow(Prcomp_res$x))}
                        grouping <- input$PCA_Group
                        if(grouping == 'None'){
                    # plot without grouping variable
                    pc_plot_no_groups  <- ggplot(PCA_ResDF, aes_string(input$PCA_xcomp, input$PCA_ycomp)) +
                      geom_text(aes(label = labels),  size = 5) +
                      coord_equal() +
                      xlab(paste0(input$PCA_xcomp, " (", var_expl_x, "% explained variance)")) +
                      ylab(paste0(input$PCA_ycomp, " (", var_expl_y, "% explained variance)"))
                    pc_plot_no_groups
                          }  else {
                    #plot with grouping variable
                    PCA_ResDF$ExpVar <-  as.factor(FactorDF[, grouping])
                    pc_plot_groups  <- ggplot(PCA_ResDF, aes_string(input$PCA_xcomp, input$PCA_ycomp, fill = 'ExpVar', colour = 'ExpVar')) +
                      stat_ellipse(geom = "polygon", alpha = 0.1) +
                      geom_text(aes(label = labels),  size = 5) +
                      scale_colour_discrete(guide = FALSE) +
                      guides(fill = guide_legend(title = "groups")) +
                      theme(legend.position="top") +
                      coord_equal() +
                      xlab(paste0(input$PCA_xcomp, " (", var_expl_x, "% explained variance)")) +
                      ylab(paste0(input$PCA_ycomp, " (", var_expl_y, "% explained variance)"))
                    # the plot
                    #pc_plot_groups <- ggplotly(pc_plot_groups)
                    pc_plot_groups
                  }
                })
                        output$PCA_ScreePlot <- renderPlot({
                  type <- input$ScreePlotType
                  Prcomp_res <- PCA_Data()$Prcomp_res
                  ScreeData <- Prcomp_res$sdev^2
                          yvar <- switch(type, pev = ScreeData / sum(ScreeData), cev = cumsum(ScreeData) / sum(ScreeData))
                  yvar.lab <- switch(type, pev = 'proportion of explained variance', cev = 'cumulative proportion of explained variance')
                          ScreeDataDF <- data.frame(PC = 1:length(ScreeData), yvar = yvar)
                  ScreeDataDFLine <- ScreeDataDF
                          p <- ggplot(data = ScreeDataDF, aes(x = PC, y = yvar)) + xlab('principal component number') +
                    ylim(c(0,input$ScreeYMax)) + xlim(c(0,(input$nCompScree + 1))) +
                    ylab(yvar.lab) +  theme(text = element_text(size=12)) +
                    geom_bar(stat="identity", fill="steelblue") + geom_point() + geom_line()
                  p
                })
                    
                output$LoadingSelect_UI <- renderUI({
                Prcomp_res <- PCA_Data()$Prcomp_res$rotation
                CompOptions <- colnames(Prcomp_res)
                selectInput(inputId = "LoadingSelect", label = "Component to show Loadings for", choices = CompOptions, selected = CompOptions[1])
                })
                
                output$ShowNLoading_UI <- renderUI({
                Prcomp_res <- PCA_Data()$Prcomp_res$rotation
                nVars <- nrow(Prcomp_res)
                sliderInput(inputId = "ShowNloading", label = "Number of Loading Variables to Show", min = 1, max = 200, value = 5, step = 1)
                })
                        PCA_LoadingData <- reactive({
                shiny::req(input$ShowNloading, input$ShowNloading)
                        Prcomp_res <-  PCA_Data()$Prcomp_res
                aload <- abs(Prcomp_res$rotation)
                loadings <- sweep(aload, 2, colSums(aload), "/")
                        SelectedPC <- input$LoadingSelect
                PCLoading <- loadings[,SelectedPC]
                PCLoading <- sort(PCLoading, decreasing = T)
                        loadingsDF <- data.frame(PCLoading)
                loadingsDF$Var <- rownames(loadingsDF)
                loadingsDF
                })
                        output$PCA_LoadingPlot <- renderPlot({
                shiny::req(input$ShowNloading, input$ShowNloading)
                DF <- PCA_LoadingData()
                DF <- DF[1:(input$ShowNloading),]
                                p <- ggplot(data = DF, aes(x = reorder(Var, -PCLoading), y = PCLoading)) + xlab('Variable') +
                geom_bar(stat = "identity", fill="steelblue") +
                theme(text = element_text(size=12)) + theme(axis.text.x = element_text(angle = 90))
                p
                })
            
            # BioQC Analysis
            BioQCData <- eventReactive(input$PerformBioQCAnalysis, {
                message("Loading Expression Set for BioQC")
                #ExpressionMatrix <- readRDS("~/GeoWizard/TestObjects/ExpressionMatrix.GeneSymbol.rds")
                ExpressionMatrix <- GSEdata$ExpressionMatrix()
                BioQCData <- RunBioQC(ExpressionMatrix)
                message("BioQC Analysis Finished")
                BioQCData
            })
            
            output$BioQCPlotInput_UI <- renderUI({
                FactorDF <- ExperimentalDesign$ControlFactorDF()
                #FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
                FactorNames <- colnames(FactorDF)
                selectInput(inputId = "BioQCPlotInput" , label = "Cluster by Factor:", choices = FactorNames)
            })
            
            output$BioQCPlot <- renderPlotly({
            message("Loading Heatmap Data for Plotting")
            BioQCData <- BioQCData()
            message("Filter Number of signature to show by use input")
            BioQCDataFiltered <- tail(BioQCData, n = input$NumberOfHeatmapSignatures)
            
            # Load Factor DF
            #FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
            FactorDF <- ExperimentalDesign$ControlFactorDF()
            
            SelectedFactor <- factor(FactorDF[,input$BioQCPlotInput]) ; message("User selected factor for H.Clustering")
            nFactorLevels <- length(levels(SelectedFactor))
                    # Make HeatMap
            
            HeatMapBioQC <- try(
                heatmaply(
                x = BioQCDataFiltered,
                dendrogram = "column",
                k_col = nFactorLevels,
                col_side_colors = SelectedFactor))
                    if (class(HeatMapBioQC) == 'try-error') {
                stop("Please ensure you selected GeneSymbol Annotations on the previous tab and click Perform BioQC Analysis button again")
            } else { return(HeatMapBioQC)}
            })
            
            
            output$BioQProfileInput_UI <- renderUI({
                req(input$PerformBioQCAnalysis)
                BioQCData <- BioQCData()
                TissueProfile <- rownames(BioQCData)
                selectInput(inputId = "BioQProfileInput" , label = "Sample Profiles", choices = TissueProfile, selected = TissueProfile[1:3], multiple = T)
            })
            
            
            output$BioQCProfilePlot <- renderPlotly({
            req(input$BioQProfileInput)
            BioQCRes <- BioQCData()
            TissueInput <- input$BioQProfileInput
            p <- BioQCProfile(BioQCRes = BioQCRes, TissueSelection = TissueInput)
            ggplotly(p) %>% layout(legend = list(orientation = "h", y = 1.2, yanchor = "top"))
            })
              
            #'
            #'
            #'
            #'
            output$DownloadDataInfoBox <- renderValueBox({
            shiny::req( input$GsmTableSelect,input$GplTableSelect)
            GSE <- input$GsmTableSelect
            GPL  <- input$GplTableSelect
            valueBox(value = GSE, subtitle = GPL, icon = icon('check-circle'),color = "blue")
            })

            #'
            #'
            #'
            output$nGSESamples <- renderValueBox({
            shiny::req(input$GeneAnnotationType)
            message("rendering nSamples Info Box")
            ExpressionMatrix <- GSEdata$ExpressionMatrix()
            nSamples <- ncol(ExpressionMatrix)
            valueBox( nSamples, "Number of Samples in GSE", icon = icon("list"), color = "purple")
            })

            
            #'
            #'
            #'
            output$nGSEGenes <- renderValueBox({
            shiny::req(input$GeneAnnotationType)
            message("rendering nGenes Info Box")
            ExpressionMatrix <- GSEdata$ExpressionMatrix()
            nGenes <- nrow(ExpressionMatrix)
            valueBox(value=nGenes, subtitle="Number of Genes in GSE", icon=icon("vial"), color="yellow")
            })

            ############################################################### Expression Analysis
            
            ExpressionAnalysis <- reactiveValues()
            
            output$DiffExMethod_UI <- renderUI({
                DataSetType <- input$ExpressionDataType
                if (DataSetType == "RNAseq" | DataSetType == "ssRNAseq") { selectedMethod <- "DESeq2"
                } else { selectedMethod <- "Limma" }
                selectInput(inputId = "DiffExMethod", label = "Difference Expression Analysis Method",choices = c("EdgeR", "Limma", "DESeq2"), selected = selectedMethod)
            })


            ExpressionAnalysis$LimmaResults <- eventReactive(input$SubmitDEA, {
                # Functional Testing
                #ExpressionMatrix <- readRDS("~/GeoWizard/TestObjects/GSE69967_ExpressionMatrix.rds")
                #DesignMatrix <- readRDS("~/GeoWizard/TestObjects/GSE69967_DesignMatrix.rds")
                #ContrastMatrix <- readRDS("~/GeoWizard/TestObjects/GSE69967_ContrastMatrix.rds")
            
                message("Loading Objects from for limma analysis")
                ExpressionMatrix <- GSEdata$ExpressionMatrix()
                DesignMatrix <- ExperimentalDesign$DesignMatrix()
                ContrastMatrix <- ExperimentalDesign$Contrast()
                
                if(!is.null(ExpressionMatrix) & !is.null(DesignMatrix) &!is.null(ContrastMatrix)){
                message("Performing Limma DEA")
                res <- LimmaOutput(ExpressionMatrix,DesignMatrix,ContrastMatrix)}
                return(res)
           })
            
            
            ############ Volcano Plot
            
            #' Render the Pvalue threshold box for the volcano Plot
            #' 
            #' @todo Determine method for suggesting P value
            output$PValThres <- renderUI({
            numericInput(inputId = "PValThresInput", label = "Adjusted -log(pValue) Threshold", value = 15, min = 1, step = 0.5)
            })
                    output$LogFCThres <- renderUI({
            numericInput(inputId = "LogFCThresInput", label = "Log2FC Threshold", value = 1, min = 0, max = 5, step = 0.5)
            })
            
            output$SelectContrast_UI <- renderUI({
                shiny::req(input$DiffExMethod)
                LimmaTable <- ExpressionAnalysis$LimmaResults()
                selectInput(inputId = "Volcanoplot_SelectContrast", label = "Select Contrast", choices = unique(LimmaTable$Contrast))
            })
            
            
            #' Filtere Data by Use selected Contrast
            #' Then Filter Data according to Pvalue and Log input cutoff
            #' 
            #' @input PValThresInput
            #' @input LogFCThresInput
            #' @output Limma Table with new threshold factor column for coloring
            #' data points greater/lower than certain thresholds different colours
            ExpressionAnalysis$VolcanoPlotData <- reactive({
                LimmaTable <- ExpressionAnalysis$LimmaResults()
                LimmaTable <- as.data.frame(LimmaTable)
                
                selectedContrast <- input$Volcanoplot_SelectContrast
                LimmaTable <- LimmaTable %>% dplyr::filter(Contrast == selectedContrast) %>%
                mutate(adj.P.Val = p.adjust(P.Value, method = input$MultiTestCorr))
                
                pValueThresHold <- input$PValThresInput
                logFCThresHold <- input$LogFCThresInput
                LimmaTable <- LimmaTable %>%
                    mutate(LogThreshold = abs(logFC) > logFCThresHold) %>%
                    mutate(PThreshold = as.numeric(-log(LimmaTable$adj.P.Val) >= pValueThresHold)) %>%
                    mutate(Threshold = paste(LogThreshold, PThreshold))
                LimmaTable
            })
            
            ExpressionAnalysis$VolcanoPlotData <- reactive({
                LimmaTable <- ExpressionAnalysis$VolcanoPlotData()
                
                p <- ggplot(LimmaTable, aes(x = logFC, y = -log(adj.P.Val), color = factor(Threshold))) + geom_point() + theme_grey()
                    if (input$VolcanoPlot_showColor) {
                if (input$VolcanoPlot_ThemeSelect == "default") { p <- p }
                else if (input$VolcanoPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
                else if (input$VolcanoPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
                else if (input$VolcanoPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
                else if (input$VolcanoPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
                else if (input$VolcanoPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
                else if (input$VolcanoPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
                }
            
                if (length(BoxPlot_main) > 0) { p <- p + labs(title = VolcanoPlot_main)} else {p <- p + labs(title = "BoxPlot of Gene Series Data")}
                if (length(BoxPlot_xlab) > 0) { p <- p + labs(x = VolcanoPlot_xlab)}
                if (length(BoxPlot_ylab) > 0) { p <- p + labs(y = VolcanoPlot_ylab)}
                
                if(input$VolacanoPlot_LogLine) {p <- p + geom_vline(xintercept = c(logFCThresHold, -logFCThresHold), linetype="dashed", color="red", size=1.2)}
                if(input$VolacanoPlot_PvalLine) {p <- p + geom_hline(yintercept = pValueThresHold, linetype="dashed", color="red", size=1.2)}
            
                p <- p + theme(axis.text = element_text(size = 14, hjust = 1)) +
                    theme(axis.title = element_text(size = 14)) +
                    theme(legend.text=element_text(size=14))
            
                if (length(input$VolcanoPlot_HighlightGene) > 1) {
                    SelectedGenes <- input$VolcanoPlot_HighlightGene
                    pValueThresHold <- input$PValThresInput
                    logFCThresHold <- input$LogFCThresInput
                    
                    HighlightData <- LimmaTable %>% filter(gene %in% SelectedGenes)
                    p <- p + 
                        geom_point(data = HighlightData, aes(x = logFC, y = -log(adj.P.Val)), colour="red", size=5) +
                        geom_label(data=HighlightData,aes(x = logFC, y = -log(adj.P.Val), label=gene, size=14, vjust=1, colour = "black"))
                } else { p <- p }
            
                p <- p + scale_x_continuous(limits = c(-3, 3))
                p <- p + theme(legend.position = "bottom")
                p
                
            })
            
            output$VolcanoPlot <- renderPlot({
                ExpressionAnalysis$VolcanoPlotData()
            })
            
            output$VolcanoPlotly <- renderPlotly({
                p <- ExpressionAnalysis$VolcanoPlotData
                p <- ggplotly(p)
                p
            })
            
            output$VolcanoPlot_HighlightGene_UI <- renderUI({
            LimmaTable <- ExpressionAnalysis$LimmaResults()
            
            fluidRow(
            selectInput(inputId = "VolcanoPlot_HighlightGene", label = "Highlight Gene", choices = LimmaTable$gene, multiple = T),
            checkboxInput(inputId = "VolcanoPlot_PlotInteractive", label = "Interactive Plot"))
            })
            
            output$VolcanoPlot_TopTable <- renderDataTable({
            LimmaTable <- ExpressionAnalysis$LimmaResults()
            datatable(data = LimmaTable)
            DT::datatable(data = LimmaTable, rownames = TRUE,
                        class = 'compact', extensions = 'Buttons', 
                        options = list(scrollY = '500px', scrollX = T, paging = T, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')))
            })
            
            
            #' The Plot output is not hgard coded to allow changes to the plot height and width
            #' and to toggle the interactive plots
            #'
            #'
            output$VolcanoPlot_Output <- renderUI({
                plotOutput(outputId = "VolcanoPlot",height = input$VolcanoPlot_Height, width = input$VolcanoPlot_Width) %>% withSpinner(color = "#0dc5c1")
                plotOutput(outputId = "VolcanoPlotly",height = input$VolcanoPlot_Height, width = input$VolcanoPlot_Width) %>% withSpinner(color = "#0dc5c1")
                
            })
            
        
            ######### Hist
                 # output$HistFactorSelect <- renderUI({
         #   shiny::req(input$GsmTableSelect, input$DownloadGEOData)
         #   MeltedDF <- GSEdata$FactorGMTMelt()
         #   FactorOptions <- grep(pattern = "ExpVar[0-9]", x = colnames(MeltedDF), value = T)
         #   selectInput(inputId = "HistFactorSelectInput",
         #    label = "Fill by Factor",
         #    choices = FactorOptions,
         #    selected = FactorOptions[1])
         # })
         #
         # output$HistPlotGMT <- renderPlot({
         #   GMTHistPlot(GSEgmtDF = GSEdata$FactorGMTMelt(),
         #   HistPlotType = input$HistPlotType,
         #   PlotFactor = input$HistFactorSelectInput,
         #   SampleSize = input$HistSampleSize)
         # })
        

                         ############ MA Plot
        output$MALogFCThres <- renderUI({
              numericInput(inputId = "MALogFCThresInput",
                           label = "LogFC Threshold",
                           value = 1,
                           min = 0,
                           max = 5,
                           step = 0.5)
         })
        
        output$MAPlot <- renderPlot({
            logFCThresHold <- input$MALogFCThresInput
            LimmaTable <- ExpressionAnalysis$LimmaResults()
            LimmaTable <- as.data.frame(LimmaTable)
            LimmaTable <- LimmaTable %>% mutate(Threshold = abs(logFC) > logFCThresHold)
            ggplot(LimmaTable, aes(x = AveExpr, y = logFC, color = factor(Threshold))) + geom_point() + theme_grey()
        })
    
    
    
    


     ##################################################################################### Clustering

        
        output$HeatMapSelectContrast_UI <- renderUI({
                shiny::req(input$DiffExMethod)
                LimmaTable <- ExpressionAnalysis$LimmaResults()
                selectInput(inputId = "HeatMap_SelectContrast", label = "Select Contrast", choices = unique(LimmaTable$Contrast))
        })
        
        
        # #Import/Select Data ----
        HeatMapData <- reactiveValues()
                 
        HeatMapData$FactorGMT <- reactive({
            
        message("Generating HeatMapData FactorGMT")
        LimmaTable <- ExpressionAnalysis$LimmaResults()
    
        selectedContrast <- input$HeatMap_SelectContrast
        LimmaTable <- LimmaTable %>% dplyr::filter(Contrast == selectedContrast)
        nGenes <- input$HeatMap_nGenes

        TopGenes <- LimmaTable[1:nGenes,1]
        TopGenes[TopGenes == ""] <- NA
        TopGenes <- na.omit(TopGenes)

        #ExpressionMatrix <- readRDS("~/GeoWizard/TestObjects/GSE69967_ExpressionMatrix.rds")
        #FactorDF <- readRDS("~/GeoWizard/TestObjects/GSE69967_FactorDF.rds")
        FactorDF <- ExperimentalDesign$ControlFactorDF()
        FactorDF <- as.data.frame(FactorDF)
        
        
        FactorGMT <- GenFactorGMT(ExpressionMatrix = ExpressionMatrix, FactorDF = ControlFactorDF)
        View(FactorGMT)
        colnames(FactorGMT) <- make.names(colnames(FactorGMT), unique=TRUE)
        FactorGMT
    })



     #Color Pallete UI ----
     output$colUI<-renderUI({

       colSel='Vidiris'
       if(input$transform_fun=='cor') colSel='RdBu'
       if(input$transform_fun=='is.na10') colSel='grey.colors'

       selectizeInput(inputId ="pal", label ="Select Color Palette",
                      choices = c('Vidiris (Sequential)'="viridis",
                                  'Magma (Sequential)'="magma",
                                  'Plasma (Sequential)'="plasma",
                                  'Inferno (Sequential)'="inferno",
                                  'Magma (Sequential)'="magma",
                                  'Magma (Sequential)'="magma",

                                  'RdBu (Diverging)'="RdBu",
                                  'RdYlBu (Diverging)'="RdYlBu",
                                  'RdYlGn (Diverging)'="RdYlGn",
                                  'BrBG (Diverging)'="BrBG",
                                  'Spectral (Diverging)'="Spectral",

                                  'BuGn (Sequential)'='BuGn',
                                  'PuBuGn (Sequential)'='PuBuGn',
                                  'YlOrRd (Sequential)'='YlOrRd',
                                  'Heat (Sequential)'='heat.colors',
                                  'Grey (Sequential)'='grey.colors'),
                      selected=colSel)

     })

     #Manual Color Range UI ----
     output$colRng=renderUI({
       if(!is.null(HeatMapData$FactorGMT())) {
        rng=range(HeatMapData$FactorGMT(),na.rm = TRUE)
        }else{
            rng=range(mtcars) # TODO: this should probably be changed
        }
        # sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)
        n_data = nrow(HeatMapData$FactorGMT())

        min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
        min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
        min_value = ifelse(input$transform_fun=='cor',-1,rng[1])

        max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
        max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
        max_value = ifelse(input$transform_fun=='cor',1,rng[2])
        a_good_step = 0.1 # (max_range-min_range) / n_data

        list(
        numericInput("colorRng_min", "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
        numericInput("colorRng_max", "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
       )

     })


     #Building heatmaply ----
     interactiveHeatmap <- eventReactive(input$RunHeatMaply, {

       HeatMapData$FactorGMT <- HeatMapData$FactorGMT()
       ss_num =  sapply(HeatMapData$FactorGMT, is.numeric) # in order to only transform the numeric values

       if(input$transpose) HeatMapData$FactorGMT=t(HeatMapData$FactorGMT)
       if(input$transform_fun!='.'){
         if(input$transform_fun=='is.na10'){
           updateCheckboxInput(session = session,inputId = 'showColor',value = T)
           HeatMapData$FactorGMT[, ss_num]=is.na10(HeatMapData$FactorGMT[, ss_num])
         }
         if(input$transform_fun=='cor'){
           updateCheckboxInput(session = session,inputId = 'showColor',value = T)
           updateCheckboxInput(session = session,inputId = 'colRngAuto',value = F)
           HeatMapData$FactorGMT=cor(HeatMapData$FactorGMT[, ss_num],use = "pairwise.complete.obs")
         }
         if(input$transform_fun=='log') HeatMapData$FactorGMT[, ss_num]= apply(HeatMapData$FactorGMT[, ss_num],2,log)
         if(input$transform_fun=='sqrt') HeatMapData$FactorGMT[, ss_num]= apply(HeatMapData$FactorGMT[, ss_num],2,sqrt)
         if(input$transform_fun=='normalize') HeatMapData$FactorGMT=heatmaply::normalize(HeatMapData$FactorGMT)
         if(input$transform_fun=='scale') HeatMapData$FactorGMT[, ss_num] = scale(HeatMapData$FactorGMT[, ss_num])
         if(input$transform_fun=='percentize') HeatMapData$FactorGMT=heatmaply::percentize(HeatMapData$FactorGMT)
       }


       # if(!is.null(input$TopTable_true_search_columns))
       #   data.in=data.in[activeRows(input$TopTable_true_search_columns,data.in),]
       # if(input$colRngAuto){
       #   ColLimits=NULL
       # }else{
       #   ColLimits=c(input$colorRng_min, input$colorRng_max)
       # }

       distfun_row = function(x) dist(x, method = input$distFun_row)
       distfun_col =  function(x) dist(x, method = input$distFun_col)

       hclustfun_row = function(x) hclust(x, method = input$hclustFun_row)
       hclustfun_col = function(x) hclust(x, method = input$hclustFun_col)


       p <- heatmaply(HeatMapData$FactorGMT,
                      main = input$main,xlab = input$xlab,ylab = input$ylab,
                      row_text_angle = input$row_text_angle,
                      column_text_angle = input$column_text_angle,
                      dendrogram = input$dendrogram,
                      branches_lwd = input$branches_lwd,
                      seriate = input$seriation,
                      colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
                      distfun_row =  distfun_row,
                      hclustfun_row = hclustfun_row,
                      distfun_col = distfun_col,
                      hclustfun_col = hclustfun_col,
                      k_col = input$c,
                      k_row = input$r,
                      limits = ColLimits) %>%
         layout(margin = list(l = input$l, b = input$b, r='0px'))

       p$elementId <- NULL

       p

     })

     #Render Plot ----
     output$HeatMapPlotly <- renderPlotly({
         input$RunHeatMaply
         interactiveHeatmap()
       })




     #Render Data Table ----
     output$RenderTopTable <- renderUI({
       if(is.null(ExpressionAnalysis$LimmaResults()) == 0){
       return("No data to show")
       } else {
       tableOutput("TopTable")
       }
       })

     output$TopTable <- renderDataTable({
       datatable(ExpressionAnalysis$LimmaResults(), server = T, filter='top',
        extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'),
        options = list(
        dom = 't',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        fixedColumns = TRUE,
        fixedHeader = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE
        ))

     })
    
    
     
     
     ########################{ Disconnect from SQLite Server on Exit

     session$onSessionEnded(function() {
          message("Disconnecting from GEOmetadb.sqlite")
          con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
          dbDisconnect(con)
     })


     

     
     
     

     
     

     
     
     
     
}

