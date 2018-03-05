server <- function(input, output, session) {
    GeoSearchResults <- reactiveValues()
  
    #'
    #'
    #'
    #'
    output$MolSelectFromLibrary <- renderUI({
        if (input$MolSelectFilter == "TextInput") {
            SearchInputUI <- textAreaInput(inputId = "MolSelectText", 
                label = "Comma Separated Text input", 
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
        if (isTruthy(input$MolSelectText)) {
            message("Text Query Detected")
            MolQueryText <- gsub(pattern = "\n|\t", replacement = "", x = input$MolSelectText)
            MolQuery <- unlist(str_split(MolQueryText, pattern = ","))
        } else if (isTruthy(input$MolSelectInput)){
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
        #GseDescDF <- as.data.frame(GseDescDF) 
        DT::datatable(GseDescDF, rownames = FALSE, class = "compact",
            options = list( autoWidth = FALSE, scrollY = '350px', paging = FALSE, order = list(list(6,'desc')))) %>%
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
    
    output$nStudiesPlotTaxon <- renderPlot({
        shiny::req(input$GseSummaryData_rows_all)
        input <- 'taxon'
        titleText <- TitleCase(input)
        plotdata <- GeoSearchResults$GseSummaryTableData
        plotdata <- plotdata()
          
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
    })
    
    output$nStudiesPlotGdsType <- renderPlot({
        shiny::req(input$GseSummaryData_rows_all)
        input <- 'gdsType'
        titleText <- TitleCase(input)
        plotdata <- GeoSearchResults$GseSummaryTableData()
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
        #shiny::req(input$KeepForExpVarAsign)
        GSEChoices <- input$KeepForExpVarAsign
        selectInput(inputId = "GsmTableSelect", label = "Select Dataset", choices = GSEChoices)
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

    
    output$infobox_selectedGSE <- renderUI({
        shiny::req(input$KeepForExpVarAsign, input$GsmTableSelect, input$GplTableSelect)
        FilteredTable <-SQLSearchData$SelectedGSETable()
        
        SpeciesInfo <- FilteredTable %>% select(taxon) %>% as.character
        SamplesInfo <- FilteredTable %>% select(n_samples) %>% as.character
        GdsInfo <- FilteredTable <- FilteredTable %>% select(gdsType) %>% as.character
        
        fluidRow(
        column(4, h4(strong("n samples:")), h5(SamplesInfo)),
        column(4, h4(strong("species:")), h5(SpeciesInfo)),
        column(4, h4(strong("gdsType:")), h5(GdsInfo))
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
        shiny::req(input$KeepForExpVarAsign, input$GsmTableSelect, input$GplTableSelect)
        
        SqlQueryResDF <- SQLSearchData$FilteredResultDF()
        SqlQueryResDF <- SqlQueryResDF %>% select(-one_of(c("series_id","taxon","keyword","gpl","gsm")))
        message("Making SQL Summary Table")
        DT::datatable(data = SqlQueryResDF , rownames = FALSE, class = 'row-border', 
            options = list(scrollY = '400px', dom = 't', paging = FALSE, autoWidth = TRUE,scrollX = T)) %>%
            formatStyle(names(SqlQueryResDF), color = 'black', backgroundColor = 'white', fontWeight = 'bold')
     })

    #################{ Classify ExpVars
    ExperimentalDesign <- reactiveValues()
     
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
    shiny::req(input$GseGsmTable_rows_all, input$KeepForExpVarAsign, input$GsmTableSelect, input$GplTableSelect)
        input$GsmTableSelect
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
        checkboxOptions <- checkboxGroupInput(
            inputId = "UsefulColumnsCheckbox",
            label = "Factors that describe detected experimental design cohorts",
            choices = colnames(ExpFactorDF),
            selected = names(RecVars),
            inline = T)
        fluidRow(column(12,checkboxOptions))
        })
    
    ########################{ View Current Factor Col Selection
    #'
    #'
    #'
    #'
    
    
    ########################{ Render Inputs to Filter Factors
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
    
    ########################{ Take Levels from inputs and determine rows of DF
    #' @input
    #' @req
    #' @output
    ExperimentalDesign$FilteredFactorDF <- reactive({
        #shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
        message("Running RowsToKeep for Factor Level Filtering")
        FactorDF <- ExperimentalDesign$ExpFactorDF()
        FactorDF <- FactorDF %>% select(one_of(input$UsefulColumnsCheckbox))
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
        
        ExperimentalDesign$RowsToKeep <- RowsToKeep
        FilteredFactorDF <- FactorDF[RowsToKeep,]
        
        FilteredFactorDF <- as.data.frame(FilteredFactorDF)
        if (ncol(FilteredFactorDF) < 2) {
            colnames(FilteredFactorDF) <- "ExpVar1"}
       FilteredFactorDF
       })
     
    #######################{ Output Table with Factor Selection
      
    ##### Unique Factor Table
    # output$ImportantFactorTable <- DT::renderDataTable({
    #     #shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
    #     ExpFactorDF <- ExperimentalDesign$FilteredFactorDF()
    #     
    #     DT::datatable(data = ExpFactorDF, extensions = 'ColReorder', class = 'compact',
    #         options = list( dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px',paging = FALSE,
    #         columnDefs = list(list(width = '150px', targets = c(1:ncol(ExpFactorDF)))),
    #         colReorder = list(realtime = FALSE))) %>%
    #         formatStyle(names(ExpFactorDF), color = 'black', fontWeight = 'bold')
    #   })
      
    ##### Full Factor Table
    output$FullFactorTable <- DT::renderDataTable({
        message("Rendering Factor Data Table")
        input$WhereVarData
        input$UsefulColumnsCheckbox
        input$RefreshFullFactorTable
        FactorDF <- ExperimentalDesign$FilteredFactorDF()
        
        DT::datatable(data = FactorDF, extensions = 'ColReorder',class = 'compact',
            options = list(dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px', paging = FALSE,
                columnDefs = list(list(width = '150px', targets = c(1:ncol(FactorDF)))),
                colReorder = list(realtime = FALSE))) %>%
            formatStyle(names(FactorDF),color = 'black',fontWeight = 'bold')
        })
    ##### Excluded Factor Table
        output$ExcludedFactorTable <- DT::renderDataTable({
        message("Rendering Excluded Factor Data Table")
        input$WhereVarData
        input$UsefulColumnsCheckbox
        input$RefreshExcludedFactorTable
        
        FactorDF <- ExperimentalDesign$FilteredFactorDF()
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
    output$DesignMat_SummaryTable <- DT::renderDataTable({
        shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
        ExpFactorDF <- ExperimentalDesign$FilteredFactorDF()
        ExpFactorDF <- data.frame(ExpFactorDF)
        
        DT::datatable(data = ExpFactorDF, extensions = 'ColReorder', class = 'compact',
            options = list( dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px',paging = FALSE,
            columnDefs = list(list(width = '150px', targets = c(1:ncol(ExpFactorDF)))),
            colReorder = list(realtime = FALSE))) %>%
            formatStyle(names(ExpFactorDF), color = 'black', fontWeight = 'bold')
      })
     
     
      ########################{ Annotate the data table
     
     output$RearrangeLevels <- renderUI({
         FactorDF <- ExperimentalDesign$FilteredFactorDF()
         NamesIndex <- colnames(FactorDF)
         lapply(NamesIndex, function(ColName) {
             ColLevels <- factor(FactorDF[, ColName])
             inputName <- paste("Levels_", ColName, sep = "")
             selectInput(
                 inputId = inputName,
                 label = paste("Selected control level for", ColName),
                 choices = c(levels(ColLevels), "none")
             )
         })
     })
      
     ExperimentalDesign$ControlFactorDF <- reactive({
         shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
         input$WhereVarData
         DesignDF <- ExperimentalDesign$FilteredFactorDF()
         
         NamesIndex <- colnames(DesignDF)
         ResDF <- lapply(NamesIndex, function(ColName) {
             ResultVector <- DesignDF[, ColName]
             inputName <- paste("Levels_", ColName, sep = "")
             
             InputControlLevel <- input[[inputName]]
             OtherLevels <- levels(factor(ResultVector))
             levels(ResultVector) <-
                 unique(c(InputControlLevel, OtherLevels))
             ResultVector
         })
         
         names(ResDF) <- NamesIndex
         ResDF <- data.frame(do.call(cbind, ResDF))
         ResDF <- ResDF %>% dplyr::arrange_all()
         ResDF
     })
     
      ##################### Formula Input
     
    ExperimentalDesign$DesignMatrixInput <- eventReactive(input$SubmitFormula, {
        DesignDF <- ExperimentalDesign$ControlFactorDF()
        Designformula <- input$formulaInputDesign
            
        DesignExpression <- try(as.formula(Designformula))
        if (class(DesignExpression)[1] == "try-error") { stop("Caught an error trying to make Design Matrix") 
        } else { DesignMatrix <- model.matrix(as.formula(DesignExpression), DesignDF)}
        DesignMatrix
        })
    
    output$DesignMatrixRename_UI <- renderUI({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        lapply(colnamesIndex, function(FactorNameIndex){
            origtext <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            checkinpuLabel <- paste("Rename column",FactorNameIndex, "-", substr(origtext, start=1, stop=30))

            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            textInputLabel <- paste("new column", FactorNameIndex, "name:")
            textInputplaceholder = str_split(origtext, pattern = " ", simplify = T)
            
            if(length(textInputplaceholder)>=3) {textInputplaceholder <- paste(textInputplaceholder[1,1:3], collapse = "_")}
            textInputplaceholder
            
            fluidRow(
            column(12, checkboxInput(checkinputID, checkinpuLabel)),
            conditionalPanel(paste("input.",checkinputID,"==1", sep = ""), 
            column(12, textInput(textInputID ,textInputLabel,textInputplaceholder))))
        })
    })
    
    ExperimentalDesign$DesignMatrix <- reactive({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        NewColNames <- 
        lapply(colnamesIndex, function(FactorNameIndex){
            colnameText <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            
            if (input[[checkinputID]] == T) {
            InputName <- input[[textInputID]]
            InputName <- gsub("[[:space:]]", "", InputName)
            colnameText <- InputName
            } else {colnameText}
        })
        colnames(DesignMatrix) <- unlist(NewColNames)
        DesignMatrix
    })
    
      # output$TextAhead <- renderUI({
      #   
      #   shiny::req(input$formulaInputDesign, input$UsefulColumnsCheckbox, input$WhereVarData)
      #   DesignDF <- ExperimentalDesign$ControlFactorDF()
      #   DFname <- colnames(DesignDF)
      #   DFlevs <- as.character(
      #      lapply(DesignDF, function(x){
      #      FactorLevels <- levels(x)
      #      nLevels <- length(FactorLevels)
      #      paste(FactorLevels[1],FactorLevels[nLevels],collapse = " ")}))
      # 
      #   typeaheadOptions <- list(
      #      id="thti", 
      #      placeholder="~ ExpVar1 + ExpVar2",
      #      local=data.frame(name=paste("~ ", DFname),
      #                       info= paste("levels:", DFlevs),
      #                       valueKey = "name",
      #                       tokens=c(1:length(DFname))
      #    ))
      #   
      #   do.call(textInput.typeahead, typeaheadOptions)
      # 
      # })
     
      observeEvent(input$SubmitFormula, {
          shiny::req(input$SubmitFormula,
                     input$UsefulColumnsCheckbox,
                     input$WhereVarData)
          output$CustomExpressionTable <- DT::renderDataTable({
              DesignMatrix <- ExperimentalDesign$DesignMatrix()
              
              DT::datatable(
                  data = DesignMatrix,
                  rownames = TRUE,
                  class = 'compact',
                  extensions = 'Buttons',
                  options = list(
                      scrollY = '300px',
                      paging = FALSE,
                      dom = 'Bt',
                      #dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel')
                  )
              )
          })
      })
     
    output$ExperimentalBlocksPlot <- renderPlot({
        shiny::req(input$SubmitFormula, input$UsefulColumnsCheckbox, input$WhereVarData)
        DesignDF <- ExperimentalDesign$ControlFactorDF()
        
        DesignExpression <- try(as.formula(input$formulaInputDesign))
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
     
    ######################## Expression Analysis
    
    ###### Download the Data
    GSEdata <- reactiveValues()
      
    GSEdata$GSEeset <- eventReactive(input$DownloadGEOData, {
        shiny::req(input$GsmTableSelect, input$DownloadGEOData)
        shinyjs::show("GMTTableDiv")
        
        GSE <- input$GsmTableSelect
        GPL <- input$GplTableSelect
        message(paste("Downloading", GSE, "Data from GEO"))
        GSEeset <- LoadGEOFiles(GSE, GPL, GeoRepoPath = "~/GeoWizard/GEORepo")
        GSEeset
    })
      
    output$GeneAnnotationTypeUI <- renderUI({
        GSEeset <- GSEdata$GSEeset()
        FeatureData <- fData(GSEeset)
        if (length(FeatureData) == 0) {
        wellPanel(
            h4(icon("exclamation-triangle"),"no feature data included in eset"),
            "You can retart the application or download the raw files and process them locally")
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
      
      
      GSEdata$ExpressionMatrix <- reactive({
        shiny::req(input$GsmTableSelect)
        GSEeset <- GSEdata$GSEeset()
        FeatureData <- try(fData(GSEeset))
        message("Loading ExpressionMatrix")
        ExpressionMatrix <- exprs(GSEeset)
        
        if (length(FeatureData) == 0 | class(FeatureData) == "try-error") {
        return(ExpressionMatrix)
        } else {
        rownames(ExpressionMatrix) <- make.names(FeatureData[,input$GeneAnnotationType])
        return(ExpressionMatrix)
        }
      })
     
      #### Column 1 - GMT File Tab
      output$GMTFileTable <- DT::renderDataTable({
          ExpressionMatrix <- GSEdata$ExpressionMatrix()
          ExpressionMatrix <- as.data.frame(ExpressionMatrix)
          DT::datatable(
              data = ExpressionMatrix,
              rownames = TRUE,
              class = 'compact',
              extensions = 'Buttons',
              options = list(
                  scrollX = T,
                  scrollY = '300px',
                  paging = T,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
              )
          )
      })
      
      
      GSEdata$FactorGMT <- reactive({
          ControlFactorDF <- ExperimentalDesign$ControlFactorDF()
          ExpressionMatrix <- GSEdata$ExpressionMatrix()
          message("Generating FactorGMT")
          FactorGMT <-
              GenFactorGMT(ExpressionMatrix = ExpressionMatrix, FactorDF = ControlFactorDF)
          FactorGMT
      })
     
      GSEdata$FactorGMTMelt <- reactive({
          shiny::req(input$GsmTableSelect)
          FactorGMT <- GSEdata$FactorGMT()
          #FactorGMT <- FactorGMT[input$GMTFileTable_rows_all,]
          if (is.data.frame(FactorGMT)) {
              message("Melting FactorGMT for plotting")
              FactorGMTMelt <- melt(FactorGMT)
          } else {
              stop("Factor GMT File not loaded properly")
          }
          FactorGMTMelt
      })
      
      output$RawDataQC <- renderDataTable({
          if (input$RawDataTableMelt == "GMT") {
              TableData <- GSEdata$ExpressionMatrix()
          } else if (input$RawDataTableMelt == "FactorGMTMelt") {
              TableData <- GSEdata$FactorGMTMelt()
          } else {
              stop("Data not loaded properly")
          }
          
          DT::datatable(
              data = as.data.frame(TableData),
              rownames = TRUE,
              class = 'compact',
              extensions = 'Buttons',
              options = list(
                  scrollX = F,
                  scrollY = '300px',
                  paging = T,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
              )
          )
      })
     
      ######### Box Plot
      output$BoxFactorSelect <- renderUI({
          shiny::req(shiny::req(input$GsmTableSelect))
          
          FactorGMTMelt <- GSEdata$FactorGMTMelt()
          FactorOptions <-
              grep(pattern = "ExpVar",
                   x = colnames(FactorGMTMelt),
                   value = T)
          selectInput(
              inputId = "BoxFactorSelectInput",
              label = "Fill by Factor",
              choices = FactorOptions,
              selected = FactorOptions[1]
          )
      })
     
      output$BoxPlotly <- renderPlotly({
        shiny::req(input$BoxFactorSelectInput)
        
        FactorGMTMelt = GSEdata$FactorGMTMelt()
     
        if (input$BoxPlot_IndpVar == "Sample") { 
          if (input$BoxPlot_PlotBy == "Overall Distribution") { 
            GeneSample <- sample(x = FactorGMTMelt$GSM, size = input$BoxPlot_nGenes)
            FactorGMTMelt <- FactorGMTMelt %>% filter(GSM %in% GeneSample)
            AesX <- FactorGMTMelt$GSM
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            xlabtext <- "GSMs in Dataset"
            legPos <- "top"
            
          } else if (input$BoxPlot_PlotBy == "Factor Distribution") {;message("Factor")
            AesX <- FactorGMTMelt[,input$BoxFactorSelectInput]
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            xlabtext <- "Experimental Factors"
            legPos <- "top"
          }
          
        } else if (input$BoxPlot_IndpVar == "Gene") {
          GeneSample <- sample(x = FactorGMTMelt$variable, size = input$BoxPlot_nGenes)
          FactorGMTMelt <- FactorGMTMelt %>% filter(variable %in% GeneSample)
          
          if (input$BoxPlot_PlotBy == "Overall Distribution") {
            AesX <- FactorGMTMelt$variable
            FactorGMTMelt <- FactorGMTMelt
            AesFill <- "red"
            xlabtext <- "Assayed Genes"
            legPos <- "none"
            
          } else if (input$BoxPlot_PlotBy == "Factor Distribution") {
            AesX <- FactorGMTMelt$variable
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            xlabtext <- "Assayed Genes"
            legPos <- "top"
          }
        }
        
        p <- ggplot(data = FactorGMTMelt, aes(y = FactorGMTMelt$value, x = AesX, fill = AesFill)) +
             theme(legend.position = legPos) +  
             ylab(label = "Expression Level") +
             xlab(label = xlabtext) +
             guides(fill=guide_legend(title="Experimental Factor Groups")) +
             theme(axis.text.x = element_text(angle = 90)) + 
             theme(axis.text = element_text(size = 14)) +
             theme(axis.title = element_text(size = 14)) 
        
        if (input$BoxPlot_Type == "Boxplot") { p <- p + geom_boxplot()
        } else if (input$BoxPlot_Type == "Violin Plot") {p <- p + geom_violin()
        } else if (input$BoxPlot_Type == "Line Plot") { p <- p # bean plot code
        }
        
        if (input$BoxPlot_showData==1) {
          JitterWidth <- input$BoxPlot_JitterWidth
          if (input$BoxPlot_showDataOption == "jitter") { p <- p + geom_jitter(width = JitterWidth) 
          } else if(input$BoxPlot_showDataOption == "quasirandom"){ p <- p + geom_quasirandom(width = JitterWidth)
          } else if(input$BoxPlot_showDataOption == "beeswarm"){ p <- p + geom_beeswarm(width = JitterWidth)
          } else if(input$BoxPlot_showDataOption == "tukey"){ p <- p + geom_quasirandom(width = JitterWidth, method = "tukey")
          } else if(input$BoxPlot_showDataOption == "frowney"){ p <- p + geom_quasirandom(width = JitterWidth, method = "frowney")
          } else if(input$BoxPlot_showDataOption == "smiley"){ p <- p + geom_quasirandom(width = JitterWidth, method = "smiley")
          } else { NULL }
        }
        
        
        if (input$BoxPlot_PlotAxisFlip==1) { p <- p + coord_flip()}
        if (length(input$BoxPlot_main) > 0) { p <- p + labs(title = input$BoxPlot_main)}
        if (length(input$BoxPlot_xlab) > 0) { p <- p + labs(x = input$BoxPlot_xlab)}
        if (length(input$BoxPlot_ylab) > 0) { p <- p + labs(y = input$BoxPlot_ylab)}
        
        #sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180)
        #sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
        
        p <- ggplotly(p)
        p
      })
      
      output$BoxPlotUI <- renderUI({
        if(input$BoxPlot_showPlotSize){ 
          plotHeight <- input$BoxPlot_Height
          plotWidth <- input$BoxPlot_Width
        } else { 
          plotHeight <- 600
          plotWidth <- 800
        }
        plotlyOutput(outputId = "BoxPlotly",height = plotHeight, width = plotWidth) %>% withSpinner(color = "#0dc5c1")
        })
     
      ## *** Download EPS file ***
      output$downloadPlotEPS <- downloadHandler(
        filename <- function() { paste('Boxplot.eps') },
        content <- function(file) {
          postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = input$myWidth/72, height = input$myHeight/72)
          ## ---------------
          generateBoxPlot(dataM())
          ## ---------------
          dev.off()
        },
        contentType = 'application/postscript'
      )
      ## *** Download PDF file ***
      output$downloadPlotPDF <- downloadHandler(
        filename <- function() { paste('Boxplot.pdf') },
        content <- function(file) {
          pdf(file, width = input$myWidth/72, height = input$myHeight/72)
          ## ---------------
          generateBoxPlot(dataM())
          ## ---------------
          dev.off()
        },
        contentType = 'application/pdf' # MIME type of the image
      )
      ## *** Download SVG file ***
      output$downloadPlotSVG <- downloadHandler(
        filename <- function() { paste('Boxplot.svg') },
        content <- function(file) {
          svg(file, width = input$myWidth/72, height = input$myHeight/72)
          ## ---------------
          generateBoxPlot(dataM())
          ## ---------------
          dev.off()
        },
        contentType = 'image/svg'
      )

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
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

    # BioQC Analysis
     observeEvent(input$PerformBioQCAnalysis, {
     output$BioQCPlot <- renderPlot({
          message("Loading Expression Set for BioQC")
          GeneSymbolGSEeset <- GSEdata$GSEeset()
          GeneSymbolGSEeset <- ConvertGSEAnnotations(GSEeset = GeneSymbolGSEeset, AnnotationType = input$GeneAnnotationType)
          RunBioQC(GeneSymbolGSEeset)
          })
     })

    # PCA Plot
     DataPCA <- reactive({
       GSEeset <- GSEdata$GSEeset()
       ArrayData <- exprs(GSEeset)
       pca_output <- prcomp(na.omit(the_data), center = T, scale. = T)
       
     })
     
     output$PCA_xcomp <- renderUI({
       
       
       # drop down selection
       selectInput(inputId = "the_pcs_to_plot_x", 
                   label = "X axis:",
                   choices= colnames(pca_output), 
                   selected = 'PC1')
     })

     output$PCA <- renderPlot({
       GSEeset <- GSEdata$GSEeset()
       



       ListPlotPCA <- PlotPCA(ArrayData = ArrayData)

       DataPCA$CA <- ListPlotPCA$CA
       DataPCA$Scree <- ListPlotPCA$Scree
       DataPCA$Cont <- ListPlotPCA$Cont

       ListPlotPCA$PCA
       })

     output$CA <- renderPlot({ DataPCA$CA })
     output$Scree <- renderPlot({ DataPCA$Scree })
     output$Cont <- renderPlot({ DataPCA$Cont })

    #  
    #  ###################################################################### Expression Analysis
    #  ExpressionAnalysis <- reactiveValues()
    #  
    #  ExpressionAnalysis$LimmaResults <- reactive({
    #    GSEeset <- GSEdata$GSEeset() #Expression Set
    #    DesignMatrix <- ExperimentalDesign$DesignMatrix() #Matrix
    #    
    #    if(!is.null(GSEeset) & !is.null(DesignMatrix)){
    #      ArrayData <- exprs(GSEeset) #Matrix
    #      DesignMatrix <- DesignMatrix
    #      LimmaOutput(ArrayData,DesignMatrix)
    #      message("Performing Limma DEA")
    #    } else {
    #      message("GSEeset not Loaded")
    #      NULL
    #      }
    #    
    #    
    #    })
    #  
    #  ############ Volcano Plot
    # 
    #  output$PValThres <- renderUI({
    #       numericInput(inputId = "PValThresInput",
    #                    label = "pValue Threshold",
    #                    value = 2,
    #                    min = 1,
    #                    step = 0.5)
    #  })
    # 
    #  output$LogFCThres <- renderUI({
    #       numericInput(inputId = "LogFCThresInput",
    #                    label = "LogFC Threshold",
    #                    value = 1,
    #                    min = 0,
    #                    max = 5,
    #                    step = 0.5)
    #  })
    # 
    #  output$VolcanoPlot <- renderPlot({
    #    shiny::req(input$SubmitFormula)
    #    
    #    pValueThresHold <- input$PValThresInput
    #    logFCThresHold <- input$LogFCThresInput
    #      
    #    LimmaTable <- ExpressionAnalysis$LimmaResults()
    #    LimmaTable <- as.data.frame(LimmaTable)
    #    
    #    LimmaTable <- LimmaTable %>%
    #     mutate(Threshold = abs(logFC) > logFCThresHold) %>%
    #       mutate(Threshold = as.numeric(Threshold)) %>%
    #         mutate(Threshold = Threshold + as.numeric(-log(LimmaTable$adj.P.Val) >= pValueThresHold))
    #      
    #    ggplot(LimmaTable, aes(x = logFC, y = -log(adj.P.Val), color = factor(Threshold > 1))) + geom_point() + theme_grey() + facet_wrap(~ExpVar)
    #   
    # 
    #    })
    #  
    #  
    #  ############ MA Plot
    #  
    #  output$MALogFCThres <- renderUI({
    #       numericInput(inputId = "MALogFCThresInput",
    #                    label = "LogFC Threshold",
    #                    value = 1,
    #                    min = 0,
    #                    max = 5,
    #                    step = 0.5)
    #  })
    #  
    #  output$MAPlot <- renderPlot({
    #       logFCThresHold <- input$MALogFCThresInput
    # 
    #       LimmaTable <- ExpressionAnalysis$LimmaResults()
    #       LimmaTable <- as.data.frame(LimmaTable)
    #       LimmaTable <- LimmaTable %>% mutate(Threshold = abs(logFC) > logFCThresHold)
    # 
    #       ggplot(LimmaTable, aes(x = AveExpr, y = logFC, color = factor(Threshold))) +
    #            geom_point() +
    #            theme_grey()
    #       
    #       })
    #  
    #  ##################################################################################### Clustering
    #  
    # # #Import/Select Data ----
    #  HeatMapData <- reactiveValues()
    # 
    #  HeatMapData$FactorGMT <- reactive({
    #    shiny::req(input$formulaInputDesign, input$SubmitFormula)
    # 
    #    LimmaTable <- ExpressionAnalysis$LimmaResults()
    #    LimmaTable <- LimmaTable %>% arrange_(input$TopTableFilter)
    #    
    #    nGenes <- input$nGenes
    #    
    #    TopGenes <- LimmaTable[1:nGenes,1]
    #    TopGenes[TopGenes == ""] <- NA
    #    TopGenes <- na.omit(TopGenes)
    #    
    #    FactorDF <- ExperimentalDesign$ControlFactorDF()
    #    FactorDF <- as.data.frame(FactorDF)
    #    
    #    GSEeset <- GSEdata$GSEeset()
    #    FactorGMT <- GenFactorGMT(GSEeset, FactorDF)
    #    colnames(FactorGMT) <- make.names(colnames(FactorGMT), unique=TRUE)
    #    
    #    ColumnsToKeep <- colnames(FactorGMT)
    #    ColumnsToKeep <- grep(pattern = "GSM|ExpVar",x = ColumnsToKeep, value = T)
    #    ColumnsToKeep <- c(ColumnsToKeep, TopGenes)
    #    
    #    FactorGMT <- FactorGMT %>% select(one_of(ColumnsToKeep))
    #    
    # })
    # 
    #  
    #  
    #  #Color Pallete UI ----
    #  output$colUI<-renderUI({
    #    
    #    colSel='Vidiris'
    #    if(input$transform_fun=='cor') colSel='RdBu'
    #    if(input$transform_fun=='is.na10') colSel='grey.colors'
    #    
    #    selectizeInput(inputId ="pal", label ="Select Color Palette",
    #                   choices = c('Vidiris (Sequential)'="viridis",
    #                               'Magma (Sequential)'="magma",
    #                               'Plasma (Sequential)'="plasma",
    #                               'Inferno (Sequential)'="inferno",
    #                               'Magma (Sequential)'="magma",
    #                               'Magma (Sequential)'="magma",
    #                               
    #                               'RdBu (Diverging)'="RdBu",
    #                               'RdYlBu (Diverging)'="RdYlBu",
    #                               'RdYlGn (Diverging)'="RdYlGn",
    #                               'BrBG (Diverging)'="BrBG",
    #                               'Spectral (Diverging)'="Spectral",
    #                               
    #                               'BuGn (Sequential)'='BuGn',
    #                               'PuBuGn (Sequential)'='PuBuGn',
    #                               'YlOrRd (Sequential)'='YlOrRd',
    #                               'Heat (Sequential)'='heat.colors',
    #                               'Grey (Sequential)'='grey.colors'),
    #                   selected=colSel)
    #    
    #  })
    #  
    #  #Manual Color Range UI ----
    #  output$colRng=renderUI({
    #    if(!is.null(HeatMapData$FactorGMT())) {
    #      rng=range(HeatMapData$FactorGMT(),na.rm = TRUE)
    #    }else{
    #      rng=range(mtcars) # TODO: this should probably be changed
    #    }
    #    # sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)  
    #    n_data = nrow(HeatMapData$FactorGMT())
    #    
    #    min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
    #    min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
    #    min_value = ifelse(input$transform_fun=='cor',-1,rng[1])
    #    
    #    max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
    #    max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
    #    max_value = ifelse(input$transform_fun=='cor',1,rng[2])
    #    
    #    a_good_step = 0.1 # (max_range-min_range) / n_data
    #    
    #    list(
    #      numericInput("colorRng_min", "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
    #      numericInput("colorRng_max", "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
    #    )
    #    
    #  })
    #  
    #  
    #  #Building heatmaply ----
    #  interactiveHeatmap<-observeEvent(input$SubmitDEA, {
    #    
    #    HeatMapData$FactorGMT <- HeatMapData$FactorGMT()
    #    ss_num =  sapply(HeatMapData$FactorGMT, is.numeric) # in order to only transform the numeric values
    #    
    #    if(input$transpose) HeatMapData$FactorGMT=t(HeatMapData$FactorGMT)
    #    if(input$transform_fun!='.'){
    #      if(input$transform_fun=='is.na10'){
    #        updateCheckboxInput(session = session,inputId = 'showColor',value = T)
    #        HeatMapData$FactorGMT[, ss_num]=is.na10(HeatMapData$FactorGMT[, ss_num])
    #      } 
    #      if(input$transform_fun=='cor'){
    #        updateCheckboxInput(session = session,inputId = 'showColor',value = T)
    #        updateCheckboxInput(session = session,inputId = 'colRngAuto',value = F)
    #        HeatMapData$FactorGMT=cor(HeatMapData$FactorGMT[, ss_num],use = "pairwise.complete.obs")
    #      }
    #      if(input$transform_fun=='log') HeatMapData$FactorGMT[, ss_num]= apply(HeatMapData$FactorGMT[, ss_num],2,log)
    #      if(input$transform_fun=='sqrt') HeatMapData$FactorGMT[, ss_num]= apply(HeatMapData$FactorGMT[, ss_num],2,sqrt) 
    #      if(input$transform_fun=='normalize') HeatMapData$FactorGMT=heatmaply::normalize(HeatMapData$FactorGMT)
    #      if(input$transform_fun=='scale') HeatMapData$FactorGMT[, ss_num] = scale(HeatMapData$FactorGMT[, ss_num])
    #      if(input$transform_fun=='percentize') HeatMapData$FactorGMT=heatmaply::percentize(HeatMapData$FactorGMT)
    #    } 
    #    
    #    
    #    # if(!is.null(input$TopTable_true_search_columns)) 
    #    #   data.in=data.in[activeRows(input$TopTable_true_search_columns,data.in),]
    #    # if(input$colRngAuto){
    #    #   ColLimits=NULL 
    #    # }else{
    #    #   ColLimits=c(input$colorRng_min, input$colorRng_max)
    #    # }
    #    
    #    distfun_row = function(x) dist(x, method = input$distFun_row)
    #    distfun_col =  function(x) dist(x, method = input$distFun_col)
    #    
    #    hclustfun_row = function(x) hclust(x, method = input$hclustFun_row)
    #    hclustfun_col = function(x) hclust(x, method = input$hclustFun_col)
    #    
    #    
    #    p <- heatmaply(HeatMapData$FactorGMT,
    #                   main = input$main,xlab = input$xlab,ylab = input$ylab,
    #                   row_text_angle = input$row_text_angle,
    #                   column_text_angle = input$column_text_angle,
    #                   dendrogram = input$dendrogram,
    #                   branches_lwd = input$branches_lwd,
    #                   seriate = input$seriation,
    #                   colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
    #                   distfun_row =  distfun_row,
    #                   hclustfun_row = hclustfun_row,
    #                   distfun_col = distfun_col,
    #                   hclustfun_col = hclustfun_col,
    #                   k_col = input$c, 
    #                   k_row = input$r,
    #                   limits = ColLimits) %>% 
    #      layout(margin = list(l = input$l, b = input$b, r='0px'))
    #    
    #    p$elementId <- NULL
    #    
    #    p
    #    
    #  })
    #  
    #  #Render Plot ----
    #  output$heatout <- renderPlotly({
    #    if(!is.null(ExpressionAnalysis$LimmaResults)){
    #      interactiveHeatmap()    
    #    } else { NULL }
    #    
    #    })
    #  
    #  
    #  
    #  
    #  #Render Data Table ----
    #  output$RenderTopTable <- renderUI({
    #    if(is.null(ExpressionAnalysis$LimmaResults()) == 0){
    #    return("No data to show")
    #    } else {
    #    tableOutput("TopTable")
    #    }
    #    })
    #  
    #  output$TopTable <- renderDataTable({
    #    datatable(ExpressionAnalysis$LimmaResults(), server = T, filter='top',
    #     extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'),
    #     options = list(
    #     dom = 't',
    #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),
    #     colReorder = TRUE,
    #     scrollX = TRUE,
    #     fixedColumns = TRUE,
    #     fixedHeader = TRUE,
    #     deferRender = TRUE,
    #     scrollY = 500,
    #     scroller = TRUE
    #     ))
    #  
    #  })
    #    
    #  
    # 
    # 
    #  ############ BoxPlot
    #  
    #  
    #  # ############ TopTable     
    #  # 
    #  # output$TopTable <- DT::renderDataTable({
    #  #   
    #  #      LimmaTable <- ExpressionAnalysis$LimmaResults()
    #  #      LimmaTable <- as.data.frame(LimmaTable)
    #  #      
    #  #      DT::datatable(data = LimmaTable,
    #  #                    rownames = TRUE,
    #  #                    class = 'compact',
    #  #                    extensions = 'Buttons', 
    #  #                    options = list(
    #  #                         scrollY = '500px',
    #  #                         paging = T,
    #  #                         dom = 'Bfrtip',
    #  #                         buttons = c('copy', 'csv', 'excel')))
    #  # })
    #  # 
    #  # 
    #  
    
    
     
     
     ########################{ Disconnect from SQLite Server on Exit

     session$onSessionEnded(function() {
          message("Disconnecting from GEOmetadb.sqlite")
          con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
          dbDisconnect(con)
     })


     

     
     
     

     
     

     
     
     
     
}

