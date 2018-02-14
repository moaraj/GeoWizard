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
     GeoSearchResults$ResGSETable <- reactive({   
          withBusyIndicatorServer("MolSelectButton", {
            shinyjs::show("nStudiesPlotDiv", time = 1, anim = TRUE, animType = "fade")
            MolQuery <- as.character(unlist(input$MolSelectInput))
            message("Called MultiGSEQuery function")
            ResGSETable <- MultiGSEQuery(MolQuery = MolQuery)
            ResGSETable
          })
     })
     })
     
     GeoSearchResults$GseSummaryTableData <- reactive({
          GseDescDF <- GeoSearchResults$ResGSETable()
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
     
     GeoSearchResults$SelectedRowsGSETable <- reactive({
       message("Filtering GSE table by row selection")
       GseDescDF <- GeoSearchResults$GseSummaryTableData()
       GseDescDFSelected <- GseDescDF[input$GseSummaryData_rows_selected, ]
       GseDescDFSelected
     })
     
     
     observeEvent(input$MolSelectButton, {
       shiny::req(input$MolSelectInput)
     output$GseSummaryData <- DT::renderDataTable({
       GseDescDF <- GeoSearchResults$GseSummaryTableData()
       
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
       shiny::req(input$MolSelectInput)
          nTotalStudies <- GeoSearchResults$nTotalStudies
          valueBox(
               paste0("Total Datasets:", length(nTotalStudies)),
               "Add species to view all studies" , 
               icon = icon("book"),
               color = "blue"
          )
    })
     
     output$nStudiesSelectedIndicator <- renderValueBox({
       if(is.null(input$GseSummaryData_rows_selected)){
         rowSelection <- 0
       } else {
         rowSelection <- input$GseSummaryData_rows_selected
         rowSelection <- length(rowSelection)
       }
      
       valueBox(
               paste0("Studies selected:",rowSelection),"Click Table Rows to Select Studies" , 
               icon = icon("list"),
               color = "blue"
          )
     })
     

     
     ########################{ Plot GSE Summary Data
     
     output$nStudiesPlotTaxon <- renderPlot({
       shiny::req(input$MolSelectInput)
       if(is.null(GeoSearchResults$ResGSETable))
         return(NULL)
               
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
     })
     
     output$nStudiesPlotGdsType <- renderPlot({
       shiny::req(input$MolSelectInput)
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

     observeEvent( input[["AnalyzeSelectedDatasets"]], { updateTabItems(session, "TabSet", "GSMMetadata")})
     
     ################################### GSM Metadata TabItem
     
     DataSelection <- reactiveValues()
     
     ########################{ Data Selection and Filter
     
     output$GsmTabletoKeep <- renderUI({
       shiny::req(input$GseSummaryData_rows_selected)
          message("Dataframe for Data filtering")
          GseDescDF <- GeoSearchResults$SelectedRowsGSETable()
          
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
       GSEChoices <- input$KeepForExpVarAsign
       
       if(is.null(GSEChoices) | is.null(SQLSearchData$ResultDF))
         return(NULL)
       
       radioButtonsOptions <- list(
               inputId = "GsmTableSelect",
               label = "Analyze Selected Dataset",
               choices = GSEChoices)
          
       do.call(radioButtons,radioButtonsOptions) 
     })
     
  ####################################{ Table Showing Metadata Tables Containing ExpVars}
  
     SQLSearchData <- reactiveValues()
     
     ########################{ SQL Search
    
     #observeEvent(input$AnalyzeSelectedDatasets, {
     SQLSearchData$ResultDF <- reactive({
       message("Generating ReseltDF for SQL Search")
       shiny::req(input$GseSummaryData_rows_selected)
       input$AnalyzeSelectedDatasets
       
       GseTableData <- GeoSearchResults$SelectedRowsGSETable()
       
       if(is.null(GseTableData) | class(GseTableData) != "data.frame"){
         message(paste("class GseTableData", class(GseTableData)))
         return(NULL)
       }
         
       message("SQL Query of Selected Datasets")
       Result <- SqlQueryMain(GseTableData)
       ResultDF <- data.frame(Result, stringsAsFactors = F)
       ResultDF
       
       })
     #})

     SQLSearchData$FilteredResultDF <- reactive({
       shiny::req(input$GsmTableSelect)
       ResultDF <- SQLSearchData$ResultDF()
       GsmMetaDatatoShow <- input$GsmTableSelect
       
       if(is.null(ResultDF)){
        message("Result DF line 322 is NULL")
       return(NULL)
       }
       
       message("Filtering SQL Query Res for Selected GSE")
       FilteredResultDF <- ResultDF %>%
        select(series_id, gsm, keyword, taxon, gsm.title, description, characteristics_ch1) %>%
        filter(series_id %in% GsmMetaDatatoShow)
       
       FilteredResultDF
       })
      
    
      
     ##################{ Render GSM Meta data

     output$GseGsmTable <- DT::renderDataTable({
       message("Retreiving Datatable Data")
       SqlQueryResDF <- SQLSearchData$FilteredResultDF()

       if (is.null(SqlQueryResDF))
         return(NULL)

       SqlQueryResDF <- SqlQueryResDF %>% select(c(-1,-3,-4))
       message("Making SQL Summary Table")
       DT::datatable(data = SqlQueryResDF ,
          rownames = FALSE,
          class = 'row-border',
          options = list(scrollY = '400px',
          dom = 't',
          paging = FALSE,
          autoWidth = TRUE)) %>%
          formatStyle(names(SqlQueryResDF),
          color = 'black',
          backgroundColor = 'white',
          fontWeight = 'bold')
     })

    #################{Classify ExpVars}


     ExperimentalDesign <- reactiveValues()

     ExperimentalDesign$ExpClassFactorDF <- reactive({
       shiny::req(input$WhereVarData)
       message("Processing SQL Table Output")
       ExpFactorDF <- SQLSearchData$FilteredResultDF()

       if(is.null(ExpFactorDF))
         return(NULL)
       
       message("Classify the Summary and Return the Filtered GSE GSM DF")
       ExpFactorClassSummary <- ClassSummary(ExpFactorDF)
       message("Expands Characteristics Column")
       FactorColumnNames <- input$WhereVarData
       ExpFactorClassSummary <- GseGsmCharExpand(ExpFactorClassSummary, FactorColumnNames)
       ExpFactorClassSummary
     })


     #######################{ Data frame of all factors with more than one level }

     ExperimentalDesign$ExpFactorDF <- reactive({
          ExpandedDF <- ExperimentalDesign$ExpClassFactorDF()
          if(is.null(ExpandedDF))
            return(NULL)

          UseFulExpVarsColNames <- grep(pattern = "ExpVar[[:digit:]]", x = colnames(ExpandedDF), value = T)
          UseFulExpVarsDF <- data.frame(ExpandedDF[,UseFulExpVarsColNames])
          colnames(UseFulExpVarsDF) <- UseFulExpVarsColNames
          UseFulExpVarsDF
     })


     ########################{ Useful Factor Classification

     ExperimentalDesign$ClassGSMTextRV <- reactive({
          message("Fetching ClassDFList")
          ExpFactorDF <- ExperimentalDesign$ExpFactorDF()
          if(is.null(ExpFactorDF))
            return(NULL)

          ClassGsmText(ExpFactorDF)
     })

     ExperimentalDesign$DefaultClassRV <- reactive({
          message("Importing ClassListDF")
          ClassResList <- ExperimentalDesign$ClassGSMTextRV()

          if (is.null(ClassResList)) 
            return(NULL)

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
       shiny::req(input$WhereVarData)
       ExpFactorDF <- ExperimentalDesign$ExpFactorDF()
       RecVars <- ExperimentalDesign$DefaultClassRV()
       if (is.null(ExpFactorDF) | is.null(RecVars))
         return(NULL)
       
          checkboxOptions <- list(
               inputId = "UsefulColumnsCheckbox",
               label = "Factors that describe detected experimental design cohorts",
               choices = colnames(ExpFactorDF),
               selected = names(RecVars),
               inline = T)

          do.call(checkboxGroupInput, checkboxOptions)
     })


     ########################{ View Current Factor Col Selection

     ExperimentalDesign$FactorDF <- reactive({
       shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
       message("filtering DataTable with Default ExpVarSelection")
       ExpFactorDF <- ExperimentalDesign$ExpFactorDF()
       ExpFactorDF <- ExpFactorDF %>% select(one_of(input$UsefulColumnsCheckbox)) # Select Only the Exp Vars that are selected in the UsefulColumnsCheckbox
     })
     

     ########################{ Render Inputs to Filter Factors
     output$FilterGSMbyFactor <- renderUI({
      shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
      FactorDF <- ExperimentalDesign$FactorDF()
      if(is.null(FactorDF))
        return(NULL)

      NamesIndex <- colnames(FactorDF)
      FactorLevelInput <-
        lapply(NamesIndex, function(ColName){
        ColLevels <- FactorDF[,ColName]

      selectInput(inputId = paste("Gsm_",ColName, sep = ""),
        label = paste("Filter levels in", ColName),
        choices = unique(ColLevels),
        selected = unique(ColLevels),
        multiple = T,
        selectize = T)
        })
      })


     ########################{ Take Levels from inputs and determine rows of DF

      ExperimentalDesign$RowsToKeep <- reactive({
      shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)  
      
      FactorDF <- ExperimentalDesign$FactorDF()
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
        stop("Error in FactorDF, restart app or select different factor columns")
      }
  
      RowsToKeep
      })
      
      
      ExperimentalDesign$FilteredFactorDF <- reactive({
        RowsToKeep <- ExperimentalDesign$RowsToKeep()
        FactorDF <- ExperimentalDesign$FactorDF()# dependencies of FactorDF  shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
        FilteredFactorDF <- FactorDF[RowsToKeep,] 
      })

     #######################{ Output Table with Factor Selection

     ExperimentalDesign$UniqueFilteredFactorDF <- reactive({
       FactorDF <- ExperimentalDesign$FilteredFactorDF()
       UniqueFilteredFactorDF <- unique(FactorDF)
       })


     output$ImportantFactorTable <- DT::renderDataTable({
      shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
       ExpFactorDF <- ExperimentalDesign$UniqueFilteredFactorDF()
       ExpFactorDF <- data.frame(ExpFactorDF)

      
      DT::datatable(data = ExpFactorDF, extensions = 'ColReorder', class = 'compact',
        options = list( dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px',paging = FALSE,
        columnDefs = list(list(width = '150px', targets = c(1:ncol(ExpFactorDF)))),
        colReorder = list(realtime = FALSE))) %>%
        formatStyle(names(ExpFactorDF), color = 'black', fontWeight = 'bold')
     })
     

     output$FullFactorTable <- DT::renderDataTable({
       shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
       FactorDF <- ExperimentalDesign$FactorDF()
       FactorDF <- as.data.frame(FactorDF)

       DT::datatable(data = FactorDF, extensions = 'ColReorder',class = 'compact',
        options = list(dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px', paging = FALSE,
                       columnDefs = list(list(width = '150px', targets = c(1:ncol(FactorDF)))),
                       colReorder = list(realtime = FALSE))) %>%
        formatStyle(names(FactorDF),color = 'black',fontWeight = 'bold')
     })
     
    observe({
    output$ExcludedFactorTable <- DT::renderDataTable({
    shiny::req(input$UsefulColumnsCheckbox, input$WhereVarData)
          
    ExcludedFactorDF <- ExperimentalDesign$ExpFactorDF()
    ExcludedFactorDF <- ExcludedFactorDF %>% select(-one_of(input$UsefulColumnsCheckbox))
    ExcludedFactorDF <- as.data.frame(ExcludedFactorDF)
    
    DT::datatable(data = ExcludedFactorDF, extensions = 'ColReorder',class = 'compact',
      options = list(dom = 't', autoWidth = TRUE, scrollX = T, scrollY = '500px', paging = FALSE,
        columnDefs = list(list(width = '150px', targets = c(1:ncol(ExcludedFactorDF)))),
        colReorder = list(realtime = FALSE))) %>%
        formatStyle(names(ExcludedFactorDF),color = 'black',fontWeight = 'bold')
    })
    }) 
    
    ################################### GSM Metadata TabItem 
    observeEvent( input[["GoToDesignPage"]], { updateTabItems(session, "TabSet", "DesignMatrix")})




     ################################################## Design Matrix
     ################################################## Design Matrix
     ################################################## Design Matrix

    #  output$DesignMat_SummaryTable <- DT::renderDataTable({
    #       GsmDF <- ExperimentalDesign$UniqueFilteredFactorDF()
    #       if(is.null(GsmDF))
    #         return(NULL)
    #       
    #       DT::datatable(data = GsmDF,
    #                     extensions = 'ColReorder',
    #                     class = 'compact',
    #                     options = list(
    #                          dom = 't',
    #                          autoWidth = TRUE,
    #                          scrollX = T,
    #                          scrollY = '500px',
    #                          paging = FALSE,
    #                          columnDefs = list(list(width = '150px', targets = c(1:ncol(GsmDF)))),
    #                          colReorder = list(realtime = FALSE))) %>%
    #            formatStyle(names(GsmDF),
    #                        color = 'black',
    #                        fontWeight = 'bold')
    #  })
    # 
    #  
    #  ########################{ Annotate the data table
    #  
    #  output$RearrangeLevels <- renderUI({
    #    FactorDF <- ExperimentalDesign$FilteredFactorDF()
    #    
    #    if (is.null(FactorDF)) 
    #      return(NULL)
    #      
    #    NamesIndex <- colnames(FactorDF)
    #    sapply(NamesIndex, function(ColVect){
    #        ColLevels <- FactorDF[ColVect]
    #        selectInput(inputId = ColVect,
    #                    label = paste("Selected control level for", ColVect),
    #                    choices = unique(ColLevels))
    #      })
    #      
    #    
    #       
    #  })
    #  
    #  
    #  controlLevelsDF <- reactive({
    #       DesignDF <- ExperimentalDesign$FilteredFactorDF()
    #       if(is.null(DesignDF))
    #         return(NULL)
    #       
    #       NamesIndex <- colnames(DesignDF)
    #       message("Changing control factor level")
    #       
    #       ResDF <- lapply(NamesIndex, function(ColName){
    #            ResultVector <- DesignDF[,ColName]
    #            controlLevels <- input[[ColName]]
    #            otherLevels <- levels(factor(ResultVector))
    #            levels(ResultVector) <- unique(c(controlLevels, otherLevels))
    #            ResultVector
    #       })
    #       
    #       names(ResDF) <- NamesIndex
    #       ResDF <- data.frame(do.call(cbind, ResDF))
    #       ResDF
    #  })
    #  
    #  #####################
    #  
    # 
    #  observeEvent(input$SubmitFormula, {
    #    DesignDF <- controlLevelsDF()
    #    Designformula <- input$formulaInputDesign
    #    
    #    if(length(Designformula) == 0 & is.null(DesignDF))
    #      return(NULL)
    #     
    #    DesignExpression <- as.formula(Designformula)
    #    ExperimentalDesign$DesignMatrix <- model.matrix(as.formula(DesignExpression), DesignDF)
    #  })
    #  
    #  # output$TextAhead <- renderUI({
    #  #      localDF <- controlLevelsDF()
    #  #      DFname <- colnames(localDF)
    #  #      DFlevs <- as.character(
    #  #      lapply(localDF, function(x){ 
    #  #           FactorLevels <- levels(x)
    #  #           nLevels <- length(FactorLevels)
    #  #           
    #  #           paste(FactorLevels[1],
    #  #                 FactorLevels[nLevels], 
    #  #                 collapse = " ")
    #  #           }))
    #  # 
    #  #      textInput.typeaheadOptions <- list(
    #  #           id="thti",
    #  #           placeholder="~ ExpVa1 + ExpVar2",
    #  #           local=data.frame(name=paste("~ ", DFname), 
    #  #                            info= paste("levels:", DFlevs)),
    #  #           valueKey = "name",
    #  #           tokens=c(1:length(DFname))
    #  #      )
    #  #      
    #  #      do.call(textInput.typeahead, textInput.typeaheadOptions)
    #  #      
    #  # })
    #  
    #  
    #  observeEvent(input$SubmitFormula, {
    # 
    #    output$CustomExpressionTable <- DT::renderDataTable({
    #      DesignMatrix <- ExperimentalDesign$DesignMatrix()
    #      if (is.null(DesignMatrix))
    #        return(NULL)
    #      
    #      
    #     DT::datatable(data = DesignMatrix,
    #     rownames = TRUE,
    #     class = 'compact',
    #     extensions = 'Buttons', 
    #     options = list(
    #       scrollY = '300px',
    #       paging = FALSE,
    #       dom = 'Bfrtip',
    #       buttons = c('copy', 'csv', 'excel'))
    #     )
    #     
    #       })
    #    })
    #  
    #  
    #  #########################
    #  
    #  output$ExperimentalBlocksPlot <- renderPlot({
    #    shiny::req(input$formulaInputDesign)
    #    DesignExpression <- as.formula(input$formulaInputDesign)
    #    DesignDF <- controlLevelsDF()
    #    
    #    if (!is.null(DesignExpression) & !is.null(DesignDF)) {
    #      message("Rendering Block Plot")
    #      try(vcd::mosaic(DesignExpression, DesignDF))
    #    } else { return("Click 'Use' Detected Design or Enter Valid Design Matrix Formula") }
    #    
    #    })
    #  
    #  
    #  
    #  ################################################## Expression Analysis
    #  ################################################## Expression Analysis
    #  ################################################## Expression Analysis
    #  
    #  
    #  ####Download the Data
    #  GSEdata <- reactiveValues()
    #  
    #  observeEvent(input$DownloadGEOData, {
    #    GSE <- input$GsmTableSelect
    #    if (is.null(GSE))
    #      return(NULL)
    #      
    #    shinyjs::show("GMTTable")
    #    message("Downloading Data from GEO")
    #    GSEeset <- LoadGEOFiles(GSE, GeoRepoPath = "~/GeoWizard/GEORepo")
    #    GSEdata$GSEeset <- GSEeset
    #    
    #    })
    #  
    #    GSEdata$FactorGMT <- reactive({
    #      FilteredFactorDF <- ExperimentalDesign$FilteredFactorDF()
    #      GSEeset <- GSEdata$GSEeset()
    #     
    #     if (!is.null(FilteredFactorDF & !is.null(GSEeset))) {
    #       message("Generating FactorGMT")
    #       GenFactorGMT(GSEeset, FilteredFactorDF)  
    #     }
    #    })
    #       
    #    GSEdata$FactorGMTMelt <- reactive({ 
    #      FactorGMT <- GSEdata$FactorGMT()
    #      if (!is.null(FactorGMT)) {
    #        melt(FactorGMT)
    #      } else {
    #        message("GMT File not Loaded properly")
    #        NULL
    #      }
    #      })    
    #       
    # 
    #  
    #  ######### Column 1 - GMT File Tab
    #  
    #  output$GMTFileTable <- renderDataTable({
    #       DF <- exprs(GSEdata$GSEeset)
    #       DT::datatable(data = DF,
    #                     rownames = TRUE,
    #                     class = 'compact',
    #                     extensions = 'Buttons', 
    #                     options = list(
    #                          scrollX = T,
    #                          scrollY = '300px',
    #                          paging = T,
    #                          dom = 'Bfrtip',
    #                          buttons = c('copy', 'csv', 'excel')))
    #       })
    #  
    #  ######### Box
    #  
    #  output$BoxFactorSelect <- renderUI({
    #       MeltedDF <- GSEdata$FactorGMTMelt()
    #       if (!is.null(MeltedDF)) {
    #         FactorOptions <- grep(pattern = "ExpVar[0-9]", x = colnames(MeltedDF), value = T)
    #         selectInput(inputId = "BoxFactorSelectInput",
    #                     label = "Fill by Factor",
    #                     choices = FactorOptions,
    #                     selected = FactorOptions[1])
    #       }
    #     })
    # 
    #  output$BoxPlotGMT <- renderPlot({
    #    GSEgmtDF = GSEdata$FactorGMTMelt()
    #    if (!is.null(GSEgmtDF)) {
    #      GMTBoxplot(GSEgmtDF = GSEgmtDF,
    #                 BoxPlotType = input$BoxPlotType,
    #                 PlotBy = input$BoxPlotBy,
    #                 PlotFactor = input$BoxFactorSelectInput,
    #                 SampleSize = input$BoxSampleSize)
    #    }
    #    })
    #  
    #  ######### Hist
    #  
    #  output$HistFactorSelect <- renderUI({
    #       MeltedDF <- GSEdata$FactorGMT()
    #       FactorOptions <- grep(pattern = "ExpVar[0-9]", x = colnames(MeltedDF), value = T)
    #       selectInput(inputId = "HistFactorSelectInput",
    #                   label = "Fill by Factor",
    #                   choices = FactorOptions,
    #                   selected = FactorOptions[1])
    #  })
    #  
    #  output$HistPlotGMT <- renderPlot({
    #       GMTHistPlot(GSEgmtDF = GSEdata$FactorGMT(),
    #                   HistPlotType = input$HistPlotType,
    #                   PlotFactor = input$HistFactorSelectInput,
    #                   SampleSize = input$HistSampleSize)
    #  })
    # 
    #  
    #  ####################################################################### QC Analysis
    #  
    #  ############# BioQC Analysis
    #  
    #  observeEvent(input$PerformBioQCAnalysis, {
    #  output$BioQCPlot <- renderPlot({
    #       message("Loading Expression Set for BioQC")
    #       GeneSymbolGSEeset <- GSEdata$GSEeset
    #       GeneSymbolGSEeset <- ConvertGSEAnnotations(GSEeset = GeneSymbolGSEeset, AnnotationType = input$GeneAnnotationType)
    #       RunBioQC(GeneSymbolGSEeset)
    #       })
    #  })
    #  
    #  
    #  ################ PCA Plot
    #  DataPCA <- reactiveValues()
    #  
    #  output$PCA <- renderPlot({
    #    GSEeset <- GSEdata$GSEeset()  #Expression Set
    #    
    #    GSEeset <- GSEdata$GSEeset
    #    ArrayData <- exprs(GSEeset)
    #       
    #    ListPlotPCA <- PlotPCA(ArrayData = ArrayData)
    #       
    #    DataPCA$CA <- ListPlotPCA$CA
    #    DataPCA$Scree <- ListPlotPCA$Scree
    #    DataPCA$Cont <- ListPlotPCA$Cont
    #       
    #    ListPlotPCA$PCA
    #    })
    #  
    #  output$CA <- renderPlot({ DataPCA$CA })
    #  output$Scree <- renderPlot({ DataPCA$Scree })
    #  output$Cont <- renderPlot({ DataPCA$Cont })
    #  
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
    #    FactorDF <- ExperimentalDesign$FilteredFactorDF()
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
    #  ########################{ Disconnect from SQLite Server on Exit
    #  
    #  session$onSessionEnded(function() {
    #       message("Disconnecting from GEOmetadb.sqlite")
    #       con <- dbConnect(SQLite(),'GEOmetadb.sqlite')
    #       dbDisconnect(con)
    #  })
    #  
    #  
     

     
     
     

     
     

     
     
     
     
}

