server <- shinyServer(function(input, output) {
    
    ###### Download the Data
    
    #'
    #'
    GSEdata <- reactiveValues()
    
    #'
    #'
    #'
    #'
    #'
    output$DownloadDataInfoBox <- renderUI({
    column(12,
    tags$div(id="pane",
    fluidRow(valueBox(width = 12, "GSE:", "GPL:" )),
    tags$style(type="text/css","#pane{font-size:20px;}")))    
    })
    
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
        DataSourceSelection <- input$DataSourceSelection
        if(DataSourceSelection == 1) {
            ExpressionMatrix <- GSEdata$MatrixAnnotated(); message("Annotated GMT Matrix asigned to reactive value: GSEdata$ExpressionMatrix")
        } else if(DataSourceSelection == 2) { 
            ExpressionMatrix <- GSEdata$GMTinput_CSV(); message("GMTinput_CSV asigned to reactive value: GSEdata$ExpressionMatrix")
        }
        return(ExpressionMatrix)
      })
    
    
    #'
    #'
    #'
    #'
    GSEdata$FactorGMT <- reactive({
        #ControlFactorDF <- ExperimentalDesign$ControlFactorDF()   change
        ControlFactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
        ExpressionMatrix <- GSEdata$ExpressionMatrix()
        FactorGMT <- GenFactorGMT(ExpressionMatrix = ExpressionMatrix, FactorDF = ControlFactorDF); message("line 657: Generating FactorGMT")
        return(FactorGMT)
      })
    
    #'
    #'
    #'
    #'
    GSEdata$FactorGMTMelt <- reactive({
        shiny::req(input$GsmTableSelect)
        FactorGMT <- GSEdata$FactorGMT()
        #FactorGMT <- FactorGMT[input$GMTFileTable_rows_all,]
        if (is.data.frame(FactorGMT)) {
            message("Melting FactorGMT for plotting")
            FactorGMTMelt <- melt(FactorGMT) ; message("FactorGMTMelt loaded")
            return(FactorGMTMelt)
        
        } else { 
            return(NULL)
            stop("Factor GMT File not loaded properly")}
    })
    
    output$RawDataQC <- renderDataTable({
        if (input$RawDataTableMelt == "GMT") {
            message("Expression Matrix loaded for RawDataQC Table ")
            TableData <- GSEdata$ExpressionMatrix() 
            
        } else if (input$RawDataTableMelt == "FactorGMTMelt") {
            message("FactorGMTMelt loaded for RawDataQC Table ")
            TableData <- GSEdata$FactorGMTMelt() 
            
        } else { stop("Data not loaded properly") }
        
        DT::datatable(data = as.data.frame(TableData), rownames = TRUE, class = 'compact', extensions = 'Buttons',
            options = list( scrollX = F, scrollY = '300px', paging = T, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')))
      })
    
    
    #################### Boxplot Tab

        BoxplotData <- reactiveValues()
        
        GSEdata$FactorGMTMelt.Sampled <- reactive({
            message("Generating FactorGMTMelt reactive values for Boxplot Input")
            FactorGMT <-  GSEdata$FactorGMT()
            FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
            
            message( "Sampling Gene Matrix")
            sampledColumns <- sample(x = ((ncol(FactorDF)+1):ncol(FactorGMT)), size = input$BoxPlot_nGenes)
            message( "Filtering Gene Matrix")
            FactorGMT.Filtered <- FactorGMT[, c(1:(ncol(FactorDF)+1), sampledColumns)]
            FactorGMTMelt <- melt(FactorGMT.Filtered)
            FactorGMTMelt
        })
        
        #output$GeneSelect <- renderUI({}) # Add Specific Genes

        output$BoxFactorSelect_UI <- renderUI({
            message("Rendering Boxplot Factor Select Input")
            FactorGMTMelt <-  GSEdata$FactorGMTMelt.Sampled()
            FactorOptions <- colnames(FactorGMTMelt)[1:(length(colnames(FactorGMTMelt))-2)]
            selectInput( inputId = "BoxFactorSelectInput", label = "Group and Fill by:", choices = FactorOptions)
        })
        
        output$BoxPlot <-renderPlot({
            FactorGMTMelt <- GSEdata$FactorGMTMelt.Sampled()
            xVar <- "GSM"
            yVar <- "value"
            ggplot(FactorGMTMelt, aes_string(x = "xVar", y = "yVar")) + geom_boxplot()
            
        })




      
      
    ########## PCA
    FactorGMTCast <- reactive({
            FactorGMTCast <- GSEdata$FactorGMT()
            DataDF <- GSEdata$ExpressionMatrix()
            FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
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
            ExpressionMatrix <- GSEdata$ExpressionMatrix()
            
            BioQCData <- RunBioQC(ExpressionMatrix)
            message("BioQC Analysis Finished")
            BioQCData
        })
        
        
        output$BioQCPlotInput_UI <- renderUI({
            #FactorDF <- ExperimentalDesign$ControlFactorDF() 
            FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
            FactorNames <- colnames(FactorDF)
            selectInput(inputId = "BioQCPlotInput" , label = "Cluster by Factor:", choices = FactorNames)
        })
        
        
        output$BioQCPlot <- renderPlotly({
        message("Loading Heatmap Data for Plotting")
        BioQCData <- BioQCData()
        message("Filter Number of signature to show by use input")
        BioQCDataFiltered <- tail(BioQCData, n = input$NumberOfHeatmapSignatures)
        
        # Load Factor DF
        #FactorDF <- ExperimentalDesign$ControlFactorDF() 
        FactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
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
        
        output$nGSESamples <- renderValueBox({
        shiny::req(input$GeneAnnotationType)
        message("rendering nSamples Info Box")
        ExpressionMatrix <- GSEdata$ExpressionMatrix()
        nSamples <- ncol(ExpressionMatrix)
        valueBox( nSamples, "Number of Samples in GSE", icon = icon("list"), color = "purple")
        })
        
        output$nGSEGenes <- renderValueBox({
        shiny::req(input$GeneAnnotationType)
        message("rendering nGenes Info Box")
        ExpressionMatrix <- GSEdata$ExpressionMatrix()
        nGenes <- nrow(ExpressionMatrix)
        valueBox( nGenes, "Number of Genes in GSE", icon = icon("dna"), color = "yellow")
        })
        
        
        
        ###################### Expression Analysis
        ExpressionAnalysis <- reactiveValues()

        ExpressionAnalysis$LimmaResults <- reactive({
        GSEeset <- GSEdata$GSEeset() #Expression Set
        #DesignMatrix <- ExperimentalDesign$DesignMatrix() #Matrix

       if(!is.null(GSEeset) & !is.null(DesignMatrix)){
         ArrayData <- exprs(GSEeset) #Matrix
         DesignMatrix <- DesignMatrix
         LimmaOutput(ArrayData,DesignMatrix)
         message("Performing Limma DEA")
       } else {
         message("GSEeset not Loaded")
         NULL
         }


       })

     ############ Volcano Plot

     output$PValThres <- renderUI({
          numericInput(inputId = "PValThresInput",
                       label = "pValue Threshold",
                       value = 2,
                       min = 1,
                       step = 0.5)
     })

     output$LogFCThres <- renderUI({
          numericInput(inputId = "LogFCThresInput",
                       label = "LogFC Threshold",
                       value = 1,
                       min = 0,
                       max = 5,
                       step = 0.5)
     })
     
    output$SelectContrast_UI <- renderUI({
        LimmaTable <- LimmaTable
        selectInput(inputId = "Volcanoplot_SelectContrast", label = "Select Contrast", choices = unique(LimmaTable$Contrast))
     })

        output$VolcanoPlot <- renderPlot({
        #shiny::req(input$SubmitFormula)

        pValueThresHold <- input$PValThresInput
        logFCThresHold <- input$LogFCThresInput

        #LimmaTable <- ExpressionAnalysis$LimmaResults()
        #LimmaTable <- as.data.frame(LimmaTable)
        selectedContrast <- input$Volcanoplot_SelectContrast
        LimmaTable <- LimmaTable %>% dplyr::filter(Contrast == selectedContrast)
        
        LimmaTable <- LimmaTable %>%
        mutate(Threshold = abs(logFC) > logFCThresHold) %>%
        mutate(Threshold = as.numeric(Threshold)) %>%
        mutate(Threshold = Threshold + as.numeric(-log(LimmaTable$adj.P.Val) >= pValueThresHold))
        
        p <- ggplot(LimmaTable, aes(x = logFC, y = -log(adj.P.Val), color = factor(Threshold > 1))) + geom_point() + theme_grey()
        
        if (input$VolcanoPlot_showColor) {
            if (input$VolcanoPlot_ThemeSelect == "default") { p <- p }
            else if (input$VolcanoPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
            else if (input$VolcanoPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
            else if (input$VolcanoPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
            else if (input$VolcanoPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
            else if (input$VolcanoPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
            else if (input$VolcanoPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
        }
          
        p
       })
        
        
        


})

