ui <- shinyUI(
    dashboardPage(
    dashboardHeader(),
    dashboardSidebar(sidebarMenu(menuItem("Download and QC", tabName = "DataQC", icon = icon("download")))),
    
    dashboardBody(
        
        
    tabItems(
    
        tabItem(
        tabName = "DataQC",
        fluidRow(
        column(10,
        #column(12, div( id = "GSMMetadataWarning_Down", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        #fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))), 
        
        
        tabBox(title = "Raw Data Statistics",
        width = 12,
        
        tabPanel("Download Data",
        
        fluidRow(
        column(4,   #Input Column
        
        wellPanel(
        fluidRow(
        column(12,
        
        
        
        column(12,uiOutput("DownloadDataInfoBox")),
        #column(12, uiOutput("DownloadInputAcession")
        column(12, uiOutput("InputSourceGMT")),
        column(12,
               
        conditionalPanel('input.DataSourceSelection==1',
        tags$hr(),
        column(6,textInput(inputId = "GsmTableSelect",label = "GSE input", value = GSE)),
        column(6,textInput(inputId = "GplTableSelect",label = "GPL input", value = GPL)),
        column(12,actionButton( inputId = "DownloadGEOData", label = "Download", icon = icon("download"), block = T)),
        column(12, hr()),
        uiOutput("GeneAnnotationTypeUI"),hr()),
        
        conditionalPanel('input.DataSourceSelection==2',
        tags$hr(),                 
        fileInput("GMTcsv", "Choose GMT File", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
        tags$hr(),
        column(4,
        strong("Header"),
        checkboxInput("CSVheader", "Data has header", TRUE)),
        column(4,radioButtons("CSVsep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
        column(4,radioButtons("CSVquote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))
        ),
        
        conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>'))
        ),
        
        column(12, radioButtons(inputId = "ExpressionDataType",
            label = "Gene Expression Data Type",
            choiceNames = c("MicroArray", "NGS Sequencing", "Single Cell Sequencing"),
            choiceValues = c("mArray", "RNAseq", "ssRNAseq"),
            selected = "mArray")),
        br(),
        column(12, radioButtons( 
            inputId = "RawDataTableMelt",  
            label = "Which Data Matrix to Show" ,  
            choiceNames = c("Gene Matrix","Melted Gene Matrix with Factors"),
            choiceValues = c("GMT", "FactorGMTMelt"), 
            inline = T))
        
        ) # Well Panel Fluid Row
        ) # Well Panel Column
        ) # Conditional Well Panel CSV input
        ),
        
        column(8, #Table Column
        wellPanel(
        fluidRow(
            
        h4("Gene Expression Matrix"),hr(),
        column(12,
        DT::dataTableOutput("RawDataQC")
        )))) # Table Column
        
        ) # TabPanel Fluid Row
        ), # Download Data tabPanel
    
        tabPanel("BioQC", style = "margin-left :10px; margin-right :10px",
        fluidRow(
        
        column(4, 
        wellPanel(
        fluidRow(
        
        h4("BioQC Options"),
        column(12, actionButton(inputId = "PerformBioQCAnalysis",  label = "Perform BioQC Analysis",  size = "large")),
        column(12,
        helpText(
        paste(
          "BioQC performs quality control of high-throughput expression data based on ",
          "tissue gene signatures. It can detect tissue heterogeneity in gene " ,
          "expression data. The core algorithm is a Wilcoxon-Mann-Whitney ",
          "test that is optimised for high performance."))),
        hr(),
        column(12, uiOutput("BioQProfileInput_UI") %>% withSpinner(color = "#0dc5c1"))
        ) # Input Well Panel Fluid Row
        ) # Input Well Panel
        ), # Input Column
                       
        column(8,
        plotOutput(outputId = "BioQCPlot") %>% withSpinner(color = "#0dc5c1"),
        plotOutput(outputId = "BioQCProfilePlot") %>% withSpinner(color = "#0dc5c1")
        )  # Plot Column 
        )  # Page Fluid Row
        )  # tabPanel BioQC"
        )
        )
        )

) # Tab Item
) # Tab Items
) # Dasboard Boday
) # DashBoard Page
) # Shiny UI

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
        inline = T, 
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
        shinyjs::show("GMTTableCSV")
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
        if(input$DataSourceSelection == 1) {ExpressionMatrix <- GSEdata$MatrixAnnotated()
        } else if(input$DataSourceSelection == 2) { ExpressionMatrix <- GSEdata$GMTinput_CSV()}
        return(ExpressionMatrix)
      })
    
    #'
    #'
    #'
    #'
    #'
    # output$GMTcsvTable <- DT::renderDataTable({
    #     shiny::req(shiny::req(input$GsmTableSelect, input$DownloadGEOData))
    #     DF <- GSEdata$ExpressionMatrix()
    #     DT::datatable(data = as.data.frame(DF), rownames = TRUE,  extensions = 'Buttons', 
    #         options = list( 
    #             scrollY = '300px', 
    #             scrollX = TRUE, 
    #             dom = 'Bfrtip',
    #             buttons = c('copy', 'csv', 'excel')))
    # })
    
    #'
    #'
    #'
    #'
    GSEdata$FactorGMT <- reactive({
        #ControlFactorDF <- ExperimentalDesign$ControlFactorDF()   change
        ControlFactorDF <- readRDS(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.rds")
        ExpressionMatrix <- GSEdata$ExpressionMatrix()
        message("Generating FactorGMT")
        FactorGMT <- GenFactorGMT(ExpressionMatrix = ExpressionMatrix, FactorDF = ControlFactorDF)
        return(FactorGMT)
      })
    
    #'
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
            FactorGMTMelt <- melt(FactorGMT)
        } else { stop("Factor GMT File not loaded properly") }
        return(FactorGMTMelt)
    })
    
    output$RawDataQC <- renderDataTable({
        if (input$RawDataTableMelt == "GMT") {
            TableData <- GSEdata$ExpressionMatrix()
        } else if (input$RawDataTableMelt == "FactorGMTMelt") {
            TableData <- GSEdata$FactorGMTMelt()
        } else { stop("Data not loaded properly") }
        
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
    
    
    
    ######## Boxplot Tab
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
     
      output$BoxPlotly <- renderPlot({
        shiny::req(input$BoxFactorSelectInput)
        FactorGMTMelt = GSEdata$FactorGMTMelt()
     
        if (input$BoxPlot_IndpVar == "Sample") { 
          if (input$BoxPlot_PlotBy == "Overall Distribution") { 
            GeneSample <- sample(x = FactorGMTMelt$GSM, size = input$BoxPlot_nGenes)
            FactorGMTMelt <- FactorGMTMelt %>% filter(GSM %in% GeneSample)
            AesX <- FactorGMTMelt$GSM
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            GroupVar <- FactorGMTMelt$GSM
            xlabtext <- "GSMs in Dataset"
            legPos <- "top"
            
          } else if (input$BoxPlot_PlotBy == "Factor Distribution") {;message("Factor")
            AesX <- FactorGMTMelt[,input$BoxFactorSelectInput]
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            GroupVar <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
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
            GroupVar <- NULL
            xlabtext <- "Assayed Genes"
            legPos <- "none"
            
          } else if (input$BoxPlot_PlotBy == "Factor Distribution") {
            AesX <- FactorGMTMelt$variable
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            GroupVar <- NULL
            xlabtext <- "Assayed Genes"
            legPos <- "top"
          }
        }
        red <- "red"
        p <-
            ggplot(data = FactorGMTMelt, aes_string(y = FactorGMTMelt$value, x = AesX, group = GroupVar, fill = AesFill)) +
            #ggplot(data = FactorGMTMelt, aes(y = FactorGMTMelt$value, x = AesX, group = AesFill)) +
            theme(legend.position = legPos) +  
            ylab(label = "Expression Level") +
            xlab(label = xlabtext) +
            guides(fill=guide_legend(title="Experimental Factor Groups")) +
            theme(axis.text.x = element_text(angle = input$BoxPlot_column_text_angle)) + 
            theme(axis.text = element_text(size = 14, hjust = 1)) +
            theme(axis.title = element_text(size = 14)) +
            theme(legend.text=element_text(size=14))
        
        if (input$BoxPlot_Type == "Boxplot") {
            p <- p + geom_boxplot(varwidth = F, position = "dodge")
            
        } else if (input$BoxPlot_Type == "Violin Plot") {
            p <- p + geom_violin()
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
        
        if (input$BoxPlot_showMargins==1) {
            p <-
                p + theme(plot.margin = margin(
                input$BoxPlot_margin_top,
                input$BoxPlot_margin_right,
                input$BoxPlot_margin_bottom,
                input$BoxPlot_margin_left, 
                "cm"))
        }
        
        if (input$BoxPlot_showColor) {
            if (input$BoxPlot_ThemeSelect == "default") { p <- p }
            else if (input$BoxPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
            else if (input$BoxPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
            else if (input$BoxPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
            else if (input$BoxPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
            else if (input$BoxPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
            else if (input$BoxPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
        }
          
        if (input$BoxPlot_PlotAxisFlip==1) { p <- p + coord_flip()}
        if (length(input$BoxPlot_main) > 0) { p <- p + labs(title = input$BoxPlot_main)}
        if (length(input$BoxPlot_xlab) > 0) { p <- p + labs(x = input$BoxPlot_xlab)}
        if (length(input$BoxPlot_ylab) > 0) { p <- p + labs(y = input$BoxPlot_ylab)}
        
        #sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180)
        #sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
    
        #p <- ggplotly(p)
        p
      })
      
        output$BoxPlotUI <- renderUI({
            if(input$BoxPlot_showPlotSize){ 
            plotHeight <- input$BoxPlot_Height
            plotWidth <- input$BoxPlot_Width
            
            fluidRow( column(12, plotOutput(outputId = "BoxPlotly", height = plotHeight, width = plotWidth) %>% 
                withSpinner(color = "#0dc5c1")))
            } else { 
            fluidRow( column(12, plotOutput(outputId = "BoxPlotly") %>% 
                withSpinner(color = "#0dc5c1")))
            }
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
      
      
    ########## PCA
    FactorGMTCast <- reactive({
            FactorGMTCast <- GSEdata$FactorGMT()
            DataDF <- GSEdata$ExpressionMatrix()
            FactorDF <- readRDS(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.rds")
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
        #BioQCData <- reactive({
            message("Loading Expression Set for BioQC")
            ExpressionMatrix <- GSEdata$ExpressionMatrix()
            BioQCData <- RunBioQC(GMT = ExpressionMatrix)
            BioQCData
        })
        
        output$BioQCPlot <- renderPlot({
        BioQCData <- BioQCData()
        BioQCHeatmap(RunBioQCres = BioQCData)
        })
        
        output$BioQProfileInput_UI <- renderUI({
            req(input$PerformBioQCAnalysis)
            BioQCData <- BioQCData()
            TissueProfile <- rownames(BioQCData)
            selectInput(inputId = "BioQProfileInput" , label = "Sample Profiles", choices = TissueProfile, selected = TissueProfile[1], multiple = T)
        })
        
        output$BioQCProfilePlot <- renderPlot({
        req(input$BioQProfileInput)
        BioQCData <- BioQCData()
        TissueSelection <- input$BioQProfileInput
        BioQCDataSelection <- BioQCData[c(TissueSelection),]
        BioQCDataMelt <- melt(BioQCDataSelection) 
        
        BioQCDataMelt <- filResmelt
        colnames(BioQCDataMelt) <- c("tissue", "sample", "bioqc")
        
        p <- 
            ggplot(data = BioQCDataMelt, aes(x = sample,y = bioqc, group = tissue, color = tissue)) + 
            geom_point() + geom_line() + 
            labs(title = "BioQC Score profiles for samples", x ="Sample GSM Acession", y = "BioQC Score", color = "BioQC Tissue Score") + 
            theme(legend.position="bottom") + 
            theme(axis.text.x = element_text(vjust = 1, angle = 90, size = 12)) + 
            theme(legend.text = element_text(size = 12))
        p
        })
        
        


})


shinyApp(ui = ui, server = server)


