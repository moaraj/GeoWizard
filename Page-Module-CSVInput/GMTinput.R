GeoWizard <- "~/GeoWizard/"
GeoRepo <- "~/GeoWizard/GeoRepo"
GSE <- "GSE69967"

ui <- 
    shinyUI(
    dashboardPage(
    dashboardHeader(),
    dashboardSidebar(sidebarMenu(
    menuItem("Download and QC", tabName = "DataQC", icon = icon("download")),
    menuItem("Expression Analysis", tabName = "DifferentialAnalysis", icon = icon("bar-chart")))),
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
        
        h4("Data Source"),
        column(12, uiOutput("InputSourceGMT")),
        column(12, tags$hr()),
                                 
        conditionalPanel('input.DataSourceSelection==1',
        h4("GEO Accession"),
        column(12,
        column(6,textInput(inputId = "GsmTableSelect",label = "GSE input", value = "GSE69967")),
        column(6,textInput(inputId = "GplTableSelect",label = "GPL input", value = "GPL")),
        column(12,actionButton( inputId = "DownloadGEOData", label = "Download Data from GEO", icon = icon("download"), block = T)),
        column(12, hr()),
        uiOutput("GeneAnnotationTypeUI"),hr())),
        
        conditionalPanel('input.DataSourceSelection==2',
        h4("CSV Import Options"),
        column(12,
        fileInput("GMTcsv", "Choose GMT File", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
        column(4,
        strong("Header"),
        checkboxInput("CSVheader", "Data has header", TRUE)),
        column(4,radioButtons("CSVsep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
        column(4,radioButtons("CSVquote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))),
        column(12, tags$hr())
        ),
        
        
        column(12,
        conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>'))
        ),
        
        h4("Expression Matrix"),
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
        DT::dataTableOutput("RawDataQC"),
        column(4, actionButton(inputId = "RefreshRawDataQCTable", label = "Refresh Table",icon = icon('refresh')))),
        fluidRow(
        column(12, hr()),
        column(12,valueBoxOutput("DownloadDataInfoBox"),valueBoxOutput("nGSESamples"),valueBoxOutput("nGSEGenes")))
        
        ))) # Table Column
        
        ) # TabPanel Fluid Row
        ), # Download Data tabPanel
    
        tabPanel("BioQC", style = "margin-left :10px; margin-right :10px",
        fluidRow(
        
        column(4, 
        wellPanel(
        fluidRow(
        
        h4("BioQC Options"),
        column(12,
        helpText(
        paste(
          "BioQC performs quality control of high-throughput expression data based on ",
          "tissue gene signatures. It can detect tissue heterogeneity in gene " ,
          "expression data. The core algorithm is a Wilcoxon-Mann-Whitney ",
          "test that is optimised for high performance."))),
        column(12, hr()),
        column(12, actionButton(inputId = "PerformBioQCAnalysis",  label = "Perform BioQC Analysis",  size = "large")),
        column(12, hr()),
        h4("Heatmap Options"),
        column(12, sliderInput(inputId = "NumberOfHeatmapSignatures", label = "Heatmap Number of Signatures to Plot", min = 1, max = 100, value = 10)),
        column(12, uiOutput("BioQCPlotInput_UI") %>% withSpinner(color = "#0dc5c1")),
        column(12, hr()),
        h4("BioQC Profile Options"),
        column(12, uiOutput("BioQProfileInput_UI") %>% withSpinner(color = "#0dc5c1"))
        ) # Input Well Panel Fluid Row
        ) # Input Well Panel
        ), # Input Column
                       
        column(8,
        
        wellPanel(
        fluidRow(
        h4("BioQC Heatmap"),
        column(12,
        fluidRow(
        plotlyOutput(outputId = "BioQCPlot") %>% withSpinner(color = "#0dc5c1")    
        )))
        ),
        
        wellPanel(
        fluidRow(
        h4("BioQC Profile"),
        plotlyOutput(outputId = "BioQCProfilePlot") %>% withSpinner(color = "#0dc5c1")
        )
        )
        
        
        )  # Plot Column 
        )  # Page Fluid Row
        ),  # tabPanel BioQC"
    
        tabPanel(title = "BoxPlots",
        fluidRow(
        column(4,
                 
        wellPanel(
        fluidRow(
        column(12,
                        
        h4('Plot Selection'), 
        column(6, selectizeInput( inputId = "BoxPlot_IndpVar", label = "Independant Variable", choices = c("Sample" = "s", "Gene" = "g"), selected = "g")),
        column(6, selectizeInput(inputId = "BoxPlot_PlotBy", label = "Data to Plot", choices = c("Overall Distribution" = "o", "Factor Distribution" = "f"), selected = "")),
        column(6, uiOutput("BoxPlot_GeneSelect_UI")),
        column(6, uiOutput("BoxPlot_FactorSelect_UI")),
        
        ##########################################################################

        
        column(12, selectizeInput(inputId = "BoxPlot_Type",label = "Plot Type",choices = c("BoxPlot", "Violin Plot", "Histogram"), selected = "BoxPlot")),
        column(12, sliderInput("BoxPlot_nGenes", "Portion of Genes to Sample", min = 1, max = 100, value = 10, step = 10)),
                        
        h4('Plot Options'), 
        column(4,checkboxInput('BoxPlot_showData','Show Data')),
        column(4,checkboxInput('BoxPlot_showDataMean','Show Sample Means')),
        column(4,checkboxInput('BoxPlot_AddWhiskers','Change Whisker Defintion')),
        column(4,checkboxInput('BoxPlot_AddNotches','Add Notches')),
        column(4,checkboxInput('VariableWidth','Variable Width Box')),
        column(4,checkboxInput('BoxPlot_PlotAxisFlip','Axis Flip')),
                        
        conditionalPanel('input.BoxPlot_showData==1', 
        column(12, br(),hr(),h4("Data Point Plotting Options")),
        radioButtons(inputId = "BoxPlot_showDataOption", label = "", choices = c("jitter", "quasirandom", "beeswarm", "tukey"), selected = "jitter",inline = T),
        sliderInput(inputId = "BoxPlot_JitterAlpha", label = "Data Point alpha", min = 0,max = 1,step = 0.1,value = 1),
        sliderInput(inputId = "BoxPlot_JitterWidth", label = "Data Point Plot Area Width", min = 0,max = 2,step = 0.05,value = 0.1)
        )),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showDataMean==1',hr(),
        h4("Sample Mean Options"),             
        radioButtons(inputId = "SampleMeanConfidenceInterval", label = "Define Confidence Interval of Means", choices = list("85%"=0.85, "90%"=0.90, "95%"=0.95, "99%"=0.99), selected = "95%", inline = T)
        )),
                   
        column(12,
        conditionalPanel('input.BoxPlot_AddWhiskers==1',hr(),
        h4("Definition of Whisker Extent"),             
        radioButtons(inputId = "BoxPlot_WhiskerType", label = "", choices = list("Tukey"=0, "Spear"=1, "Altman"=2), selected = 0, inline = T),             
        conditionalPanel('input.BoxPlot_WhiskerType==0', "Tukey - whiskers extend to data points that are less than 1.5 x IQR away from 1st/3rd quartile"),
        conditionalPanel('input.BoxPlot_WhiskerType==1', "Spear - whiskers extend to minimum and maximum values"),
        conditionalPanel('input.BoxPlot_WhiskerType==2', "Altman - whiskers extend to 5th and 95th percentile (use only if n>40)")
        )),
                   
        column(12,hr(),h4('Additional Parameters')),
        
        column(3,checkboxInput('BoxPlot_showColor','Color')),
        column(3,checkboxInput('BoxPlot_showLabs','Labels & Title')),
        column(3,checkboxInput('BoxPlot_showPlotSize','Plot Size')),
        column(3,checkboxInput('BoxPlot_showMargins','Margins')),
        hr(),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showColor==1',
        hr(),
        h4('Color Manipulation'),
        selectInput(inputId = "BoxPlot_ThemeSelect", label = "Select Theme:", choices = c("default","theme_gray", "theme_bw", "theme_light", "theme_dark", "theme_minimal", "theme_classic")),
        sliderInput("BoxPlot_ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
        checkboxInput('BoxPlot_colRngAuto','Auto Color Range',value = T)
        
        )),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showLabs==1',
        hr(),
        h5('Widget Layout'),
        column(4,textInput('BoxPlot_main','Title','')),
        column(4,textInput('BoxPlot_xlab','X Title','Experimental Group')),
        column(4,textInput('BoxPlot_ylab','Y Title','Expression Levels')),
        sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180),
        sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
        )),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showPlotSize==1',
        hr(),
        h4('Plot Size Options'),
        numericInput("BoxPlot_Height", "Plot height:", value=550),
        numericInput("BoxPlot_Width", "Plot width:", value=750)
        )),
        
        column(12,
        conditionalPanel('input.BoxPlot_showMargins==1',
        hr(),
        h4('Plot Margin Options'),
        column(3, numericInput("BoxPlot_margin_top", "Top:", value=0.1, step = 0.1)),
        column(3, numericInput("BoxPlot_margin_bottom", "Bottom:", value=0.4, step = 0.1)),
        column(3, numericInput("BoxPlot_margin_right", "Right:", value=1, step = 0.1)),
        column(3, numericInput("BoxPlot_margin_left", "Left:", value=1, step = 0.1))
        ))))
        ), # BoxPlots tabPanel Input Column

        column(8,
        wellPanel(
        fluidRow(
        column(12, h4("BoxPlot")),
        column(12, plotOutput("BoxPlot_ggplot") %>% withSpinner(color = "#0dc5c1")),
        column(12, hr()),
        
        column(2,actionButton(inputId = "RefreshBoxPlotSample", label = "Resample Genes",icon = icon('refresh'))),
        column(2,actionButton(inputId = "RefreshPlot", label = "Refresh Plot",icon = icon('refresh'))),
        column(2,checkboxInput(inputId = "BoxPlot_ToggleLegend", label = "Show Legend")),
        column(4,checkboxInput(inputId = "BoxPlot_ToggleInteractive", label = "Render Interactive Plot"))
        )
        )  # Well Panel for Plots
        )  # Second Column of Page
        )  # tabPanel Fluid Row
        ), # tabPanel(title = "BoxPlots"
        
        tabPanel(title = "PCA",
        fluidRow(
        column(4,
                 
        wellPanel(
        fluidRow(

        h4("PCA options"),
        column(3, checkboxInput(inputId = "PCA_center",label = "Center Data")),
        column(3, checkboxInput(inputId = "PCA_scale", label = "Scale Data", value = 1)),
        column(3, checkboxInput(inputId = "MakeScree", label = "Scree Plot", value = 1)),
        column(3, checkboxInput(inputId = "MakeLoading", label = "Loadings", value = 1)),
        
        column(6, uiOutput("PCA_GroupUI")),
        column(6, uiOutput("PCA_LabelUI")),
        column(6, uiOutput("PCA_xcomp_UI")),
        column(6, uiOutput("PCA_ycomp_UI")),
        column(12, hr()),
        
        conditionalPanel(condition = "input.MakeScree == 1",
        column(12,h4("Scree Plot Options")),
        column(12,
        sliderInput(inputId = "nCompScree", label = "Number of Components in Scree plot", min = 1, max = 20, value = 5, step = 1),
        sliderInput(inputId = "ScreeYMax", label = "Y Max for Screer Plot", min = 0, max = 1, value = 1, step = 0.1),
        selectInput(inputId = "ScreePlotType", label = "Scree Plot Type", choices = c("pev", "cev"), selected = "pev"),
        conditionalPanel(condition = "input.ScreePlotType == 'pev'",            
        "'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. "),
        conditionalPanel(condition = "input.ScreePlotType == 'cev'",
        "'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.")
        )),
        
        conditionalPanel(condition = "input.MakeLoading == 1",
        column(12,hr()),
        column(12, h4("Loadings Plot Options")),
        column(12, uiOutput("LoadingSelect_UI")),
        column(12, uiOutput("ShowNLoading_UI")))
        ))),
        
        column(6,
        h3("PCA Biplot"),
        plotlyOutput("PCA_BiPlot")%>% withSpinner(color = "#0dc5c1"),
        conditionalPanel(condition = "input.MakeScree == 1",
        h3("Scree Plot"),
        plotOutput("PCA_ScreePlot")%>% withSpinner(color = "#0dc5c1")),
        conditionalPanel("input.MakeLoading == 1",
        plotOutput("PCA_LoadingPlot")%>% withSpinner(color = "#0dc5c1")
        ))

        )  # PCA Tabpanel fluid Row
        )  # PCA Tabpanel
    
    
        )  # Raw Data Statistics tabBox
        )  # DataQC tab Column
        )  # DataQC tab Fluid Row
        ), # DataQC Tab Item
    
        tabItem(
        tabName = "DifferentialAnalysis",
        fluidRow(
            
        column(12, div( id = "GSMMetadataWarning_Exp", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))),
        
        column(12,
        tabBox(title = "Expression Analysis",
        width = 12,
        
        tabPanel(title = "Volcano Plot",
        
        fluidRow(
        column(4,
        wellPanel(
        fluidRow(
        
        h4("Expression Analysis Options"),
        column(12, selectInput(inputId = "DiffExMethod", label = "Difference Expression Analysis Method",choices = c("EdgeR", "Limma", "DESeq2"))),
        column(12, actionButton("SubmitDEA","Perform Differntial Expression Analysis")),
        
        column(12, hr()),
        h4("Significance Threshold"),
        column(12, selectInput(inputId = "MultiTestCorr", label = "Multiple Testing Correction", choices = c("None", "Bonferroni", "Benjamini–Hochberg", "Westfall-Young"))),
        column(12, uiOutput("PValThres")),
        column(12, uiOutput("LogFCThres")),
        column(12, hr()),
        
        h4("Volcano Plot Options"),
        column(12, uiOutput("SelectContrast_UI")),
        column(4, checkboxInput(inputId = "VolacanoPlot_PvalLine", label = "p-value line")),
        column(4, checkboxInput(inputId = "VolacanoPlot_LogLine", label = "logFC line")),
        column(4, checkboxInput(inputId = "VolacanoPlot_labelhitt", label = "label hits")),
        
        column(12,hr(),h4('Additional Parameters')),
        column(3,checkboxInput('VolcanoPlot_showColor','Color')),
        column(3,checkboxInput('VolcanoPlot_showLabs','Labels & Title')),
        column(3,checkboxInput('VolcanoPlot_showPlotSize','Plot Size')),
        column(3,checkboxInput('VolcanoPlot_showMargins','Margins', value = 1)),
                           
        column(12,
        conditionalPanel('input.VolcanoPlot_showColor==1',
        hr(),
        h4('Color Manipulation'),
        selectInput(inputId = "VolcanoPlot_ThemeSelect", label = "Select Theme:", choices = c("default","theme_gray", "theme_bw", "theme_light", "theme_dark", "theme_minimal", "theme_classic")),
        sliderInput("VolcanoPlot_ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
        checkboxInput('VolcanoPlot_colRngAuto','Auto Color Range',value = T)
        )),
                   
        column(12,
        conditionalPanel('input.VolcanoPlot_showLabs==1',
        hr(),
        h4('Labels & Title'),
        column(4,textInput('VolcanoPlot_main','Title','')),
        column(4,textInput('VolcanoPlot_xlab','X Title','')),
        column(4,textInput('VolcanoPlot_ylab','Y Title','')),
        sliderInput('VolcanoPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180),
        sliderInput('VolcanoPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
        )),
                   
        column(12,
        conditionalPanel('input.VolcanoPlot_showPlotSize==1',
        hr(),
        h4('Plot Size Options'),
        numericInput("VolcanoPlot_Height", "Plot height:", value=550),
        numericInput("VolcanoPlot_Width", "Plot width:", value=750)
        )),
        
        column(12,
        conditionalPanel('input.VolcanoPlot_showMargins==1',
        hr(),
        h4('Plot Margin Options'),
        column(3, numericInput("VolcanoPlot_margin_top", "Top:", value=0.1, step = 0.1)),
        column(3, numericInput("VolcanoPlot_margin_bottom", "Bottom:", value=0.4, step = 0.1)),
        column(3, numericInput("VolcanoPlot_margin_right", "Right:", value=1, step = 0.1)),
        column(3, numericInput("VolcanoPlot_margin_left", "Left:", value=1, step = 0.1))
        ))
               
        
        )  # Fluid Row inside well panel
        )  # Input Well Panel
        ),  # Input Column
        
        column(8,
        wellPanel(
        fluidRow(
        h4("Volcano Plot"),
        plotOutput("VolcanoPlot")
        )  # Volcano Plot Fluid Row
        ),  # Volcano Plot Well Panel
                      
        wellPanel(
        fluidRow(
        h4("Factor BoxPlot for selected gene"),
        plotOutput("Volcano_BoxPlot")
        ) # Box Plot Fluid Row
        ) # Box plot Well Panel       
        ) # Plot Panel Column
        )
        ),
        
        tabPanel(title = "Clustering",
        fluidRow(
        column(4,
        wellPanel(
        
        h4('Gene Selection'), 
        column(width=12,sliderInput("nGenes", "Number of Differentially Expressed Genes to Show", min = 1, max = 100, value = 10)),
        column(width=12,selectizeInput("TopTableFilter", "Sort genes by", c("ID","logFC","AveExpr","t","P.Value"),"logFC")),
        h4('Data Preprocessing'),
        
        column(width=4,selectizeInput('transpose','Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
        column(width=4,selectizeInput("transform_fun", "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize',"Missing values"='is.na10', Correlation='cor'),selected = '.')),
        uiOutput('annoVars'),
        column(12, h4('Row dendrogram')),
        column(width=6,selectizeInput("distFun_row", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
        column(width=6,selectizeInput("hclustFun_row", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
        column(width=12,sliderInput("r", "Number of Clusters", min = 1, max = 15, value = 2)),    

        br(),hr(),h4('Column dendrogram'),
        column(width=6,selectizeInput("distFun_col", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
        column(width=6,selectizeInput("hclustFun_col", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
        column(width=12,sliderInput("c", "Number of Clusters", min = 1, max = 15, value = 2)),
                                                 
        br(),hr(),  h4('Additional Parameters'),
                                                 
        column(3,checkboxInput('showColor','Color', value = T)),
        column(3,checkboxInput('showMargin','Layout')),
        column(3,checkboxInput('showDendo','Dendrogram')),
        hr(),
        conditionalPanel('input.showColor==1',
        hr(),
        h4('Color Manipulation'),
        uiOutput('colUI'),
        sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
        checkboxInput('colRngAuto','Auto Color Range',value = T),
        conditionalPanel('!input.colRngAuto',uiOutput('colRng'))
        ),
                                                 
        conditionalPanel('input.showDendo==1',
        hr(),
        h4('Dendrogram Manipulation'),
        selectInput('dendrogram','Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
        selectizeInput("seriation", "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
        sliderInput('branches_lwd','Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
        ),         
                                                 
        conditionalPanel('input.showMargin==1',
        hr(),
        h4('Widget Layout'),
        column(4,textInput('main','Title','')),
        column(4,textInput('xlab','X Title','')),
        column(4,textInput('ylab','Y Title','')),
        sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
        sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180),
        sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
        sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40)
        )
        )
        ),
                                        
        column(8,
        wellPanel(
        tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, icon("clone"), 'Download Heatmap as HTML'),
        tags$head(tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
        plotlyOutput("heatout",height='600px')
        )
        )
        )
        ), #tabPanel
        
        tabPanel("Top Table", 
        fluidRow(
        column(4,
        
        wellPanel(
        fluidRow(
        
        h4("Differential Expression")
        
        )
        )
        ),  # Input Well Panel Columns
        
        column(8,
        wellPanel(
        fluidRow(
        uiOutput("RenderTopTable")
            
        )  # Well Panel Fluid Row
        )  # Well Panel
        )  # Table half of Page column
        )  # Fluid Row for the Page
        )
        
        
        ) # Expression Analysis
        ) # Second Column of the Page
        ) # Fluid Row that Makes up the page
        ) # Differential Analysis Tabitem
    
    
    
    
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
    
    ExperimentalDesign <- reactiveValues()
    ExperimentalDesign$ControlFactorDF <- reactive({
        ControlFactorDF <- read.csv(file = "~/GeoWizard/TestObjects/GSE69967_FactorDF.csv")
        ControlFactorDF
    })
    
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
            FactorGMT.sampled <- sampleFactorGMT(FactorGMT = GSEdata$FactorGMT(),
                nFactors = ncol(ExperimentalDesign$ControlFactorDF()),
                SpecificGenes = input$BoxPlot_GeneSelect,
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
        
        #'
        #'
        #'
        #'
        #'
        output$DownloadDataInfoBox <- renderValueBox({
        shiny::req( input$GsmTableSelect,input$GplTableSelect)
        GSE <- input$GsmTableSelect
        GPL  <- input$GplTableSelect
        valueBox(value = GSE, subtitle = paste("Design and Contrast Matrix for", GPL),icon = icon('check-circle'),color = "blue")
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
        #'
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


shinyApp(ui = ui, server = server)


