shinyUI(
    dashboardPage(
    dashboardHeader(),
    dashboardSidebar(sidebarMenu(
    menuItem("Download and QC", tabName = "DataQC", icon = icon("download")),
    menuItem("Expression Analysis", tabName = "DifferentialAnalysis", icon = icon("bar-chart")))),
    dashboardBody(
        
        
    tabItems(
        
        tabItem( tabName = "DataQC",
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
        h4("Data Source"),
        column(12, uiOutput("InputSourceGMT")),
        column(12, tags$hr()),
                                 
        conditionalPanel('input.DataSourceSelection==1',
        h4("GEO Accession"),
        column(12,
        column(6,textInput(inputId = "GsmTableSelect",label = "GSE input", value = "GSE69967")),
        column(6,textInput(inputId = "GplTableSelect",label = "GPL input", value = "GPL")),
        column(12,actionButton( inputId = "DownloadGEOData", label = "Download", icon = icon("download"), block = T)),
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
        column(12,valueBoxOutput("nGSESamples"),valueBoxOutput("nGSEGenes")))
        
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
        plotlyOutput(outputId = "BioQCPlot") %>% withSpinner(color = "#0dc5c1")
        )
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
    
        tabPanel(title = "Boxplots",
        fluidRow(
        column(4,
                 
        wellPanel(
        fluidRow(
        column(12,
                        
        h4('Plot Selection'), 
        column(6, selectizeInput( inputId = "BoxPlot_IndpVar", label = "Independant Variable", choices = c("Sample" = "s", "Gene" = "g"), selected = "")),
        column(6, selectizeInput(inputId = "BoxPlot_PlotBy", label = "Data to Plot", choices = c("Overall Distribution" = "o", "Factor Distribution" = "f"), selected = "1")),
        column(6, uiOutput("BoxFactorSelect_UI")),
        
        ##########################################################################

        
        column(12, selectizeInput(inputId = "BoxPlot_Type",label = "Plot Type",choices = c("Boxplot", "Violin Plot", "Histogram"), selected = "Boxplot")),
        column(12, sliderInput("BoxPlot_nGenes", "Portion of Genes to Sample", min = 1, max = 1000, value = 100, step = 100)),
                        
        h4('Plot Options'), 
        column(4,checkboxInput('BoxPlot_showData','Show Data')),
        column(4,checkboxInput('BoxPlot_showDataMean','Show Sample Means')),
        column(4,checkboxInput('BoxPlot_AddWhiskers','Change Whisker Defintion')),
        column(4,checkboxInput('BoxPlot_AddNotches','Add Notches')),
        column(4,checkboxInput('VariableWidth','Variable Width Box')),
        column(4,checkboxInput('BoxPlot_PlotAxisFlip','Axis Flip')),
                        
        conditionalPanel('input.BoxPlot_showData==1', 
        column(12, br(),hr(),h4("Data Point Plotting Options")),
        radioButtons(inputId = "BoxPlot_showDataOption", label = "", choices = c("jitter", "quasirandom", "beeswarm", "tukey", "frowney", "smiley"), selected = "jitter",inline = T),
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
        column(3,checkboxInput('BoxPlot_showMargins','Margins', value = 1)),
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
        column(4,textInput('BoxPlot_xlab','X Title','')),
        column(4,textInput('BoxPlot_ylab','Y Title','')),
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
        ), # Boxplots tabPanel Input Column

        column(8,
        wellPanel(
        fluidRow(
        column(12, h4("Boxplot")),
        column(12, plotOutput("BoxPlot") %>% withSpinner(color = "#0dc5c1")),
        column(12, hr()),
        column(4, actionButton(inputId = "RefreshPlot", label = "Refresh Plot",icon = icon('refresh'))),
        column(4, checkboxInput(inputId = "BoxPlot_ToggleInteractive", label = "Render Interactive Plot"))
        ))))
        ), # tabPanel(title = "Boxplots"
    
        
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