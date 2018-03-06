GeoWizard <- "~/GeoWizard"
GeoRepo <- "~/GeoWizard/GeoRepo"
setwd(GeoWizard)

## app.R ##
if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')

appCSS <- "
loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}

body {
  font-family: 'Roboto', sans-serif;
}


large .selectize-input { line-height: 40px; }
large .selectize-dropdown { line-height: 30px; }

.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}

.box { margin-bottom: 10; } 

[class*='col-lg-'],[class*='col-md-'],
[class*='col-sm-'],[class*='col-xs-']{
  padding-right:5 !important;
  padding-left:5 !important;
}


.gly-spin {
  -webkit-animation: spin 2s infinite linear;
-moz-animation: spin 2s infinite linear;
-o-animation: spin 2s infinite linear;
animation: spin 2s infinite linear;
}

@-moz-keyframes spin {
0% {
-moz-transform: rotate(0deg);
}
100% {
-moz-transform: rotate(359deg);
}
}
@-webkit-keyframes spin {
0% {
-webkit-transform: rotate(0deg);
}
100% {
-webkit-transform: rotate(359deg);
}
}
@-o-keyframes spin {
0% {
-o-transform: rotate(0deg);
}
100% {
-o-transform: rotate(359deg);
}
}
@keyframes spin {
0% {
-webkit-transform: rotate(0deg);
transform: rotate(0deg);
}
100% {
-webkit-transform: rotate(359deg);
transform: rotate(359deg);
}
}



.spinner {
  margin: 100px auto;
  width: 50px;
  height: 40px;
  text-align: center;
  font-size: 10px;
}

.spinner > div {
  background-color: #333;
  height: 100%;
  width: 6px;
  display: inline-block;
  
  -webkit-animation: sk-stretchdelay 1.2s infinite ease-in-out;
  animation: sk-stretchdelay 1.2s infinite ease-in-out;
}

.spinner .rect2 {
  -webkit-animation-delay: -1.1s;
  animation-delay: -1.1s;
}

.spinner .rect3 {
  -webkit-animation-delay: -1.0s;
  animation-delay: -1.0s;
}

.spinner .rect4 {
  -webkit-animation-delay: -0.9s;
  animation-delay: -0.9s;
}

.spinner .rect5 {
  -webkit-animation-delay: -0.8s;
  animation-delay: -0.8s;
}

@-webkit-keyframes sk-stretchdelay {
  0%, 40%, 100% { -webkit-transform: scaleY(0.4) }  
  20% { -webkit-transform: scaleY(1.0) }
}

@keyframes sk-stretchdelay {
  0%, 40%, 100% { 
    transform: scaleY(0.4);
    -webkit-transform: scaleY(0.4);
  }  20% { 
    transform: scaleY(1.0);
    -webkit-transform: scaleY(1.0);
  }
}

"


ui <- dashboardPage(
     dashboardHeader(title = "Roche's GeoWizard"),
     
     ## Sidebar content
     dashboardSidebar(
        sidebarMenu(
        id = "TabSet",
        menuItem("Query Datasets",tabName = "GSESummary",icon = icon("search")),
        menuItem("Filter GSM Metadata", tabName = "GSMMetadata", icon = icon("filter")),
        menuItem("Design and Contrast Matrix", icon = icon("th"), tabName = "DesignMatrix"),
        menuItem("Download and QC", tabName = "DataQC", icon = icon("download")),
        menuItem("Expression Analysis", tabName = "DifferentialAnalysis", icon = icon("bar-chart")),
        menuItem("Export and Save", tabName = "ReverseSerach", icon = icon("database")),
        menuItem("Contact and Citations", tabName = "Contact", icon = icon("phone"))
        )
     ),
     
    ## Body content
    dashboardBody(
        inlineCSS(appCSS),
        useShinyjs(),
        tabItems(
        
        tabItem(
        tabName = "GSESummary",
                         
        fluidRow(
        box(width = 4,
          status = 'warning',
          title = "Inputs for GEO Query",
          solidHeader = TRUE,
        
        fluidRow(
        column(6,uiOutput('MolSelectFromLibrary')),
        
        column(6,
        selectInput(
        inputId = "MolSelectFilter",
        label = "Keyword Libraries",
        choices = c("David's TA Molecules" = "DAVID", "Text Input" = "TextInput",  "FDA approved Drugs" = "FDA", "SPARK" = "SPARK Library"),
        selected = "DAVID")), #selectInput
        
        column(6),
        # actionButton(inputId = "StructureSerach",
        #     label = HTML("Retrieve Datasets for<br />Related Chemotypes<br />"),
        #     class = "btn-warning",
        #     width = '100%')),
        
        column(6,
        actionButton(
        inputId = "MolSelectButton",
        label = HTML("Retrieve Datasets<br />for Selected Molecules<br />"),
        class = "btn-warning",width = '100%'))
        )    # Box Fluid Row    
        ),    # Box - Inputs for GEO Query
        
        # Inputs for Geo Query Box
        box(
          title = "Filter Search Results",
          solidHeader = TRUE, 
          status = "warning",
          width = 8,
        
        fluidRow(
        column(3,
        sliderInput(inputId = "SampleSizeSlider", 
          label = "Min Sample Size", 
          value = 1, 
          min = 1, 
          max = 300 
        )
        ),
                                       
        column(3,
        checkboxGroupInput(
        inputId = "ExpTypes",
        label = "Perturbation Types  to Query",
        choices =  c("Small Molecule Perturbations", "Biological Marcro Molecuels","Genetic Perturbations"),
        selected = c("Small Molecule Perturbations", "Biological Marcro Molecuels","Genetic Perturbations")
        )
        ),
        
        column(3, uiOutput("TaxonSelection")),
        column(3, radioButtons( inputId = "FullSummaryCheck", label = "Show full Summary", choices = c("Full Text", "Truncated"), selected = "Truncated", inline = F))
        ), # Fluid Row - Box
        br()) # Filter Seacrh Results Box
        ), #First Fluid Row, Summary, Filter and Infor Box
        
        fluidRow(
        tabBox(
        side = "right",
        title = "Geo Search Result Plots",
        id = "PlotTabSet", 
        height = "500px",
        width = 4,
        
        tabPanel("taxon",
        status = "primary", 
        plotOutput("nStudiesPlotTaxon") %>% withSpinner(color = "#0dc5c1")),
                                 
        tabPanel("gdsType",
        plotOutput("nStudiesPlotGdsType") %>% withSpinner(color = "#0dc5c1"))
        ), #Plot Type Tabbox

        box(status = "primary", 
        solidHeader = TRUE,
        title = "Table of Datasets Matching Keywords",
        width = 8,
        height = '500px',
        
        fluidRow(
        style = "margin-left :10px; margin-right :10px",
        DT::dataTableOutput("GseSummaryData")
        )
        ) # Datatable Box
        ), # Fluid Row Table and Plot
                         
        fluidRow(
        infoBoxOutput('nTotalStudiesIndicator'),
        infoBoxOutput("nStudiesSelectedIndicator"),
        column(1,
        
        fluidRow(
        actionButton(
        inputId = "AnalyzeSelectedDatasets",
        label = "Analyze Selected Datasets  ",
        icon = icon("arrow-right"),
        style='padding:4px; font-size:200%; font-weight: bold; color:#0573B7'
        )
        ),
        
        fluidRow(
        actionButton(
        inputId = "ExpressAnalyse",
        label = HTML("Automated Express Analysis"),
        icon = icon("angle-double-right", lib = "font-awesome"),
        style='padding:4px; font-size:200%; font-weight: bold; color:#0573B7'
        )
        )
        ),
        
        column(1,
        conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>')
        )
        )
        )
        ), # tabItem - GSESummary 
              
        tabItem(tabName = "GSMMetadata",
        fluidRow(
        
        # Warning Bar when so inputs on GSEtable are selected 
        column(12, div( id = "GSMMetadataWarning", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))),
        
        
        column(12,
        div( id = "GSMMetadataLoading",
        conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        box(title = "GSE Loading", solidHeader = T, width = 12, background = "yellow",
        fluidRow(column(8, offset = 2, h1(icon("exclamation-triangle"), "Please wait while data is retrieved data from GEO"))),
        HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>')
        )
        )
        ), # GSE Search data Loading Message
           
        
        column(8,
        fluidRow(
        column(12,
        box(title = "Dataset Selection",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        
        fluidRow(
        column(2, uiOutput('GseTabletoKeep_UI')),
        column(2, uiOutput('GseTabletoAnalyze_UI')),
        column(2, uiOutput("GplTabletoAnalyze_UI")),
        #column(3, hr()),
        column(6,uiOutput("infobox_selectedGSE"))
        )
        )
        ), # Top Row
        
        column(6,
        box(title = "Metadata columns with Experimental Variables",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        div(
        class = "overflowbox",
        
        checkboxGroupInput(
        inputId = "WhereVarData",
        label = "Select columns useful experimental\nfactor levels can be found",
        choices =  c("gsm.title","description","characteristics_ch1"),
        selected = c("characteristics_ch1","gsm.title"),
        inline = T
        ),
                          
        fluidRow(
        style="margin-left :10px; margin-right :10px", 
        dataTableOutput("GseGsmTable")
        )
        )
        )
        ), # Column Dataselction
                    
        column(6,
        box(title = "Select variables to use",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        div(
        class = "overflowbox",
        
        fluidRow(
        style="margin-left :10px; margin-right :10px",
        uiOutput("PickFactorColumns")
        
        )
        )
        ),
        
        box(title = "Classification Results",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        uiOutput("FactorRenameAndClass_UI")
        ),
        
        box(title = "Filter Factor Levels",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        div(
        class = "overflowbox",
        
        fluidRow(
        style="margin-left :5px; margin-right :5px",
        uiOutput(outputId = "FilterGSMbyFactor")
        ),  
                              
        fluidRow(
        style="margin-left :5px; margin-right :5px",
        
        actionButton(
        inputId = 'FilterGSMLevels',
        label = "Reset Filter",
        class = "btn-primary"
        ),
                                 
        actionButton(
        inputId = "HideGSMLevelsFilter",
        label = 'Hide Filter Boxes',
        class = "btn-primary"
        )
        )
        )
        )
        ) # Column Classifications
        ) # Frist Two Thids of Page Fluid flow
        
        ), # Column | X | X |   |
        
        column(4, # Column |   |   | X |
               
        tabBox(title = "Experimental Factors",
        width = 12,
        side = "right",
        selected = "All Factor Levels",
        
        tabPanel("Ignored Factor Levels", 
        fluidPage(
        column(12, helpText(paste(
            "These factor leves are not used to construct the Design and contrast matricies",
            "but they are stored as meta data tags for retrieving data from a database used",
            "to store differential expression analysis results"))),
        column(12, DT::dataTableOutput("ExcludedFactorTable") %>% withSpinner(color = "#0dc5c1")),
        column(4, actionButton("RefreshExcludedFactorTable", "Refresh Table"))
        )
        ),
        
        tabPanel("All Factor Levels", 
        fluidRow(
        column(12, DT::dataTableOutput("FullFactorTable") %>% withSpinner(color = "#0dc5c1")),
        column(4, actionButton("RefreshFullFactorTable", "Refresh Table"))
        )
        )
        ),
                         
        box(title = "Make Design and Contrast Matrix",
        solidHeader = T,
        status = 'danger',
        background = 'red',
        width = 12,
        actionButton("GoToDesignPage", "Use factors for analysis", width = "100%")
        )  # Box - GoToDesignPageBox
        )  # Column - Last third of the Page
        )  # Page Fluid Row containing the Column Layoutdat
        )  # GSmMetaDataPage div
        ), # TabItem - GSMMetadata
              
        tabItem(tabName = "DesignMatrix",

        fluidRow(
        column(12, div( id = "GSMMetadataWarning_Design", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))), 

        column(4,
        box(title = "Summary of Factors",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
        dataTableOutput("DesignMat_SummaryTable")%>% withSpinner(color = "#0dc5c1")
        )  # Box
        ), # First Page Column
                       
        column(4,
        box(title = "Design Matrix Inputs", 
        width = 12,
        solidHeader = T,
        status = "primary",
        collapsible = T,
        
        fluidRow(
        column(12,
               
        column(12,textInput( inputId = "formulaInputDesign", label = "Model Matrix Formula Input", placeholder = "~ Expvar1 + Expvar2")),
        column(12, p("The detected baseline or control level for each factor can be changed below")),
        column(12, uiOutput("RearrangeLevels")),
        column(12, actionButton(inputId = "SubmitFormula", label = "Generate Design Matrix"))
        )  # Design Matrix Box spanning Column
        )  # Design Matrix Box Fluid Row
        ), # Design Matrix Box
        
        box(title = "Contrast Matrix Inputs", 
        width = 12,
        solidHeader = T,
        status = "primary",
        collapsible = T,
        
        fluidRow(
        column(12,
        p("How should expression levels be compared between groups?"),
        column(12, checkboxInput(inputId = "ContrastLevels", "Contrasts between experimental variable factor levels", width = "100%", value = T)),
        column(12, checkboxInput(inputId = "ContrastInteractions", "Interaction term of differtial expression in bewteen two factor levels")),
        column(12, checkboxInput(inputId = "ContrastCustom", "Add custom contrast input")),
        conditionalPanel('input.ContrastCustom==1',
        column(12, textInput(inputId = "CustomContrastInput",label = "Custom Contrast Input"))),
        column(12, actionButton(inputId = "SubmitContrasts", label = "Generate Contrast Matrix"))
        
        )  # Contrast Matrix Box Spanning Column
        )  # Contrast Matrix Box Fluid Row
        )  # Contrast Matrix Box
        ), # Second Page Column
                          
        column(4,
        tabBox(
        width = 12,
        tabPanel( title = "Blocks", 
        fluidRow(
        column(12, h4("Experimental Blocks")),
        column(12, wellPanel(plotOutput("ExperimentalBlocksPlot") %>% withSpinner(color = "#0dc5c1"))),
        column(12, hr(),h4("Differential Expression Contrasts")),
        
        
        
        conditionalPanel('input.ContrastLevels==1',
        column(12, wellPanel(fluidRow(
        column(12, helpText(paste("Compare expression between samples from different levels in an experimental factor.",
                                  "Example: liver cells vs. liver cells + 1mg ibuprofen"))),
        column(12, img(src='Contrast1.png', align = "left", width = "95%")))))),
        
        conditionalPanel('input.ContrastInteractions==1',
        column(12, wellPanel(fluidRow(
        column(12, helpText(paste(
            "Diffential expression analysis between control and perturbation in different conditions.",
            "Example: liver cells vs. liver cell + 1mg ibuprofen, incubated for 1 hour vs. 12 hours"))),
        column(12, img(src='Contrast2.png', align = "left", width = "95%"))))))
        )
        ),
        
        tabPanel(
        title = "Design Matrix", 
        fluidRow(column(12,
        h4("Design Matrix"),
        
        column(12, wellPanel(fluidRow(DT::dataTableOutput(outputId = 'CustomExpressionTable') %>% withSpinner(color = "#0dc5c1")))),
        hr(),h4("Rename Design Factor Columns"),
        column(12, uiOutput("DesignMatrixRename_UI"))
        )
        )
        ),
        
        tabPanel(
        title = "Contrast Matrix",
        h4("Contrast Matrix"),
        fluidRow(column(12,
        column(12, wellPanel(fluidRow(DT::dataTableOutput(outputId="ContrastMatrixTable") %>% withSpinner(color = "#0dc5c1"))))
        )
        )
        )
        )
        )  # Third Page Column
        
        )  # FluidRow that Structures page into 3 Columns
        ), # Design Matix TabItem 
        
        tabItem(
        tabName = "DataQC",
        fluidRow(
        column(12, div( id = "GSMMetadataWarning_Down", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))), 
        
        column(10,
               
        tabBox(title = "Raw Data Statistics",width = 12,
        tabPanel("Download Data",
        fluidRow(
        column(4,
        
        fluidRow(
        column(12,
               
        wellPanel(
        h4("GMT File"),
        width = 12,
        solidHeader = T,
        status = "primary",
        
        column(12,renderUI("GMTtoDownloadorUpload_UI")),
        actionButton(
          inputId = "DownloadGEOData",
          label = "Download",
          icon = icon("download"),
          block = T, width = 12),
        hr(),
        radioButtons(inputId = "ExpressionDataType",
            label = "Gene Expression Data Type",
            choiceNames = c("MicroArray", "NGS Sequencing", "Single Cell Sequencing"),
            choiceValues = c("mArray", "RNAseq", "ssRNAseq"),
            selected = "mArray"),
        br(),
        uiOutput("GeneAnnotationTypeUI"),
        br(),
        radioButtons(
            inputId = "RawDataTableMelt", 
            label = "Which Data Matrix to Show" , 
            choiceNames = c("Gene Matrix","Melted Gene Matrix with Factors"),
            choiceValues = c("GMT", "FactorGMTMelt"), 
            inline = T)
        
        
        )  # Well Panel
        
        
        )  # InputPanel Spanning Column
        )  # InputPenal Spanning FluidRow
        ), # First Column of TabIteM
                      
        column(8,
        
        h4("Datatable"),
        hr(),
        DT::dataTableOutput("RawDataQC") %>% withSpinner(color = "#0dc5c1")
        )  # Raw Data Column 2
        )  # Raw Data Tabpage Fluid Row
        ),  # tabPanelView/Save Raw Data          
        
                  
        tabPanel(title = "Boxplots",
        fluidRow(
        column(4,
                 
        wellPanel(
        fluidRow(
        column(12,
                        
        h4('Plot Selection'), 
        column(6, selectizeInput( inputId = "BoxPlot_IndpVar", label = "Independant Variable", choices = c("Sample", "Gene"), selected = "")),
        column(6, selectizeInput(inputId = "BoxPlot_PlotBy", label = "Data to Plot", choices = c("Overall Distribution", "Factor Distribution"), selected = "Overall Distribution" )),
        column(12, uiOutput("BoxFactorSelect")),
        column(12, selectizeInput(inputId = "BoxPlot_Type",label = "Plot Type",choices = c("Boxplot", "Violin Plot", "Line Plot"), selected = "Boxplot")),
        column(12, sliderInput("BoxPlot_nGenes", "Number of Genes to Sample", min = 1, max = 100, value = 10)),
                        
        h4('Plot Options'), 
        column(4,checkboxInput('BoxPlot_showData','Show Data')),
        column(4,checkboxInput('BoxPlot_showDataMean','Show Sample Means')),
        column(4,checkboxInput('BoxPlot_AddWhiskers','Change Whisker Defintion')),
        column(4,checkboxInput('BoxPlot_AddNotches','Add Notches')),
        column(4,checkboxInput('VariableWidth','Variable Width Box')),
        column(4,checkboxInput('BoxPlot_PlotAxisFlip','Axis Flip')),
                        
        conditionalPanel('input.BoxPlot_showData==1', 
        column(12, br(),hr()),
        h4("Data Point Plotting Options"),             
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
        
        column(4,checkboxInput('BoxPlot_showColor','Color')),
        column(4,checkboxInput('BoxPlot_showMargin','Labels and Title')),
        column(4,checkboxInput('BoxPlot_showPlotSize','Plot Size')),
        hr(),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showColor==1',
        hr(),
        h4('Color Manipulation'),
        sliderInput("BoxPlot_ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
        checkboxInput('BoxPlot_colRngAuto','Auto Color Range',value = T)
        
        )),
                   
        column(12,
        conditionalPanel('input.BoxPlot_showMargin==1',
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
        ))
                   
        ))),
            
        column(6,
        fluidRow(
        column(12, uiOutput("BoxPlotUI") %>% withSpinner(color = "#0dc5c1")),
        column(4, actionButton(inputId = "RefreshPlot", label = "Refresh Plot",icon = icon('refresh')))
        ))
        )
        ), # tabPanel(title = "Boxplots"
                          
        tabPanel("Histogram",
        fluidRow(
        column(4,     
        wellPanel(
        radioButtons(inputId = "HistPlotType",
          label = "BoxPlotBy",
          inline = F,
          choices = c("Sample", "Gene", "Factor"),
          selected = "Sample"
        ),
                            
        uiOutput("HistFactorSelect"),
        numericInput(inputId = "HistSampleSize",
          label = "Number of GSM or Gene's to Sample",
            value = 10,
            min = 1,
            step = 10
        )
        ) # Input Well Panel
        ),# Input Column
                            
        column(8,
        plotOutput("HistPlotGMT") %>% withSpinner(color = "#0dc5c1")
        )  # Plot Column
        )  # tabPanel Fluid Row
        ), # tabPanel("Histogram"
                          
        tabPanel(title = "PCA",
         
        column(6, 
        plotOutput("PCA")%>% withSpinner(color = "#0dc5c1")
        ),
        
        column(6,
        plotOutput("CA")%>% withSpinner(color = "#0dc5c1")
        ),
        
        fluidRow(
        column(6,
        plotOutput("Scree")%>% withSpinner(color = "#0dc5c1")
        ),
        
        column(6,
        plotOutput("Cont")%>% withSpinner(color = "#0dc5c1")
        )
        )
        ),
                       
        tabPanel("BioQC", style = "margin-left :10px; margin-right :10px",
                       
        fluidRow(
        column(4,     
        wellPanel(
        actionButton(inputId = "PerformBioQCAnalysis", 
          label = "Perform BioQC Analysis", 
          size = "large"
        ),
                       
        helpText(
        paste(
          "BioQC performs quality control of high-throughput expression data based on ",
          "tissue gene signatures. It can detect tissue heterogeneity in gene " ,
          "expression data. The core algorithm is a Wilcoxon-Mann-Whitney ",
          "test that is optimised for high performance."))
        
        )  # Input Well Panel
        ), # Input Column
                       
        column(8,
        plotOutput(outputId = "BioQCPlot") %>% withSpinner(color = "#0dc5c1")
        )  # Plot Column 
        )  # Page Fluid Row
        )  # tabPanel BioQC"
        
        
        )  # tabBox(title = "Raw Data Statistics",width = 12
        )  # FluidRow
        )  # Column(10
        ),  #tabItem( tabName = "DataQC"
      

        
        tabItem(tabName = "DifferentialAnalysis",
        fluidRow(
            
        column(12, div( id = "GSMMetadataWarning_Exp", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))),
        
        column(12,
        tabBox(title = "Expression Analysis",
          width = 12,
        tabPanel("Top Table", 
        uiOutput("RenderTopTable")
        ),
              
        tabPanel(title = "Volcano Plot",
        wellPanel(
        fluidRow(
        column(4,
        actionButton("SubmitDEA","Perform Differntial Expression Analysis")
        ),
                                       
        column(4,
        uiOutput("PValThres")
        ),
        
        column(4,
        uiOutput("LogFCThres")
        )
        )
        ),
        
        fluidRow(
        column(12,
        plotOutput("VolcanoPlot")
        )
        )
        ),
                                
        tabPanel(title = "MA Plot",
        fluidRow(
        column(4,
        wellPanel(
        uiOutput("MALogFCThres")
        )
        ),
              
        column(12,
        plotOutput("MAPlot")
        )
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
                                                 
        br(),hr(),h4('Row dendrogram'),
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
                                
        tabPanel("BoxPlot", 
        uiOutput("EABoxPlotOptions"),
        plotOutput("EABoxPlot")
        )
        
        ) # Expression Analysis
        ) # Second Column of the Page
        ) # Fluid Row that Makes up the page
        ),# Differential Analysis Tabitem
        
        tabItem(
        tabName = "Contact",
        fluidRow(
        
        column(4,
        box(title = "Contact Information",
            status = "warning",
            solidHeader = T,
            width = 12,
            
        fluidRow(
        column(12, h3("Moaraj Hasan")),
        column(12, img(src='moaraj.jpg', align = "left", width = "75%")),
        column(12, "Questions, concerns, coffee? Feel free to get in touch."),
        column(12, p('Developer and Maintainer: Moaraj Hasan')),
        column(12, p('GitRepo: https://github.com/moaraj/GeoWizard'))
        )
        )
        ), # First Column of Page
        
        column(4,
        box(title = "Citations",
            status = "warning",
            solidHeader = T,
            width = 12,
        fluidRow(
        column(12, h3("Citations")),
        column(12, h4("Filter GSM Meta Page")),
        column(12, h4("Design Matrix")),
        column(12, h4("Download and QC")),
        column(12, h4("Expression Analysis")),
        column(12, h4("Export and Save"))
        )
        )
        ),# Second Column of Page
        
        column(4,
        box(title = "Acknowledgements",
            status = "warning",
            solidHeader = T,
            width = 12,
        fluidRow(
        column(12, h3("Roche BEDA Team")),
        column(12, h3("David")),
        column(12, h3("Martin")),
        column(12, h3("Nikos")),
        column(12, h3("Tony"))
        )
        )
        ) # Third Column of Page
        
        ) # Page Fluid Row
        ) # Contact Tab Item
        
        
        ) # tabItems
        ) # DashboardBody
        ) # Dashboard Page
