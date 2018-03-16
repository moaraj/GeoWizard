enableBookmarking(store = "url")
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
        menuItem("Export and Save", tabName = "ExportSave", icon = icon("database")),
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
        label = "GEO Query Type",
        choices = c("GEO Series Accession" = "Accession", "Test Molecules" = "DAVID", "Text Input" = "TextInput",  "FDA approved Drugs" = "FDA", "SPARK" = "SPARK Library"),
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
        choices =  c("Small Molecule Perturbations", "Biological Macro Molecules","Genetic Perturbations"),
        selected = c("Small Molecule Perturbations", "Biological Macro Molecules","Genetic Perturbations")
        )
        ),
        
        column(3, uiOutput("TaxonSelection")),
        column(2, radioButtons( inputId = "FullSummaryCheck", label = "Show full Summary", choices = c("Full Text", "Truncated"), selected = "Truncated", inline = F)),
        column(1, shiny::actionButton(inputId='YouTube1', label="", style='padding:4px; font-size:200%;',
        icon = icon("question-circle"), width = '100%' , onclick ="window.open('https://www.youtube.com/watch?v=bhjrIexvJW4&index=1&t=0s&list=PL5P7savB6d69_JokGNpwGm0mZX_6aXmes', '_blank')"))
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
        
        tabPanel("taxon", status = "primary", shinyjs::hidden(div(id="SearchResPlot1", plotOutput("nStudiesPlotTaxon") %>% withSpinner(color = "#0dc5c1")))),
        tabPanel("gdsType", shinyjs::hidden(div( id="SearchResPlot2", plotOutput("nStudiesPlotGdsType") %>% withSpinner(color = "#0dc5c1"))))), #Plot Type Tabbox

        box(status = "primary", 
        solidHeader = TRUE,
        title = "Table of Datasets Matching Keywords",
        width = 8,
        height = '500px',
        
        fluidRow( column(12, DT::dataTableOutput("GseSummaryData")))
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
        column(12, 
        # Warning Bar when so inputs on GSEtable are selected 
        
        
        column(12,
        fluidRow(
        column(12,
        div(id = "GSMMetadataWarning", 
        box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))),
        
        
        column(12,fluidRow( column(12,div(id = "GSMMetadataLoading",
        conditionalPanel(
        condition="$('html').hasClass('shiny-busy')",
        box(title = "GSE Loading", solidHeader = T, width = 12, background = "yellow",
        fluidRow(column(8, offset = 2, h1(icon("exclamation-triangle"), "Please wait while data is retrieved from GEO"))),
        HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>'))))))), # GSE Search data Loading Message
           
        
        column(8, # Column | X | X |   |
        fluidRow( 
        column(12,
        box(title = "Dataset Selection",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        
        fluidRow(
        column(2, uiOutput('GseTabletoAnalyze_UI')),
        column(2, uiOutput("GplTabletoAnalyze_UI")),
        column(8,uiOutput("infobox_selectedGSE")))
        )
        ), # Top Row
        
        column(6,
        box(title = "Metadata columns with Experimental Variables",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        
        checkboxGroupInput(
        inputId = "WhereVarData",
        label = "Select columns useful experimental\nfactor levels can be found",
        choices =  c("gsm.title","description","characteristics_ch1"),
        selected = c("characteristics_ch1"),
        inline = T
        ),
                          
        fluidRow(column(12, dataTableOutput("GseGsmTable")))
        )
        ), # Column Dataselction
                    
        column(6,
        box(title = "Select variables to use",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        
        fluidRow(
        column(12, uiOutput("PickFactorColumns")))),
        
        box(title = "Classification Results",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        uiOutput("FactorRenameAndClass_UI")
        ),
        
        box(title = "Filter Rows and Factor Levels",
        solidHeader = T,
        status = "primary",
        width = 12,
        collapsible = T,
        height = "50%",
        
        fluidRow(column(12,
        column(12, uiOutput("RemoveSpecificRows_UI")),
        column(12 ,uiOutput(outputId = "FilterGSMbyFactor")),
        column(4, actionButton( inputId = 'FilterGSMLevels', label = "Reset Filter", class = "btn-primary")),
        column(4, actionButton( inputId = "HideGSMLevelsFilter", label = 'Hide Filter Boxes', class = "btn-primary"))))) # Filter factor level box
        
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
        column(12, hr()),
        column(4, actionButton("RefreshFullFactorTable", "Refresh Table"))
        )
        )
        ),
                         
        box(title = "Make Design and Contrast Matrix",
        solidHeader = T,
        status = 'danger',
        background = 'red',
        width = 12,
        
        fluidRow(
        column(6, actionButton("GoToDesignPage", "Use factors for analysis", width = "100%")),
        column(4, bookmarkButton()),
        column(2, shiny::actionButton(inputId='YouTube2', label="", style='padding:4px; font-size:100%;',
        icon = icon("question-circle"), width = '100%' , onclick ="https://www.youtube.com/watch?v=7DG7a4mei3o&list=PL5P7savB6d69_JokGNpwGm0mZX_6aXmes&index=2', '_blank')"))
        
        )
        )  # Box - GoToDesignPageBox
        
        )  # Column - Last third of the Page
        )  # Page Fluid Row containing the Column Layoutdat
        )  # GSmMetaDataPage div
        ), # TabItem - GSMMetadata
              
        tabItem(tabName = "DesignMatrix",

        fluidRow(
        column(12, 
        div( id = "GSMMetadataWarning_Design", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        fluidRow( column(12, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))), 

        column(4,
        box(title = "Summary of Factors",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
        fluidRow(column(12, dataTableOutput("DesignMat_SummaryTable")%>% withSpinner(color = "#0dc5c1")))
        ),  # Box
        
        box(title = "Rename Factors",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
            height = "500px",
        
        fluidRow(
        column(12, wellPanel(fluidRow(column(12, uiOutput("RenameFactorDF_UI"))))))
        )
        ), # First Page Column
                       
        column(4,  # Second Page Column
        
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
        column(6, actionButton(inputId = "SubmitFormula", label = "Generate Design Matrix")),
        column(6, actionButton(inputId = "DesignMatrixHelp", 
                                label = "Design Matrix Intro", 
                                icon = icon("info-circle"),
                                onclick = "window.open('https://www.youtube.com/watch?v=2UYx-qjJGSs', '_blank')"))
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
        #column(12, checkboxInput(inputId = "ContrastLevels", "Contrasts between experimental variable factor levels", width = "100%", value = T)),
        #column(12, checkboxInput(inputId = "ContrastInteractions", "Interaction term of differtial expression in bewteen two factor levels")),
        #column(12, checkboxInput(inputId = "ContrastCustom", "Add custom contrast input")),
        
        #conditionalPanel('input.ContrastCustom==1',
        #    column(12, br()),
            column(6, actionButton("addContrast", "Add custom contrast", width = "100%")),
            column(6, actionButton("removeContrast", "Remove custom contrast", width = "100%")),
            column(12, br()),
            column(12, helpText("Please do not use spaces, '+', '-', '*' or ':'in the input title below")),
            column(12, uiOutput("UserContrasts")),
            column(12, DT::dataTableOutput("UserContrastMatrix")),
            column(12, hr()),
            column(6, radioButtons( inputId = "UseContrastOption", "User Contrast Input", inline = F, choices = c("Append to Contrast Matrix"="appendtocont", "Use as Contrast Matrix"="cont"))),
        #),  # Conditional Input Panel
        
        column(6, style = "margin-top: 5px;", actionButton("GenerateUserContrast", "Generate Contrast Matrix", style = "font-weight: bold;",width = "100%"))
        )  # Contrast Matrix Box Spanning Column
        )  # Contrast Matrix Box Fluid Row
        )  # Contrast Matrix Box
        ), # Second Page Column
                          
        column(4,   # Third Page Column
        tabBox(     # Design/Contrast/Blocks tab box
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
        column(12, wellPanel(fluidRow(DT::dataTableOutput(outputId="ContrastMatrixTable") %>% withSpinner(color = "#0dc5c1")))))))
        ),   # Design/Contrast/Blocks tab box
        
        box(title = "Use Matricies for Differential Expression Analysis",
        solidHeader = T,
        status = 'danger',
        background = 'red',
        width = 12,
        fluidRow(
        column(6, actionButton("GoToQCPage", "Use Design and Contrast Matrix", width = "100%")),
        column(4, bookmarkButton()),
        column(2, shiny::actionButton(inputId='YouTube3', label="", style='padding:4px; font-size:100%;',
        icon = icon("question-circle"), width = '100%' , onclick ="window.open('https://www.youtube.com/watch?v=-LCxPyVXK5w&t=0s&list=PL5P7savB6d69_JokGNpwGm0mZX_6aXmes&index=3', '_blank')"))
        
        
        
        )
        )  # Box - GoToDesignPageBox
        
        
        )   # Third Page Column
        )   # FluidRow that Structures page into 3 Columns
        ),  # Design Matix TabItem 
        

        
        tabItem(
        tabName = "DataQC",
        fluidRow(
        column(10,
        #column(12, div( id = "GSMMetadataWarning_Down", box(title = "", height = "200px", solidHeader = T, width = 12, background = "red",
        #fluidRow( column(12, offset = 2, h1(icon("exclamation-triangle"),"Please download and select a dataset from the table on 'Query Datasets' page")))))), 
        
        tabBox(
        title = "Raw Data Statistics",
        id = "QCDataTabBox",
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
        #column(6,textInput(inputId = "GsmTableSelect",label = "GSE input", value = "GSE69967")), # Only For Modular Testing
        #column(6,textInput(inputId = "GplTableSelect",label = "GPL input", value = "GPL")), # Only For Modular Testing
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
            
        column(12,h4("Gene Expression Matrix"),hr()),
        column(12, DT::dataTableOutput("RawDataQC") %>% withSpinner(color = "#0dc5c1")),
        column(12, hr()),
        column(3, actionButton(inputId = "QCTableToDiffExp", width = "100%", label = "Differential Expression",icon = icon('table'))),
        column(3, actionButton(inputId = "QCTableToBioQC", width = "100%", label = "BioQC Analysis",icon = icon('search'))),
        column(3, actionButton(inputId = "QCTableToBoxplot", width = "100%", label = "Distribution Boxplots",icon = icon('bar-chart'))),
        column(3, actionButton(inputId = "QCTableToPCA", width = "100%", label = "PCA Analysis",icon = icon('bar-chart'))),
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
        column(12, sliderInput("BoxPlot_nGenes", "Portion of Genes to Sample", min = 1, max = 100, value = 50, step = 10)),
                        
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
        column(12, plotOutput("BoxPlot_ggplot", height = "600px") %>% withSpinner(color = "#0dc5c1")),
        column(12, hr()),
        
        column(3,actionButton(inputId = "RefreshBoxPlotSample", width = "100%", label = "Refresh Gene Selection",icon = icon('refresh'))),
        column(3,actionButton(inputId = "RefreshPlot", width = "100%", label = "Refresh Plot",icon = icon('refresh'))),
        column(2,checkboxInput(inputId = "BoxPlot_ToggleLegend", label = "Show Legend")),
        column(4,checkboxInput(inputId = "BoxPlot_ToggleInteractive", label = "Render Interactive Plot"))
        )
        )  # Well Panel for Plots
        )  # Second Column of Page
        )  # tabPanel Fluid Row
        ), # tabPanel(title = "BoxPlots"
        
        tabPanel(title = "PCA", id = "PCA",
        fluidRow(
        column(4,
                 
        wellPanel(
        fluidRow(

        h4("PCA options"),
        column(3, checkboxInput(inputId = "PCA_center",label = "Center Data", value =1)),
        column(3, checkboxInput(inputId = "PCA_scale", label = "Scale Data", value=1)),
        column(3, checkboxInput(inputId = "MakeScree", label = "Scree Plot", value=1)),
        column(3, checkboxInput(inputId = "MakeLoading", label = "Loadings", value=1)),
        
        column(6, uiOutput("PCA_GroupUI")),
        column(6, uiOutput("PCA_LabelUI")),
        column(6, uiOutput("PCA_xcomp_UI")),
        column(6, uiOutput("PCA_ycomp_UI")),
        column(12, hr()),
        
        conditionalPanel(condition = "input.MakeScree == 1",
        column(12,h4("Scree Plot Options")),
        column(12,
        sliderInput(inputId = "nCompScree", label = "Number of Components in Scree plot", min = 1, max = 20, value = 10, step = 1),
        sliderInput(inputId = "ScreeYMax", label = "Y Max for Screer Plot", min = 0, max = 1, value = 0.5, step = 0.1),
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
        tabBox(title = "Expression Analysis", id = "ExpressionAnalysis",
        width = 12,
        
        tabPanel(title = "Volcano Plot",
        
        fluidRow(
        column(4,
        wellPanel(
        fluidRow(
        
        h4("Expression Analysis Options"),
        column(12, uiOutput("DiffExMethod_UI")),
        column(12, actionButton("SubmitDEA","Perform Differntial Expression Analysis")),
        column(12, hr()),
        
        h4("Significance Threshold"),                                                   #choices = list("Tukey"=0, "Spear"=1, "Altman"=2)
        column(6, uiOutput("PValThres")),
        column(6, uiOutput("LogFCThres")),
        column(12, selectInput(inputId = "MultiTestCorr", label = "Multiple Testing Correction", 
        choices = c("None" = "none", "Holm" = "holm", "Hochberg" = "hochberg","Bonferroni" = "bonferroni", "Benjamini–Hochberg" = "BH", "Benjamini-Hochberg-Yekutieli" = "BY"), selected = "BH")),  
        column(12, hr()),
        
        h4("Volcano Plot Options"),
        column(12, uiOutput("SelectContrast_UI")),
        column(4, checkboxInput(inputId = "VolacanoPlot_PvalLine", label = "p-value line", value = T)),
        column(4, checkboxInput(inputId = "VolacanoPlot_LogLine", label = "logFC line", value = T)),
        column(4, checkboxInput(inputId = "VolacanoPlot_labelhits", label = "label hits")),
        
        column(12,hr(),h4('Additional Parameters')),
        column(3,checkboxInput('VolcanoPlot_showColor','Color')),
        column(3,checkboxInput('VolcanoPlot_showLabs','Labels & Title')),
        column(3,checkboxInput('VolcanoPlot_showPlotSize','Plot Size')),
        column(3,checkboxInput('VolcanoPlot_showMargins','Margins')),
                           
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
        numericInput("VolcanoPlot_Height", "Plot height:", value=500),
        numericInput("VolcanoPlot_Width", "Plot width:", value=800)
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
        
        #wellPanel(fluidRow(
        box(title = "Plot Output",
        status = "warning",
        solidHeader = T,
        collapsible = T,
        width = 12,
        fluidRow(
        column(12, uiOutput("VolcanoPlot_HighlightGene_UI")),
        column(12, hr()),
        column(12, uiOutput("VolcanoPlot_Output"))
        )
        ),  # box(title = "Plot Output",
                      
        box(title = "Top Table",
        status = "warning",
        solidHeader = T,
        width = 12,
        collapsible = T,
        #wellPanel(
        fluidRow( column(12, dataTableOutput("VolcanoPlot_TopTable")))
        ) # box(title = "Top Table",
        )
        )
        ),
        
        tabPanel(title = "Clustering",
        fluidRow(
        column(4,
        wellPanel(
        
        h4('Gene Selection'), 
        column(width=12, uiOutput("HeatMapSelectContrast_UI")),
        column(width=12,sliderInput("HeatMap_nGenes", "Number of Differentially Expressed Genes to Show", min = 1, max = 100, value = 10)),
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
        plotlyOutput("HeatMapPlotly",height='600px'),
        actionButton("RunHeatMaply", "Refresh Heatmap", icon = icon("refresh"))
        )
        )
        )
        ) #tabPanel
        
        
        
        ) # Expression Analysis
        ) # Second Column of the Page
        ) # Fluid Row that Makes up the page
        ), # Differential Analysis Tabitem

        
        tabItem(tabName = "ExportSave",
                
        box(title = "Raw Data",
        status = "warning",
        solidHeader = T,
        width = 4,
        h4("Download Raw Data"),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadExpressionMatrix",label = "Expression Matrix")),
        column(12, actionButton(inputId = "DownloadFactorExpressionMatrix",label = "Factor Expression Matrix")),
        column(12, actionButton(inputId = "DownloadFactorDF",label = "Factor Dataframe")),
        column(12, actionButton(inputId = "DownloadEsetRDS",label = "eset as RData")),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadDesignMatrix",label = "Design Matrix")),
        column(12, actionButton(inputId = "DownloadContrastMatrix",label = "Contrast Matrix")),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadPlots",label = "Download all Plots in current state")),
        column(12, actionButton(inputId = "DownloadAll",label = "Download as compressed CSV zip file")),
        column(12, actionButton(inputId = "DownloadAll",label = "Download All data RData"))
        ),
                                
        box(title = "Filtered Data",
        status = "warning",
        solidHeader = T,
        width = 4,
        h4("Download Raw Filtered Data"),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadExpressionMatrix",label = "Expression Matrix")),
        column(12, actionButton(inputId = "DownloadFilteredFactorExpressionMatrix",label = "Factor Expression Matrix")),
        column(12, actionButton(inputId = "DownloadFilteredFactorDF",label = "Factor Dataframe")),
        column(12, actionButton(inputId = "DownloadFilteredEsetRDS",label = "eset as RData")),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadFilteredDesignMatrix",label = "Design Matrix")),
        column(12, actionButton(inputId = "DownloadFilteredContrastMatrix",label = "Contrast Matrix")),
        column(12, hr()),
        column(12, actionButton(inputId = "DownloadFilteredPlots",label = "Download all Plots in current state")),
        column(12, actionButton(inputId = "DownloadFilteredAll",label = "Download as compressed CSV zip file")),
        column(12, actionButton(inputId = "DownloadFilteredAll",label = "Download All data RData"))
        )
        
        ),
        
        
        
        tabItem(
        tabName = "Contact",
        fluidRow(
        
        column(4,
        box(
        title = "Contact Information",
        status = "warning",
        solidHeader = T,
        width = 12,
            
        fluidRow(
        column(12, img(src='roche.jpg', align = "left", width = "100%")),
        column(12, strong("Questions, concerns, coffee? Feel free to get in touch.")),
        column(12, p('Developers and Maintainers:')),
        column(12, p('Moaraj Hasan: Moaraj [at] Moaraj.com')),
        column(12, p('David Zhang: jitao_david.zhang [at] roche.com')))
        ),
        
        box(
        title = "Documentation",
        status = "warning",
        solidHeader = T,
        width = 12,
        column(6, 
        shiny::actionButton(inputId='GithubHowTo', label="How-To & Docs", style='padding:4px; font-size:200%',
        icon = icon("book"), width = '100%' , onclick ="window.open('https://moaraj.github.io/GeoWizard/', '_blank')")),
        
        column(6, 
        shiny::actionButton(inputId='GithubDoc', label="ShinyApp Doc", style='padding:4px; font-size:200%',
        icon = icon("book"), width = '100%' , onclick ="window.open('https://moaraj.github.io/GeoWizard-ShinyApp/', '_blank')"))
        ),
        
        box(
        title = "GitHub Links",
        status = "warning",
        solidHeader = T,
        width = 12,
            
        column(6, 
        shiny::actionButton(inputId='GithubLink', label="Github Repo", style='padding:4px; font-size:200%',
        icon = icon("github"), width = '100%' , onclick ="window.open('https://github.com/moaraj/GeoWizard', '_blank')")),
        column(6, shiny::actionButton(inputId='GithubIssue', label="Submit Issue", style='padding:4px; font-size:200%',
        icon = icon("exclamation-triangle"), width = '100%' , onclick ="window.open('https://github.com/moaraj/GeoWizard/issues', '_blank')"))
        )
        
        ), # First Column of Page
        
        column(4,
        box(
        title = "Citations",
        status = "warning",
        solidHeader = T,
        width = 12,
        height = "300px",
        fluidRow(
        column(12, h3("Citations")),
        column(12, h4("Filter GSM Meta Page")),
        column(12, h4("Design Matrix")),
        column(12, h4("Download and QC")),
        column(12, h4("Expression Analysis")),
        column(12, h4("Export and Save")))
        ),

        box(
        title = "Acknowledgements",
        status = "warning",
        solidHeader = T,
        width = 12,
        height = "300px",
        fluidRow(
        column(12, h3("Roche BEDA Team")),
        column(12, div(
        strong("David"),"for the incredible guidance, his insight great vision for spotting the most important questions.",
        strong("Martin"), "creating a great team cohesion and spirited conversations on everything from carpentry to metaphysics.",
        strong("Laura"), "for her incredible pointedness and clear view of issues and pragmatic methods of resolution"
        )))
        ) # Aknowledgements Box
        
        ) # Second Column of Page
        ) # Page Fluid Row
        ) # Contact Tab Item
        
        ) # tabItems
        ) # DashboardBody
        ) # Dashboard Page
