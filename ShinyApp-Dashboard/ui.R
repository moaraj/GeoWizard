GeoRepo <- "~/GeoWizard/"
setwd(GeoRepo)

## app.R ##
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)

library(ggplot2)
library(vcd)
library(gplots)
library(ggridges)
library(RColorBrewer)
library(DT)

library(limma)
library(Biobase)
library(BioQC) #install.packages("https://bioarchive.galaxyproject.org/BioQC_1.4.0.tar.gz", repos = NULL)
library(GEOquery)
library(vcd)
library(GEOmetadb)

library(plotly)
library(heatmaply)

library(FactoMineR)
library(factoextra)

source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")
source(file = "GeoFileHandling.R")
source(file = "QCAnalysis.R")
source(file = "ExpressionAnalysis.R")

source(file = "MoleculeLibraries/MoleculeLibraries.R")
source(file = "GeoTrainingSets/KeyWords.r")
source(file = "helpers.R")
source(file = "SeriesHanding.R")


if(!file.exists('GEOmetadb.sqlite')) getSQLiteFile()
con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(paste('Connected Database Tables:', dbListTables(con)))



appCSS <- "

#loading-content {
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

#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }

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

"


ui <- dashboardPage(
     dashboardHeader(title = "Roche's GeoWizard"),
     
     ## Sidebar content
     dashboardSidebar(
          sidebarMenu(
          id = "TabSet",
          menuItem("Query Datasets",tabName = "GSESummary",icon = icon("search")),
          menuItem("Filter GSM Metadata", tabName = "GSMMetadata", icon = icon("filter")),
          menuItem( "Design Matrix", icon = icon("th"), tabName = "DesignMatrix"),
          menuItem("Download and QC", tabName = "DataQC", icon = icon("download")),
          menuItem("Expression Analysis", tabName = "DifferentialAnalysis", icon = icon("bar-chart")),
          menuItem("Molecule To Target Search", tabName = "ReverseSerach", icon = icon("database"))
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
                                               
          column(6,
          div(id = 'MolSelectLibraryDiv',
          uiOutput('MolSelectFromLibrary')
          ),
                                               
          div(
          id = 'MolSelectTextDiv',
          textAreaInput(inputId = "MolSelectTextBox",
            height = "150px",
            label = "Comma Separated Text input",
            value = c("Mycophenolate mofetil,\nTofacitinib")
          )
          ),
          
          actionButton(inputId = "StructureSerach",
            label = HTML("Retrieve Datasets for<br />Related Chemotypes<br />"),
            class = "btn-warning",
            width = '100%')
          ),
                                               
          column(6,
          selectInput(
          inputId = "MolSelectFilter",
          label = "Molecule Libraries",
          choices = c( "Text Input" = "TextInput", 
                       "FDA approved Drugs" = "FDA", 
                       "David's TA Molecules" = "DAVID",
                       "SPARK" = "SPARK Library"),
          selected = "DAVID"
          ), #radioButtons
                                               
          actionButton(
            inputId = "MolSelectButton",
            label = HTML("Retrieve Datasets<br />for Selected Molecules<br />"),
            class = "btn-warning",width = '100%'
          ) # actionButton
          )
          ),
          
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
                      max = 300 )
          ),
                                               
          column(3,
          checkboxGroupInput(
          inputId = "ExpTypes",
          label = "Perturbation Types  to Query",
          choices = c("Small Molecule",
                      "Biologics",
                      "Genetic Perutbations"),
          selected = c("Small Molecule",
                       "Biologics",
                       "Genetic Perutbations")
          )
          ),
          
          column(3,
          uiOutput("TaxonSelection")
          ),
                                               
          column(3,
          radioButtons( inputId = "FullSummaryCheck",
            label = "Show full Summary", 
            choices = c("Full Text", "Truncated"), 
            selected = "Truncated",  
            inline = F)
          )
          ), # Fluid Row - Box
          br()
          ) # Filter Seacrh Results Box
          
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
          valueBoxOutput("Keyword"),
          valueBoxOutput("Taxon"),
          valueBoxOutput("nSamples")
          ),
                         
          fluidRow(
          column(4,
          box(title = "Dataset Selection",
          solidHeader = T,
          status = "primary",
          width = 12,
          collapsible = T,
          
          fluidRow(
          column(6, 
          uiOutput('GsmTabletoKeep') %>% withSpinner(color = "#0dc5c1")
          ),
          
          column(6, 
          uiOutput('GsmTabletoShow') %>% withSpinner(color = "#0dc5c1"))
          )
          ),
                                  
                                         
          box(title = "Metadata columns with Experimental Variables",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
            
          conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>')
          ),  
                                         
          checkboxGroupInput(
            inputId = "WhereVarData",
            label = "Select columns useful experimental\nfactor levels can be found",
            choices =  c("gsm.title","description","characteristics_ch1"),
            selected = "characteristics_ch1",
            inline = T
            ),
                                        
          fluidRow(
            style="margin-left :10px; margin-right :10px",
            dataTableOutput("GseGsmTable")
            )
            )
            ), # Column Dataselction
                              
          column(4,
          box(title = "Experimental Classification Results",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
          
          conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>')
          ),  
            
          fluidRow(
            style="margin-left :10px; margin-right :10px",
          uiOutput("PickFactorColumns")
          ),
                                         
          fluidRow(
              style="margin-left :10px; margin-right :10px",
          radioButtons(
            inputId = "ViewAllorFiltVar",
            label = "Which Var Columns to Display",
            choices = c(
              "Display All Experimental Variable" = "All",
              "Display Filtered Experimental Variable" = "Filt"),
            selected = "Filt",
            inline = F
            )
          )
          ),
                                         
          box(title = "Filter Factor Levels",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = T,
            
          conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          HTML('<button class="btn btn-default"><i class="glyphicon glyphicon-refresh gly-spin"></i></button>')
          ),  
                                            
          uiOutput(outputId = "FilterGSMbyFactor"),
            
          fluidRow(
            style="margin-left :10px; margin-right :10px",
            
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
          ), # Column Classifications
                              
            column(4,
            tabBox(title = "Experimental Factors",
              width = 12,
              side = "right",
              selected = "Unique Factor Levels",
            
            tabPanel("Ignored Factor Levels", 
            helpText(paste(
              "These factor leves are not used to construct the Design and contrast matricies",
              "but they are stored as meta data tags for retrieving data from a database used",
              "to store differential expression analysis results")
              ),
            DT::dataTableOutput("ExcludedFactorTable") %>% withSpinner(color = "#0dc5c1")
            ),  
                                              
            tabPanel("All Factor Levels", 
            DT::dataTableOutput("FullFactorTable") %>% withSpinner(color = "#0dc5c1")
            ),
                                            
            tabPanel("Unique Factor Levels",
            DT::dataTableOutput("ImportantFactorTable") %>% withSpinner(color = "#0dc5c1")
            )
            ),
                                     
            box(title = "",
              solidHeader = T,
              status = 'danger',
              colour = 'red',
              width = 12,
            
            actionButton("GoToDesignPage", "Use factors for analysis", width = "100%")
            )
            )
            ) # Page Fluid Row containing the Column Layout
            ), # TabItem - GSMMetadata
                    
            tabItem(tabName = "DesignMatrix",
            fluidRow(
            column(4,
            box(title = "Summary of Factors",
              solidHeader = T,
              status = "primary",
              width = 12,
              collapsible = T,
            
            dataTableOutput("DesignMat_SummaryTable"))
            ), # First Page Column
                                 
            column(4,
            box(title = "Use Formula to Generate Design Matrix",
              solidHeader = T,
              status = "primary",
              width = 12,
              collapsible = T,
                                            
            uiOutput(outputId = "TextAhead"),
                                            
            textInput(
              inputId = "formulaInputDesign",
              label = "Model Matrix Formula Input",
              placeholder = "~ Expvar1 + Expvar2"
              ),
                                            
            h4(paste("The detected baseline or control level",
              "for each factor can be changed below")),
                                            
            uiOutput("RearrangeLevels"),
                                            
            actionButton(inputId = "SubmitFormula",
              class = "btn-primary",
              label = "Generate Design Matrix")
              ),
                                        
            box(title = "Use Detected Design to make Design Matrix",
              solidHeader = T,
              status = "primary",
              width = 12,
              collapsible = T
            )
            ), # Second Page Column
                                        
            column(4,
            box(title = "Design Matrix",
              solidHeader = T,
              status = "primary",
              width = 12,
              collapsible = T,
                                         
            DT::dataTableOutput(outputId = 'CustomExpressionTable')
            ),
                                     
            box(title = "Experimental Blocks",
              solidHeader = T,
              status = "primary",
              width = 12,
              collapsible = T,
                                         
            plotOutput("ExperimentalBlocksPlot") %>% withSpinner(color = "#0dc5c1")
            
            )
            ) # Third Page Column
            ) #FluidRow that Structures page into 3 Columns
            ), #Design Matix TabItem 
                    
            tabItem(
            tabName = "DataQC",
            fluidRow(
            column(4,  
            
            box(title = "GMT File",
              width = 12,
              solidHeader = T,
              status = "primary",
            
            fluidRow(
            column(10, 
            offset = 1,
            tags$p(tags$strong("Download Data from SRA/GEO")),
                        
            actionButton(
              inputId = "DownloadGEOData",
              label = "Download",
              icon = icon("download"),
              block = T
            ),
            br(),
            radioButtons(inputId = "ExpressionDataType",
                        label = "Gene Expression Data Type",
                        choiceNames = c("MicroArray", "NGS Sequencing", "Single Cell Sequencing"),
                        choiceValues = c("mArray", "RNAseq", "ssRNAseq"),
                        selected = "mArray"),
            br(),
            selectInput(inputId = "GeneAnnotationType",
              label = "Gene Annotations",
              choices = c("Gene Title" = "Gene Title",
                "Entrez Gene ID" = "ENTREZ_GENE_ID",
                "Gene Symbol" = "Gene Symbol",
                "RefSeq ID" = "RefSeq Transcript ID"),
              selected = "Gene Title",
              multiple = F,
              selectize = T)
            
            )  # Column
            ), # Fluid Row
                        
            hr(),
            h3("Gene Matrix"),
            hidden(
            div( id = "GMTTableDiv",
            DT::dataTableOutput("GMTFileTable") %>% withSpinner(color = "#0dc5c1")
            )  # GMTFileTable
            )  # Hidden
            )  # Box
            ), # First Column for Page
                                 
            column(8,
            tabBox(title = "Raw Data Statistics",
              width = 12,
            tabPanel(title = "Boxplots",
            fluidRow(
            column(4,
            
            wellPanel(
              style = "margin-left :10px; margin-right :10px",
            
            h4("Count Matrix BoxPlot"),
            radioButtons(
              inputId = "BoxPlotType",
              label = "BoxPlotType",
              inline = F,
              choices = c("Sample", "Gene"),
              selected = "Sample"
              ),
                                                      
            radioButtons(inputId = "BoxPlotBy",
              label = "BoxPlotBy",
              inline = F,
              choices = c("Overall", "Factor"),
              selected = "Factor"
            ),
            
            uiOutput("BoxFactorSelect"),
            
            numericInput(inputId = "BoxSampleSize",
              label = "Number of GSM or Gene's to Sample",
              value = 10,
              min = 1,
              step = 10
            )
            )
            ), # Input Well Panel
                                                 
            column(8,
            plotOutput(outputId = "BoxPlotGMT") %>% withSpinner(color = "#0dc5c1")
            )
            ) # TabPanel Fluid Row [input] [Plot]
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
            fluidRow(
            column(6, 
            plotOutput("PCA")%>% withSpinner(color = "#0dc5c1")
            ),
            
            column(6,
            plotOutput("CA")%>% withSpinner(color = "#0dc5c1")
            )
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
            ), # tabPanel BioQC"
            
            tabPanel("View/Save Raw Data",
            wellPanel(
            radioButtons(
              inputId = "RawDataTableMelt", 
              label = "Which Data Matrix to Show" , 
              choiceNames = c("Gene Matrix","Melted Gene Matrix with Factors"),
              choiceValues = c("GMT", "FactorGMTMelt"), 
              inline = T
              )
              
            ),
            h4("Datatable"),
            hr(),
            DT::dataTableOutput("RawDataQC") %>% withSpinner(color = "#0dc5c1")
            )  # tabPanel RawDataQC
            
            
            )  # tabBox(title = "Raw Data Statistics"
            )  # Column for Page
            )  # Fluid Row For the TabItem
            ), # tabItem(tabName = "DataQC"
            
            tabItem(tabName = "DifferentialAnalysis",
            fluidRow(
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
            )
            ) # Second Column of the Page
            ) #Fluid Row that Makes up the page
            ) #Differential Analysis Tabitem
            ) #tabItems
            ) #DashboardBody
            ) #Dashboard Page
