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


tags$style(type='text/css', '.selectize-dropdown-content {max-height: 100; }')
td {word-wrap: break-word}

.nav-tabs {font-size: 20px}
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
                                                    textAreaInput(
                                                         inputId = "MolSelectTextBox",
                                                         height = "150px",
                                                         label = "Comma Separated Text input",
                                                         value = c("Mycophenolate mofetil,\nTofacitinib"))
                                                    ),
                                               
                                               
                                               actionButton(
                                                    inputId = "StructureSerach",
                                                    label = HTML("Retrieve Datasets for<br />Related Chemotypes<br />"),
                                                    class = "btn-warning",width = '100%'

                                               )
                                               
                                               ),
                                               
                                               column(6,
                                                    selectInput(
                                                    inputId = "MolSelectFilter",
                                                    label = "Molecule Libraries",
                                                    choices = c(
                                                         "Text Input" = "TextInput",
                                                         "FDA approved Drugs" = "FDA",
                                                         "David's TA Molecules" = "DAVID",
                                                         "SPARK" = "SPARK Library"
                                                    ),
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
                                               sliderInput(
                                                    inputId = "SampleSizeSlider",
                                                    label = "Min Sample Size",
                                                    value = 1,
                                                    min = 1,
                                                    max = 300
                                               )),
                                               
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
                                               )),
                                               
                                               column(3,
                                                      uiOutput("TaxonSelection")
                                                      ),
                                               
                                               column(3,
                                               radioButtons(
                                                    inputId = "FullSummaryCheck",
                                                    label = "Show full Summary",
                                                    choices = c("Full Text", "Truncated"),
                                                    selected = "Truncated", 
                                                    inline = F
                                               ))
                                               
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
                                                plotOutput("nStudiesPlotTaxon") %>%
                                                     withSpinner(color = "#0dc5c1")),
                                       
                                       tabPanel("gdsType",
                                                plotOutput("nStudiesPlotGdsType") %>%
                                                     withSpinner(color = "#0dc5c1"))
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
                                  column(2,
                                  
                                   fluidRow(       
                                  actionButton(
                                           inputId = "AnalyzeSelectedDatasets",
                                           label = "Analyze Selected Datasets  ",
                                           icon = icon("arrow-right"),
                                           style='padding:4px; font-size:200%; font-weight: bold; color:#0573B7')),
                                  
                                  fluidRow(
                                  actionButton(
                                       inputId = "ExpressAnalyse",
                                       label = HTML("Automated Express Analysis"),
                                       icon = icon("fast-forward"),
                                       style='padding:4px; font-size:200%; font-weight: bold; color:#0573B7'))
                                  )
                               )
                                  
                                  
                                  
                             
                        ), # tabItem - GSESummary 
                    
                    tabItem(tabName = "GSMMetadata",
                            
                         fluidRow(
                         valueBoxOutput("Keyword"),
                         valueBoxOutput("Taxon"),
                         valueBoxOutput("nSamples")),
                         
                         fluidRow(
                              column(4,
                                         box(title = "Dataset Selection",
                                             solidHeader = T,
                                             status = "primary",
                                             width = 12,
                                             collapsible = T,
                                             
                                             fluidRow(
                                                  column(6, uiOutput('GsmTabletoKeep') %>% withSpinner(color = "#0dc5c1")),
                                                  column(6, uiOutput('GsmTabletoShow') %>% withSpinner(color = "#0dc5c1"))
                                             )),
                                  
                                         
                                         box(title = "Metadata columns with Experimental Variables",
                                             solidHeader = T,
                                             status = "primary",
                                             width = 12,
                                             collapsible = T,
                                         
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
                                         
                                         fluidRow(
                                              style="margin-left :10px; margin-right :10px",
                                              uiOutput("PickFactorColumns")),
                                         
                                         fluidRow(
                                              style="margin-left :10px; margin-right :10px",
                                              radioButtons(
                                                   inputId = "ViewAllorFiltVar",
                                                   label = "Which Var Columns to Display",
                                                   choices = c(
                                                        "Display All Experimental Variable" = "All",
                                                        "Display Filtered Experimental Variable" = "Filt"
                                                   ),
                                                   selected = "Filt",
                                                   inline = F
                                              )
                                         )),
                                         
                                              
                                     
                                     box( title = "Filter Factor Levels",
                                          solidHeader = T,
                                          status = "primary",
                                          width = 12,
                                          collapsible = T,
                                          
                                          uiOutput(outputId = "FilterGSMbyFactor"),
                                          fluidRow(
                                               style="margin-left :10px; margin-right :10px",
                                               actionButton(
                                                    inputId = 'FilterGSMLevels',
                                                    label = "Reset Filter",
                                                    class = "btn-primary"),
                                                   
                                                   actionButton(
                                                        inputId = "HideGSMLevelsFilter",
                                                        label = 'Hide Filter Boxes',
                                                        class = "btn-primary")
                                                   )
                                          )
                                     ), # Column Classifications
                              
                              column(4,
                                     tabBox(title = "Detected ExpVars",
                                            #collapsible = T,
                                            #solidHeader = T,
                                            #status = "danger",
                                            width = 12,
                                            side = "right",
                                            selected = "Selected Factors",
                                            
                                            tabPanel("All Factors", 
                                                     DT::dataTableOutput("FullFactorTable") %>% 
                                                          withSpinner(color = "#0dc5c1")),
                                            
                                            tabPanel("Selected Factors",
                                                   DT::dataTableOutput("ImportantFactorTable") %>% 
                                                        withSpinner(color = "#0dc5c1"))
                                            ),
                                     
                                     box(title = "",
                                         solidHeader = T,
                                         status = 'danger',
                                         colour = 'red',
                                         width = 12,
                                         
                                         actionButton("GoToDesignPage","Use factors for analysis", width = "100%")
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
                                            
                                            dataTableOutput("DesignMat_SummaryTable")),
                                        
                                        box(title = "Detected Factor Identifications")
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
                                         
                                         plotOutput("ExperimentalBlocksPlot") %>% 
                                              withSpinner(color = "#0dc5c1")
                                         
                                         )
                                     
                              ) # Third Page Column
                                        
                              
                                 
                                 
                                 
                                 ) #FluidRow that Structures page into 3 Columns
                            ), #Design Matix TabItem 
                    
                    tabItem(tabName = "DataQC",
                            fluidRow(
                                 column(8,
                                 
                                 tabBox(title = "Raw Data Statistics", 
                                        width = 12,
                                        
                                        tabPanel(title = "GMT File",
                                           solidHeader = T,
                                           status = "primary",
                                           
                                           actionButton( inputId = "DownloadData",
                                                label = "Download Data from GEO",
                                                width = 12 ),
                                           hr(),
                                           
                                           hidden(div( id = "GMTTable",
                                                       dataTableOutput("GMTFileTable") %>% withSpinner(color = "#0dc5c1")
                                           ))
                                      ),
                                      
                                      tabPanel(title = "Boxplots",
                                               fluidRow(
                                                    style = "margin-left :10px; margin-right :10px",
                                                    h4("Count Matrix BoxPlot"),
                                                
                                                column(2,radioButtons(inputId = "BoxPlotType",
                                                                     label = "BoxPlotType",
                                                                     inline = F,
                                                                     choices = c("Sample", "Gene"),
                                                                     selected = "Sample")),
                                                
                                                column(2,radioButtons(inputId = "BoxPlotBy",
                                                                     label = "BoxPlotBy",
                                                                     inline = F,
                                                                     choices = c("Overall", "Factor"),
                                                                     selected = "Factor")),
                                                
                                                column(2,uiOutput("BoxFactorSelect")),
                                                
                                                column(2,numericInput(inputId = "BoxSampleSize",
                                                                      label = "Number of GSM or Gene's to Sample",
                                                                      value = 10,
                                                                      min = 1,
                                                                      step = 10))
                                                ), # tabPanel(title = "Boxplots"
                                           
                                           fluidRow(
                                                style = "margin-left :10px; margin-right :10px",
                                                plotOutput(outputId = "BoxPlotGMT") %>% withSpinner(color = "#0dc5c1")
                                           )
                                      ),
                                      
                                      tabPanel("Histogram",
                                               fluidRow(
                                                    column(2,radioButtons(inputId = "HistPlotType",
                                                                         label = "BoxPlotBy",
                                                                         inline = F,
                                                                         choices = c("Sample", "Gene", "Factor"),
                                                                         selected = "Sample")),
                                                    
                                                    column(2, uiOutput("HistFactorSelect")),
                                                    column(2,numericInput(inputId = "HistSampleSize",
                                                                          label = "Number of GSM or Gene's to Sample",
                                                                          value = 10,
                                                                          min = 1,
                                                                          step = 10))),
                                               fluidRow( 
                                                    column(10, offset = 1,
                                                           plotOutput("HistPlotGMT") %>% withSpinner(color = "#0dc5c1")) )
                                               
                                               ), # tabPanel("Histogram"
                                      
                                      tabPanel(title = "PCA", 
                                               fluidRow(column(6, plotOutput("PCA")%>% withSpinner(color = "#0dc5c1")),
                                                        column(6, plotOutput("CA")%>% withSpinner(color = "#0dc5c1"))),
                                               
                                               fluidRow(column(6, plotOutput("Scree")%>% withSpinner(color = "#0dc5c1")),
                                                    column(6, plotOutput("Cont")%>% withSpinner(color = "#0dc5c1")))
                                      ),
                                      
                                   
                                 tabPanel("BioQC",
                                                    style = "margin-left :10px; margin-right :10px",
                                                    br(),
                                                    fluidRow(actionButton(inputId = "PerformBioQCAnalysis", 
                                                                      label = "Perform BioQC Analysis", 
                                                                      size = "large")),
                                                    fluidRow(helpText(
                                                    paste(
                                                    "BioQC performs quality control of high-throughput expression data based on ",
                                                    "tissue gene signatures. It can detect tissue heterogeneity in gene " ,
                                                    "expression data. The core algorithm is a Wilcoxon-Mann-Whitney ",
                                                    "test that is optimised for high performance."))),
                                                    
                                                    
                                                    fluidRow(
                                                         plotOutput(outputId = "BioQCPlot") %>% withSpinner(color = "#0dc5c1"))
                                           )
                                 
                                 ) # tabBox(title = "Raw Data Statistics"
                                   ) # Column for Page
                              
                            
                            ) # Fluid Row For the TabItem
                            ), 
                    
                    
                    
                    tabItem(tabName = "DifferentialAnalysis",
                            
                            fluidRow(
                                 
                                 column(8,
                                        
                                        tabBox( title = "Expression Analysis",
                                                width = 12,
                                                
                                                tabPanel(title = "Volcano Plot",
                                                         actionButton("SubmitDEA", 
                                                                      "Perform Differntial Expression Analysis"),
                                                         fluidRow(uiOutput("PValThres"),
                                                                  uiOutput("LogFCThres")),
                                                         fluidRow(plotOutput("VolcanoPlot"))
                                                         ),
                                                
                                                tabPanel(title = "MA Plot",
                                                         plotOutput("MAPlot")),
                                                
                                                tabPanel(title = "Clustering",
                                                         plotOutput("Clustering")),
                                                
                                                tabPanel("BoxPlot", 
                                                         uiOutput("EABoxPlotOptions"),
                                                         plotOutput("EABoxPlot")),
                                                
                                                tabPanel("Top Table", dataTableOutput("TopTable"))),
                                        

                                        tabBox(title = "Enrichment Analysis",
                                               width = 12,
                                               
                                               tabPanel("GO Enrichment")
                                               )
                                        
                                        ) # Second Column of the Page
                                 
                                 ) #Fluid Row that Makes up the page
                            ) #Differential Analysis Tabitem
                        
                        ) #tabItems
          
                        ) #DashboardBody
                        
                   ) #Dashboard Page
