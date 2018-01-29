## app.R ##
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)

library(ggplot2)
library(gplots)
library(RColorBrewer)
library(plotly)
library(DT)

library(limma)
library(Biobase)
library(BioQC)
library(GEOquery)
library(GEOmetadb)

GeoRepo <- "~/GeoWizard/"
setwd(GeoRepo)

source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")
source(file = "MoleculeLibraries/MoleculeLibraries.R")
source(file = "GeoTrainingSets/KeyWords.r")
source(file = "helpers.R")
source(file = "SeriesHanding.R")

con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(dbListTables(con))



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
               menuItem("Contrast Matrix", tabName = "ContrastMat", icon = icon("th")),
               menuItem("Expression Analysis", tabName = "DifferentialAnalysis", icon = icon("bar-chart")),
               menuItem("Molecule To Target Search", tabName = "ReverseSerach", badgeLabel = "new", badgeColor = "green")
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
                                        
                                         dataTableOutput("GseGsmTable"))
                                         
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
                                                   inline = T
                                              )
                                         )),
                                         
                                         box( title = "Fileter Factor Levels",
                                              solidHeader = T, 
                                              width = 12, 
                                              collapsible = T,
                                              
                                              fluidRow(
                                                   # FilterGSMbyFactor Function produces SelecInputs in Cols so this
                                                   # output needs to be in a fluid row
                                                   
                                                   style="margin-left :10px; margin-right :10px",
                                                   uiOutput(outputId = "FilterGSMbyFactor")),
                                              
                                              fluidRow(
                                                   style="margin-left :10px; margin-right :10px",
                                                   actionButton(
                                                        inputId = 'FilterGSMLevels',
                                                        label = "Filter Levels",
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
                                     
                                     box(title = "Predicted Interpretation",
                                         collapsible = T,
                                         solidHeader = T,
                                         status="danger",
                                         width = 12),
                                     
                                     
                                     tabBox(title = "ExpVar Table",
                                            #collapsible = T,
                                            #solidHeader = T,
                                            #status = "danger",
                                            width = 12,
                                            side = "right",
                                            selected = "Important Factors",
                                            
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
                    
                    tabItem(tabName = "DesignMatrix"),
                    tabItem(tabName = "ContrastMatrix"),
                    tabItem(tabName = "DifferentialAnalysis")
                        
                        ) #tabItems
          
                        ) #DashboardBody
                        
                   ) #Dashboard Page
