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



GeoWizard <- "~/GeoWizard/"
GeoRepo <- "~/GeoWizard/GeoRepo"
setwd(GeoWizard)

source(file = "GeoParse.R")
source(file = "GSMAnnotation.R")
source(file = "MoleculeLibraries/MoleculeLibraries.R")
source(file = "GeoTrainingSets/KeyWords.r")
source(file = "helpers.R")
source(file = "SeriesHanding.R")

jscode <- '
shinyjs.init = function() {
  $(".nav").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
}
'

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



con <- dbConnect(SQLite(), 'GEOmetadb.sqlite')
message(dbListTables(con))

CommonSpecies <- c(
     "Human" = "Homo sapiens" ,
     "Chimpanzee" = "Pan troglodytes",
     "Cynomogous" = "Macaca fascicularis",
     "Rabbit" = "Oryctolagus cuniculus",
     "Rat" = "Rattus norvegicus",
     "Mouse" = "Mus musculus",
     "C.elegans" = "Caenorhabditis elegans",
     "Yeast" = "Saccharomyces cerevisiae"
)


ui <- fluidPage(
     inlineCSS(appCSS),
     useShinyjs(),
     #theme = shinytheme('united'),
     theme = shinytheme('spacelab'),
     headerPanel("Roche's GeoWizard"),
     
     tabsetPanel(
          id = "GeoWizTabSetPanel",
          tabPanel(
               title = "Summary of DataSets",
               id = "GSESummary",
               wellPanel(
                    tags$h4("Inputs for GEO Query"),
                    fluidRow(
                         div(
                              id = "large",
                              
                              column(
                                   4,
                                   uiOutput('MolSelectFromLibrary'),
                                   textAreaInput(
                                        inputId = "MolSelectTextBox",
                                        label = "Comma Separated Text input",
                                        value = c("Mycophenolate mofetil,\nTofacitinib"),
                                        height = "180px"
                                   )
                              ),
                              
                              column(
                                   2,
                                   radioButtons(
                                        inputId = "MolSelectFilter",
                                        label = "Molecule Selection Filter",
                                        inline = F,
                                        choices = c(
                                             "Text Input" = "TextInput",
                                             "FDA approved Drugs" = "FDA",
                                             "David's TA Molecules" = "DAVID",
                                             "SPARK" = "SPARK Library"
                                        ),
                                        selected = "DAVID"
                                   )
                              )
                         ),
                         
                         column(1,
                                br(),
                                withBusyIndicatorUI(
                                     button =
                                          actionButton(
                                               inputId = "MolSelectButton",
                                               label = HTML("Retrieve Datasets<br />for Selected Molecules<br />"),
                                               class = "btn-primary"
                                          )
                                ))
                    ),
                    hr(),
                    fluidRow(
                         
                         column(
                              2,
                              radioButtons(
                                   inputId = "FullSummaryCheck",
                                   label = "Show full Summary",
                                   choices = c("Full Text", "Truncated"),
                                   selected = "Truncated"
                              )
                         ),
                         
                         column(
                              2,
                              sliderInput(
                                   inputId = "SampleSizeSlider",
                                   label = "Min Sample Size",
                                   value = 1,
                                   min = 1,
                                   max = 300
                              )
                         ),
                         
                         column(
                              2,
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
                         
                         column(
                              2,
                              selectInput(
                                   inputId = "SpeciesInput",
                                   label = "Species",
                                   choices = CommonSpecies,
                                   selected = CommonSpecies[1],
                                   multiple = T,
                                   selectize = T
                              )
                         )
                    ),
                    tags$hr(),
                    
                    fluidRow(
                         column(
                              1,
                              actionButton(inputId = "SelectAllDataSets",
                                           label = "Select All Datasets")
                         ),
                         column(
                              1,
                              actionButton(inputId = "ResetSelection",
                                           label = "Reset Selection")
                         ),
                         column(1,
                                withBusyIndicatorUI(
                                     actionButton(
                                          inputId = "AnalyzeSelectedDatasets",
                                          label = "Analyze Slected Datasets",
                                          icon = icon("fa-arrow-right"),
                                          class = "btn-primary"
                                     )
                                ))
                         
                    ),
                    
                    br(),
                    fluidRow(column(
                         8,
                         div(class = "BehindDataTables",
                             DT::dataTableOutput("GseSummaryData"))
                    ),
                    column(4,
                           shinyjs::hidden(
                                div(
                                     id = "nStudiesPlotDiv",
                                     fluidRow(
                                          selectInput(
                                               inputId = "GSEInfoPlotSelection",
                                               label = "Experiment Type Selection",
                                               choices = c("taxon", "gdsType")
                                          )
                                     ),
                                     fluidRow(
                                          plotOutput("nStudiesPlot",  height = "600px")  %>%
                                               withSpinner(color = "#0dc5c1")
                                     )
                                )
                           )
                           
                           ))
               )
          ),
          
          tabPanel(
               title = "GSM Metadata",
               id = "GSMMetadata",
               fluidRow(
                    column(10,
                           wellPanel(
                                h4("Dataset Selection"),
                                fluidRow(
                                     column(2,
                                          uiOutput('GsmTabletoShow') %>% withSpinner(color = "#0dc5c1")),
                                     column(2,
                                          uiOutput('GsmTabletoKeep') %>% withSpinner(color = "#0dc5c1")),
                                     column(4,DT::dataTableOutput("TaxonKeywordInfo"))
                                )
                         )
                    )
               ),
               
               column(5,
                      wellPanel(fluidRow(
                           style="margin-left :10px; margin-right :10px",
                           h3("Select Columns Containing Experimental Design Text"),
                           checkboxGroupInput(
                                
                                inputId = "WhereVarData",
                                label = "Select columns useful experimental\nfactor levels can be found",
                                choices =  c("gsm.title",
                                             "description",
                                             "characteristics_ch1"),
                                selected = "characteristics_ch1",
                                inline = T
                           ),
                           DT::dataTableOutput("GseGsmTable") %>% withSpinner(color = "#0dc5c1")
                      ))
                      ),
                      
               
                      column(5,
                             wellPanel(
                                  
                           h3("Result of Experimental Design Column Selection"),
                           
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
                           ),
                           
                           fluidRow(# FilterGSMbyFactor Function produces SelecInputs in Cols so this
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
                                ),
                                
                                actionButton(
                                     inputId = "SendTableToDesignMatTab",
                                     class = "btn-primary",
                                     label = "Use These factors for Generating Design Matrix "
                                )
                           ),
                           
                           hr(),
                           DT::dataTableOutput("DesignDataTable") %>% withSpinner(color = "#0dc5c1")
                    )
               )
          ), 
          
          
          tabPanel(
               title = "Design Matrix",
               value = "DesignMatTab",
               br(),
               sidebarPanel(
                    uiOutput('SelectAnnoGseInfo'),
                    tags$hr(),
                    radioButtons(
                         label = "How would you like to Specifc Design Matrix",
                         inputId = "HowToSpecifyDesignMatrix",
                         choices = c(
                              "Let GEO Wizard Generate Groups" = 1,
                              "Manually Generate Groups" = 2,
                              "R Expression" = 3
                         ),
                         selected = 3
                    ),
                    hr(),
                    h4("Factor Levels Table"),
                    fluidRow(
                         style="margin-left :10px; margin-right :10px",
                         DT::dataTableOutput("DesignMatGsmTable"))
               ),
               
               
               mainPanel(
                    div(
                         id = "AutoGen",
                         h3("Automatic Generation"),
                         wellPanel("Design Matrix"),
                         wellPanel("Contrast Matrix")
                    ),
                    
                    div( 
                         id = "ManGen",
                         h3("Manually Generate Groups"),
                         wellPanel(
                              "Design Matrix",
                              fluidRow(
                                   actionButton(inputId = "AddContGroup",
                                                label = "Add Control Group"),
                                   actionButton(inputId = "AddPertGroup",
                                                label = "Add Pertubation Group"),
                                   actionButton(inputId = "AddTimeGroup",
                                                label = "Add Time Series Group"),
                                   actionButton(inputId = "AddDilSGroup",
                                                label = "Add Molecule Titration Series Group")
                              )
                              
                         )
                         
                    ),
                    
                    div(
                         id = "CustomExpression",
                         h3("Make Design Matrix with r expression"),
                         wellPanel(fluidRow(
                              column(
                                   6,
                                   h3("Design Matrix Input"),
                                   textInput(
                                        inputId = "formulaInputDesign",
                                        label = "Model Matrix Formula Input",
                                        placeholder = "~ Expvar1 + Expvar2"
                                   ),
                                   
                                   h3("Design Matrix"),
                                   DT::dataTableOutput(outputId = 'CustomExpressionTable'),
                                   actionButton(inputId = "SubmitFormula",
                                                class = "btn-primary",
                                                label = "Generate Design Matrix"),
                                   
                                   actionButton(inputId = "GoToContMatTab",
                                                class = "btn-primary",
                                                label = "Generate Contrast Matrix")
                                   
                              ),
                              
                              column(4,
                                     h3("Select Control Level"),
                                     uiOutput("RearrangeLevels"),
                                     h3("Detected Experimental Design"),
                                     h4("1 level Model System: Tissue - skin"),
                                     h4("2 level Model System: Disease - Control: non-lesional, Disease: lesional"),
                                     h4("Perturbation: 1 Drug - Placebo/Tofacitinib"),
                                     h4("Time series with Skin lesional/non-lesional, with Drug Tofacitinib/Placebo"),
                                     actionButton(inputId = "UseDetectedDesign",
                                                  class = "btn-primary",
                                                  label = "Use Detected Design"))
                         ))
                    )
               )
          ), 
          
          tabPanel(
               title = "Contrast Matrix",
               id = "ContrastMatTab",
               value = "ContrastMatTab",
               
               sidebarPanel(
                    radioButtons(
                         label = "How would you like to Specifc Contrast Matrix",
                         inputId = "HowToSpecifyDesignMatrix",
                         choices = c(
                              "Let GEO Wizard Generate Groups" = 1,
                              "Manually Generate Groups" = 2),
                         selected = 1),
                    hr(),
                    
                    h3("Factor Levels Table"),
                    DT::dataTableOutput("ContrastMatGsmTable"),
                    br(),
                    h3("Design Matrix"),
                    DT::dataTableOutput("ContrastMatDesignDF")),
               
               mainPanel(
                    wellPanel(
                         column(6,
                    h3("Contrast Matrix"),
                         fluidRow(
                              column(4,
                              textInput(
                                   inputId = "formulaInputContrast",
                                   "Contrast Forumula Input",
                                   placeholder = ""
                              )
                         )
                    ),
               
               fluidRow(
                    actionButton(inputId = "AddInterGroup",
                                            class = "btn-primary",
                                            label = "Add Within Group Contrast"),
                    actionButton(inputId = "AddIntraGroup",
                                            class = "btn-primary",
                                            label = "Add between Group Contrast" ),
                    actionButton(inputId = "AddInteraction" ,
                                            class = "btn-primary",
                                            label = "Find Interaction Term")
                    ),
               hr(),
               DT::dataTableOutput('ContrastMatrixTable')
               )))),  
          
          
          tabPanel(
               title = "Gene Expression Analysis",
               id = "DifferentialAnalysis",
               
               fluidRow(
               
               column(4, offset = 1,
                    fluidRow(
                         h3("GMT File"),
                         br(),
                         actionButton(inputId = "DistrPlot", label = "Signal Distribution", class = 'btn-primary'),
                         actionButton(inputId = "GMTBoxPlot", label = "Sample Summary", class = 'btn-primary'),
                         actionButton(inputId = "GeneBoxPlot", label = "Gene Summary", class = 'btn-primary')
                    
                     ),
                    
                    fluidRow(
                         dataTableOutput(outputId = "GMTTable") %>% 
                              withSpinner(color = "#0dc5c1"))

                    # ),
                    # 
                    # column(4,
                    #      h3("BioQC Profile"),
                    #      actionButton(inputId = "BioQC", label = "BioQC Analysis"),
                    #      plotOutput(outputId = "BioQCPlot") %>%
                    #           withSpinner(color = "#0dc5c1")
                    # )
               ),
               

               column(4, 
                      fluidRow(
                           br(),
                           h3("Results"),
                           style="margin-left :10px; margin-right :10px",
                           actionButton(inputId = "VolcanoPlotButton", label = "Volcano Plot", class = 'btn-primary'),
                           actionButton(inputId = "MAPlot", label = "MA Plot", class = 'btn-primary'),
                           actionButton(inputId = "BoxPlot", label = "Boxplot", class = 'btn-primary'),
                           actionButton(inputId = "TopTable", label = "Top Table", class = 'btn-primary')),
                      br(),
                      splitLayout(
                           numericInput(
                                inputId = "PValThres" ,
                                label = "Adjusted P-Value Threshold" ,
                                value = "5",
                                width = "90%",
                                step = 0.5
                           ),
                           
                           numericInput(
                                inputId = "LogFCThres" ,
                                label = "Log2 Fold Change Threshold" ,
                                value = "2",
                                width = "90%",
                                step = 0.5
                           )
                      ),
                      
                      fluidRow(
                           plotOutput(outputId = "VolcanoPlot") %>%
                                withSpinner(color = "#0dc5c1")
                      ),
                      
                      
                             h3("BioQC Profile"),
                             actionButton(inputId = "BioQC", label = "BioQC Analysis"),
                             plotOutput(outputId = "BioQCPlot") %>%
                                  withSpinner(color = "#0dc5c1")
                      
                      
                      
               )
          )
          )
          
     ) # TabPanel Set
     ) # Fluid Page
     
