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
        ) # Download Data tabPanel
        
        )
        )
        )
        )
        )
        
    ) #dashboard body
) # Darash board page
)
server <- shinyServer(function(input, output){})

shinyApp(ui = ui, server = server)
