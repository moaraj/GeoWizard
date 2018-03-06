library(shiny)
ui <- shinyUI(
    dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(

    
    column(4,
           
    wellPanel(
    fluidRow(
    uiOutput("DownloadDataInfoBox"),
        
    column(12, uiOutput("InputSourceGMT")),
    column(12,
    hr(),
    conditionalPanel('input.DataSourceSelection==1',
    actionButton(
          inputId = "DownloadGEOData",
          label = "Download",
          icon = icon("download"),
          block = T), hr(),
    uiOutput("GeneAnnotationTypeUI"),hr()), 
           
    conditionalPanel('input.DataSourceSelection==2',
    fileInput("GMTcsv", "Choose GMT File", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
    tags$hr(),
    column(4,
    strong("Header"),
    checkboxInput("CSVheader", "Data has header", TRUE)),
    column(4,radioButtons("CSVsep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
    column(4,radioButtons("CSVquote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))
    )
    ), # Conditional Panel CSV input
    
    hr(),
    column(12,
    radioButtons(inputId = "ExpressionDataType",
        label = "Gene Expression Data Type",
        choiceNames = c("MicroArray", "NGS Sequencing", "Single Cell Sequencing"),
        choiceValues = c("mArray", "RNAseq", "ssRNAseq"),
        selected = "mArray")),
    br(),
    column(12,
    radioButtons(
            inputId = "RawDataTableMelt", 
            label = "Which Data Matrix to Show" , 
            choiceNames = c("Gene Matrix","Melted Gene Matrix with Factors"),
            choiceValues = c("GMT", "FactorGMTMelt"), 
            inline = T)),
    
    column(12,textInput(inputId = "GsmTableSelect",label = "Test GSE input",value = "GSE60482")),
    column(12,textInput(inputId = "GplTableSelect",label = "Test GSE input",value = "GPL9250"))
    )
    )
    ),
    column(6, wellPanel( DT::dataTableOutput("GMTcsvTable")))

  )
  )

)


server <- shinyServer(function(input, output) {
    
    ###### Download the Data
    GSEdata <- reactiveValues()
    
    output$DownloadDataInfoBox <- renderUI({
    column(12,
    tags$div(id="pane",
    fluidRow(valueBox(width = 12, "GSE:", "GPL:" )),
    tags$style(type="text/css","#pane{font-size:20px;}")))    
    })
    
    output$InputSourceGMT <- renderUI({
    radioButtons(inputId = "DataSourceSelection", 
        label = paste("Retreive Gene Matrix file for", "GSE"), 
        selected = 1, 
        inline = T, 
        choiceNames = c("Download from GEO", "Upload GMT as CSV or TSV"),
        choiceValues = c(1,2))
    })
    
    GSEdata$GMTinput_GEO <- eventReactive(input$DownloadGEOData, {
        shiny::req(input$GsmTableSelect, input$DownloadGEOData)
        shinyjs::show("GMTTableDiv")
        
        GSE <- input$GsmTableSelect
        GPL <- input$GplTableSelect
        message(paste("Downloading", GSE, "Data from GEO"))
        GSEeset <- LoadGEOFiles(GSE, GPL, GeoRepoPath = "~/GeoWizard/GEORepo")
        GSEeset
    })
    
    GSEdata$GMTinput_CSV <- reactive({
        req(input$GMTcsv)
        DF <- read.csv(input$GMTcsv$datapath, header = input$CSVheader, sep = input$CSVsep, quote = input$CSVquote)
        DF
    })
    
    GSEdata$GSEeset <- reactive({
        shiny::req(input$GsmTableSelect, input$DownloadGEOData)
        if (input$DataSourceSelection == 1) {
        GSEeset <- GSEdata$GMTinput_GEO()
            
        } else if(input$DataSourceSelection == 2) {
        GSEeset <- GSEdata$GMTinput_CSV()
        }
        GSEeset
        })
    
    output$GeneAnnotationTypeUI <- renderUI({
        GSEeset <- GSEdata$GSEeset()
        FeatureData <- fData(GSEeset)
        if (length(FeatureData) == 0) {
        wellPanel(
            h4(icon("exclamation-triangle"),"no feature data included in eset"),
            "You can retart the application or download the raw files and process them locally")
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
        
    output$GMTcsvTable <- DT::renderDataTable({
        DF <- GSEdata$GMTinput_CSV()
        DT::datatable(data = DF, rownames = TRUE,  extensions = 'Buttons', 
            options = list( scrollY = '300px',  dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel')))
    })

})


shinyApp(ui = ui, server = server)


