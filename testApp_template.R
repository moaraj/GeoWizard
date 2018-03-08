library(shiny)
ui <- shinyUI(fluidPage(
    
    column(4,
    wellPanel(
           fluidRow(
    
    column(12,
    fileInput("file1", "Choose CSV File", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
    tags$hr(),
    column(4,
    strong("Header"),
    checkboxInput("header", "Data has header", TRUE),
    radioButtons("disp", "Display", choices = c(Head = "head", All = "all"), selected = "head")
    ),
    column(4,radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
    column(4,radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')),
    tags$hr()
    )
    )
    )
    ),
    column(6, wellPanel( tableOutput("contents")))

  )

)


server <- shinyServer(function(input, output) {
    
    output$RearrangeLevels <- renderUI({
    radioButtons("DataSourceSelection", inputId = paste("Retreive GMT file", "GSE"), selected = 1, inline = T, choiceNames = c("Download from GEO", "Upload CSV") , choiceValues = c(1,2))
    # conditionalPanel(paste("input.",checkinputID,"==1", sep = ""))
    
    
    })
    
    
      output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })



})


shinyApp(ui = ui, server = server)


