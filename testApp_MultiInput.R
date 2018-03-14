library(shiny)
ui <- fluidPage(
    fluidRow(
    column(4,
        actionButton("addContrast", "Add custom contrast"),
        actionButton("removeContrast", "Remove custom contrast"),
        uiOutput("CustomContrasts"),
        tableOutput("ContrastMatrix"),
        htmlOutput("text"),
        actionButton("GenerateCustomContrast", "Add Custom Contrasts to Contrast Matrix")
    )
    )
  
)

server <- function(session, input, output) {
  nUserContrasts <- reactiveValues(count = 1)
  observeEvent(input$addContrast, nUserContrasts$count <- nUserContrasts$count + 1)
  observeEvent(input$removeContrast, nUserContrasts$count <- nUserContrasts$count - 1)
  
    output$CustomContrasts <- renderUI({
        nInputs <- nUserContrasts$count
        if (nInputs <= 0) { stop("cannot remove anymore contrasts")}
        lapply(1:nInputs, function(i){
            ContrastTitleId = paste("ContrastTitle", i, sep = "_")
            ContrastTitlelabel = paste("Contrast", i, "title:")
            ContrastFormulaId = paste("ContrastFormula", i, sep = "_")
            ContrastFormulaLabel = paste("Contrast", i, "formula input:")
            # Try to change inputs to render the inputs without compeltly wiping them any time a new contast is added
            fluidRow(column(6, textInput(ContrastTitleId, ContrastTitlelabel)),
            column(6, textInput(ContrastFormulaId, ContrastFormulaLabel)))
        })
    })
    
    ContrastMatrixData <- eventReactive(input$GenerateCustomContrast,{
        nInputs <- nUserContrasts$count
        CustomContrasts <- lapply(1:nInputs, function(i){
            ContrastTitleId = paste("ContrastTitle", i, sep = "_")
            ContrastTitle <- input[[ContrastTitleId]]
            message(paste("Contrast Title",ContrastTitle))
            ContrastFormulaId = paste("ContrastFormula", i, sep = "_")
            ContrastFormula <- input[[ContrastFormulaId]]
            message(paste("Contrast Title",ContrastFormulaId))
            
            ContrastInputDF <- data.frame(ContrastTitle,ContrastFormula)
            MakeContrastInputString = apply(ContrastInputDF, MARGIN = 1, paste, collapse = "=")
            
            astr=paste(MakeContrastInputString, collapse=",")
            prestr="makeContrasts("
            poststr=",levels=c('A','B','C'))"
            commandstr=paste(prestr,astr,poststr,sep="")
            message(commandstr)
            MatrixColumn <- eval(parse(text=commandstr))
            
        })    
    })
    
    
    output$ContrastMatrix <- renderTable({
        ContrastMatrixData()
    })
    
    
  
output$text <- renderUI({
    HTML(paste(sprintf("You have chosen: %s</br>", nUserContrasts$count)))
    })
}

shinyApp(ui, server)