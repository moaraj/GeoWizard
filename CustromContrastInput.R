library(shiny)
ui <- fluidPage(
    fluidRow(
    column(4,
        
        column(12, br()),
        column(6, actionButton("addContrast", "Add custom contrast", width = "100%")),
        column(6, actionButton("removeContrast", "Remove custom contrast", width = "100%")),
        column(12, br()),
        column(12, helpText("Please do not use spaces or special characters in the inputs below")),
        column(12, uiOutput("UserContrasts")),
        column(12, tableOutput("UserContrastMatrix")),
        column(12, hr()),
        column(7, radioButtons( inputId = "UseContrastOption", "User Contrast Input", inline = T, 
        choices = c("Append to Contrast Matrix", "Use as Contrast Matrix"))),
        column(5, style = "margin-top: 5px;", 
            actionButton("GenerateCustomContrast", "Generate Contrast Matrix", 
             style = "font-weight: bold;",width = "100%"))
    )
    )
  
)

server <- function(session, input, output) {
  nUserContrasts <- reactiveValues(count = 1)
  observeEvent(input$addContrast, nUserContrasts$count <- nUserContrasts$count + 1)
  observeEvent(input$removeContrast, nUserContrasts$count <- nUserContrasts$count - 1)
  
    output$UserContrasts <- renderUI({
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
    
    UserContrastMatrixData <- eventReactive(input$GenerateCustomContrast,{
        nInputs <- nUserContrasts$count
        
        UserContrasts <- lapply(1:nInputs, function(i){
            DesignMatrix <- c("A", "B", "C")
            #DesignMatrix <- ############## requires Design MAtrix Input from Shiny APP or dummy input
            
            ContrastTitleId = paste("ContrastTitle", i, sep = "_")
            ContrastTitle <- input[[ContrastTitleId]]
            message(paste("Contrast Title",ContrastTitle))
            ContrastFormulaId = paste("ContrastFormula", i, sep = "_")
            ContrastFormula <- input[[ContrastFormulaId]]
            message(paste("Contrast Title",ContrastFormulaId))
            
            if (isTruthy(ContrastFormula) & isTruthy(ContrastTitle)) {
                message(paste("Valid inputs for contrast", i, "found"))
                ContrastInputDF <- data.frame(ContrastTitle,ContrastFormula)
                MakeContrastInputString = apply(ContrastInputDF, MARGIN = 1, paste, collapse = "=")
                astr=paste(MakeContrastInputString, collapse=",")
                prestr="makeContrasts("
                poststr=",levels=DesignMatrix)"
                commandstr=paste(prestr,astr,poststr,sep="")
                message(commandstr)
                MatrixColumn <- eval(parse(text=commandstr))
                return(MatrixColumn)
                } else { return(NULL) }
        })
        
        UserContrasts <- UserContrasts[!vapply(UserContrasts, is.null, logical(1))]
        if (length(UserContrasts) == 0) { stop("Please enter valid contrast title(No Spaces) and formula")
        }
        UserContrasts <- do.call(cbind.data.frame, UserContrasts)
        UserContrasts
    })
    
    output$UserContrastMatrix <- renderTable({UserContrastMatrixData()})
    
    
  
output$text <- renderUI({
    HTML(paste(sprintf("You have chosen: %s</br>", nUserContrasts$count)))
    })
}

shinyApp(ui, server)