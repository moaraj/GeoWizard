

ui <- fluidPage(
    fluidRow(
    column(4,
    wellPanel(
    uiOutput("FactorClass_UI"))
    
    )
    )
)

server <- function(input, output) {
    
    FactorClassList <- reactiveValues()
    FactorClassList$Input <- reactive({Step_4})
    
    
    output$FactorClass_UI <- renderUI({
        FactorClassList <- FactorClassList$Input()
        
        FactorClassListIndex <- names(FactorClassList)
        lapply(FactorClassListIndex, function(Factor){
            FactorDF <- FactorClassList[[Factor]]
            
            expblockInputID <-  paste(Factor,"expblockInput", sep = "_")
            biologyInputID <- paste(Factor,"biologyInput", sep = "_")
            gsmtypeInputID <- paste(Factor,"gsmtypeInput", sep = "_")
            
            expblocks <- c("time", "drug", "biological context", "none")
            biology <- c("cellsystem", "tissue", "disease", "genetic modification", "none")
            gsmType <- c("control", "perturbation")
            
            RenameTextInputID <- paste(Factor,"TextInput", sep = "_")
            LevelOutputID <- paste(Factor,"LevelOutput", sep = "_")
            
            fluidRow(
            column(12, h4(paste(Factor, "classification"))),
            column(12, verbatimTextOutput(outputId = LevelOutputID )),
            column(4, selectInput(inputId = expblockInputID,label = "Factor Type", choices = expblocks, selected = "none")),
            column(4, selectInput(inputId = biologyInputID,label = "Biological Context", choices = biology, selected = "none")),
            column(4, textInput(inputId = RenameTextInputID, label = paste("Rename",Factor,"to:"), placeholder = "cell line"))
            )
            
            })
        })
    
    observe({
        FactorClassList <- FactorClassList$Input()
        FactorClassListIndex <- names(FactorClassList)
        lapply(FactorClassListIndex, function(Factor){
            FactorDF <- FactorClassList[[Factor]]
            LevelOutputID <- paste(Factor,"LevelOutput", sep = "_")
            
            FactorLevels <- unique(FactorDF[,1])
            nFactorLevels <- length(unique(FactorDF[,1]))
            DisplayText <- paste("factor level", 1:nFactorLevels,"text:",FactorLevels, "\n" )
            if (length(DisplayText) > 2 ) { DisplayText <- DisplayText[1:2]
            }
            output[[LevelOutputID]] <- renderText({DisplayText})
        })
    
    })

}

shinyApp(ui, server)
