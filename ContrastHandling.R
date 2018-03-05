library(shiny)
server <- shinyServer(function(input, output) {
    
    ExperimentalDesign <- reactiveValues()
    ExperimentalDesign$FilteredFactorDF <- reactive({
        iris[,4:5]
    })
    
    output$RearrangeLevels <- renderUI({
         FactorDF <- ExperimentalDesign$FilteredFactorDF()
         NamesIndex <- colnames(FactorDF)
         lapply(NamesIndex, function(ColName) {
             ColLevels <- factor(FactorDF[, ColName])
             inputName <- paste("Levels_", ColName, sep = "")
             selectInput(
                 inputId = inputName,
                 label = paste("Selected control level for", ColName),
                 choices = c(levels(ColLevels), "none")
             )
         })
     })
    
    
    ExperimentalDesign$ControlFactorDF <- reactive({
        DesignDF <- ExperimentalDesign$FilteredFactorDF()
        NamesIndex <- colnames(DesignDF)
         
        ResDF <- lapply(NamesIndex, function(ColName) {
            ResultVector <- DesignDF[, ColName]
            inputName <- paste("Levels_", ColName, sep = "")
            InputControlLevel <- input[[inputName]]
            res <- factor(ResultVector,
            levels=c(InputControlLevel, setdiff(as.character(ResultVector), InputControlLevel))
            )
         })
         names(ResDF) <- NamesIndex
         ResDF
    })
    
    ExperimentalDesign$RearrangeControlFactorDF <- reactive({
        FactorDF <- ExperimentalDesign$ControlFactorDF()
        FactorDF <- do.call(cbind.data.frame, FactorDF)
        DFindex <- c(1:nrow(FactorDF))
        FactorDF <- cbind.data.frame(DFindex, FactorDF)
        
        #shiny::req(input$formulaInputDesign)
        Designformula <- input$formulaInputDesign
        Designformula_trimmed <- gsub("\\s", "", Designformula)
        if (!grep(pattern = "^~",x = Designformula_trimmed)) {
        stop("Formula input must begin with ~")
        } else {
        Designformula_factors <- unlist(strsplit(Designformula_trimmed, split = "\\+"))
        Designformula_final <- gsub("~", "", Designformula_factors)
        }
        FormulaFactors <- grep(pattern = paste(Designformula_final,collapse = "|"), colnames(FactorDF), value = T)
        message(FormulaFactors)
        RearrangeDF <- FactorDF %>% dplyr::arrange(Species)
        
        DesignExpression <- try(as.formula(Designformula))
        if (class(DesignExpression)[1] == "try-error") { stop("Caught an error trying to make Design Matrix")
        } else { DesignMatrix <- model.matrix(as.formula(DesignExpression), FactorDF)}
        DesignMatrix
    })
    


    output$Table <- DT::renderDataTable({
        datatable(ExperimentalDesign$FilteredFactorDF())
    })
    
    output$Table_Rearrange <- DT::renderDataTable({
        shiny::req(input$Table_rows_all)
        datatable(ExperimentalDesign$RearrangeControlFactorDF())
    })
    
    # output$ModelMatrix <- DT::renderDataTable({
    #     datatable(ExperimentalDesign$DesignMatrixInput())
    # })

})

ui <- shinyUI(fluidPage(
    fluidRow(
    column(2, uiOutput('RearrangeLevels')),
    column(6,
    textInput( inputId = "formulaInputDesign", label = "Model Matrix Formula Input", placeholder = "~ Expvar1 + Expvar2"),
    column(12, actionButton(inputId = "SubmitFormula", label = "Generate Design Matrix")),
    DT::dataTableOutput('Table'),
    DT::dataTableOutput('Table_Rearrange'),
    DT::dataTableOutput('ModelMatrix'))
    )
    )
)

shinyApp(ui = ui, server = server)


