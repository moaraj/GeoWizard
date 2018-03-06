library(shiny)
ui <- shinyUI(fluidPage(
    fluidRow(column(4, uiOutput('RearrangeLevels')))

))


server <- shinyServer(function(input, output) {
    
    ExperimentalDesign <- reactiveValues()
    ExperimentalDesign$FilteredFactorDF <- reactive({
        mtcars
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


})


shinyApp(ui = ui, server = server)


