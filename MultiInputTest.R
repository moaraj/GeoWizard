ui <- fluidPage(
    uiOutput("UI")
)

server <- function(input, output) {
    UIinfo <- reactive({x <- paste("moaraj", 1:10, sep = "")})
    
    output$UI <- renderUI({
        x <- UIinfo()
        #checkboxGroupInput(inputId = "Moaraj1",label = "Moaraj1", choices = c(1:5), selected = 1, inline = F)
        y <- lapply(x, function(y){
            column(3,
            checkboxGroupInput(inputId = y, label = y,choices = 1:5,selected = 1,inline = F))
            })
        
        y <- fluidRow(y)
        y
    })
}

shinyApp(ui, server)
