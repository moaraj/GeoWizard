ui <- fluidPage(
    fluidRow(
    column(4,
    uiOutput("UI")
    )
    )
    
)

server <- function(input, output) {
    ExperimentalDesign <- reactiveValues()
    ExperimentalDesign$DesignMatrixInput <- reactive({
        colnames(mtcars)[1]<- c("moaraj is a foucce")
        mtcars})
    
    output$UI <- renderUI({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        lapply(colnamesIndex, function(FactorNameIndex){
            origtext <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            checkinpuLabel <- paste("Rename column",FactorNameIndex, "-", substr(origtext, start=1, stop=20))

            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            textInputLabel <- paste("new column", FactorNameIndex)
            textInputplaceholder = str_split(origtext, pattern = " ", simplify = T)
            
            if(length(textInputplaceholder)>=3) {textInputplaceholder <- paste(textInputplaceholder[1,2:3], collapse = "_")}
            textInputplaceholder
            
            fluidRow(
            column(6, style = "margin-top: 25px;", checkboxInput(checkinputID, checkinpuLabel)),
            conditionalPanel(paste("input.",checkinputID,"==1", sep = ""), 
            column(6, textInput(textInputID ,"",textInputplaceholder))))
        })
    })
}

shinyApp(ui, server)
