ui <- fluidPage(
    fluidRow(
    column(4,
    uiOutput("DesignMatrixRename_UI"),
    DT::dataTableOutput("Table")
    )
    )
    
)

server <- function(input, output) {
    ExperimentalDesign <- reactiveValues()
    ExperimentalDesign$DesignMatrixInput <- reactive({
        colnames(mtcars)[1]<- c("moaraj is a foucce")
        mtcars})
    
    output$DesignMatrixRename_UI <- renderUI({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        lapply(colnamesIndex, function(FactorNameIndex){
            origtext <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            checkinpuLabel <- paste("Rename column",FactorNameIndex, "-", substr(origtext, start=1, stop=20))

            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            textInputLabel <- paste("new column", FactorNameIndex, "name:")
            textInputplaceholder = str_split(origtext, pattern = " ", simplify = T)
            
            if(length(textInputplaceholder)>=3) {textInputplaceholder <- paste(textInputplaceholder[1,2:3], collapse = "_")}
            textInputplaceholder
            
            fluidRow(
            column(12, checkboxInput(checkinputID, checkinpuLabel)),
            conditionalPanel(paste("input.",checkinputID,"==1", sep = ""), 
            column(12, textInput(textInputID ,textInputLabel,textInputplaceholder))))
        })
    })
    
    ExperimentalDesign$DesignMatrixRename <- reactive({
        DesignMatrix <- ExperimentalDesign$DesignMatrixInput()
        colnamesIndex <- 1:length(colnames(DesignMatrix))
        
        NewColNames <- 
        lapply(colnamesIndex, function(FactorNameIndex){
            colnameText <- colnames(DesignMatrix)[FactorNameIndex]
            checkinputID <- paste("RenameDesign", FactorNameIndex, sep = "")
            textInputID <- paste("RenameDesignText", FactorNameIndex, sep = "")
            
            if (input[[checkinputID]] == T) {
            InputName <- input[[textInputID]]
            InputName <- gsub("[[:space:]]", "", InputName)
            colnameText <- InputName
            } else {colnameText}
        })
        colnames(DesignMatrix) <- unlist(NewColNames)
        DesignMatrix
    })
    
    output$Table <- DT::renderDataTable({
        DesignMatrix <- ExperimentalDesign$DesignMatrixRename()
        datatable(DesignMatrix)
    })
    
    
}

shinyApp(ui, server)
