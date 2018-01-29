library(shiny)
library(rhandsontable)
Step_4 

ui <- fluidPage(theme = shinytheme("cerulean"),
              column(5, offset = 1, 
                     uiOutput("tables")
              )
     )

server <- function(input,output){
     
     observe({
          if(!is.null(Step_4)) {
               max_table = length(Step_4)
               
               lst <- list()
               for(i in 1:length(Step_4)){
                    lst[[i]] <- unique(Step_4[[i]])
               }
               
               output$tables <- renderUI({
                    plot_output_list <- lapply(1:max_table, function(i) {
                         tablename <- paste("tablename", i, sep="")
                         rHandsontableOutput(tablename)
                    })
                    do.call(tagList, plot_output_list)
               })
               
               for (i in 1:max_table){
                    local({
                         my_i <- i
                         tablename <- paste("tablename", my_i, sep="")
                         output[[tablename]] <- renderRHandsontable({
                              rhandsontable(data =lst[[my_i]]  )
                         })
                    })    
               }
          }
     })
     
}



shinyApp(ui = ui, server = server)
