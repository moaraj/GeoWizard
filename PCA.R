library(factoextra)
library(FactoMineR)
library(shiny)

shinyApp(
     
     ui = fluidPage(
          fluidRow(
               column(12, plotOutput("PCA"))),
          fluidRow(
               column(6, plotOutput("ElbowPlot")),
               column(6, plotOutput("CorrelationCircle"))),
          
          fluidRow(
               column(6, plotOutput("Contrib1")),
               column(6, plotOutput("Contrib2"))
               )
          
          
          
          
     ),
     
     server = function(input, output) {
          
          PCAData <- reactiveValues()
          
          PCAData$Res <- reactive({
               dat <- mtcars
               PCA(dat, graph = F, scale.unit = T)
          })
          
          output$PCA <- renderPlot({
               fviz_pca_ind(PCAData$Res(), col.ind = "cos2",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE)
          })
          
          output$ElbowPlot <- renderPlot({
               fviz_eig(PCAData$Res() , addlabels = TRUE, ylim = c(0, 50)) # ScreePlot
          })
          
          output$CorrelationCircle <- renderPlot({
               fviz_pca_var(PCAData$Res(), col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
          })
          
          output$Contrib1 <- renderPlot({
               fviz_contrib(PCAData$Res(), choice = "var", axes = 1, top = 10) # Contributions of variables to PC1
               })
          
          output$Contrib2 <- renderPlot({
               fviz_contrib(PCAData$Res(), choice = "var", axes = 2, top = 10) # Contributions of variables to PC1
          })
          
          
          
          
          
     }
)
