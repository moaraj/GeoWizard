
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
            
            
            fluidRow(
            column(4,  
            wellPanel(    
            
            fluidRow(
            column(12,
            
            
            column(12,h3("PCA options")),
            column(3, checkboxInput(inputId = "PCA_center",label = "Center Data")),
            column(3, checkboxInput(inputId = "PCA_scale", label = "Scale Data", value = 1)),
            column(3, checkboxInput(inputId = "MakeScree", label = "Scree Plot", value = 1)),
            column(3, checkboxInput(inputId = "MakeLoading", label = "Loadings", value = 1)),
            hr(),
            column(6, uiOutput("PCA_GroupUI")),
            column(6, uiOutput("PCA_LabelUI")),
            column(6, uiOutput("PCA_xcomp_UI")),
            column(6, uiOutput("PCA_ycomp_UI")),
            
            conditionalPanel(condition = "input.MakeScree == 1",
            column(12,
            hr(),h3("Scree Plot Options"),
            sliderInput(inputId = "nCompScree", label = "Number of Components in Scree plot", min = 1, max = 20, value = 5, step = 1),
            sliderInput(inputId = "ScreeYMax", label = "Y Max for Screer Plot", min = 0, max = 1, value = 1, step = 0.1),
            selectInput(inputId = "ScreePlotType", label = "Scree Plot Type", choices = c("pev", "cev"), selected = "pev"),
            conditionalPanel(condition = "input.ScreePlotType == 'pev'",
            "'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. "),
            conditionalPanel(condition = "input.ScreePlotType == 'cev'",
            "'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.")
            )),
            
            conditionalPanel(condition = "input.MakeLoading == 1",
            column(12,hr(),h3("Loadings Plot Options")),
            column(12, uiOutput("LoadingSelect_UI")),
            column(12, uiOutput("ShowNLoading_UI")))
            
            )))),
            
            column(6,
            h3("PCA Biplot"),
            plotlyOutput("PCA_BiPlot")%>% withSpinner(color = "#0dc5c1"),
            conditionalPanel(condition = "input.MakeScree == 1",
            h3("Scree Plot"),
            plotOutput("PCA_ScreePlot")%>% withSpinner(color = "#0dc5c1")),
            conditionalPanel("input.MakeLoading == 1",
            plotOutput("PCA_LoadingPlot")%>% withSpinner(color = "#0dc5c1")
            )
            )
            
            )
            
            
)

# Define server logic required to draw a histogram
server <- function(input, output) {

            
            FactorGMTCast <- reactive({
            FactorGMTMelt <- FactorGMTMelt
            FormulaCast <- colnames(FactorGMTMelt)  
            FactorNames <- FormulaCast[-grep(x = FormulaCast, pattern = "variable|value")]
              
            FormulaCast <- paste(FactorNames, collapse = " + ")
            FormulaCast <- paste(FormulaCast, "~ variable", collapse = "")
              
            FactorGMTCast <- dcast(FactorGMTMelt, FormulaCast)
            DataDF <- FactorGMTCast[,(length(FactorNames) + 1):ncol(FactorGMTCast)]
            FactorDF <- FactorGMTCast[,1:length(FactorNames)]
            return(list("FactorGMTCast" = FactorGMTCast, "DataDF" = DataDF,"FactorDF" = FactorDF))
            })
            
            
            #' @param  DataDF gene expression matrix with samples in the columns and genes in the rows
            #' @return list of Prcomp_res and PCA_ResDF
            #' Prcomp_res principle components object genertated from perfoemd PCA on gene expression data
            #' PCA_ResDF Data frame with the Prcomp_res object and Prcomp_res input matrix (x) coloumn bound
            PCA_Data <- reactive({
            PCA_DataInput <- FactorGMTCast()$DataDF
            Prcomp_res <- prcomp(na.omit(PCA_DataInput), center = as.logical(input$PCA_center), scale = as.logical(input$PCA_scale)) 
            #Prcomp_res <- prcomp(na.omit(DataDF),center = T, scale = T)
            PCA_ResDF <- cbind(PCA_DataInput, Prcomp_res$x)
            return(list("Prcomp_res" = Prcomp_res, "PCA_ResDF" = PCA_ResDF))
            })
            
            #' Render Input that allows user to select PCA grouping
            #' @param FactorDF
            output$PCA_GroupUI <- renderUI({
            FactorDF <- FactorGMTCast()$FactorDF
            FactorGrouping <- c("None", colnames(FactorDF))
            selectInput(inputId = "PCA_Group", label = "Group by Factor", choices = FactorGrouping, selected = "None", multiple = F)
            })
            
            #' Render Input that allows user to select PCA labeling factor
            output$PCA_LabelUI <- renderUI({
            FactorDF <- FactorGMTCast()$FactorDF
            FactorGrouping <- c("Sample Number", colnames(FactorDF))
            selectInput(inputId = "PCA_Label", label = "Label by Factor", choices = FactorGrouping, selected = "Sample Number", multiple = F)
            })
            
            #' Render input that allows user to select X axis of PCA Biplot
            output$PCA_xcomp_UI <- renderUI({
            Prcomp_res <- PCA_Data()$Prcomp_res$x
            CompOptions <- colnames(Prcomp_res)
            selectInput(inputId = "PCA_xcomp", label = "X Axis component", choices = CompOptions, selected = CompOptions[1])
            })

            #' Render input that allows user to select Y axis of PCA Biplot
            output$PCA_ycomp_UI <- renderUI({
            Prcomp_res <- PCA_Data()$Prcomp_res$x
            CompOptions <- colnames(Prcomp_res)
            selectInput(inputId = "PCA_ycomp", label = "Y Axis component", choices = CompOptions, selected = CompOptions[2])
            })

            #' Render PCA Biplot
            output$PCA_BiPlot <- renderPlotly({
            shiny::req(input$PCA_Group, input$PCA_Label, input$PCA_xcomp, input$PCA_ycomp)  
              
            PCA_ResDF <- PCA_Data()$PCA_ResDF
            Prcomp_res <-  PCA_Data()$Prcomp_res
            FactorDF <- FactorGMTCast()$FactorDF
              
            var_expl_x <- round(100 * Prcomp_res$sdev[as.numeric(gsub("[^0-9]", "", input$PCA_ycomp))]^2/sum(Prcomp_res$sdev^2), 1)
            var_expl_y <- round(100 * Prcomp_res$sdev[as.numeric(gsub("[^0-9]", "", input$PCA_ycomp))]^2/sum(Prcomp_res$sdev^2), 1)
            
            labeltype <- input$PCA_Label
            if (labeltype != "Sample Number") {labels <- FactorDF[, labeltype]
             } else { labels <- rownames(Prcomp_res$x)}
            
            grouping <- input$PCA_Group
            
            if(grouping == 'None'){
                # plot without grouping variable
                pc_plot_no_groups  <- ggplot(PCA_ResDF, aes_string(input$PCA_xcomp, input$PCA_ycomp)) +
                  geom_text(aes(label = labels),  size = 5) +
                  coord_equal() +
                  xlab(paste0(input$PCA_xcomp, " (", var_expl_x, "% explained variance)")) +
                  ylab(paste0(input$PCA_ycomp, " (", var_expl_y, "% explained variance)"))
                # the plot
                #pc_plot_no_groups <- ggplotly(pc_plot_no_groups)
                pc_plot_no_groups
                
              }  else {
                #plot with grouping variable
                PCA_ResDF$ExpVar <-  as.factor(FactorDF[, grouping])
                pc_plot_groups  <- ggplot(PCA_ResDF, aes_string(input$PCA_xcomp, input$PCA_ycomp, fill = 'ExpVar', colour = 'ExpVar')) +
                  stat_ellipse(geom = "polygon", alpha = 0.1) +
                  geom_text(aes(label = labels),  size = 5) +
                  scale_colour_discrete(guide = FALSE) +
                  guides(fill = guide_legend(title = "groups")) +
                  theme(legend.position="top") +
                  coord_equal() +
                  xlab(paste0(input$PCA_xcomp, " (", var_expl_x, "% explained variance)")) +
                  ylab(paste0(input$PCA_ycomp, " (", var_expl_y, "% explained variance)"))
                # the plot
                #pc_plot_groups <- ggplotly(pc_plot_groups)
                pc_plot_groups
              }
            })
            
            output$PCA_ScreePlot <- renderPlot({
              type <- input$ScreePlotType
              Prcomp_res <- PCA_Data()$Prcomp_res
              ScreeData <- Prcomp_res$sdev^2
              
              yvar <- switch(type, pev = ScreeData / sum(ScreeData), cev = cumsum(ScreeData) / sum(ScreeData))
              yvar.lab <- switch(type, pev = 'proportion of explained variance', cev = 'cumulative proportion of explained variance')
              
              ScreeDataDF <- data.frame(PC = 1:length(ScreeData), yvar = yvar)
              ScreeDataDFLine <- ScreeDataDF
              
              p <- ggplot(data = ScreeDataDF, aes(x = PC, y = yvar)) + xlab('principal component number') +
                ylim(c(0,input$ScreeYMax)) + xlim(c(0,(input$nCompScree + 1))) +
                ylab(yvar.lab) +  theme(text = element_text(size=12)) +
                geom_bar(stat="identity", fill="steelblue") + geom_point() + geom_line()
              p
            })
            
            output$LoadingSelect_UI <- renderUI({
            Prcomp_res <- PCA_Data()$Prcomp_res$rotation
            CompOptions <- colnames(Prcomp_res)
            selectInput(inputId = "LoadingSelect", label = "Component to show Loadings for", choices = CompOptions, selected = CompOptions[1])  
            })
            
            output$ShowNLoading_UI <- renderUI({
            Prcomp_res <- PCA_Data()$Prcomp_res$rotation
            nVars <- nrow(Prcomp_res)
            sliderInput(inputId = "ShowNloading", label = "Number of Loading Variables to Show", min = 1, max = 200, value = 5, step = 1)
            })
            
            PCA_LoadingData <- reactive({
            shiny::req(input$ShowNloading, input$ShowNloading)  
            
            Prcomp_res <-  PCA_Data()$Prcomp_res
            aload <- abs(Prcomp_res$rotation)
            loadings <- sweep(aload, 2, colSums(aload), "/")
            
            SelectedPC <- input$LoadingSelect
            PCLoading <- loadings[,SelectedPC]
            PCLoading <- sort(PCLoading, decreasing = T)

            loadingsDF <- data.frame(PCLoading)
            loadingsDF$Var <- rownames(loadingsDF)
            loadingsDF
            })
            
            output$PCA_LoadingPlot <- renderPlot({
            shiny::req(input$ShowNloading, input$ShowNloading)
            DF <- PCA_LoadingData()
            DF <- DF[1:(input$ShowNloading),]
            
            
            p <- ggplot(data = DF, aes(x = reorder(Var, -PCLoading), y = PCLoading)) + xlab('Variable') + 
            geom_bar(stat = "identity", fill="steelblue") +
            theme(text = element_text(size=12)) + theme(axis.text.x = element_text(angle = 90))
            p
            
            })
            
}

# Run the application 
shinyApp(ui = ui, server = server)


