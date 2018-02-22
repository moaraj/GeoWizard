
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  ui <- bootstrapPage(
    mainPanel(
      titlePanel("Interactive PCA Explorer"),
      
      tabsetPanel(
 
        
        tabPanel("Compute PCA",
                 
                 p("Choose the columns of your data to include in the PCA."),
                 p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
                 p("The PCA is automatically re-computed each time you change your selection."),
                 p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                 p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
                 uiOutput("choose_columns_pca"),
                 tags$hr(),
                 p("Select options for the PCA computation (we are using the prcomp function here)"),
                 radioButtons(inputId = 'center',  
                              label = 'Center',
                              choices = c('Shift variables to be zero centered'='Yes',
                                          'Do not shift variables'='No'), 
                              selected = 'Yes'),
                 
                 radioButtons('scale.', 'Scale',
                              choices = c('Scale variables to have unit variance'='Yes',
                                          'Do not scale variables'='No'), 
                              selected = 'Yes')
                 
        ), # end  tab
        
        
        
        tabPanel("PC Plots",
                 h2("Scree plot"),
                 p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
                 plotOutput("plot2", height = "300px"),
                 tags$hr(),
                 h2("PC plot: zoom and select points"),
                 p("Select the grouping variable."),
                 p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
                 uiOutput("the_grouping_variable"),
                 tags$hr(),
                 p("Select the PCs to plot"),
                 uiOutput("the_pcs_to_plot_x"),
                 uiOutput("the_pcs_to_plot_y"),
                 tags$hr(),
                 
                 p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                 p("Then select points on zoomed plot below to get more information about the points."),
                 p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
                 plotOutput ("z_plot1", height = 400,
                             brush = brushOpts(
                               id = "z_plot1Brush",
                               resetOnNew = TRUE)),
                 tags$hr(),
                 
                 p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
                 
                 plotOutput("z_plot2", height = 400,
                            brush = brushOpts(
                              id = "plot_brush_after_zoom",
                              resetOnNew = TRUE)),
                 tags$hr(),
                 p("Details of the brushed points"),
                 tableOutput("brush_info_after_zoom")
        ), # end  tab 
        
        
        
        tabPanel("Colophon",
                 p("The code for this Shiny app is online at ", a("https://github.com/benmarwick/Interactive_PCA_Explorer", href = "https://github.com/benmarwick/Interactive_PCA_Explorer"), ". Please post any feedback, question, etc. as an ", a("issue on github", href = "https://github.com/benmarwick/Interactive_PCA_Explorer/issues/new"), "."),
                 p("The text is licensed ", a("CC-BY", href = "http://creativecommons.org/licenses/by/4.0/"), " and the code ", a(href = "https://opensource.org/licenses/MIT", "MIT"), ".")
                 
                 
        ) # end  tab 
        
        
      ))) 
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # global items 
  
  # check if pkgs are installed already, if not, install automatically:
  # (http://stackoverflow.com/a/4090208/1036500)
  list.of.packages <- c("ggplot2", 
                        "DT", 
                        "GGally",
                        "psych",
                        "Hmisc",
                        "MASS",
                        "tabplot")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # load all these
  lapply(list.of.packages, require, character.only = TRUE)
  
  server <- function(input, output) {
    
    
    pca_objects <- reactive({
      # Keep the selected columns

      the_data <- ArrayData
      # from http://rpubs.com/sinhrks/plot_pca
      pca_output <- prcomp(na.omit(the_data), 
                           center = T, 
                           scale. = T)
      # data.frame of PCs
      pcs_df <- cbind(the_data, pca_output$x)
      
      return(list(the_data = the_data, 
                  the_data_subset = the_data_subset,
                  pca_output = pca_output, 
                  pcs_df = pcs_df))
      
    })
    
    output$the_pcs_to_plot_x <- renderUI({
      pca_output <- pca_objects()$pca_output$x
      
      # drop down selection
      selectInput(inputId = "the_pcs_to_plot_x", 
                  label = "X axis:",
                  choices= colnames(pca_output), 
                  selected = 'PC1')
    })
    
    output$the_pcs_to_plot_y <- renderUI({
      pca_output <- pca_objects()$pca_output$x
      
      # drop down selection
      selectInput(inputId = "the_pcs_to_plot_y", 
                  label = "Y axis:",
                  choices= colnames(pca_output), 
                  selected = 'PC2')
    })
    
    
    
    output$plot2 <- renderPlot({
      pca_output <- pca_objects()$pca_output
      eig = (pca_output$sdev)^2
      variance <- eig*100/sum(eig)
      cumvar <- paste(round(cumsum(variance),1), "%")
      eig_df <- data.frame(eig = eig,
                           PCs = colnames(pca_output$x),
                           cumvar =  cumvar)
      ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
        geom_bar(stat = "identity", fill = "white", colour = "black") +
        geom_text(label = cumvar, size = 4,
                  vjust=-0.4) +
        theme_bw(base_size = 14) +
        xlab("PC") +
        ylab("Variances") +
        ylim(0,(max(eig_df$eig) * 1.1))
    })
    
    
    # PC plot
    pca_biplot <- reactive({
      pcs_df <- pca_objects()$pcs_df
      pca_output <-  pca_objects()$pca_output
      
      var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
      var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
      labels <- rownames(pca_output$x)
      grouping <- input$the_grouping_variable
      
      if(grouping == 'None'){
        # plot without grouping variable
        pc_plot_no_groups  <- ggplot(pcs_df, 
                                     aes_string(input$the_pcs_to_plot_x, 
                                                input$the_pcs_to_plot_y
                                     )) +
          
          
          geom_text(aes(label = labels),  size = 5) +
          theme_bw(base_size = 14) +
          coord_equal() +
          xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
          ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
        # the plot
        pc_plot_no_groups
        
        
      } else {
        # plot with grouping variable
        
        pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
        pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                                     input$the_pcs_to_plot_y, 
                                                     fill = 'fill_', 
                                                     colour = 'fill_'
        )) +
          stat_ellipse(geom = "polygon", alpha = 0.1) +
          
          geom_text(aes(label = labels),  size = 5) +
          theme_bw(base_size = 14) +
          scale_colour_discrete(guide = FALSE) +
          guides(fill = guide_legend(title = "groups")) +
          theme(legend.position="top") +
          coord_equal() +
          xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
          ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
        # the plot
        pc_plot_groups
      }
      
      
    })
    
    output$brush_info <- renderTable({
      # the brushing function
      brushedPoints(pca_objects()$pcs_df, input$plot_brush)
    })
    
    
    # for zooming
    output$z_plot1 <- renderPlot({
      
      pca_biplot() 
      
    })
    
    # zoom ranges
    zooming <- reactiveValues(x = NULL, y = NULL)
    
    observe({
      brush <- input$z_plot1Brush
      if (!is.null(brush)) {
        zooming$x <- c(brush$xmin, brush$xmax)
        zooming$y <- c(brush$ymin, brush$ymax)
      }
      else {
        zooming$x <- NULL
        zooming$y <- NULL
      }
    })
    
    
    # for zooming
    output$z_plot2 <- renderPlot({
      
      pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y) 
      
      
    })
    
    output$brush_info_after_zoom <- renderTable({
      # the brushing function
      brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
    })
    
    output$pca_details <- renderPrint({
      # 
      print(pca_objects()$pca_output$rotation)
      summary(pca_objects()$pca_output)
      
    })
    
    output$Colophon <- renderPrint({
      
      
    })
    
    
  }


# Run the application 
shinyApp(ui = ui, server = server)

