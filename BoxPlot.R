library(shiny)


ui <- fluidPage(
  fluidRow(
  column(4,
         
  wellPanel(
  fluidRow(
  column(12,
  
  
  h4('Plot Selection'), 
  column(6, selectizeInput( inputId = "BoxPlot_IndpVar", label = "Independant Variable", choices = c("Sample", "Gene"), selected = "")),
  column(6, selectizeInput(inputId = "BoxPlot_PlotBy", label = "Data to Plot", choices = c("Overall Distribution", "Factor-wise Distribution"), selected = "Overall Distribution" )),
  column(12, selectizeInput(inputId = "BoxPlot_Type",label = "Plot Type",choices = c("Boxplot", "Violin Plot", "Line Plot"), selected = "Boxplot")),
  column(12, sliderInput("BoxPlot_nGenes", "Number of Genes to Sample", min = 1, max = 100, value = 10)),

  h4('Plot Options'), 
  column(4,checkboxInput('BoxPlot_showData','Show Data')),
  column(4,checkboxInput('BoxPlot_showDataMean','Show Sample Means')),
  column(4,checkboxInput('BoxPlot_AddWhiskers','Change Whisker Defintion')),
  column(4,checkboxInput('BoxPlot_AddNotches','Add Notches')),
  column(4,checkboxInput('VariableWidth','Variable Width Box')),
  column(4,checkboxInput('BoxPlot_PlotAxisFlip','Axis Flip')),
  
  
  conditionalPanel('input.BoxPlot_showData==1', br(),hr(),
  h4("Data Point Plotting Options"),                 
  radioButtons(inputId = "BoxPlot_showDataOption", label = "", choices = c("jitter", "quasirandom", "beeswarm", "tukey", "frowney", "smiley"), selected = "jitter",inline = T),
  sliderInput(inputId = "BoxPlot_JitterWidth", label = "Data Point Plot Area Width", min = 0,max = 2,step = 0.05,value = 0.1)
  )),
  
  
  column(12,
  conditionalPanel('input.BoxPlot_showDataMean==1',hr(),
  h4("Sample Mean Options"),                 
  radioButtons(inputId = "SampleMeanConfidenceInterval", label = "Define Confidence Interval of Means", choices = list("85%"=0.85, "90%"=0.90, "95%"=0.95, "99%"=0.99), selected = "95%", inline = T)
  )),
  
  column(12,
  conditionalPanel('input.BoxPlot_AddWhiskers==1',hr(),
  h4("Definition of Whisker Extent"),                 
  radioButtons(inputId = "BoxPlot_WhiskerType", label = "", choices = list("Tukey"=0, "Spear"=1, "Altman"=2), selected = 0, inline = T),                 
  conditionalPanel('input.BoxPlot_WhiskerType==0', "Tukey - whiskers extend to data points that are less than 1.5 x IQR away from 1st/3rd quartile"),
  conditionalPanel('input.BoxPlot_WhiskerType==1', "Spear - whiskers extend to minimum and maximum values"),
  conditionalPanel('input.BoxPlot_WhiskerType==2', "Altman - whiskers extend to 5th and 95th percentile (use only if n>40)")
  )),
  

  column(12,hr(),h4('Additional Parameters')),
  
                                                                           
  column(3,checkboxInput('BoxPlot_showColor','Color')),
  column(3,checkboxInput('BoxPlot_showMargin','Labels and Title')),
  column(3,checkboxInput('BoxPlot_showPlotSize','Plot Size')),
  hr(),
  
  column(12,
  conditionalPanel('input.BoxPlot_showColor==1',
  hr(),
  h4('Color Manipulation'),
  uiOutput('colUI'),
  sliderInput("BoxPlot_ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
  checkboxInput('BoxPlot_colRngAuto','Auto Color Range',value = T),
  conditionalPanel('!input.colRngAuto',uiOutput('colRng'))
  )),
                                                                           
  column(12,
  conditionalPanel('input.BoxPlot_showMargin==1',
  hr(),
  h5('Widget Layout'),
  column(4,textInput('BoxPlot_main','Title','')),
  column(4,textInput('BoxPlot_xlab','X Title','')),
  column(4,textInput('BoxPlot_ylab','Y Title','')),
  sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180),
  sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
  
  )),
  
  column(12,
  conditionalPanel('input.BoxPlot_showPlotSize==1',
  hr(),
  h4('Plot Size Options'),
  numericInput("BoxPlot_Height", "Plot height:", value=550),
	numericInput("BoxPlot_Width", "Plot width:", value=750)
	))
  
  ))),
  
  column(6,
  fluidRow(
  column(12,uiOutput("BoxPlotUI")),
  column(4, actionButton(inputId = "RefreshPlot", label = "Refresh Plot",icon = icon('refresh')))
  ))
  )
  )

  
  
server <- function(input, output) {
  # Libraries
  library(plotly)
  library(ggplot2)
  library(reshape2)
  library(ggbeeswarm)

  
  BoxPlotData <- reactive({
    input$RefreshPlot
    DF <- iris
    DF <- melt(DF)
    })
  
  output$BoxPlotly <- renderPlotly({
    DF <- BoxPlotData()
    DF <- data.frame(DF)
    
    p <- ggplot(data = DF, aes(x = variable, y = value))
    
    if (input$BoxPlot_Type == "Boxplot") { p <- p + geom_boxplot()
    } else if (input$BoxPlot_Type == "Violin Plot") {p <- p + geom_violin()
    } else if (input$BoxPlot_Type == "Line Plot") { p <- p # bean plot code
    }
    
    if (input$BoxPlot_showData==1) {
      JitterWidth <- input$BoxPlot_JitterWidth
          if (input$BoxPlot_showDataOption == "jitter") { p <- p + geom_jitter(width = JitterWidth) 
    } else if(input$BoxPlot_showDataOption == "quasirandom"){ p <- p + geom_quasirandom(width = JitterWidth)
    } else if(input$BoxPlot_showDataOption == "beeswarm"){ p <- p + geom_beeswarm(width = JitterWidth)
    } else if(input$BoxPlot_showDataOption == "tukey"){ p <- p + geom_quasirandom(width = JitterWidth, method = "tukey")
    } else if(input$BoxPlot_showDataOption == "frowney"){ p <- p + geom_quasirandom(width = JitterWidth, method = "frowney")
    } else if(input$BoxPlot_showDataOption == "smiley"){ p <- p + geom_quasirandom(width = JitterWidth, method = "smiley")
    } else { NULL }
    }
    
    
    if (input$BoxPlot_PlotAxisFlip==1) { p <- p + coord_flip()}
    if (length(input$BoxPlot_main) > 0) { p <- p + labs(title = input$BoxPlot_main)}
    if (length(input$BoxPlot_xlab) > 0) { p <- p + labs(x = input$BoxPlot_xlab)}
    if (length(input$BoxPlot_ylab) > 0) { p <- p + labs(y = input$BoxPlot_ylab)}
    
    #sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180)
    #sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
    
    p <- ggplotly(p)
    p
  })
  
  output$BoxPlotUI <- renderUI({
    if(input$BoxPlot_showPlotSize){ 
      plotHeight <- input$BoxPlot_Height
      plotWidth <- input$BoxPlot_Width
      } else { 
      plotHeight <- NULL
      plotWidth <- NULL 
      }
    plotlyOutput(outputId = "BoxPlotly",height = plotHeight, width = plotWidth)})
  }
  


shinyApp(ui = ui, server = server)

