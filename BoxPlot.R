
BoxplotGSE <- function(
    GeneData,
    BoxPlot_IndpVar,
    BoxPlot_PlotBy,
    BoxPlot_Type,
    #BoxPlot_ThemeSelect
    #BoxPlot_showData,
    #BoxPlot_showDataOption,
    #BoxPlot_showMargins,
    #coord_flip,
    #BoxPlot_main,
    #BoxPlot_xlab,
    #BoxPlot_ylab 
    ){
    
    message("Rendering Boxplot")
    if (input$BoxPlot_IndpVar == "s") {
        if (input$BoxPlot_PlotBy == "o") {
            message("Plotting Sample Overall Distribution")
            AesX <- FactorGMTMelt.Sampled[,GSM]
            AesY <- FactorGMTMelt.Sampled[,value]
            AesFill <- factor(FactorGMTMelt.Sampled[,input$BoxFactorSelectInput])
            GroupVar <- NULL
            xlabtext <- "GSMs in Dataset"
            legPos <- "none"
        
        } else if (input$BoxPlot_PlotBy == "f") {
            message("Plotting Sample Factor Distributions")
            AesX <- FactorGMTMelt.Sampled[,input$BoxFactorSelectInput]
            AesY <- FactorGMTMelt.Sampled$value
            AesFill <- FactorGMTMelt.Sampled[,input$BoxFactorSelectInput]
            GroupVar <- factor(FactorGMTMelt.Sampled[,input$BoxFactorSelectInput])
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }
    FactorGMTMelt <- FactorGMTMelt.Genes
        
    } else if (input$BoxPlot_IndpVar == "g") {
        if (input$BoxPlot_PlotBy == "o") {
            message("Plotting overall distribution for gene selection")
            AesX <- FactorGMTMelt.Genes[,"variable"]
            AesY <- FactorGMTMelt.Sampled[,"value"]
            AesFill <- FactorGMTMelt.Genes[,"variable"]
            GroupVar <- NULL
            xlabtext <- "Gene Names"
            legPos <- "none"
        
        } else if (input$BoxPlot_PlotBy == "f") {
            message("Plotting factor distribution for gene selection")
            AesX <- FactorGMTMelt[,"variable"]
            AesY <- FactorGMTMelt.Sampled[,"value"]
            AesFill <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            GroupVar <- factor(FactorGMTMelt[,input$BoxFactorSelectInput])
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }
    FactorGMTMelt <- FactorGMTMelt.Genes
    }
    p <- ggplot(data = FactorGMTMelt, aes_string(y = AesY, x = AesX, group = GroupVar, fill = AesFill))
}

             
     
        

        
     
        
         if (input$BoxPlot_Type == "Boxplot") { p <- p + geom_boxplot(varwidth = F)
         } else if (input$BoxPlot_Type == "Violin Plot") { p <- p + geom_violin()
         } else if (input$BoxPlot_Type == "Line Plot") { p <- p }
         p
         })

         if (input$BoxPlot_showColor) {
             if (input$BoxPlot_ThemeSelect == "default") { p <- p }
             else if (input$BoxPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
             else if (input$BoxPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
             else if (input$BoxPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
             else if (input$BoxPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
             else if (input$BoxPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
             else if (input$BoxPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
         }
      
         p <- p + theme(legend.position = legPos) +
             ylab(label = "Expression Level") +
             xlab(label = xlabtext) +
             guides(fill=guide_legend(title="Experimental Factor Groups")) +
             theme(axis.text.x = element_text(angle = input$BoxPlot_column_text_angle)) +
             theme(axis.text = element_text(size = 14, hjust = 1)) +
             theme(axis.title = element_text(size = 14)) +
             theme(legend.text=element_text(size=14))
      
      
      
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
      
         if (input$BoxPlot_showMargins==1) {
             p <-
                 p + theme(plot.margin = margin(
                 input$BoxPlot_margin_top,
                 input$BoxPlot_margin_right,
                 input$BoxPlot_margin_bottom,
                 input$BoxPlot_margin_left,
                 "cm"))
         }
      
      
      
         if (input$BoxPlot_PlotAxisFlip==1) { p <- p + coord_flip()}
         if (length(input$BoxPlot_main) > 0) { p <- p + labs(title = input$BoxPlot_main)}
         if (length(input$BoxPlot_xlab) > 0) { p <- p + labs(x = input$BoxPlot_xlab)}
         if (length(input$BoxPlot_ylab) > 0) { p <- p + labs(y = input$BoxPlot_ylab)}
      
         sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180)
         sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
      
         p <- ggplotly(p)
         p
       })