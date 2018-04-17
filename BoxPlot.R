sampleFactorGMT <- function(FactorGMT, nFactors, SpecificGenes = NULL, nGenes){
    FactorCols <- 1:(nFactors + 1)
    SpecificGeneIndex <- NULL
    
    if (!missing(SpecificGenes)) {
    SpecificGeneRegEx <- paste(SpecificGenes, collapse = "|")
    SpecificGeneIndex <- grep(pattern = SpecificGeneRegEx, x = colnames(FactorGMT))
    }
    GeneIndex <- (nFactors + 2):ncol(FactorGMT)
    FactorGMT.sampled <- FactorGMT[, c(FactorCols, SpecificGeneIndex, sample(x = GeneIndex,size = nGenes)) ]
    return(FactorGMT.sampled)
}



BoxplotGSE <- function(
    FactorGMTMelt,
    BoxPlot_IndpVar,
    BoxPlot_PlotBy,
    BoxFactorSelectInput,
    BoxPlot_Type,
    BoxPlot_showColor,
    BoxPlot_ThemeSelect,
    
    BoxPlot_showData,
    BoxPlot_showDataOption,
    BoxPlot_JitterWidth,
    BoxPlot_JitterAlpha,
    Boxplot_JitterFill,
    
    BoxPlot_showMargins = F,            
    BoxPlot_margin_top = NULL,
    BoxPlot_margin_right = NULL,
    BoxPlot_margin_bottom = NULL,
    BoxPlot_margin_left = NULL,
    
    #coord_flip,
    #BoxPlot_main,
    #BoxPlot_xlab,
    #BoxPlot_ylab 
    BoxPlot_xlab_angle = 90
    ){
    
    message("Rendering Boxplot")
    if (BoxPlot_IndpVar == "s") {
        if (BoxPlot_PlotBy == "o") {
            message("Plotting Sample Overall Distribution")
            AesX <- FactorGMTMelt[,"GSM"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- factor(FactorGMTMelt[,BoxFactorSelectInput])
            GroupVar <- NULL
            xlabtext <- "GSMs in Dataset"
            legPos <- "none"
        
        } else if (BoxPlot_PlotBy == "f") {
            message("Plotting Sample Factor Distributions")
            AesX <- FactorGMTMelt[,BoxFactorSelectInput]
            AesY <- FactorGMTMelt$value
            AesFill <- FactorGMTMelt[,BoxFactorSelectInput]
            GroupVar <- factor(FactorGMTMelt[,BoxFactorSelectInput])
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }
    FactorGMTMelt <- FactorGMTMelt
        
    } else if (BoxPlot_IndpVar == "g") {
        if (BoxPlot_PlotBy == "o") {
            message("Plotting overall distribution for gene selection")
            AesX <- FactorGMTMelt[,"variable"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- FactorGMTMelt[,"variable"]
            GroupVar <- NULL
            xlabtext <- "Gene Names"
            legPos <- "none"
        
        } else if (BoxPlot_PlotBy == "f") {
            message("Plotting factor distribution for gene selection")
            AesX <- FactorGMTMelt[,"variable"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- factor(FactorGMTMelt[,BoxFactorSelectInput])
            GroupVar <- factor(FactorGMTMelt[,BoxFactorSelectInput])
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }

    }
    
    p <- ggplot(data = FactorGMTMelt, aes_string(y = AesY, x = AesX, group = GroupVar, fill = AesFill))
    
    if (BoxPlot_Type == "Boxplot") { p <- p + geom_boxplot(varwidth = F)
    } else if (BoxPlot_Type == "Violin Plot") { p <- p + geom_violin()
    } else if (BoxPlot_Type == "Line Plot") { p <- p }
    
    
    if (BoxPlot_showColor) {
        if (BoxPlot_ThemeSelect == "default") { p <- p }
        else if (BoxPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
        else if (BoxPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
        else if (BoxPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
        else if (BoxPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
        else if (BoxPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
        else if (BoxPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
    }
    
    
    p <- p + theme(legend.position = legPos) 
    p <- p + ylab(label = "Expression Level")
    p <- p + xlab(label = xlabtext)
    p <- p + scale_x_discrete(label=abbreviate)
    p <- p + guides(fill=guide_legend(title="Experimental Factor Groups")) 
    p <- p + theme(axis.text.x = element_text(angle = BoxPlot_xlab_angle)) 
    p <- p + theme(axis.text = element_text(size = 12, hjust = 1)) 
    p <- p + theme(axis.title = element_text(size = 14)) 
    p <- p + theme(legend.text=element_text(size=14))
    
    
    if (BoxPlot_showData==1) {
       if (BoxPlot_showDataOption == "jitter") { p <- p + geom_jitter(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else if(BoxPlot_showDataOption == "quasirandom"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else if(BoxPlot_showDataOption == "beeswarm"){ p <- p + geom_beeswarm(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else if(BoxPlot_showDataOption == "tukey"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "tukey", alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else if(BoxPlot_showDataOption == "frowney"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "frowney", alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else if(BoxPlot_showDataOption == "smiley"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "smiley", alpha = BoxPlot_JitterAlpha, fill =  Boxplot_JitterFill)
       } else { NULL }
    }
    
    

    if (BoxPlot_showMargins==1) {
        p <-p + theme(plot.margin = margin(
            BoxPlot_margin_top,
            BoxPlot_margin_right,
            BoxPlot_margin_bottom,
            BoxPlot_margin_left,
            "cm"))}

    return(p)
}

testGenes <- colnames(FactorGMT)[40:50]
test <- sampleFactorGMT(FactorGMT = FactorGMT, nFactors = 2, SpecificGenes = testGenes, nGenes = 10)
FactorGMTMelt <- melt(test)


BoxPlot_IndpVar = "s"
BoxPlot_IndpVar = "g"
BoxPlot_PlotBy = "o"
BoxFactorSelectInput <- colnames(FactorGMTMelt)[2]
BoxPlot_Type <- "Boxplot"
BoxPlot_showColor <- T
BoxPlot_ThemeSelect <- "theme_gray"
BoxPlot_xlab_angle <- 45
BoxPlot_showData <- T
BoxPlot_showDataOption <- "quasirandom"
BoxPlot_JitterWidth <- 0.1
BoxPlot_JitterAlpha <- 0.5
Boxplot_JitterFill <- AesFill

BoxplotGSE(FactorGMTMelt,BoxPlot_IndpVar, BoxPlot_PlotBy, 
                BoxFactorSelectInput, BoxPlot_Type, 
                BoxPlot_showColor, BoxPlot_ThemeSelect, 
                BoxPlot_xlab_angle, 
                BoxPlot_showData, BoxPlot_showDataOption, BoxPlot_JitterWidth, BoxPlot_JitterAlpha, Boxplot_JitterFill)
       # 
       # 
       # 

 
       # 
       # 
       # 
       #   if (BoxPlot_PlotAxisFlip==1) { p <- p + coord_flip()}
       #   if (length(BoxPlot_main) > 0) { p <- p + labs(title = BoxPlot_main)}
       #   if (length(BoxPlot_xlab) > 0) { p <- p + labs(x = BoxPlot_xlab)}
       #   if (length(BoxPlot_ylab) > 0) { p <- p + labs(y = BoxPlot_ylab)}
       # 
       #   sliderInput('BoxPlot_row_text_angle','Row Text Angle',value = 0,min=0,max=180)
       #   sliderInput('BoxPlot_column_text_angle','Column Text Angle',value = 45,min=0,max=180)
       # 
       #   p <- ggplotly(p)
       #   p
       # })