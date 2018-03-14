#' Return a subsampled Factor GMT with specific genes and/or gene sample size
#' @param FactorGMT a dataframe containing GSM, Factor Columns followd by Gene Count columns
#' @param nFactors number of factor columns in factorGMT
#' @SpecificGenes character vector of specific gene to include in subsample
#' @nGenes number of genes to include in sampled data
sampleFactorGMT <- function(FactorGMT, nFactors, SpecificGenes = NULL, nGenes){
    message("Generating sampled Factor GMT")
    FactorCols <- 1:(nFactors + 1)
    SpecificGeneIndex <- NULL
    
    if (!missing(SpecificGenes) & nchar(SpecificGenes) > 3) {  # Find different way other than nchar
    message("Generating sampled Factor GMT - Specific gene query detected")
    Regexinput <- paste("\\b", SpecificGenes, "\\b", sep = "") # Define and beginning of the search word
    SpecificGeneRegEx <- paste(SpecificGenes, collapse = "|") # collapse all regex pattern with and statement
    SpecificGeneIndex <- grep(pattern = SpecificGeneRegEx, x = colnames(FactorGMT))
    if(length(SpecificGeneIndex) > 20 ){ SpecificGeneIndex <- SpecificGeneIndex[1:20]}
    }
    GeneIndex <- (nFactors + 2):ncol(FactorGMT)
    FactorGMT.sampled <- FactorGMT[, c(FactorCols, SpecificGeneIndex, sample(x = GeneIndex,size = nGenes)) ]
    return(FactorGMT.sampled)
}

# testGenes <- colnames(FactorGMT)[40:50]
# test <- sampleFactorGMT(FactorGMT = FactorGMT, nFactors = 2, SpecificGenes = testGenes, nGenes = 10)
# test <- sampleFactorGMT(FactorGMT = FactorGMT, nFactors = 2, SpecificGenes = NULL, nGenes = 10)
# FactorGMTMelt <- melt(test)


#' Generate GGplot BoxPlot from a FactorGMT input
#'
#'
#'
#'
#'
#'
BoxPlotGSE <- function(
    FactorGMTMelt,
    BoxPlot_IndpVar,
    BoxPlot_PlotBy,
    
    BoxFactorSelectInput,
    BoxPlot_Type,
    BoxPlot_showColor,
    BoxPlot_ThemeSelect,
    BoxPlot_ToggleLegend = T,
    
    BoxPlot_showData = NULL,
    BoxPlot_showDataOption = NULL,
    BoxPlot_JitterWidth = NULL,
    BoxPlot_JitterAlpha = NULL,
    BoxPlot_JitterFill = NULL,
    
    BoxPlot_showMargins = F,            
    BoxPlot_margin_top = NULL,
    BoxPlot_margin_right = NULL,
    BoxPlot_margin_bottom = NULL,
    BoxPlot_margin_left = NULL,
    
    BoxPlot_PlotAxisFlip = F,
    BoxPlot_main = NULL,
    BoxPlot_xlab = NULL,
    BoxPlot_ylab = NULL,
    BoxPlot_xlab_angle = 90
    ){
    
    message("Rendering BoxPlot")
    if (BoxPlot_IndpVar == "s") {
        if (BoxPlot_PlotBy == "o") {
            message("Plotting Sample Overall Distribution")
            AesX <- FactorGMTMelt[,"GSM"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- factor(FactorGMTMelt[,BoxFactorSelectInput])
            GroupVar <- NULL
            xlabtext <- "GSMs in Dataset"
            legPos <- "top"
        } else if (BoxPlot_PlotBy == "f") {
            message("Plotting Sample Factor Distributions")
            AesX <- FactorGMTMelt[,BoxFactorSelectInput]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- FactorGMTMelt[,BoxFactorSelectInput]
            GroupVar <- NULL
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }
    } else if (BoxPlot_IndpVar == "g") {
        if (BoxPlot_PlotBy == "o") {
            message("Plotting overall distribution for gene selection")
            AesX <- FactorGMTMelt[,"variable"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- FactorGMTMelt[,"variable"]
            GroupVar <- FactorGMTMelt[,"variable"]
            xlabtext <- "Gene Names"
            legPos <- "top"
        } else if (BoxPlot_PlotBy == "f") {
            message("Plotting factor distribution for gene selection")
            AesX <- FactorGMTMelt[,"variable"]
            AesY <- FactorGMTMelt[,"value"]
            AesFill <- FactorGMTMelt[,BoxFactorSelectInput]
            GroupVar <- NULL
            xlabtext <- "Experimental Factors"
            legPos <- "top"
    }
    }
    
    if (!BoxPlot_ToggleLegend) { legPos <- 'none'}
    p <- ggplot(data = FactorGMTMelt, aes_string(y = AesY, x = AesX, group = GroupVar, fill = AesFill))
    
    if (BoxPlot_showColor) {
        if (BoxPlot_ThemeSelect == "default") { p <- p }
        else if (BoxPlot_ThemeSelect == "theme_gray") {p <- p + theme_gray()}
        else if (BoxPlot_ThemeSelect == "theme_bw") {p <- p + theme_bw()}
        else if (BoxPlot_ThemeSelect == "theme_light") {p <- p + theme_light()}
        else if (BoxPlot_ThemeSelect == "theme_dark") {p <- p + theme_dark()}
        else if (BoxPlot_ThemeSelect == "theme_minimal") {p <- p + theme_minimal()}
        else if (BoxPlot_ThemeSelect == "theme_classic") {p <- p + theme_classic()}
    }
    
    p <- 
        p + theme(legend.position = legPos) + 
        ylab(label = "Expression Level") + 
        xlab(label = xlabtext) +
        scale_x_discrete(label=abbreviate) +
        theme(axis.text.x = element_text(angle = 90)) +
        guides(fill=guide_legend(title="Experimental Factor Groups")) +
        theme(axis.text = element_text(size = 14, hjust = 1)) +
        theme(axis.title = element_text(size = 14)) +
        theme(legend.text=element_text(size=14))
    
    if (BoxPlot_Type == "BoxPlot") { p <- p + geom_boxplot(varwidth = F)
    } else if (BoxPlot_Type == "Violin Plot") { p <- p + geom_violin()
    } else if (BoxPlot_Type == "Line Plot") { p <- p }

    if (BoxPlot_showData==1) {
       if (BoxPlot_showDataOption == "jitter") { p <- p + geom_jitter(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else if(BoxPlot_showDataOption == "quasirandom"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else if(BoxPlot_showDataOption == "beeswarm"){ p <- p + geom_beeswarm(width = BoxPlot_JitterWidth, alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else if(BoxPlot_showDataOption == "tukey"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "tukey", alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else if(BoxPlot_showDataOption == "frowney"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "frowney", alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else if(BoxPlot_showDataOption == "smiley"){ p <- p + geom_quasirandom(width = BoxPlot_JitterWidth, method = "smiley", alpha = BoxPlot_JitterAlpha, fill =  BoxPlot_JitterFill)
       } else { NULL }
    }
    
    if (BoxPlot_PlotAxisFlip == 1) { p <- p + coord_flip()}
    if (length(BoxPlot_main) > 0) { p <- p + labs(title = BoxPlot_main)} else {p <- p + labs(title = "BoxPlot of Gene Series Data")}
    if (length(BoxPlot_xlab) > 0) { p <- p + labs(x = BoxPlot_xlab)}
    if (length(BoxPlot_ylab) > 0) { p <- p + labs(y = BoxPlot_ylab)}
    if (BoxPlot_showMargins==1) {p <-p + theme(plot.margin = margin( BoxPlot_margin_top, BoxPlot_margin_right, BoxPlot_margin_bottom, BoxPlot_margin_left,"cm"))}
    return(p)
}



# BoxPlot_IndpVar = "g"
# BoxPlot_IndpVar = "s"
# 
# BoxPlot_PlotBy = "f"
# BoxPlot_PlotBy = "o"
# 
# BoxFactorSelectInput <- colnames(FactorGMTMelt)[2]
# BoxPlot_Type <- "BoxPlot"
# BoxPlot_showColor <- T
# BoxPlot_ThemeSelect <- "theme_gray"
# BoxPlot_xlab_angle <- 45
# BoxPlot_showData <- T
# BoxPlot_showDataOption <- "quasirandom"
# BoxPlot_JitterWidth <- 0.1
# BoxPlot_JitterAlpha <- 0.5
# BoxPlot_JitterFill <- "red"
# 
# BoxPlot_showMargins = F
# BoxPlot_margin_top = NULL
# BoxPlot_margin_right = NULL
# BoxPlot_margin_bottom = NULL
# BoxPlot_margin_left = NULL
# 
# BoxPlot_PlotAxisFlip <- F
# BoxPlot_main <- "Plot"
# BoxPlot_xlab <- NULL
# BoxPlot_ylab <- NULL
# BoxPlot_xlab_angle = 90
# 
# BoxPlotGSE(FactorGMTMelt,BoxPlot_IndpVar, BoxPlot_PlotBy, 
#             BoxFactorSelectInput, BoxPlot_Type, 
#             BoxPlot_showColor, BoxPlot_ThemeSelect, 
#             BoxPlot_showData = F, BoxPlot_showDataOption, BoxPlot_JitterWidth = 0.1 , BoxPlot_JitterAlpha = 0.25, BoxPlot_JitterFill = "red",
#             BoxPlot_showMargins = F, BoxPlot_margin_top = NULL, BoxPlot_margin_right = NULL, BoxPlot_margin_bottom = NULL, BoxPlot_margin_left = NULL,
#             BoxPlot_PlotAxisFlip <- F,
#             BoxPlot_main, BoxPlot_xlab, BoxPlot_ylab, BoxPlot_xlab_angle
#             )
