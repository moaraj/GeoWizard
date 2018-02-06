library(ggridges)
ggplot(SampleArrayDataMeltDF, aes(x = value, group = ExpVar3.Text, fill =ExpVar3.Text, position="identity" )) 

FactorOptions <- colnames(SampleArrayDataMeltDF)

ggplot(SampleArrayDataMeltDF, aes(x = value, y = SampleArrayDataMeltDF[,2], height = ..density..)) + 
     geom_density_ridges(stat = "binline", bins = 20, scale = 4, draw_baseline = T) + 
     labs(title="Expression Counts Per Group", x="Number of Counts", y = "Proportion of Genes") +
     theme(legend.position="none") +  theme_classic()

ggplot(SampleArrayDataMeltDF, aes(y = value, x = ExpVar3.Text, fill = ExpVar3.Text)) + geom_boxplot() +
     theme(legend.position = "bottom")

ggplot(SampleArrayDataMeltDF, aes(y = value, x = GSM, fill = ExpVar3.Text)) + geom_boxplot() +
     theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 90))
