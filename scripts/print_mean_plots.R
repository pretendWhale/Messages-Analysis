library(dplyr)
library(ggplot2)
summary <- read_csv("summary.message4.csv")
summary <- na.omit(summary)
summary <- data.frame(summary)

target.factor <- as.character(unique(summary$factor))

target.outcome.variables <- as.character(unique(summary$Outcome_Variable))

target.groups <- as.character(unique(summary$filter))

print.plot <- function (rows) {
        ggplot(rows,aes(x=FactorLevel,y=mean,fill=filter.value))+
                geom_bar(stat="identity", position=position_dodge())+
                geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),position=position_dodge(.8), 
                              width = 0.25)+
                geom_text(aes(label=n), vjust=1.6, color="black", position=position_dodge(.9), size=3.5)+
                labs(y=toString(outcome), x=toString(factor)) 
}

        
pdf("plots.pdf", onefile = TRUE)

for (outcome in target.outcome.variables) {

        for (factor in target.factor) {
                
                for (group in target.groups) {
                        
                        factor.rows <- summary[summary$factor == factor,] 
                        target.outcome.rows <- factor.rows[factor.rows$Outcome_Variable == outcome, ]  
                        target.filter <- target.outcome.rows[target.outcome.rows$filter == group,  ]
                        Plot <-print.plot(target.filter) 
                        print(Plot)
                        
                        }
                }
}
dev.off()

