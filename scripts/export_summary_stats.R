library(dplyr)

#load csv file df 
#df<- read.csv("./Analysis/df.csv")

target.outcome.variables <- as.character(unique(df$Outcome_Variable)) 
target.outcome.variables <- target.outcome.variables[grepl("2", target.outcome.variables)]

factor.values <- c("Msg2_1Attention",
                   "Msg2_2State",
                   "Msg2_3Social",
                   "Msg2_4Cognitive",
                   "Msg2_5Action",
                   "Msg2_6ActionFormat")

filter.columns <- c("gender",
                    "engnative",
                    "educationlevel",
                    "Accuracy1Group",
                    "Accuracy2Group",
                    "SignedAccuracy1Group",
                    "SignedAccuracy2Group",
                    "Long.Short.Exp2.Group",
                    "FastSlow.Read.Exp1.Group",
                    "FastSlow.Read.Exp2.Group")

calculate.summary <- function(rows) {
        
        table <- rows %>%
                group_by(FactorLevel, Outcome_Variable) %>%
                summarise(mean = mean(Value),
                          min = min(Value),
                          max = max(Value),
                          sd = sd(Value),
                          n = n(),
                          SE = sd((Value)/sqrt(n())))
        
        data.frame(table)
}

all.stats <- NULL

for (f in factor.values) {
  
  factor.rows <- df[df$FactorRecoded == f,] # keep only the rows where "FactorRecorded" column has value f
  
  target.factor.rows <- factor.rows[factor.rows$Outcome_Variable %in% target.outcome.variables, ] # keep only the rows where "Outcome_Variable" has one of the values we want to keep
  
  factor.stats <- cbind(factor=f,
                        filter="ALL",
                        filter.value="ALL",
                        calculate.summary(target.factor.rows))
  
  all.stats <- rbind(all.stats, factor.stats)
  
  for (column in filter.columns) {
    for (value in as.character(unique(factor.rows[, column]))) {
      
      filtered.factor.rows <- target.factor.rows[target.factor.rows[, column] == value, ]
      
      factor.stats.filtered <- cbind(factor=f,
                                     filter=column,
                                     filter.value=value,
                                     calculate.summary(filtered.factor.rows))
      
      all.stats <- rbind(all.stats, factor.stats.filtered) # append stats for current filtering to stats for current factor
    }
  }
}
print(all_stats)

summary <- print(all.stats)

write.csv(summary, "summary.message2.csv")