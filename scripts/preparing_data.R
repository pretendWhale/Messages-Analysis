require(ggplot2)
require(dplyr)
require(tidyr)
require(readr)
require(purrr)
require(tibble)
require(stringr)
require(magrittr)

setwd("C:/Users/alsop/Google Drive/2018/1. Analysis to Do/UMAP (1)/data")

rawdata <- read_csv("MotimsgsAXIS.csv")
scores <- read_csv("question_scores.csv")

data <- merge(rawdata, scores, by.x="workerid", by.y="id", all.x=T)

data <- data[c( 50:71,77:96, 99:101, 104:178) , ]


variablesOfInterest <- c("worker_id", 
                         "q1confidence_1",
                         "q2confidence_1", 
                         "q3confidence_1", 
                         "q4confidence_1", 
                         "msg2name", "msg3name", 
                         "msg4name", "msg2rating_1", 
                         "msg3rating_1", "msg4rating_1", 
                         "Timingmsg2_3", 
                         "Timingmsg3_3", "Timingmsg4_3", 
                         "TimingQ1_3","TimingQ2_3", 
                         "TimingQ3_3", 
                         "TimingQ4_3", 
                         "TimingQ1prompt_3",
                         "TimingQ2prompt_3", 
                         "TimingQ3prompt_3", 
                         "TimingQ4prompt_3", 
                         "q1expprompt",
                         "q2expprompt", 
                         "q3expprompt", 
                         "q4expprompt", 
                         "TimingQ1analysis_3",
                         "TimingQ2analysis_3", 
                         "TimingQ3analysis_3", 
                         "TimingQ4analysis_3", 
                         "q1practiceprompt",
                         "q2practiceprompt", 
                         "q3practiceprompt", 
                         "q4practiceprompt", 
                         "age", 
                         "gender", 
                         "engnative", 
                         "educationlevel", 
                         "q1expprompt",
                         "q2expprompt", 
                         "q3expprompt", 
                         "q4expprompt", 
                         "q1_score.y", 
                         "q2_score.y", 
                         "q3_score.y", 
                         "q4_score")


#subset all exported data if column name matches the list of variables of interest
df <- data[, variablesOfInterest]

#list column names to rename them for easy in-R manipulation
colnames(df)

colnames(df) <- c("id", 
                  "confidence_q1", 
                  "confidence_q2", 
                  "confidence_q3", 
                  "confidence_q4", 
                  "m2", 
                  "m3", 
                  "m4", 
                  "m2rating", 
                  "m3rating", 
                  "m4rating", 
                  "tot_m2_read", 
                  "tot_m3_read",
                  "tot_m4_read", 
                  "tot1",
                  "tot2", 
                  "tot3", 
                  "tot4", 
                  "tot_exp1",
                  "tot_exp2", 
                  "tot_exp3", 
                  "tot_exp4", 
                  "explain_q1",
                  "explain_q2", 
                  "explain_q3", 
                  "explain_q4", 
                  "tot_read_exp1",
                  "tot_read_exp2", 
                  "tot_read_exp3", 
                  "tot_read_exp4", 
                  "more_tasks1",
                  "more_tasks2", 
                  "more_tasks3", 
                  "more_tasks4", 
                  "age", 
                  "gender", 
                  "engnative", 
                  "educationlevel", 
                  "explain_q1_Length",
                  "explain_q2_Length", 
                  "explain_q3_Length", 
                  "explain_q4_Length",
                  "q1binary", 
                  "q2binary", 
                  "q3binary", 
                  "q4binary") 

#and add some empty columns for variables that we will create
df[c("q1_signed", "q2_signed", "q3_signed", "q4_signed")] <- NA

#creating new variables: annotation length
df$explain_q1_Length <- nchar(df$explain_q1)
df$explain_q2_Length <- nchar(df$explain_q2)
df$explain_q3_Length <- nchar(df$explain_q3)
df$explain_q4_Length <- nchar(df$explain_q4)

#create new variables: signed accuracy
df$confidence_q1 <- as.numeric(df$confidence_q1)
df$confidence_q2 <- as.numeric(df$confidence_q2)
df$confidence_q3 <- as.numeric(df$confidence_q3)
df$confidence_q4 <- as.numeric(df$confidence_q4)

df$q1binary <- as.numeric(df$q1binary)
df$q2binary <- as.numeric(df$q2binary)
df$q3binary <- as.numeric(df$q3binary)
df$q4binary <- as.numeric(df$q4binary)

df$q1_signed <- df$q1binary * df$confidence_q1
df$q2_signed <- df$q2binary * df$confidence_q2
df$q3_signed <- df$q3binary * df$confidence_q3
df$q4_signed <- df$q4binary * df$confidence_q4

df$m2rating <- as.numeric(df$m2rating)
df$m3rating <- as.numeric(df$m3rating)
df$m4rating <- as.numeric(df$m4rating)

df$tot_m2_read <- as.numeric(df$tot_m2_read)
df$tot_m3_read <- as.numeric(df$tot_m3_read)
df$tot_m4_read <- as.numeric(df$tot_m4_read)

df$tot1 <- as.numeric(df$tot1)
df$tot2 <- as.numeric(df$tot2)
df$tot3 <- as.numeric(df$tot3)
df$tot4 <- as.numeric(df$tot4)

df$tot_exp1 <- as.numeric(df$tot_exp1)
df$tot_exp2 <- as.numeric(df$tot_exp2)
df$tot_exp3 <- as.numeric(df$tot_exp3)
df$tot_exp4 <- as.numeric(df$tot_exp4)

df$explain_q1_Length <- as.numeric(df$explain_q1_Length)
df$explain_q2_Length <- as.numeric(df$explain_q2_Length)
df$explain_q3_Length <- as.numeric(df$explain_q3_Length)
df$explain_q4_Length <- as.numeric(df$explain_q4_Length)


df$tot_read_exp1 <- as.numeric(df$tot_read_exp1)
df$tot_read_exp2 <- as.numeric(df$tot_read_exp2)
df$tot_read_exp3 <- as.numeric(df$tot_read_exp3)
df$tot_read_exp4 <- as.numeric(df$tot_read_exp4)

df$more_tasks1 <- as.numeric(df$more_tasks1)
df$more_tasks2 <- as.numeric(df$more_tasks2)
df$more_tasks3 <- as.numeric(df$more_tasks3)
df$more_tasks4 <- as.numeric(df$more_tasks4)

df$q1_signed <- as.numeric(df$q1_signed)

df$age <- as.numeric(df$age)
df$gender <- as.character(df$gender)
df$engnative <- as.numeric(df$engnative)
df$educationlevel <- as.numeric(df$educationlevel)

library(car)

df$SignedAccuracy1Group <- df$q1_signed
df$SignedAccuracy1Group<-recode(df$SignedAccuracy1Group,"c(-10,-9,-8,-7,-6,-5)='Very Wrong'")
df$SignedAccuracy1Group<-recode(df$SignedAccuracy1Group,"c(-4,-3,-2,-1,0,1,2,3,4)='Average'")
df$SignedAccuracy1Group<-recode(df$SignedAccuracy1Group,"c(5,6,7,8,9,10)='Very Right'")

df$SignedAccuracy2Group <- df$q2_signed
df$SignedAccuracy2Group<-recode(df$SignedAccuracy2Group,"c(-10,-9,-8,-7,-6,-5)='Very Wrong'")
df$SignedAccuracy2Group<-recode(df$SignedAccuracy2Group,"c(-4,-3,-2,-1,0,1,2,3,4)='Average'")
df$SignedAccuracy2Group<-recode(df$SignedAccuracy2Group,"c(5,6,7,8,9,10)='Very Right'")

df$SignedAccuracy3Group <- df$q3_signed
df$SignedAccuracy3Group<-recode(df$SignedAccuracy3Group,"c(-10,-9,-8,-7,-6,-5)='Very Wrong'")
df$SignedAccuracy3Group<-recode(df$SignedAccuracy3Group,"c(-4,-3,-2,-1,0,1,2,3,4)='Average'")
df$SignedAccuracy3Group<-recode(df$SignedAccuracy3Group,"c(5,6,7,8,9,10)='Very Right'")

df$SignedAccuracy4Group <- df$q4_signed
df$SignedAccuracy4Group<-recode(df$SignedAccuracy4Group,"c(-10,-9,-8,-7,-6,-5)='Very Wrong'")
df$SignedAccuracy4Group<-recode(df$SignedAccuracy4Group,"c(-4,-3,-2,-1,0,1,2,3,4)='Average'")
df$SignedAccuracy4Group<-recode(df$SignedAccuracy4Group,"c(5,6,7,8,9,10)='Very Right'")

df$Accuracy1Group <- df$q1binary
df$Accuracy1Group<-recode(df$Accuracy1Group,"c(-1)='Problem 1 Wrong'")
df$Accuracy1Group<-recode(df$Accuracy1Group,"c(1)='Problem 1 Right'")

df$Accuracy2Group <- df$q2binary
df$Accuracy2Group<-recode(df$Accuracy2Group,"c(-1)='Problem 2 Wrong'")
df$Accuracy2Group<-recode(df$Accuracy2Group,"c(1)='Problem 2 Right'")

df$Accuracy3Group <- df$q3binary
df$Accuracy3Group<-recode(df$Accuracy3Group,"c(-1)='Problem 3 Wrong'")
df$Accuracy3Group<-recode(df$Accuracy3Group,"c(1)='Problem 3 Right'")

df$Accuracy4Group <- df$q4binary
df$Accuracy4Group<-recode(df$Accuracy4Group,"c(-1)='Problem 4 Wrong'")
df$Accuracy4Group<-recode(df$Accuracy4Group,"c(1)='Problem 4 Right'")



#Length of explanation they wrote on previous problem ---q2expprompt, q3expprompt, q4expprompt
expl1nchar<- summary(df$explain_q1_Length)
df <- mutate(df, Long.Short.Exp1.Group = ifelse(df$explain_q1_Length > expl1nchar[[3]], "Long", "Short"))

expl2nchar<- summary(df$explain_q2_Length)
df <- mutate(df, Long.Short.Exp2.Group = ifelse(df$explain_q2_Length > expl2nchar[[3]], "Long", "Short"))

expl3nchar<- summary(df$explain_q3_Length)
df <- mutate(df, Long.Short.Exp3.Group = ifelse(df$explain_q3_Length > expl3nchar[[3]], "Long", "Short"))

expl4nchar<- summary(df$explain_q4_Length)
df <- mutate(df, Long.Short.Exp4.Group = ifelse(df$explain_q4_Length > expl4nchar[[3]], "Long", "Short"))

# Time spent reading explanation on previous problem
tot_read_exp1<- summary(df$tot_read_exp1)
df <- mutate(df, FastSlow.Read.Exp1.Group = ifelse(df$explain_q1_Length > expl1nchar[[3]], "Fast", "Slow"))

tot_read_exp2<- summary(df$tot_read_exp2)
df <- mutate(df, FastSlow.Read.Exp2.Group = ifelse(df$explain_q2_Length > expl2nchar[[3]], "Fast", "Slow"))

tot_read_exp3<- summary(df$tot_read_exp3)
df <- mutate(df, FastSlow.Read.Exp3.Group = ifelse(df$explain_q3_Length > expl3nchar[[3]], "Fast", "Slow"))

tot_read_exp4<- summary(df$tot_read_exp4)
df <- mutate(df, FastSlow.Read.Exp4.Group = ifelse(df$explain_q4_Length > expl4nchar[[3]], "Fast", "Slow"))

library(dplyr)
library(tidyr)
library(splitstackshape)

##if for wide data, see below, if for long data jump over this section
#df <- cSplit(df, splitCols=c("m2","m3","m4"), sep="-")
#old_column_names <- c("m2_1", 
#                "m2_2", 
#                "m2_3", 
#                "m2_4", 
#                "m2_5", 
#                "m2_6", 
#                "m3_1", 
#                "m3_2", 
#                "m3_3", 
#                "m3_4", 
#                "m3_5", 
#                "m3_6", 
#                "m4_1", 
#                "m4_2", 
#                "m4_3", 
#                "m4_5", 
#                "m4_6")

#new_column_names <- factor(c("Msg2_1Attention", 
#                       "Msg2_2State", 
#                       "Msg2_3Social", 
#                       "Msg2_4Cognitive", 
#                       "Msg2_5Action", 
#                       "Msg2_6ActionFormat", 
#                       "Msg3_1Attention", 
#                       "Msg3_2State", 
#                       "Msg3_3Social", 
#                       "Msg3_4Cognitive", 
#                       "Msg3_5Action", 
#                       "Msg3_6ActionFormat", 
#                       "Msg4_1Attention", 
#                       "Msg4_2State", 
#                       "Msg4_3Social", 
#                       "Msg4_4Cognitive", 
#                       "Msg4_5Action", 
#                       "Msg4_6ActionFormat"))

#fix this names(df) <- new_column_names[match(names(df), old_column_names)]
#old_values <- c("personal", "generic", "none", "deeper", "attempt", "none", "right", "wrong", "none", "effort", "progress", "none", "altenative", "effort", "none", "binary", "open", "none")
#new_values <- factor(c("1personal", "2generic", "3none", "1deeper", "2attempt", "3none", "1right", "2wrong", "3none", "1effort", "2progress", "3none", "1altenative", "2effort", "3none", "1binary", "2open", "3none"))
#df$LevelRecoded <- new_values[match(df$Level, old_values)]

####if for long data see below

#NEED TO CHANGE BELOW TO INCLUDE PRE-TEST
#need to change this below so that outcomes variables are subsetted on a vetor, not manually

contextual.variables <- c("age",
              "gender", 
              "engnative",
              "educationlevel",
              "q1_signed",
              "SignedAccuracy1Group",
              "Long.Short.Exp1.Group",
              "FastSlow.Read.Exp1.Group",
              "q2_signed",
              "SignedAccuracy2Group",
              "Long.Short.Exp2.Group",
              "FastSlow.Read.Exp2.Group",
              "q3_signed",
              "SignedAccuracy3Group",
              "Long.Short.Exp3.Group",
              "FastSlow.Read.Exp3.Group",
              "q4_signed",
              "SignedAccuracy4Group",
              "Long.Short.Exp4.Group",
              "FastSlow.Read.Exp4.Group")

#outcome.variables <- c(2:5,9:36,42:45,50:53) but need to write out by hand, or feed from an external pdf, to automate this

df <- gather(df, 
             key = "Outcome_Variable",
             value = "Value", c(2:5,9:34,39:50))

df <- cSplit(df, 
             splitCols=c("m2","m3","m4"), 
             sep="-")

df <- gather(df,
             key = "Factor",
             value = "Level",
             c(m2_1:m2_6,m3_1:m3_6,m4_1:m4_6))

#FOR NOW removing prior knowledge


old.values <- c("m2_1", 
                "m2_2", 
                "m2_3", 
                "m2_4", 
                "m2_5", 
                "m2_6", 
                "m3_1", 
                "m3_2", 
                "m3_3", 
                "m3_4", 
                "m3_5", 
                "m3_6", 
                "m4_1", 
                "m4_2", 
                "m4_3", 
                "m4_5", 
                "m4_6")

new.values <- factor(c("Msg2_1Attention", 
                       "Msg2_2State", 
                       "Msg2_3Social", 
                       "Msg2_4Cognitive", 
                       "Msg2_5Action", 
                       "Msg2_6ActionFormat", 
                       "Msg3_1Attention", 
                       "Msg3_2State", 
                       "Msg3_3Social", 
                       "Msg3_4Cognitive", 
                       "Msg3_5Action", 
                       "Msg3_6ActionFormat", 
                       "Msg4_1Attention", 
                       "Msg4_2State", 
                       "Msg4_3Social", 
                       "Msg4_4Cognitive", 
                       "Msg4_5Action", 
                       "Msg4_6ActionFormat"))

df$FactorRecoded <- new.values[match(df$Factor, old.values)]

old.values <- c("personal", 
                "generic", 
                "none", 
                "deeper", 
                "attempt", 
                "none", 
                "right", 
                "wrong", 
                "none", 
                "effort", 
                "progress", 
                "none", 
                "altenative", 
                "effort", 
                "none", 
                "binary", 
                "open", 
                "none")

new.values <- factor(c("1personal", 
                       "2generic", 
                       "3none", 
                       "1deeper", 
                       "2attempt", 
                       "3none", 
                       "1right", 
                       "2wrong", 
                       "3none", 
                       "1effort", 
                       "2progress", 
                       "3none", 
                       "1altenative", 
                       "2effort", 
                       "3none", 
                       "1binary", 
                       "2open", 
                       "3none"))

df$LevelRecoded <- new.values[match(df$Level, old.values)]

df$id <- as.character(df$id)
df$Outcome_Variable <- as.character(df$Outcome_Variable)
df$Value <- as.numeric(df$Value)
df$Level <- NULL

df$FactorLevel <- paste(df$FactorRecoded, df$LevelRecoded, sep="_")
df$FactorLevel <- as.factor(df$FactorLevel)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

df <- completeFun(df, "Value")
