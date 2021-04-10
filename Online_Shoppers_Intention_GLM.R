library(caTools)
library(dplyr)
setwd("C:/Users/mrrob/Desktop/Machine Learning York University/Course 1/Project 1 Shopping Intention")

df=read.csv("online_shoppers_intention.csv", header = TRUE)

df$PageValuesCategory[df$PageValues==0]<-'ZERO'
df$PageValuesCategory[df$PageValues>0 & df$PageValues<10]<-'LOW'
df$PageValuesCategory[df$PageValues>=10 & df$PageValues<50]<-'MEDIUM'
df$PageValuesCategory[df$PageValues>=50]<-'HIGH'
df$PageValuesCategory <- factor(df$PageValuesCategory, levels = c('ZERO','LOW','MEDIUM','HIGH'))
df$Month <- factor(df$Month, levels = c('Jan','Feb','Mar','Apr','May','June','Jul','Aug','Sep','Oct','Nov','Dec'))
df$VisitorType[df$VisitorType=='Other'] <- 'New_Visitor'
df$VisitorType <- factor(df$VisitorType, levels = c('New_Visitor','Returning_Visitor'))

tempdf <- df[df$Revenue==TRUE,] %>% count(Region, sort=TRUE) %>% mutate(perc = n/sum(n)*100) %>% mutate(region_bin = ifelse(perc>10,Region,0)) %>% dplyr::select(Region, region_bin)
df <- left_join(df,tempdf, by= "Region")

tempdf <- df[df$Revenue==TRUE,] %>% count(OperatingSystems, sort=TRUE) %>% mutate(perc = n/sum(n)*100) %>% mutate(operatingsystems_bin = ifelse(perc>10,OperatingSystems,0)) %>% dplyr::select(OperatingSystems, operatingsystems_bin)
df <- left_join(df,tempdf, by= "OperatingSystems")

tempdf <- df[df$Revenue==TRUE,] %>% count(Browser, sort=TRUE) %>% mutate(perc = n/sum(n)*100) %>% mutate(browser_bin = ifelse(perc>10,Browser,0)) %>% dplyr::select(Browser, browser_bin)
df <- left_join(df,tempdf, by= "Browser")

tempdf <- df[df$Revenue==TRUE,] %>% count(TrafficType, sort=TRUE) %>% mutate(perc = n/sum(n)*100) %>% mutate(traffictype_bin = ifelse(perc>10,TrafficType,0)) %>% dplyr::select(TrafficType, traffictype_bin)
df <- left_join(df,tempdf, by= "TrafficType")

df$traffictype_bin[is.na(df$traffictype_bin)]<-0
df$browser_bin[is.na(df$browser_bin)]<-0

set.seed(292)
sample <- sample.split(df$Revenue, SplitRatio = 0.90)
traindf <- subset(df,sample==TRUE)
testdf <- subset(df,sample==FALSE)

traindf <- dplyr::select(traindf, -PageValues,-OperatingSystems,-Browser,-Region,-TrafficType)
testdf <- dplyr::select(testdf, -PageValues,-OperatingSystems,-Browser,-Region,-TrafficType)

str(traindf)

tempdf <- traindf %>% count(Revenue) %>% mutate(perc=n/sum(n))
tempdf
threshold_probability <- ifelse(tempdf$perc[tempdf$Revenue==TRUE]<0.20,0.4,0.5)
threshold_probability

model <- glm(Revenue ~ ., family = binomial(link = "logit"), data = traindf)
summary(model)

predicted_revenue_prob <- predict(model,testdf,type = 'response')
predicted_revenue <- ifelse(predicted_revenue_prob>threshold_probability,TRUE,FALSE)
eval_table<-table(testdf$Revenue,predicted_revenue)
eval_table<-as.data.frame(eval_table)
eval_table

mis_classification <- round(mean(predicted_revenue != testdf$Revenue)*100,2)
mis_classification
accuracy<- 100-mis_classification
accuracy
sensitivity <- round(eval_table[eval_table$Var1==TRUE & eval_table$predicted_revenue==TRUE,"Freq"] / sum(eval_table[eval_table$Var1==TRUE,"Freq"])*100,2)
sensitivity
specificity <- round(eval_table[eval_table$Var1==FALSE & eval_table$predicted_revenue==FALSE,"Freq"] / sum(eval_table[eval_table$Var1==FALSE,"Freq"])*100,2)
specificity
