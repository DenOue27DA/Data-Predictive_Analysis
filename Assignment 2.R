

#Step 1
library(tidyverse)

#Step 2
titanicData <- read_csv("titanic.csv")

#Step 3
glimpse(titanicData)

#Step 4
checkNA = c()
checkNA <- is.na(titanicData$survived)
TRUE %in% checkNA
  #if return FALSE, column does not contain NA values. 
  #Otherwise, NA values exist in column. 

#Step 5
titanicData$embarked <- replace_na(titanicData$embarked,"Unknown")
  #example of replacing character NA values with "unknown"

avg_age = mean(na.omit(titanicData$age))
titanicData$age <- replace_na(titanicData$age,avg_age)
  #example of replacing NA numeric values with average

#Step 6
titanicData$survived <- as.logical(titanicData$survived)
  #numerical 0s and 1s representing true or false can be converted to logical

titanicData$pclass <- factor(titanicData$pclass)
titanicData$sex <- factor(titanicData$sex)
titanicData$sibsp <- factor(titanicData$sibsp)
titanicData$parch <- factor(titanicData$parch)
titanicData$embarked <- factor(titanicData$embarked)
titanicData$class <- factor(titanicData$class)
titanicData$who <- factor(titanicData$who)
titanicData$deck <- factor(titanicData$deck)
titanicData$embark_town <- factor(titanicData$embark_town)
titanicData$alive <- factor(titanicData$alive)
  #all character columns have limited categories, 
  #and can be converted to factor type

#Step 7

titanicData %>% ggplot(aes(age))+geom_histogram(bins=80,color=I("green"))+scale_x_continuous()
  #Age distribution of passengers on the titanic shown on a histogram

titanicData %>% ggplot(aes(x=sex,fill=sex))+geom_bar(alpha=1.8)+theme_dark()+scale_fill_manual(values =c("blue","violet"))
  #Counts of sex (male vs female) shown as a bar chart. 

titanicData %>% ggplot(aes(x=age, y=fare))+geom_point(color="navy")+geom_smooth(color="hotpink")
  #scatterplot showing fare and age data pairs. 
  #geom_smooth feature added to show trendline. 

titanicData %>% ggplot(aes(y=fare,x="passenger"))+geom_boxplot()+facet_wrap(~class, scales = "free")+geom_point()+theme(axis.title.x = element_blank())
  #fare paid by class of passenger presented in boxplots by class. 

