#Assignment 1 – Working with Dataframes

#Question 1
dframe <- data.frame("person"=c("Stan","Francine","Steve","Roger","Hayley","Klaus"),"sex"=factor(c("M","F","M","M","F","M")),"funny"=factor(c("High","Med","Low","High","Med","Med")))

#Question 2
age <- c(41,41,15,1600,21,60)
cbind(dframe,age)
