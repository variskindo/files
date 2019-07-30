---
title: "Assignment3_James_Callens"
author: "James Callens"
date: "July 29, 2019"
output: word_document
---
##Initial Setup
Set up library, ceate and set working directory, read csv files and set them equall to objects.
```{r, results = "hide"}
library(ggplot2)
library(dplyr)
library(reshape2)
setwd("C:/Users/PC/Desktop/class/")
dat1 = read.csv("dataset1.csv") 
dat2 = read.csv("dataset2.csv") 
dat3 = read.csv("dataset3.csv") 
dat4 = read.csv("dataset4.csv")
```

Assignment 3
- Use the R package ggplot2 to replicate plot1.pdf for each of the datasets 1, 2 and 3.

Dataset 1
        - Replicate plot1.pdf 
        - Here you will see that "years" is on the y-axis and "DSI" is on the x-axis.
        - Recreate this plot by reversing the x and y-axis since x-axis is typically time. 
                - Now years is on the x-axis and DSI is the y-axis.
                
Dataset 2 
        - Describe any issues that arise from using the expanded dataset 2 to replicate the plot1.pdf.
        
Dataset 3 
    - Use ggplot2 to create a graphic using all the variables to express as many relationships as clearly as possible. 
    - For each variable and relationship explain what technique you used and why.
    
Dataset 4 
        - Use ggplot2 to create a graphic to explain the relationship between two variables. 
                - You may use one of the methods described in class, in the ggplot2 book, or another method of your choosing. 
                - Explain why you chose the method you did and what the pros and cons are of the method that you chose.
                
= Use dplyr or data.table if you choose to manipulate the data if you need to do so.
- For each of the datasets submit your code along with explanations of what you could or could not replicate
- Submit your assignments in an rmd files and / or MS Word files. Please make sure that you can open and run the *.rmd file at your end.

Assignments will be graded based on the following:
        - Clarity and quality of graphics submitted
        - Quality of explanations
        - Innovation expressed in graphics used and code deployed.


```{r}
#Supplied graphic
knitr::include_graphics("sample_plot.pdf")
```


##Dataset_1
```{r}
#Recreate graph of dataset1 (part 1)
ggplot(data = dat1, aes(x = DSI, y = years, size = DSI, colour = years)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1)

#Recreate graph of dataset1 (part 2)
ggplot(data = dat1, aes(x = years, y = DSI, size = DSI, colour = years)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1)

#Make improvements on dataset1 graph
ggplot(data = dat1, aes(x = years, y = DSI, size = DSI, colour = years)) + 
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
geom_smooth(method = "lm", se = TRUE, level = 0.95, colour = "black", linetype = "solid", alpha = 0.5)
```



##Dataset_2
```{r, results = "hide"}
#Restraucture dataset2 and recreate supplied graph of dataset2 (part 1)
head(dat2)
dat2 = subset(dat2, years2 >= "2000")
```
```{r}
ggplot(data = dat2, aes(x = DSI2, y = years2, size = DSI2, colour = years2)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "DSI", y = "years")  

#Restraucture dataset2 and recreate supplied graph of dataset2 (part 2)
ggplot(data = dat2, aes(x = years2, y = DSI2, size = DSI2, colour = years2)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "years", y = "DSI")

#Make improvements on dataset2 graph
ggplot(data = dat2, aes(x = years2, y = DSI2, size = DSI2, colour = years2)) + 
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "years", y = "DSI") +
geom_smooth(method = "lm", se = TRUE, level = 0.95, colour = "black", linetype = "solid", alpha = 0.5)
```



##Dataset_3
```{r, results = "hide"}
#restraucture dataset2 and recreate supplied graph of dataset3 (part 1)
head(dat3)
dat3 = select(dat3, X, years2, DSI2)
dat3 = subset(dat3, years2 >= "2000" | dat3$DSI2 >= min(dat1$DSI))
```
```{r}
ggplot(data = dat3, aes(x = DSI2, y = years2, size = DSI2, colour = years2)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "DSI", y = "years") 

#restraucture dataset2 and recreate supplied graph of dataset3 (part 2)
ggplot(data = dat3, aes(x = years2, y = DSI2, size = DSI2, colour = years2)) + 
theme(legend.position = "none") +  
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "years", y = "DSI") 

#Make improvements on dataset3 graph
ggplot(data = dat3, aes(x = years2, y = DSI2, size = DSI2, colour = years2)) + 
scale_color_gradient(low = "black", high = "turquoise3") +
geom_point(shape = 4, stroke = 1) +
labs(x = "years", y = "DSI") +
geom_smooth(method = "lm", se = TRUE, level = 0.95, colour = "black", linetype = "solid", alpha = 0.5)
```
```{r, results = "hide"}
#Use all variables in dataset3
dat3_new = read.csv("dataset3.csv")
head(dat3)
dat3_new = melt(data = dat3_new, id.var = "years2", measure.vars = c("DSI2","PSI2","QSI2"))
head(dat3_new)
colnames(dat3_new) = c("Years", "Group", "Value")
dat3_new$Years = as.numeric(dat3_new$Years)
dat3_new$Value = as.numeric(dat3_new$Value)

#DSI2_min = min(dat3$DSI2)
#DSI2_max = max(dat3$DSI2)
#PSI2_min = min(dat3$PSI2)
#PSI2_max =max(dat3$PSI2)
#QSI2_min = min(dat3$QSI2)
#QSI2_max =max(dat3$QSI2)
```
```{r}


ggplot(data = dat3_new, aes(x = Years, y = Value, col = Group)) + 
geom_point() +
labs(x = "Years", y = "Group") +
geom_smooth(method = "lm", level = 0.95, colour = "black", 
            linetype = "solid", alpha = 0.5, na.rm = TRUE)    

#scatter plot
ggplot(data = dat3_new, aes(x = Years, y = Value, col = Group)) + 
geom_point() +
facet_wrap(~ Group) +    
labs(x = "Years", y = "Group") +
geom_smooth(method = "lm", level = 0.95, colour = "black", 
            linetype = "solid", alpha = 0.5, na.rm = TRUE)

#box plot
ggplot(data = dat3_new, aes(x = Years, y = Value, col = Group)) +  
      geom_boxplot(data = dat3_new, aes(x = Years, y = Value, col = Group)) +
      labs(x = "Years", y = "Group") 
```



##Dataset_4
```{r, results = "hide"}
dat4 = read.csv("dataset4.csv")
head(dat4)
unique(dat4$category)
dat4$category = factor(dat4$category,levels = c("Very Bad", "Bad", "Okay", "Good", "Very Good"))
```
```{r}
df1 = subset(dat4, category = "Very Bad")
colnames(df1) = c("category", "Very Bad")
df1 = as.data.frame(df1[,-1])
colnames(df1) = c("Very Bad")
df2 = subset(dat4, category = "Bad")
colnames(df2) = c("category", "Bad")
df2 = as.data.frame(df2[,-1])
colnames(df2) = c("Bad")
df3 = subset(dat4, category = "Okay")
colnames(df3) = c("category", "Okay")
df3 = as.data.frame(df3[,-1])
colnames(df3) = c("Okay")
df4 = subset(dat4, category = "Good")
colnames(df4) = c("category", "Good")
df4 = as.data.frame(df4[,-1])
colnames(df4) = c("Good")
df5 = subset(dat4, category = "Very Good")
colnames(df5) = c("category", "Very Good")
df5 = as.data.frame(df5[,-1])
colnames(df5) = c("Very Good")
df = Reduce(function(x, y) merge(x, y, all=TRUE),list(df1, df2, df3, df4, df5))
df
df = as.data.frame(df1$`Very Bad`)
colnames(df) = c("Very Bad", "Bad")
df$Bad = df2$Bad
df$Okay = df3$Okay
df$Good = df4$Good
df$`Very Good` = df5$`Very Good` 
vb = sd(df$`Very Bad`)
b = sd(df$`Bad`)
o = sd(df$`Okay`)
g = sd(df$`Good`)
vg = sd(df$`Very Good`)
df1$low = df1$`Very Bad` - vb 
df1$high = df1$`Very Bad` + vb 
df2$low = df2$Bad - vb 
df2$high = df2$Bad + vb
df3$low = df3$Okay - vb 
df3$high = df3$Okay + vb
df4$low = df4$Good - vb 
df4$high = df4$Good + vb
df5$low = df5$`Very Good` - vb 
df5$high = df5$`Very Good` + vb
df = Reduce(function(x, y) merge(x, y, all=TRUE),list(df1, df2, df3, df4, df5))
df =  t(df)
df
#box plot
ggplot(data = dat4, aes(category, value)) + geom_point() + 
geom_boxplot(stat = "identity", width = 0.5, fill = "darkblue") +
geom_errorbar(aes(ymin = low, ymax = high), width = .2,
                 position = position_dodge(.9)) +  
theme_minimal()  

#random forest calculation
model1 = randomForest::randomForest(category ~ ., data = dat4, proximity=TRUE)
model2 = randomForest::randomForest(category ~., data=dat4, ntree=1000, 
                     proximity=TRUE)
model1
model2

```



