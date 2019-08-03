---
title: "assignment4"
author: "James Callens"
date: "August 1, 2019"
output: word_document
---
For this assignment, you will be graded on the following criteria:
	- Quality and clarity of the visuals.
	- Explanations provided to clarify specific points that you want to explain
	- Creative thinking
```{r}
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(plotly)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
setwd("C:/Users/jimca/Desktop/class/")
data = read.csv("king_data.csv")
```


#####What is happening to price over time (yr_built)?
```{r}
#View first few rows of the dataframe
head(data)

#Scatter plot
ggplot(data = data, aes(x = yr_built, y = price)) +
labs(x = "Built Year", y = "Price") +
ggtitle(label = "Display of Price Over Home Build Year: King County") +
geom_point() +
geom_smooth(method = "lm", se = TRUE, level = 0.95, linetype = "solid", alpha = 0.5) +
scale_y_continuous(labels = comma)  

#Scatter plot (log value of price)
ggplot(data = data, aes(x = yr_built, y = log(price))) +
labs(x = "Built Year", y = "Log(Price)") +
ggtitle(label = "Display of Log(Price) Over Home Build Year: King County") +
geom_point() +
geom_smooth(method = "lm", se = TRUE, level = 0.95, linetype = "solid", alpha = 0.5) +
scale_y_continuous(labels = comma) 

#Density plot
ggplot(data = data, aes(x = yr_built, y = price)) +
labs(x = "Built Year", y = "Price ($)") +
ggtitle(label = "Display of Log(Price) Over Home Build Year: King County") +
geom_area(aes(x = yr_built, y = price), colour = "darkblue") +  
scale_y_continuous(labels = comma)

#Scatter plot (year in decades)
ggplot(data = data, aes(x = decade, y = price, colour = decade)) +
labs(x = "Built Year (Decades)", y = "Price ($)", colour = "Decades") +
ggtitle(label = "Display of Price Over Home Build Year: King County") +
geom_point() +
scale_y_continuous(labels = comma)  

#Box plot (years in decades)
ggplot(data = data, aes(x = decade, y = price, colour = decade, group = decade)) +
geom_boxplot(data = data, aes(x = decade, y = price)) +
geom_point(data = data, aes(x = decade, y = price)) +
labs(x = "Built Year (Decades)", y = "Price ($)", colour = "Decades") +
ggtitle(label = "Display of Price Over Home Build Year: King County") +
scale_y_continuous(labels = comma)  
```


#####What is happening to price over living space (Specific field)?
```{r}

```


#####What is happening to price over time and space?
```{r}


```

#####- (Extra credit) 
Try to think about how you can use the zip code data for location information and map price to zipcodes
- Feel free to explore the data with questions such as:
	- Does condition impact prices across time?
	-Does grade impact prices across time
	- If you can figure out the maps then are location, grade, and prices concentrated in certain	zipcodes
	- You could even run a regression model with price as the dependent variable and the other fields as predictors. This is just to explore the data, its potential patterns and think of interesting visualizations.
```{r}
library(maps)
library(tools)
library(RCurl)
library(ggthemes)
data = read.csv("king_data.csv")
head(data)
data$date = gsub("T000000", "", data$date)
data = subset(data, zipcode > 0, select=c(date, yr_built, zipcode, lat, long, price, grade, id))
colnames(data) = c("date", "yr_built", "Zipcode", "lat", "long", "price", "grade", "id")
data$region = "WA"
############################################
US = map_data("usa")
US = US[,-c(5)]
US = US[,-c(3)]
states = map_data("state")
US = Reduce(function(x, y) merge(x, y, by = c("lat", "long")),list(US, states))
US = US[,-c(3:4)]
US = US[,-c(4)]
US = US[,-c(5)]
############################################
# washington state graphic of listed zipcodes in dataset
ggplot()+
geom_map(data = filter(US, region %in% c('washington')), map = states, mapping = aes( map_id = region, x = long, y = lat), fill = 'grey') +
geom_point(data = filter(data, region %in% c('WA')), aes(x = long, y = lat, fill = Zipcode, group = Zipcode, color = Zipcode), size = 0.1) +
labs(fill = "Zipcode",title = "", x="", y="") +  
scale_y_continuous(breaks=c()) +
scale_x_continuous(breaks=c()) +
theme_map() 
############################################
dat = subset(data, region = 'WA')
dat2 = filter(US, region %in% c('washington'))
dat2
plot_ly(data, x = ~long, y = ~lat) %>% 
layout(plot_bgcolor='rgb(254, 247, 234)')

g = ggplot() +  
geom_point(data = dat, mapping = aes(x = long, y = lat, fill = price, group = Zipcode, color = Zipcode), size = 0.1) +
labs(fill = "Zipcode",title = "", x="", y="") +
scale_y_continuous(breaks=c()) +
scale_x_continuous(breaks=c()) +  
theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_blank())  
g
g = ggplotly(g)
g 
############################################
ggplot()+
geom_map(data = filter(US, region %in% c('washington')), map = states, mapping = aes( map_id = region, x = long, y = lat), fill = 'grey') +
#geom_polygon(data = filter(US, region %in% c('washington')), aes(x = long, y = lat), color = NA, fill = NA) +  
geom_point(data = filter(data, region %in% c('WA')), aes(x = long, y = lat, fill = Zipcode, group = Zipcode), size = 0.1, color = "red") +
scale_fill_gradient(low = "antiquewhite", high = "darkred") +
#scale_color_gradient(low = "antiquewhite", high = "darkred") +  
labs(fill = "Zipcode",title = "", x="", y="") +  
scale_y_continuous(breaks=c()) +
scale_x_continuous(breaks=c()) +
theme_map() +
theme(panel.background = element_blank())
```











