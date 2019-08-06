# pound sign is a comment
# everything is doable with code
# many things are doable with clicks
# highlight something and 'run', cmd+enter or ctrl+enter on line(s)
help(setwd) # help is always there
setwd("~/Desktop/R_Demo") # Folder on your computer
MetaphorData <- read_delim("R_MetaphorData.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  # import data, easier to do with GUI
df <- MetaphorData #rename dataset, <- gives var attributes
df = MetaphorData  #same thing... harder to read later
#df meaning DataFrame 

View(df) # EVERYTHING is case sensitive
head(df) #first couple rows
summary(df) #summaries
df$Party #just one $column
unique(df$Party) #unique values 

mean(df$NumMeta) #oh! average 
range(df$NumMeta) # okay...
median(df$NumMeta) # yeah...

hist(df$NumMeta)   # Visuals! histogram
boxplot(df$NumMeta~df$Party) #boxplot 
  # boxplot(data$variable~data$group) #boxplot 
boxplot(df$NumMeta~df$Gen) #other plot... can be saved, better with GUI
#Save from script
  #png("example.png", width = 350, height = "350") #open file
  #boxplot(df$NumMeta~df$Gen) #generate plot
  #dev.off() #save file

df$MetaPerLen = df$NumMeta/df$TxtLen # new row, for consistent size
  # controlling for length. Density not total
summary(df$MetaPerLen) #summary of one row
head(df) 
aggregate(NumMeta~Party, df, mean) # compare means
aggregate(MetaPerLen~Party, df, mean)
aggregate(MetaPerLen~Party, df, sd) #sd is standard deviation

hist(df$MetaPerLen)
abline(v = mean(df$MetaPerLen), #label with extra line
       col = "red",
       lwd = 2)

MetaPerLenSD <- sd(df$MetaPerLen) #save just one value
abline(v = mean(df$MetaPerLen)+MetaPerLenSD, #add a positive SD line
       col = "blue",
       lwd = 2)
abline(v = mean(df$MetaPerLen)-MetaPerLenSD, # negative SD line
       col = "blue",
       lwd = 2)
t.test(MetaPerLen~Party, df) # inferential test
  # p-value represents probability
  # is p<.05?
boxplot(df$MetaPerLen~df$Party) # ... what are those outliers doing?

## let's make the data a little messy
df[1,3] <- "Independent"; df[1,5] <- 5000 #put in values
#dataframe[row, column] <- new value
unique(df$Party) #unique values
df[df$Party == "Independent", ] <- "Indy500"
#data[when row == "X", ] <- "New Value"
# don't forget the comma! [row,col] - either can be empty

# == is important!! It means 'equal' instead of 'assign'
# | for OR, & for AND, ! for NOT
df2 <- subset(df, Party == "Democrat" | Party == "Republicans") 
df2 <- subset(df, Party != "Independent" & TxtLen < 1000) 
# subsetting is important for cleaning and limited to specified kinds
# easy way to get rid of extreme value
unique(df2$Party) # oh, good, we fixed it

##Now, do the same thing, with other data ##
# http://www.sthda.com/english/wiki/r-built-in-data-sets
# summary(mtcars); summary(PlantGrowth); summary(ToothGrowth)
# 1) pick a dataset
# 2) pick a variable, look at boxplots and histograms
# 3) find a t-test that looks interesting
#     one interval/continuous variable over 2 nominal labels
# 4) what if you want to know relationship between 2 continuous variables?
#     what's a correlation? how do you run it in R? 
#     what's the output mean? google says... 
install.packages("Hmisc"); library(Hmisc) #packages 'add to' R, rcorr(x,y)
# 5) what if you want to re-label something? R_KoalaStudies.csv
#     compare koala weight by location: USA versus Australia
#     #data[when row == "X", ] <- "New Value"
#     #data$col <- ifelse(data$col == "conditional", "new", "alternative")
