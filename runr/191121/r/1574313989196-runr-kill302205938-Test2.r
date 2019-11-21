#**************Start of header****************
# Title: R script for Online Test 2 of data analysis module
#
#*********************************************
#
# Description: Data Analysis Module ENVB40370 online test 2 script
#
# Name of author: Christine Coppinger
# Contact email: christine.coppinger@ucdconnect.ie
# Date created:  3 October 2019
# ********************************************
# ********************************************

rm(list=ls())     # Clear data from R's memory

# Set the working directory
setwd("~/5. Pollination Ecology/ANALYSIS/Rdirectory/
      ENVB40370_practice/DataAnalysisModule")      


#*********************************************
#
# Importing the data into R
# ARABIDOPSIS.CSV data is stored in as a csv

df = read.table('ARABIDOPSIS.CSV', header = T, sep=',')

# Become familiar with the data and how it's structured

head(df)     # Display the first 6 lines of the data
tail(df)     # Display the last 6 lines of the data
summary(df)  # Display overview of the dataframe
str(df)      # Display the structure of the data and data types of variables

#####***********Question 1***********************#####

# Q-Q plots to visualise data - which one of the variables is
# being visualised in the displayed Q-Q plot

library(ggplot2)  # Load ggplot

# A quantile-quantile plot for variable fruit
# Add a title that reminds us which variable is being plotted
ggplot(data=df,                                   # Define data to plot
       aes(sample=fruit))+
  geom_qq()+                                     # Draw QQ plot points
  geom_qq_line()+                                   # Draw QQ plot line
  labs(title='Normal Q-Q Plot for fruit')+ # Add a title
  theme_bw()                                     # Set the background to white

# A quantile-quantile plot for variable dfl
# Add a title that reminds us which variable is being plotted
ggplot(data=df,                                   # Define data to plot
       aes(sample=dfl))+
  geom_qq()+                                     # Draw QQ plot points
  geom_qq_line()+                                   # Draw QQ plot line
  labs(title='Normal Q-Q Plot for dfl')+ # Add a title
  theme_bw()                                     # Set the background to white

# A quantile-quantile plot for variable rlvs
# Add a title that reminds us which variable is being plotted
ggplot(data=df,                                   # Define data to plot
       aes(sample=rlvs))+
  geom_qq()+                                     # Draw QQ plot points
  geom_qq_line()+                                   # Draw QQ plot line
  labs(title='Normal Q-Q Plot for rlvs')+ # Add a title
  theme_bw()                                     # Set the background to white

# A quantile-quantile plot for variable height
# Add a title that reminds us which variable is being plotted
ggplot(data=df,                                   # Define data to plot
       aes(sample=height))+
  geom_qq()+                                     # Draw QQ plot points
  geom_qq_line()+                                   # Draw QQ plot line
  labs(title='Normal Q-Q Plot for height')+ # Add a title
  theme_bw()                                     # Set the background to white

# A quantile-quantile plot for variable garden
# Add a title that reminds us which variable is being plotted
ggplot(data=df,                                   # Define data to plot
       aes(sample=garden))+
  geom_qq()+                                     # Draw QQ plot points
  geom_qq_line()+                                   # Draw QQ plot line
  labs(title='Normal Q-Q Plot for garden')+ # Add a title
  theme_bw()                                     # Set the background to white


###### Q2 #####

# What is the median total full fruit count at harvest
# from exp garden South Carolina

# Create a subset of data with authority 9
df_SC = droplevels(subset(df, garden=='SC'))

summary(df_SC)   # Check that the subset has worked

median(df_SC$totfruit, na.rm=TRUE)  #median totfruit of subsetted data
signif(median(df_SC$totfruit, na.rm=TRUE),2) # Answer to 2 significant figures


##### Q3 #####

# What's the 5% quantile of the variable branches from
# plants in lineid called 'NA-60'?

# Subset the data frame and remove unwanted levels
df_NA60 = droplevels(subset(df, lineid=='NA-60'))
summary(df_NA60)    # Check the new subsetted data

# 5% quantiles of the variable branches
quantile(df_NA60$branches, prob=0.05, na.rm=T) 
signif(quantile(df_NA60$branches, prob=0.05, na.rm=T),2)


##### Q4 #####

# The mean rosette leaf diam at bolting (rdiam)
# for plants from pop SCP grown at exp. garden SC is
# 7.23 cm (3 sig fig)
# Use boostrapping to estimate the standard error of this mean


# Subset of plants from pop SCP grown at garden SC
df_Q4 = droplevels(subset(df, population=='SCP' & garden=='SC'))

summary(df_Q4)   # Check that the subset has worked

# Check the st. dev. of the subsetted Nitrate data
sd(df_Q4$rdiam, na.rm=TRUE) 
mean(df_Q4$rdiam, na.rm=TRUE)

# Create a variable that will contain the means 
# of the resampled data
resample_mean = array(NA, dim=10000)

# Use a for loop to resample the data 10000 times
for (i in 1:10000){
  resample = sample(df_Q4$rdiam, replace=T)
  # Save the sd of the resampled data in the array created
  resample_mean[i] = mean(resample, na.rm=TRUE)
}

# Calculate the standard deviation of the resampled means
# i.e. the standard error of the Sample mean
sd(resample_mean, na.rm=T)
signif(sd(resample_mean, na.rm=T),2)


##### Q5 ######

#The stand. dev. rosette leaf diam at bolding (rdiam)
# for plants from pop 'Tol' grown at exp. garden 'SC'
# is 7.87?? (3 sig fig) - is this not the mean?
# St. dev is 2.29 cm (3 sig fig)

# Subset of plants from pop Tol grown at garden SC
df_Q5 = droplevels(subset(df, population=='Tol' & garden=='SC'))

summary(df_Q5)   # Check that the subset has worked

# Check the st. dev. of the subsetted Nitrate data
sd(df_Q5$rdiam, na.rm=TRUE) 
mean(df_Q5$rdiam, na.rm=TRUE)

# Create a variable that will contain the means 
# of the resampled data
resample_sd = array(NA, dim=10000)

# Use a for loop to resample the data 10000 times
for (i in 1:10000){
  resample = sample(df_Q5$rdiam, replace=T)
  # Save the sd of the resampled data in the array created
  resample_sd[i] = sd(resample, na.rm=TRUE)
}

# Calculate the standard deviation of the resampled standard deviations
# i.e. the standard error of the Sample standard deviation
sd(resample_sd, na.rm=T)
signif(sd(resample_sd, na.rm=T),2)
