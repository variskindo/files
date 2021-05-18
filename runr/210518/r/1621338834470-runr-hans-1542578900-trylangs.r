#ACTIVITY_1_NUMBER_1
#Extract waiting variable
faithful[,"waiting"]
attach(faithful)

#get the average of less than or equal to 50 mins. of waiting time to next eruption
subset(faithful, waiting<=50)
library(dplyr)
average_of_less_than_50_minutes_of_waiting_time_to_next_eruption <- faithful %>% filter(waiting<=50) %>%
  summarise(average_waiting=mean(waiting))

#print average of waiting 
average_of_less_than_50_minutes_of_waiting_time_to_next_eruption













#ACTIVITY_1_NUMBER_2
#create vector
number_two <- (1:40)
#print vector
number_two
#create matrix of 8 x 5. 
number_two_matrix <- matrix(number_two, nrow = 8, ncol = 5, byrow = F, dimnames = list(c("Apple","Banana","Orange","Grapes","Mango",
                                                                                         "Limes","Watermelons","Apricots"),
                                                                                       c("Blue","Red", "White", "Green", "Yellow")))
#print matrix
number_two_matrix

#A.extract row of Apple, Grapes, and Apricots 
number_two_matrix[c("Apple","Grapes","Apricots"),]















#ACTIVITY_1_NUMBER_3
#create factor
level_of_agreement_about_mass_testing <- c(replicate(60,"Strongly Agree"),
                                           replicate(50,"Agree"),
                                           replicate(30,"Disagree"),
                                           replicate(10,"Strongly Disagree"))
#print the factor
level_of_agreement_about_mass_testing

#rate the level of agreement
f_level_of_agreement_about_mass_testing <- factor(level_of_agreement_about_mass_testing, 
                                                  ordered=T, levels= c("Strongly Disagree",
                                                                       "Disagree","Agree",
                                                                       "Strongly Agree"))

#print the rating of level of agreement
f_level_of_agreement_about_mass_testing                                                  

















#ACTIVITY_1_NUMBER_4
#create a data frame; name the vectors
#Class
Class_Names <- c("Mathematics", "Chemistry", "Writing", "Art", "History", "Music", "Physical Education")
Class_Names

#Exams
Final_Scores <- c(88.0, 87.67, 86.00, 91.33, 84.00, 91.00, 89.33)
Final_Scores

#creating data frame
number_four_class_and_exams <- data.frame(Class_Names, Final_Scores)
number_four_class_and_exams

#extract elements from final scores vectors
number_four_class_and_exams$Class_Names
number_four_class_and_exams$Final_Scores

#create two new vector: liberal arts and fine arts
liberal_arts <- subset(number_four_class_and_exams, Final_Scores<= 86.00)
liberal_arts

#creating vector for fine arts
fine_arts <- subset(number_four_class_and_exams, Final_Scores>= 91)
fine_arts

library(dplyr)


#Calculate the average of the liberal arts and fine arts
ave_LA <- liberal_arts %>%  summarise (average=mean(Final_Scores))
ave_LA

ave_FA <- fine_arts %>% summarise (average=mean(Final_Scores))
ave_FA 

#calculate grade point average
GPA <- number_four_class_and_exams %>% summarise (average= mean(Final_Scores))
GPA

# compare the final_scores to GPA. store the logical output in a vector named above average
above_average <- ifelse(Final_Scores >= 88.19, T, F)
above_average





















#ACTIVITY_1_NUMBER_5
library(readxl)
#I used google sheets in case the file on my desktop didn't work
install.packages('gsheet')
library(gsheet)
SAMPLE_DATA <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1DNoIv2kDbpEkT8MUiyhSYYtSuObXfKZx/edit#gid=1724844200')
SAMPLE_DATA

library(dplyr)

#Take the first 70% of the observation and name it as "DATA1"
DATA1 <- SAMPLE_DATA %>%slice(1:3459)
#print DATA1
DATA1

#Remove the variable Language, Science, Numerical, and English
sel1 <- DATA1 %>% select(-c("Language", "Science", "Numerical", "eng"))
#print sel1
sel1

#Rename the variable "GenIfo" as "General Information"
rename_gen_info <- sel1 %>% rename(General_Information="GenInfo")

#Include only BSME and BSRE students with 85 and above grade in mathematics
Fil1 <- sel1 %>%
  filter(CourseCode%in%c("BSME","BSRE") &
           mat>=85)
#print Fil1
Fil1

#Summarize your data to find out who has the highest average grades in mathematics based on gender
sum1<- sel1 %>% group_by(Gender) %>%
  summarise(average=mean(mat))
#print sum1
sum1



