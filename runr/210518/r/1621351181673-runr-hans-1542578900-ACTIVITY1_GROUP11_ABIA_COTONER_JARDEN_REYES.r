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
# name the vectors
#A. Exams
Final_Scores <- c(88.0, 87.67, 86.00, 91.33, 84.00, 91.00, 89.33)
Final_Scores

#B. Class
Class_Names <- c("Mathematics", "Chemistry", "Writing", "Art", "History", "Music", "Physical Education")
Class_Names

#C. assign class names to each grades final scores vector
names(Final_Scores)<- c("Mathematics", "Chemistry", "Writing", "Art", "History", "Music", "Physical Education")
print(Final_Scores)


#D. extract elements from final scores vectors

#create two new vector: liberal arts and fine arts
Liberal_Arts<-Final_Scores[c("Writing","History")]
Liberal_Arts

#creating vector for fine arts
Fine_Arts <- Final_Scores[c("Art", "Music")]
Fine_Arts



#E. Calculate the average of each new vector (liberal arts & fine arts)
ave_LA <- mean(Liberal_Arts)
ave_LA
ave_FA <- mean(Fine_Arts)
ave_FA

#F. calculate grade point average
GPA <- mean(Final_Scores)
GPA

#G. compare the final_scores to GPA. store the logical output in a vector named above average
above_average <- Final_Scores >= GPA
above_average























#ACTIVITY_1_NUMBER_5
install.packages("readxl")
library(readxl)
SAMPLE_DATA <- read_excel("C:/Users/HAILAN-PC/Desktop/EDA/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)
attach(SAMPLE_DATA)
library(dplyr)



#Take the first 70% of the observation and name it as "DATA1"
seventy_percent <- 0.70*nrow(SAMPLE_DATA)
DATA1 <- SAMPLE_DATA %>%slice(1:seventy_percent)
#print DATA1
DATA1


#rename the variable eng to English
DATA2 <- DATA1 %>% rename(English="eng")
DATA2


#Remove the variable Language, Science, Numerical, and English
sel1 <- DATA2 %>% select(-c("Language", "Science", "Numerical", "English"))
#print sel1
sel1


#Rename the variables mat,sci, & GenInfo
DATA3 <- sel1 %>% rename(Mathematics= "mat")
DATA3
DATA4 <- DATA3 %>% rename(Science="sci")
DATA4
DATA5 <- DATA4 %>% rename(General_Information ="GenInfo")
DATA5



#Include only BSME and BSRE students with 85 and above grade in mathematics
Fil1 <- DATA5 %>%
  filter(CourseCode%in%c("BSME","BSRE") &
           Mathematics>=85)
#print Fil1
Fil1


#Summarize your data to find out who has the highest average grades in mathematics based on gender
sum1<- Fil1 %>% group_by(Gender) %>%
  summarise(Average_Mathematics=mean(Mathematics))
#print sum1
sum1


