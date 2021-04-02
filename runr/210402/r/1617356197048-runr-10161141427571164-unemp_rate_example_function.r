# Tidying data for males
# setwd("code_examples")

data_file_male <- "data/1978-2017-australiansdg-indicator-8-5-2a-males.csv"
data_file_female <- "data/1978-2017-australiansdg-indicator-8-5-2a-females.csv"

### function to tide up the data
tide_data <- function(data, gender){
  data_tidier <- data %>% 
    separate(Month, into = c("Month", "Year"), sep = "\\-")
  
  # view(data_tidier)
  # swap the two columns
  temp <- data_tidier$Year[24:length(data_tidier$Year)]
  data_tidier$Year[24:length(data_tidier$Year)] <- 
    data_tidier$Month[24:length(data_tidier$Month)]
  data_tidier$Month[24:length(data_tidier$Month)] <- temp
  
  data_tidier$Year <- parse_integer(data_tidier$Year)
  
  # change to years
  data_tidier$Year[data_tidier$Year > 17 ] = 
    data_tidier$Year[data_tidier$Year > 17] + 1900
  
  data_tidier$Year[data_tidier$Year <= 17] = 
    data_tidier$Year[data_tidier$Year <= 17] + 2000
  view(data_tidier)
  col_names <- colnames(data_tidier)
  data_tidier <- rename(data_tidier, 
                             age_24=col_names[3], 
                             age_34=col_names[4],
                             age_44=col_names[5],
                             age_54=col_names[6],
                             age_64=col_names[7],
                             age_65=col_names[8])
  
  data_tidier <- data_tidier %>% 
    gather(age_group, unemp_rate, age_24: age_65)
  
  return(data_tidier)
}
### end of the tiding up function

# load libraries
library(tidyverse)

### for males
# 1- Read CSV data
male_data <- read_csv(data_file_male, na = '-')

# 2- tide the data a bit
male_data_tidier <- tide_data(male_data, "Male")

# 3- add `male` to the age groups 
male_data_tidier$age_group <- paste("Male", male_data_tidier$age_group, sep = "-")

male_data_tidier

### for females
# 1- Read CSV data
female_data <- read_csv(data_file_female, na = '-')

# 2- tide the data a bit
female_data_tidier <- tide_data(female_data, "Female")

# 3- add `male` to the age groups 
female_data_tidier$age_group <- paste("Female", female_data_tidier$age_group, sep = "-")

female_data_tidier

# put the two datasets together
all_data <- rbind(male_data_tidier, female_data_tidier)

# put the male and female into a separate column 
all_data_tidier <- all_data %>% 
  separate(age_group, into = c("gender", "age_group"), sep = "\\-")

# visualise the relationship between the unemployment rates and the age groups 
# for both the males and females   
all_data_tidier %>%
  group_by(age_group, gender) %>%
  summarise(avg_rate= mean(unemp_rate, na.rm = TRUE)) %>%
  ggplot(aes(age_group, avg_rate, fill = gender)) +
  geom_bar(position="dodge", stat = "identity") + 
  ggtitle("Unemployment Rate per Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Unemploymet Rate")


# save the data into one csv file
write_csv(all_data_tidier, "data/1978-2017-australiansdg-indicator-all.csv")
