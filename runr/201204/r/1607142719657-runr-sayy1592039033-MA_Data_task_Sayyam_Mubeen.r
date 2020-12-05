##### Medicare Advantage Data Manipulation Task
##### Sayyam Mubeen 
#### Started: 19:36 11/29/2020
#### Ended: 19:59 11/29/2020

library(dplyr)
library(openxlsx)
## Set working directory: Change to where the csv file is stored.
setwd("C:/Users/Sayyam/Desktop/NW_data_task/Medicare_Advantage")

df<- read.csv("scp-1205.csv", header = FALSE, col.names = c("countyname","state","copntract","healthplan",
                                                            "typeofplan","countyssa","eligibles","enrollees",
                                                            "penetration","ABrate"))
## Converting Missing Values as zero
df$eligibles[is.na(df$eligibles)]= 0  ## Converting NAs to 0 

df$enrollees[is.na(df$enrollees)]= 0

df$penetration[is.na(df$penetration)]=0

df<- df %>%  ## Creating necceasarry varibales for total enrollees and total penetration per county
  
  group_by(countyname,state) %>% 
  
  mutate(totalenrollees = sum(enrollees) ) %>%  ## Total enrollees
  
  mutate(totalpenetration = 100*(totalenrollees/eligibles)) ## Total penetrations

req<- df %>%  ### Creating required Dataset 
  filter(countyname != "UNDER-11 " & countyname != "Unusual SCounty Code ") %>%  ## Filtering out unknown states and, 
  ### assuming that they are territories
 
  group_by(countyname,state,countyssa,eligibles,totalenrollees,totalpenetration) %>%  ## groupung by county and state and,
  ###to bring other variables since they are going be same for all county state combinations
  
  summarise(numberofplans1 = sum(enrollees>10), numberofplans2 = sum(penetration>0.5)) %>% ## Generating required variables,
  ### Having neccessary conditions
  
  arrange(state,countyname) %>% ## sorting by state and countyname 
  
  # Optional: arranging columns according to desired order 
  select(countyname,state,numberofplans1,numberofplans2,countyssa,eligibles,totalenrollees,totalpenetration)  

write.csv(req, "required_datatset.csv") # Converting file to csv as output for required dataset

### the required dataset is found in the depository under required_dataset.csv
### Finished in approximately 23 minutes.


