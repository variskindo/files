####################################################################################################################################################
####################################################################################################################################################
############################################################  Dissertation Analyses  ###############################################################
####################################################################################################################################################
####################################################################################################################################################
######################
### Pre-Processing ###
######################
rm(list=ls())
# Set working directory
setwd("C:/Users/tcarse2/OneDrive/Studies/Trust/Dissertation/Data/Working Data/")

# Load libraries
library(psych)
library(lme4)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(data.table)

#################
### Load data ###
#################
data1<-read.csv("Dissertation_IdioTrust.csv",header=T,na.strings = c(""))

######################
### Creating a PIN ###
######################
data1$PIN<-seq(from=1, to=length(data1$Progress))
data1$PIN<-as.factor(data1$PIN)

##################################
### Extracting Number of Goals ###
##################################
data1$numGoals<-(25-rowSums(is.na(data1[22:46])))

########################################################################
### Creating variables for the continuous measure of goal importance ###
######################### Most Important Goals #########################
########################################################################
data1$g1Imp<-(as.numeric(data1$Q5.2)-1)
data1$g2Imp<-(as.numeric(data1$Q6.2)-1)
data1$g3Imp<-(as.numeric(data1$Q7.2)-1)

#############################
### Least Important Goals ###
#############################
data1$g4Imp<-(as.numeric(data1$Q10.2)-1)
data1$g5Imp<-(as.numeric(data1$Q9.2)-1)
data1$g6Imp<-(as.numeric(data1$Q8.2)-1)

#########################################
### Creating Goal Relevance Variables ###
######### Most Important Goals ##########
#########################################
# Goal 1 Strength
Goal1StrRel<-subset(data1, select = c(PIN,Q14.1_1:Q14.1_30)) %>%
  gather(Goal,Relevance,
  c(Q14.1_1:Q14.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal1StrRel<-(as.numeric(Goal1StrRel$Relevance)-1)

# Goal 1 Weakness
Goal1WeakRel<-subset(data1, select = c(PIN,Q15.1_1:Q15.1_30)) %>%
    gather(Goal,Relevance,
    c(Q15.1_1:Q15.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal1WeakRel<-(as.numeric(Goal1WeakRel$Relevance)-1)

# Goal 2 Strength
Goal2StrRel<-subset(data1, select = c(PIN,Q16.1_1:Q16.1_30)) %>%
    gather(Goal,Relevance,
    c(Q16.1_1:Q16.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal2StrRel<-(as.numeric(Goal2StrRel$Relevance)-1)

# Goal 2 Weakness
Goal2WeakRel<-subset(data1, select = c(PIN,Q17.1_1:Q17.1_30)) %>%
    gather(Goal,Relevance,
    c(Q17.1_1:Q17.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal2WeakRel<-(as.numeric(Goal2WeakRel$Relevance)-1)

# Goal 3 Strength
Goal3StrRel<-subset(data1, select = c(PIN,Q18.1_1:Q18.1_30)) %>%
    gather(Goal,Relevance,
    c(Q18.1_1:Q18.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal3StrRel<-(as.numeric(Goal3StrRel$Relevance)-1)

# Goal 3 Weakness
Goal3WeakRel<-subset(data1, select = c(PIN,Q19.1_1:Q19.1_30)) %>%
    gather(Goal,Relevance,
    c(Q19.1_1:Q19.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal3WeakRel<-(as.numeric(Goal3WeakRel$Relevance)-1)

#############################
### Least Important Goals ###
#############################
# Goal 4 Strength
Goal4StrRel<-subset(data1, select = c(PIN,Q24.1_1:Q24.1_30)) %>%
    gather(Goal,Relevance,
    c(Q24.1_1:Q24.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal4StrRel<-(as.numeric(Goal4StrRel$Relevance)-1)

# Goal 4 Weakness
Goal4WeakRel<-subset(data1, select = c(PIN,Q25.1_1:Q25.1_30)) %>%
    gather(Goal,Relevance,
    c(Q25.1_1:Q25.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal4WeakRel<-(as.numeric(Goal4WeakRel$Relevance)-1)

# Goal 5 Strength
Goal5StrRel<-subset(data1, select = c(PIN,Q22.1_1:Q22.1_30)) %>%
    gather(Goal,Relevance,
    c(Q22.1_1:Q22.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal5StrRel<-(as.numeric(Goal5StrRel$Relevance)-1)

# Goal 5 Weakness
Goal5WeakRel<-subset(data1, select = c(PIN,Q23.1_1:Q23.1_30)) %>%
    gather(Goal,Relevance,
    c(Q23.1_1:Q23.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal5WeakRel<-(as.numeric(Goal5WeakRel$Relevance)-1)

# Goal 6 Strength
Goal6StrRel<-subset(data1, select = c(PIN,Q20.1_1:Q20.1_30)) %>%
    gather(Goal,Relevance,
    c(Q20.1_1:Q20.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal6StrRel<-(as.numeric(Goal6StrRel$Relevance)-1)

# Goal 6 Weakness
Goal6WeakRel<-subset(data1, select = c(PIN,Q21.1_1:Q21.1_30)) %>%
    gather(Goal,Relevance,
    c(Q21.1_1:Q21.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal6WeakRel<-(as.numeric(Goal6WeakRel$Relevance)-1)

#####################################
### Creating Goal-Based Trust DVs ###
#####################################
###########################
### Trust for Project 1 ###
###########################
# Trust
Goal1TrustDi<-subset(data1, select = c(PIN,Q26.1_1:Q26.1_30)) %>%
    gather(Goal,Trust,
    c(Q26.1_1:Q26.1_30)) %>% 
    group_by(PIN) %>% 
    summarise_all(list(~na.omit(.)[1]))
data1$Goal1TrustDi<-as.numeric(Goal1TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G1TrustExt<-as.numeric(data1$Q26.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G1DistrustExt<-as.numeric(-data1$Q26.3)
# Lean Toward Trust vs Distrust
data1$G1TrustLean<-as.numeric(data1$Q26.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal1TrustBP<-ifelse(data1$Goal1TrustDi==1,data1$G1TrustExt,
                           ifelse(data1$Goal1TrustDi==2,data1$G1DistrustExt,
                           ifelse(data1$TrustLean==1,1,
                           ifelse(data1$TrustLean==2,-1,NA))))

# Rely
Goal1RelyDi<-subset(data1, select = c(PIN,Q26.5_1:Q26.5_30)) %>%
  gather(Goal,Trust,
  c(Q26.5_1:Q26.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal1RelyDi<-as.numeric(Goal1RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G1RelyExt<-as.numeric(data1$Q26.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G1UnrelyDiExt<-as.numeric(-data1$Q26.7)
# Lean Toward Trust vs Distrust
data1$G1RelyLean<-as.numeric(data1$Q26.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal1RelyBP<-ifelse(data1$Goal1RelyDi==1,data1$G1RelyExt,
                           ifelse(data1$Goal1RelyDi==2,data1$G1UnrelyDiExt,
                           ifelse(data1$RelyLean==1,1,
                           ifelse(data1$RelyLean==2,-1,NA))))

###########################
### Trust for Project 2 ###
###########################
# Trust
Goal2TrustDi<-subset(data1, select = c(PIN,Q27.1_1:Q27.1_30)) %>%
  gather(Goal,Trust,
  c(Q27.1_1:Q27.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal2TrustDi<-as.numeric(Goal2TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G2TrustExt<-as.numeric(data1$Q27.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G2DistrustExt<-as.numeric(-data1$Q27.3)
# Lean Toward Trust vs Distrust
data1$G2TrustLean<-as.numeric(data1$Q27.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal2TrustBP<-ifelse(data1$Goal2TrustDi==1,data1$G2TrustExt,
                           ifelse(data1$Goal2TrustDi==2,data1$G2DistrustExt,
                           ifelse(data1$G2TrustLean==1,1,
                           ifelse(data1$G2TrustLean==2,-1,NA))))

# Rely
Goal2RelyDi<-subset(data1, select = c(PIN,Q27.5_1:Q27.5_30)) %>%
  gather(Goal,Trust,
  c(Q27.5_1:Q27.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal2RelyDi<-as.numeric(Goal2RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G2RelyExt<-as.numeric(data1$Q27.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G2UnrelyDiExt<-as.numeric(-data1$Q27.7)
# Lean Toward Trust vs Distrust
data1$G2RelyLean<-as.numeric(data1$Q27.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal2RelyBP<-ifelse(data1$Goal2RelyDi==1,data1$G2RelyExt,
                          ifelse(data1$Goal2RelyDi==2,data1$G2UnrelyDiExt,
                          ifelse(data1$G2RelyLean==1,1,
                          ifelse(data1$G2RelyLean==2,-1,NA))))

###########################
### Trust for Project 3 ###
###########################
# Trust
Goal3TrustDi<-subset(data1, select = c(PIN,Q28.1_1:Q28.1_30)) %>%
  gather(Goal,Trust,
  c(Q28.1_1:Q28.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal3TrustDi<-as.numeric(Goal3TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G3TrustExt<-as.numeric(data1$Q28.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G3DistrustExt<-as.numeric(-data1$Q28.3)
# Lean Toward Trust vs Distrust
data1$G3TrustLean<-as.numeric(data1$Q28.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal3TrustBP<-ifelse(data1$Goal3TrustDi==1,data1$G3TrustExt,
                           ifelse(data1$Goal3TrustDi==2,data1$G3DistrustExt,
                           ifelse(data1$G3TrustLean==1,1,
                           ifelse(data1$G3TrustLean==2,-1,NA))))

# Rely
Goal3RelyDi<-subset(data1, select = c(PIN,Q28.5_1:Q28.5_30)) %>%
  gather(Goal,Trust,
  c(Q28.5_1:Q28.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal3RelyDi<-as.numeric(Goal3RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G3RelyExt<-as.numeric(data1$Q28.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G3UnrelyDiExt<-as.numeric(-data1$Q28.7)
# Lean Toward Trust vs Distrust
data1$G3RelyLean<-as.numeric(data1$Q28.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal3RelyBP<-ifelse(data1$Goal3RelyDi==1,data1$G3RelyExt,
                          ifelse(data1$Goal3RelyDi==2,data1$G3UnrelyDiExt,
                          ifelse(data1$G3RelyLean==1,1,
                          ifelse(data1$G3RelyLean==2,-1,NA))))

###########################
### Trust for Project 4 ###
###########################
# Trust
Goal4TrustDi<-subset(data1, select = c(PIN,Q31.1_1:Q31.1_30)) %>%
  gather(Goal,Trust,
  c(Q31.1_1:Q31.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal4TrustDi<-as.numeric(Goal4TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G4TrustExt<-as.numeric(data1$Q31.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G4DistrustExt<-as.numeric(-data1$Q31.3)
# Lean Toward Trust vs Distrust
data1$G4TrustLean<-as.numeric(data1$Q31.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal4TrustBP<-ifelse(data1$Goal4TrustDi==1,data1$G4TrustExt,
                           ifelse(data1$Goal4TrustDi==2,data1$G4DistrustExt,
                           ifelse(data1$G4TrustLean==1,1,
                           ifelse(data1$G4TrustLean==2,-1,NA))))

# Rely
Goal4RelyDi<-subset(data1, select = c(PIN,Q31.5_1:Q31.5_30)) %>%
  gather(Goal,Trust,
  c(Q31.5_1:Q31.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal4RelyDi<-as.numeric(Goal4RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G4RelyExt<-as.numeric(data1$Q31.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G4UnrelyDiExt<-as.numeric(-data1$Q31.7)
# Lean Toward Trust vs Distrust
data1$G4RelyLean<-as.numeric(data1$Q31.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal4RelyBP<-ifelse(data1$Goal4RelyDi==1,data1$G4RelyExt,
                          ifelse(data1$Goal4RelyDi==2,data1$G4UnrelyDiExt,
                          ifelse(data1$G4RelyLean==1,1,
                          ifelse(data1$G4RelyLean==2,-1,NA))))

###########################
### Trust for Project 5 ###
###########################
# Trust
Goal5TrustDi<-subset(data1, select = c(PIN,Q30.1_1:Q30.1_30)) %>%
  gather(Goal,Trust,
  c(Q30.1_1:Q30.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal5TrustDi<-as.numeric(Goal5TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G5TrustExt<-as.numeric(data1$Q30.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G5DistrustExt<-as.numeric(-data1$Q30.3)
# Lean Toward Trust vs Distrust
data1$G5TrustLean<-as.numeric(data1$Q30.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal5TrustBP<-ifelse(data1$Goal5TrustDi==1,data1$G5TrustExt,
                           ifelse(data1$Goal5TrustDi==2,data1$G5DistrustExt,
                           ifelse(data1$G5TrustLean==1,1,
                           ifelse(data1$G5TrustLean==2,-1,NA))))

# Rely
Goal5RelyDi<-subset(data1, select = c(PIN,Q30.5_1:Q30.5_30)) %>%
  gather(Goal,Trust,
  c(Q30.5_1:Q30.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal5RelyDi<-as.numeric(Goal5RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G5RelyExt<-as.numeric(data1$Q30.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G5UnrelyDiExt<-as.numeric(-data1$Q30.7)
# Lean Toward Trust vs Distrust
data1$G5RelyLean<-as.numeric(data1$Q30.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal5RelyBP<-ifelse(data1$Goal5RelyDi==1,data1$G5RelyExt,
                          ifelse(data1$Goal5RelyDi==2,data1$G5UnrelyDiExt,
                          ifelse(data1$G5RelyLean==1,1,
                          ifelse(data1$G5RelyLean==2,-1,NA))))

###########################
### Trust for Project 6 ###
###########################
# Trust
Goal6TrustDi<-subset(data1, select = c(PIN,Q29.1_1:Q29.1_30)) %>%
  gather(Goal,Trust,
  c(Q29.1_1:Q29.1_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal6TrustDi<-as.numeric(Goal6TrustDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G6TrustExt<-as.numeric(data1$Q29.2)
# Extent Ss distrusts Best Friend for Project 1
data1$G6DistrustExt<-as.numeric(-data1$Q29.3)
# Lean Toward Trust vs Distrust
data1$G6TrustLean<-as.numeric(data1$Q29.4)
# Creating bipolar measure of Trust-Distrust
data1$Goal6TrustBP<-ifelse(data1$Goal6TrustDi==1,data1$G6TrustExt,
                           ifelse(data1$Goal6TrustDi==2,data1$G6DistrustExt,
                           ifelse(data1$G6TrustLean==1,1,
                           ifelse(data1$G6TrustLean==2,-1,NA))))

# Rely
Goal6RelyDi<-subset(data1, select = c(PIN,Q29.5_1:Q29.5_30)) %>%
  gather(Goal,Trust,
  c(Q29.5_1:Q29.5_30)) %>% 
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1]))
data1$Goal6RelyDi<-as.numeric(Goal6RelyDi$Trust)
# Extent Ss Trusts Best Friend for Project 1
data1$G6RelyExt<-as.numeric(data1$Q29.6)
# Extent Ss distrusts Best Friend for Project 1
data1$G6UnrelyDiExt<-as.numeric(-data1$Q29.7)
# Lean Toward Trust vs Distrust
data1$G6RelyLean<-as.numeric(data1$Q29.8)
# Creating bipolar measure of Trust-Distrust
data1$Goal6RelyBP<-ifelse(data1$Goal6RelyDi==1,data1$G6RelyExt,
                          ifelse(data1$Goal6RelyDi==2,data1$G6UnrelyDiExt,
                          ifelse(data1$G6RelyLean==1,1,
                          ifelse(data1$G6RelyLean==2,-1,NA))))

##########################################
### Creating Composite Measures of SITS ###
##########################################
# Subsetting the factor Overall Trust from the SITS
SITStrust<-subset(data1, select=c(Q32.1:Q32.9))
# Converting the scale to be from -4:+4
SITStrust<-SITStrust-5
# Reverse-scoring items
SITStrust$Q32.4<-(-SITStrust$Q32.4)
SITStrust$Q32.9<-(-SITStrust$Q32.9)
# Adding the average of the items back to dataframe
data1$SITStrust<-rowMeans(SITStrust)
# Here's an alternate way to create the variable that's better but doesn't center the variables
# I did this second and as a way to ensure the first version does what it's supposed to
data1<-data1 %>%
  rowwise() %>%
  mutate(SITStrust2 = mean(c(Q32.1,Q32.2,Q32.3,9-Q32.4,Q32.5,Q32.6,
                             Q32.7,Q32.8,9-Q32.9)))
cor(data1$SITStrust,data1$SITStrust2, use = "pairwise.complete.obs")
# r = 1, so it checks

# Subsetting the factor Emotional Trust from the SITS
SITSemo<-subset(data1, select=c(Q32.10:Q32.16))
# Converting the scale to be from -4:+4
SITSemo<-SITSemo-5
# Reverse-scoring items
SITSemo$Q32.10<-(-SITSemo$Q32.10)
# Adding the average of the items back to dataframe
data1$SITSemo<-rowMeans(SITSemo)

# Subsetting the factor Reliability from the SITS
SITSrely<-subset(data1, select=c(Q32.17:Q32.23))
# Converting the scale to be from -4:+4
SITSrely<-SITSrely-5
# Adding the average of the items back to dataframe
data1$SITSrely<-rowMeans(SITSrely)

# Checking correlations among factors
SITStest<-subset(data1, select = c(SITStrust,SITSemo,SITSrely))
cor(na.omit(SITStest))
#############################################
#           SITStrust    SITSemo   SITSrely #
# SITStrust 1.0000000 0.4949231   0.6653781 #
# SITSemo   0.4949231 1.0000000   0.5519472 #
# SITSrely  0.6653781 0.5519472   1.0000000 #
#############################################

##########################################
### Creating Composite Measures of ABI ###
##########################################
# Subsetting the factor Ability from the ABI
ABIable<-subset(data1, select=c(Q33.1:Q33.6))
# Converting the scale to be from -2:+2
ABIable<-ABIable-3
# Adding the average of the items back to dataframe
data1$ABIable<-rowMeans(ABIable)

# Subsetting the factor Benevolence from the ABI
ABIbenev<-subset(data1, select=c(Q33.7:Q33.11))
# Converting the scale to be from -2:+2
ABIbenev<-ABIbenev-3
# Adding the average of the items back to dataframe
data1$ABIbenev<-rowMeans(ABIbenev)

# Subsetting the factor Integrity from the ABI
ABItegridy<-subset(data1, select=c(Q33.12:Q33.17))
# Converting the scale to be from -2:+2
ABItegridy<-ABItegridy-3
# Reverse-scoring items
ABItegridy$Q33.15<-(-ABItegridy$Q33.15)
# Adding the average of the items back to dataframe
data1$ABItegridy<-rowMeans(ABItegridy)

# Checking correlations among factors
ABItest<-subset(data1, select = c(ABIable,ABIbenev,ABItegridy))
cor(na.omit(ABItest))
#############################################
#              ABIable  ABIbenev ABItegridy #
# ABIable    1.0000000 0.5448763  0.5718301 #
# ABIbenev   0.5448763 1.0000000  0.5307649 #
# ABItegridy 0.5718301 0.5307649  1.0000000 #
#############################################

###############################
### Subsetting For Analyses ###
###############################
data2<-subset(data1,select=c(PIN:Goal1TrustDi,Goal1TrustBP,Goal1RelyDi,Goal1RelyBP,Goal2TrustDi,
                             Goal2TrustBP,Goal2RelyDi,Goal2RelyBP,Goal3TrustDi,Goal3TrustBP,Goal3RelyDi,
                             Goal3RelyBP,Goal4TrustDi,Goal4TrustBP,Goal4RelyDi,Goal4RelyBP,Goal5TrustDi,
                             Goal5TrustBP,Goal5RelyDi,Goal5RelyBP,Goal6TrustDi,Goal6TrustBP,Goal6RelyDi,
                             Goal6RelyBP,SITStrust,SITSemo,SITSrely,ABIable,ABIbenev,ABItegridy)) 
str(data2)
#################################################
### Arranging columns for easier manipulation ###

#######################
### Goal Importance ###
# Select vars
impData<-data2 %>% dplyr::select(tidyselect::vars_select(names(data2), dplyr::matches('Imp')))
# Check cor
cor(impData, use = "pairwise.complete.obs")
# Add PIN
impData$PIN<-data2$PIN
# Wide to Long
impLong<-impData %>% gather(Goal,Importance,g1Imp:g6Imp)
# Rename factor levels
impLong$Goal<-mapvalues(impLong$Goal, from = c("g1Imp","g2Imp","g3Imp","g4Imp","g5Imp","g6Imp"),
                        to = c("Goal1","Goal2","Goal3","Goal4","Goal5","Goal6"))

#############################################
### Relevance of strength to each project ###
# Select vars
strengthData<-data2 %>% dplyr::select(tidyselect::vars_select(names(data2), matches('StrRel')))
# Check cor
cor(strengthData, use = "pairwise.complete.obs")
# Add PIN
strengthData$PIN<-data2$PIN
# Wide to Long
srelLong<-strengthData %>% gather(Goal,StRel,Goal1StrRel:Goal6StrRel)
# Rename factor levels
srelLong$Goal<-mapvalues(srelLong$Goal, from = c("Goal1StrRel","Goal2StrRel","Goal3StrRel","Goal4StrRel","Goal5StrRel","Goal6StrRel"),
                         to = c("Goal1","Goal2","Goal3","Goal4","Goal5","Goal6"))

#############################################
### Relevance of weakness to each project ###
# Select vars
weakData<-data2 %>% dplyr::select(tidyselect::vars_select(names(data2), matches('WeakRel')))
# Check cor
cor(weakData, use = "pairwise.complete.obs")
# Add PIN
weakData$PIN<-data2$PIN
# Wide to Long
wrelLong<-weakData %>% gather(Goal,WeakRel,Goal1WeakRel:Goal6WeakRel)
# Rename factor levels
wrelLong$Goal<-mapvalues(wrelLong$Goal, from = c("Goal1WeakRel","Goal2WeakRel","Goal3WeakRel","Goal4WeakRel","Goal5WeakRel","Goal6WeakRel"),
                         to = c("Goal1","Goal2","Goal3","Goal4","Goal5","Goal6"))

#########################
### Trust best friend ###
# Select vars
trustData<-data2 %>% dplyr::select(tidyselect::vars_select(names(data2), matches('TrustBP')))
# Check cor
cor(trustData, use = "pairwise.complete.obs")
# Add PIN
trustData$PIN<-data2$PIN
# Wide to Long
tLong<-trustData %>% gather(Goal,Trust,Goal1TrustBP:Goal6TrustBP)
# Rename factor levels
tLong$Goal<-mapvalues(tLong$Goal, from = c("Goal1TrustBP","Goal2TrustBP","Goal3TrustBP","Goal4TrustBP","Goal5TrustBP","Goal6TrustBP"),
                      to = c("Goal1","Goal2","Goal3","Goal4","Goal5","Goal6"))

###########################
### Rely on best friend ###
# Select vars
relyData<-data2 %>% dplyr::select(tidyselect::vars_select(names(data2), matches('RelyBP')))
# Check cor
cor(relyData, use = "pairwise.complete.obs")
# Add PIN
relyData$PIN<-data2$PIN
# Wide to Long
rLong<-relyData %>% gather(Goal,Rely,Goal1RelyBP:Goal6RelyBP)
# Rename factor levels
rLong$Goal<-mapvalues(rLong$Goal, from = c("Goal1RelyBP","Goal2RelyBP","Goal3RelyBP","Goal4RelyBP","Goal5RelyBP","Goal6RelyBP"),
                      to = c("Goal1","Goal2","Goal3","Goal4","Goal5","Goal6"))

##################################
### Creating Working Long Data ###
# Importance and Trust
imp.trust<-merge(tLong,impLong,by=c("Goal","PIN"))
# Adding rely
imp.trust.rely<-merge(imp.trust,rLong,by=c("Goal","PIN"))
# Adding relevance to strength
imp.trust.rely.str<-merge(imp.trust.rely,srelLong,by=c("Goal","PIN"))
# Adding relevance to weakness
data3<-merge(imp.trust.rely.str,wrelLong,by=c("Goal","PIN"))
# Adding Nomothetic Measures and Number of Goals
# Number of goals
data3$numGoals<-data2$numGoals
# SITS Overall Trust
data3$SITStrust<-data2$SITStrust
# SITS Emotional Trust
data3$SITSemo<-data2$SITSemo
# SITS Reliability
data3$SITSrely<-data2$SITSrely
# ABI Ability
data3$ABIable<-data2$ABIable
# ABI Benevolence
data3$ABIbenev<-data2$ABIbenev
# ABI Integrity
data3$ABItegridy<-data2$ABItegridy
# Converting variables to proper format
data3$Goal<-as.factor(data3$Goal)

# Adding Factor Indicating Most vs Least Important Projects
data3$impCat<-ifelse(data3$Goal=="Goal1",1,
                 ifelse(data3$Goal=="Goal2",1,
                 ifelse(data3$Goal=="Goal3",1,
                 ifelse(data3$Goal=="Goal4",0,
                 ifelse(data3$Goal=="Goal5",0,
                 ifelse(data3$Goal=="Goal6",0,NA))))))
# Converting to Factor
data3$impCat<-as.factor(data3$impCat)

data3$impCat<-factor(data3$impCat,
                     levels=c(0,1),
                     labels=c("Least Important","Most Important"))

str(data3)

#write.csv(data3, file="workingData.csv")

########################################################################################################
###################################### This Section is Deprecated ######################################
########################################################################################################
# This section was created to extract the goal categories (e.g., Academic, Occupational) for each goal #
# The lines that are no longer necessary are commented out                                             #
#                                                                                                      #
# rm(list=ls())                                                                                        #
# Set working directory                                                                                #
# setwd("C:/Users/tcarse2/OneDrive/Studies/Trust/Dissertation/Data/Working Data/")                     #
#                                                                                                      #
# Load libraries                                                                                       #
# library(psych)                                                                                       #
# library(lme4)                                                                                        #
# library(plyr)                                                                                        #
# library(tidyverse)                                                                                   #
# library(ggplot2)                                                                                     #
# library(ggstatsplot)                                                                                 #
# library(data.table)                                                                                  #
#                                                                                                      #
#                                                                                                      #
#################                                                                                      #
### Load data ###                                                                                      #
#################                                                                                      #
# data3<-read.csv("workingData.csv",header=T,na.strings = c(""))                                       #
# data1<-read.csv("Dissertation_IdioTrust.csv",header=T,na.strings = c(""))                            #
#                                                                                                      #
######################                                                                                 #
### Creating a PIN ###                                                                                 #
######################                                                                                 #
# data1$PIN<-seq(from=1, to=length(data1$Progress))                                                    #
# data1$PIN<-as.factor(data1$PIN)                                                                      #
# data3$PIN<-as.factor(data3$PIN)                                                                      #
########################################################################################################

##################################
### Extracting Goal Categories ###
##################################
## Creating a table with goal numbers for reference
## Most important goals
# Separate string by comma into each goal number
mostImp<-setDT(data1)[, strsplit(as.character(Q4.1_0_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.1_0_GROUP)
                      ][,.(Q4.1_0_GROUP = V1, PIN)]
# Number goals from 1 to 3
mostImp$Goal<-as.factor(rep(seq(from=1,to=3), length.out=length(mostImp$PIN)))
# Convert to wide format
mostImp<-mostImp %>% pivot_wider(names_from=Goal, values_from=Q4.1_0_GROUP)
# Rename variables
names(mostImp)[2:4]<-paste0("Goal",names(mostImp)[2:4])

## Least important goals
# Separate string by comma into each goal number
leastImp<-setDT(data1)[, strsplit(as.character(Q4.2_0_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.2_0_GROUP)
                       ][,.(Q4.2_0_GROUP = V1, PIN)]
# Number goals from 1 to 3
leastImp$Goal<-as.factor(rep(seq(from=6,to=4), length.out=length(leastImp$PIN)))
# Convert to wide format
leastImp<-leastImp %>% pivot_wider(names_from=Goal, values_from=Q4.2_0_GROUP)
# Rename variables
names(leastImp)[2:4]<-paste0("Goal",names(leastImp)[2:4])

# Merging Most & Least Important Goals
goalData<-merge(mostImp,leastImp, by="PIN")

### Identifying goal categories
## Most important
# Splitting Academic Projects
academicsMI<-setDT(data1)[, strsplit(as.character(Q4.3_0_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_0_GROUP)
                          ][,.(Q4.3_0_GROUP = V1, PIN)]
# Splitting Occupational Projects
occupationalMI<-setDT(data1)[, strsplit(as.character(Q4.3_1_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_1_GROUP)
                             ][,.(Q4.3_1_GROUP = V1, PIN)]
# Splitting Health/Body Projects
healthMI<-setDT(data1)[, strsplit(as.character(Q4.3_2_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_2_GROUP)
                       ][,.(Q4.3_2_GROUP = V1, PIN)]
# Splitting Interpersonal Projects
interMI<-setDT(data1)[, strsplit(as.character(Q4.3_3_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_3_GROUP)
                      ][,.(Q4.3_3_GROUP = V1, PIN)]
# Splitting Leisure Projects
leisureMI<-setDT(data1)[, strsplit(as.character(Q4.3_4_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_4_GROUP)
                        ][,.(Q4.3_4_GROUP = V1, PIN)]
# Splitting Intrapersonal Projects
intraMI<-setDT(data1)[, strsplit(as.character(Q4.3_5_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_5_GROUP)
                      ][,.(Q4.3_5_GROUP = V1, PIN)]
# Splitting Maintenance Projects
maintainMI<-setDT(data1)[, strsplit(as.character(Q4.3_6_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.3_6_GROUP)
                         ][,.(Q4.3_6_GROUP = V1, PIN)]
# Merging Academmic and Occupational MI Goals
tryHard<-merge(academicsMI,occupationalMI,all=T,by="PIN")
# Renaming Categories
tryHard<-plyr::rename(tryHard, replace=c("Q4.3_0_GROUP"="Academic","Q4.3_1_GROUP"="Occupational"))
# Adding MI Health
tryHarder<-merge(tryHard,healthMI,all=T,by="PIN")
# Renaming
tryHarder<-plyr::rename(tryHarder, replace=c("Q4.3_2_GROUP"="Health"))
# Adding MI INterpersonal
tryHardest<-merge(tryHarder,interMI,all=T,by="PIN")
# Renaming
tryHardest<-plyr::rename(tryHardest, replace=c("Q4.3_3_GROUP"="Interpersonal"))
# Adding MI Leisure
tryHardester<-merge(tryHardest,leisureMI,all=T,by="PIN")
# Renaming
tryHardester<-plyr::rename(tryHardester, replace=c("Q4.3_4_GROUP"="Leisure"))
# Adding MI Intrapersonal
tryHardestest<-merge(tryHardester,intraMI,all=T,by="PIN")
# Renaming
tryHardestest<-plyr::rename(tryHardestest, replace=c("Q4.3_5_GROUP"="Intrapersonal"))
# Adding MI Maintenance
tryHardestester<-merge(tryHardestest,maintainMI,all=T,by="PIN")
# Renaming
tryHardestester<-plyr::rename(tryHardestester, replace=c("Q4.3_6_GROUP"="Maintenance"))
# Converting to Long Format & Removing Missing Values
goalMI<-na.omit(unique(tryHardestester %>% pivot_longer(c(Academic,Occupational,Health,Interpersonal,Leisure,Intrapersonal,Maintenance), 
                                                        names_to = "Goal_Category", values_to = "Goal_Number")))

## Least important
# Splitting Academic Projects
academicsLI<-setDT(data1)[, strsplit(as.character(Q4.4_0_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_0_GROUP)
                          ][,.(Q4.4_0_GROUP = V1, PIN)]
# Splitting Occupational Projects
occupationalLI<-setDT(data1)[, strsplit(as.character(Q4.4_1_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_1_GROUP)
                             ][,.(Q4.4_1_GROUP = V1, PIN)]
# Splitting Health/Body Projects
healthLI<-setDT(data1)[, strsplit(as.character(Q4.4_2_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_2_GROUP)
                       ][,.(Q4.4_2_GROUP = V1, PIN)]
# Splitting Interpersonal Projects
interLI<-setDT(data1)[, strsplit(as.character(Q4.4_3_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_3_GROUP)
                      ][,.(Q4.4_3_GROUP = V1, PIN)]
# Splitting Leisure Projects
leisureLI<-setDT(data1)[, strsplit(as.character(Q4.4_4_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_4_GROUP)
                        ][,.(Q4.4_4_GROUP = V1, PIN)]
# Adding MI Intrapersonal
intraLI<-setDT(data1)[, strsplit(as.character(Q4.4_5_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_5_GROUP)
                      ][,.(Q4.4_5_GROUP = V1, PIN)]
# Splitting Maintenance Projects
maintainLI<-setDT(data1)[, strsplit(as.character(Q4.4_6_GROUP), ",", fixed=TRUE), by = .(PIN, Q4.4_6_GROUP)
                         ][,.(Q4.4_6_GROUP = V1, PIN)]
# Merging Academmic and Occupational MI Goals
tryHard2<-merge(academicsLI,occupationalLI,all=T,by="PIN")
# Renaming Categories
tryHard2<-plyr::rename(tryHard2, replace=c("Q4.4_0_GROUP"="Academic","Q4.4_1_GROUP"="Occupational"))
# Adding LI Health
tryHarder2<-merge(tryHard2,healthLI,all=T,by="PIN")
# Renaming
tryHarder2<-plyr::rename(tryHarder2, replace=c("Q4.4_2_GROUP"="Health"))
# Adding LI INterpersonal
tryHardest2<-merge(tryHarder2,interLI,all=T,by="PIN")
# Renaming
tryHardest2<-plyr::rename(tryHardest2, replace=c("Q4.4_3_GROUP"="Interpersonal"))
# Adding LI Leisure
tryHardester2<-merge(tryHardest2,leisureLI,all=T,by="PIN")
# Renaming
tryHardester2<-plyr::rename(tryHardester2, replace=c("Q4.4_4_GROUP"="Leisure"))
# Adding LI Intrapersonal
tryHardestest2<-merge(tryHardester2,intraLI,all=T,by="PIN")
# Renaming
tryHardestest2<-plyr::rename(tryHardestest2, replace=c("Q4.4_5_GROUP"="Intrapersonal"))
# Adding LI Maintenance
tryHardestester2<-merge(tryHardestest2,maintainLI,all=T,by="PIN")
# Renaming
tryHardestester2<-plyr::rename(tryHardestester2, replace=c("Q4.4_6_GROUP"="Maintenance"))
# Converting to Long Format & Removing Missing Values
goalLI<-na.omit(unique(tryHardestester2 %>% pivot_longer(c(Academic,Occupational,Health,Interpersonal,Leisure,Intrapersonal,Maintenance), 
                                                        names_to = "Goal_Category", values_to = "Goal_Number")))

# Merging Most & Least Important Goals
goalData2<-bind_rows(goalMI,goalLI)
# Converting to Long Format for Merging
goalData<-goalData %>% pivot_longer(c(Goal1,Goal2,Goal3,Goal4,Goal5,Goal6), 
                                   names_to = "Goal", values_to = "Goal_Number")
# Merging Goal Tibbles
finalGoal<-full_join(goalData,goalData2,by=c("PIN","Goal_Number"))

# Creating Final Dataframe
dataFinal<-full_join(data3,finalGoal,by=c("PIN","Goal"))

#write.csv(dataFinal, file="finalData.csv")

####################################################################################################################################################
####################################################################################################################################################
###############################################################  Analyses & Graphing  ##############################################################
####################################################################################################################################################
####################################################################################################################################################
####################
### Working Data ###
####################

rm(list=ls())
# Set working directory
setwd("C:/Users/tcarse2/OneDrive/Studies/Trust/Dissertation/Data/Working Data/")

# Load libraries
library(psych)
library(lme4)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(data.table)
library(gmodels)
library(texreg)

#################
### Load data ###
#################
dataFinal<-read.csv("finalData.csv",header=T,na.strings = c(""))
str(dataFinal)

# Converting Variables into Proper Format
dataFinal$PIN<-as.factor(dataFinal$PIN)
dataFinal$Goal<-as.factor(dataFinal$Goal)
dataFinal$Goal_Category<-as.factor(dataFinal$Goal_Category)
dataFinal$Trust<-(as.numeric(dataFinal$Trust)-1)
dataFinal$Importance<-as.numeric(dataFinal$Importance)
dataFinal$Rely<-as.numeric(dataFinal$Rely)
dataFinal$StRel<-as.numeric(dataFinal$StRel)
dataFinal$WeakRel<-as.numeric(dataFinal$WeakRel)
dataFinal$SITStrust<-as.numeric(dataFinal$SITStrust)
dataFinal$SITSemo<-as.numeric(dataFinal$SITSemo)
dataFinal$SITSrely<-as.numeric(dataFinal$SITSrely)
dataFinal$ABIable<-as.numeric(dataFinal$ABIable)
dataFinal$ABIbenev<-as.numeric(dataFinal$ABIbenev)
dataFinal$ABItegridy<-as.numeric(dataFinal$ABItegridy)

#dataFinal<-apply(dataFinal[4:15],2,as.numeric)
###########################
### Centering IVs by Ss ###
###########################
# Relevance to Strength
dataFinal<-ddply(dataFinal,.(PIN), plyr::mutate, rStrMean = mean(StRel))
dataFinal$StrRelC<-dataFinal$StRel-dataFinal$rStrMean
dataFinal$StrRel_GlobC <- scale(dataFinal$StRel, scale = FALSE)[,]

# Relevance to Weakness
dataFinal<-ddply(dataFinal,.(PIN), plyr::mutate, rWeakMean = mean(WeakRel))
dataFinal$WeakRelC<-dataFinal$WeakRel-dataFinal$rWeakMean
dataFinal$WeakRel_GlobC <- scale(dataFinal$WeakRel, scale = FALSE)[,]

# Importance
dataFinal<-ddply(dataFinal,.(PIN), plyr::mutate, ImpC = mean(Importance))
dataFinal$ImportanceC<-dataFinal$Importance-dataFinal$ImpC
dataFinal$Imp_GlobC <- scale(dataFinal$Importance, scale = FALSE)[,]

#############################
### Checking Correlations ###
#############################
# Attributes
cor.test(dataFinal$StrRelC,dataFinal$WeakRelC, use = "pairwise.complete.obs")
# 0.333

# DVs
cor.test(dataFinal$Trust,dataFinal$Rely, use = "pairwise.complete.obs")
# 0.716

# Creating composite criterion
dataFinal$trustRely<-(dataFinal$Trust+dataFinal$Rely)/2


################
### Graphing ###
################
###################
### Crude Hists ###
hist(dataFinal$Trust)
hist(dataFinal$Importance)
hist(dataFinal$WeakRel)
hist(dataFinal$StRel)
hist(dataFinal$SITStrust)
hist(dataFinal$SITSemo)
hist(dataFinal$SITSrely)
hist(dataFinal$ABIable)
hist(dataFinal$ABIbenev)
hist(dataFinal$ABItegridy)

######################################
### Importance Manipulation Check ###
######################################
# Graphing Trust by Importance
ImpTrust <-ggplot(data = dataFinal, 
  aes(x = ImportanceC, y=trustRely))+
  geom_point(aes(colour = PIN))+
  geom_smooth(method="lm", se = F, aes(group = PIN))+# we add group level
  xlab("Importance")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  #xlim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "none",
        legend.title = element_blank())
ImpTrust

#############################
### Relevance to Weakness ###
#############################
# Graphing by Ss with RelWeak
relWeakP <-ggplot(data = dataFinal, 
  aes(x = WeakRelC, y=trustRely))+ 
  geom_point(aes(colour = PIN))+
  geom_smooth(method = "lm", se = F, aes(group = PIN))+ # we add group level
  xlab("Relevance to Weakness")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "none",
        legend.title = element_blank())
relWeakP

# Graphing by Importance with RelWeak
relWeakImp <-ggplot(data = dataFinal, 
  aes(x = WeakRelC, y=trustRely))+ 
  geom_smooth(method = "lm", se = T, aes(group = impCat, colour=impCat))+# we add group level
  xlab("Relevance to Weakness")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
relWeakImp

# Graphing by Importance with RelWeak
relWeak <-ggplot(data = dataFinal, 
  aes(x = WeakRelC, y=trustRely))+ 
  geom_smooth(method = "lm", se = T)+
  xlab("Relevance to Weakness")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
relWeak

##############################
### Releveance to Strength ###
##############################
# Graphing by Ss with RelStr
relStrP <-ggplot(data = dataFinal, 
  aes(x = StrRelC, y=trustRely))+ 
  geom_point(aes(colour = PIN))+
  geom_smooth(method = "lm", se = F, aes(group = PIN))+# we add group level
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "none",
        legend.title = element_blank())
relStrP

# Graphing by Importance with Relstr
relStrImp <-ggplot(data = dataFinal, 
  aes(x = StrRelC, y=trustRely))+ 
  geom_smooth(method = "lm", se = T, aes(group = impCat, colour=impCat))+ # add group level
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
relStrImp

# Graphing by Strength
relStr <-ggplot(data = dataFinal, 
  aes(x = StrRelC, y=trustRely))+ 
  geom_smooth(method = "lm", se = T)+
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
relStr

##################
### Importance ###
##################
# Graphing by Importance with Relstr
ProjectImportance <-ggplot(data = dataFinal, 
  aes(x = ImportanceC, y=trustRely))+ 
  geom_smooth(method = "lm", se = T, aes(group = impCat, colour=impCat))+ # add group level
  xlab("Continuous Importance")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
ProjectImportance

###############
### By Goal ###
###############
# Subsetting a random 4 PINs
set.seed(123)
byGoalSample<- sample(dataFinal$PIN,4)
byGoalSample2<-dataFinal[dataFinal$PIN %in% byGoalSample,]

# Graphing by Ss with RelStr
byGoal <-ggplot(data = byGoalSample2, 
  aes(x = Goal, y=trustRely))+
  facet_grid(vars(PIN))+
  geom_point(aes(colour = PIN))+
  geom_smooth(method = "lm", se = F, aes(group = PIN))+# we add group level
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme(legend.position = "none")
byGoal

byGoal2 <-ggplot(data = byGoalSample2, 
  aes(x = Goal, y=trustRely))+
  facet_grid(vars(PIN))+
  geom_point(aes(colour = PIN))+
  geom_line(aes(colour = PIN,group = 1))+
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme(legend.position = "none")
byGoal2

# This is in the dissertation manuscript
byGoal3 <-ggplot(data = byGoalSample2, 
  aes(x = Goal, y=trustRely))+
  facet_grid(vars(PIN))+
  geom_point(aes(colour = PIN))+
  geom_line(aes(colour = PIN,group = 1))+
  xlab("Relevance to Strength")+ylab("Trust")+ # add labels
  ylim(-4, 4)+
  theme_bw()+
  theme(panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.position = "top",
        legend.title = element_blank())
byGoal3

################
### Analyses ###
################
####################
### Descriptives ###
####################
describe(dataFinal$numGoals)
#  n    mean    sd   median  trimmed  mad  min max  range skew  kurtosis  se
# 2406  13.96  4.95     13    13.53  4.45   6   25    19  0.64   -0.32   0.1

########################
### Random Structure ###
Null.Model<-lmer(trustRely ~ 1 + (1+impCat|PIN), 
                 data=dataFinal, REML=F)
summary(Null.Model)
#ICC.Model(Null.Model)

null.PCA<-rePCA(Null.Model)
summary(null.PCA)
# -1 cor between intercept and slope
# Intercept accounts for all the var
# Slope should be dropped

##########################
### Manipulation check ###
# Checking whether the Most Important Goals are rated as more important than the Least Important Goals
T1<-lmer(Importance~impCat
           + (1+impCat|PIN), data=dataFinal, REML=F)
summary(T1)
# Model failed to converge so reducing random structure
T1.1<-lmer(Importance~impCat
         + (1+impCat||PIN), data=dataFinal, REML=F)
summary(T1.1)
# Model failed to converge so reducing random structure
T1.2<-lmer(Importance~impCat
           + (0+impCat|PIN), data=dataFinal, REML=F)
summary(T1.2)
# Model Converged
# This is a cchange and suggests that T1.2 is the best random structure
T1.3<-lmer(Importance~impCat
           + (1|PIN), data=dataFinal, REML=F)
summary(T1.3)
anova(T1.2,T1.3)
# The manipulation worked
# The Most Important Goals are rated more importantly

# Checking whether Goal Importance varies across categories
# Setting reference as Academic (highest trust)
dataFinal <- within(dataFinal, Goal_Category <- relevel(Goal_Category, ref = "Academic"))
T2<-lmer(Importance~Goal_Category
           + (1+impCat|PIN), data=dataFinal, REML=F)
summary(T2)
# Using same random structure as above
T2.1<-lmer(Importance~Goal_Category
         + (0+impCat|PIN), data=dataFinal, REML=F)
summary(T2.1)
# Only Leisure and Maintenance rated as less important than Academic goals
# Using reduced Random Structure
T2.1<-lmer(Importance~Goal_Category
           + (1|PIN), data=dataFinal, REML=F)
summary(T2.1)
# Printing Model Results for Dissertation
htmlreg(list(T2.1),file = "ImpByCat", 
        single.row = FALSE, stars = c(0.001, 0.01, 0.05,0.1),digits=3,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)

# Setting reference as Occupational (highest trust)
dataFinal <- within(dataFinal, Goal_Category <- relevel(Goal_Category, ref = "Interpersonal"))
# Looking at potential differences
T2.2<-lmer(Importance~Goal_Category
           + (0+impCat|PIN), data=dataFinal, REML=F)
summary(T2.2)


# Checking whether Trust varies across Goal Categories
T3<-lmer(trustRely~Goal_Category
           + (1+impCat|PIN), data=dataFinal, REML=F)
summary(T3)
# Using same random structure
T3.1<-lmer(trustRely~Goal_Category
         + (0+impCat|PIN), data=dataFinal, REML=F)
summary(T3.1)
# Using reduced random structure
T3.2<-lmer(trustRely~Goal_Category
           + (1|PIN), data=dataFinal, REML=F)
summary(T3.2)
# Friends are trusted less for Leisure Projects
# Printing Model Results for Dissertation
htmlreg(list(T3.1),file = "TrustByCat", 
        single.row = FALSE, stars = c(0.001, 0.01, 0.05,0.1),digits=3,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)

# Setting reference as Occupational (highest trust)
dataFinal <- within(dataFinal, Goal_Category <- relevel(Goal_Category, ref = "Occupational"))
T4<-lmer(Trust~Goal_Category
         + (1+impCat|PIN), data=dataFinal, REML=F)
summary(T4)
# Using same random structure
T4.1<-lmer(Trust~Goal_Category
         + (0+impCat|PIN), data=dataFinal, REML=F)
summary(T4.1)
# Using reduced random structure
T4.2<-lmer(Trust~Goal_Category
           + (1|PIN), data=dataFinal, REML=F)
summary(T4.2)
# Friends are trusted less for Leisure than for occupational

################################
### Primary without controls ###
omgplzwork<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat
                    + (1+impCat|PIN), data=dataFinal, REML=F)
summary(omgplzwork) # Correlation between intercept & slope: -1

# Blocking correlation
primaryNoCor<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat
                    + (1+impCat||PIN), data=dataFinal, REML=F)
summary(primaryNoCor) # Correlation between intercept & slope: 1

# Removing intercept
primaryNoInt<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat
                    + (0+impCat||PIN), data=dataFinal, REML=F)
summary(primaryNoInt) # Barely any effect on residual

# Removing slope
primaryNoSlope<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat
                    + (1|PIN), data=dataFinal, REML=F)
summary(primaryNoSlope) # Small increase in the residual

#############################
### Primary with controls ###
omgplzwork2<-lmer(trustRely ~ SITStrust + SITSemo + SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat
                  + (1+impCat|PIN), data=dataFinal, REML=F)
summary(omgplzwork2)

# Blocking correlation
omgplzwork3<-lmer(trustRely ~ SITStrust + SITSemo + SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat
                  + (1+impCat||PIN), data=dataFinal, REML=F)
# Singular fit but results are not much different
summary(omgplzwork3)

# Removing Slope
omgplzwork4<-lmer(trustRely ~ SITStrust + SITSemo + SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork4)

# Adding Intercept of Relevance to strength/weakness
omgplzwork5<-lmer(trustRely ~ SITStrust + SITSemo + SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork5)
# Categorical measure of Importance is now non-sig
# Slope btw RelStrength & Trust increased
# RelStrength intercept is sig
# Variance attributed to intercept decreased


########################################
### Removing Nonpredictive variables ###
########################################
# Removing SITS one at a time
omgplzwork6<-lmer(trustRely ~ SITSemo + SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork6)
# Slight REDUCTION in the residual and INCREASE in Intercept
# Checking model fit
anova(omgplzwork5,omgplzwork6)
# No difference

# Removing SITS one at a time
omgplzwork7<-lmer(trustRely ~ SITSrely 
                  + ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork7)
# Slight REDUCTION in the residual and INCREASE in Intercept
# Checking model fit
anova(omgplzwork6,omgplzwork7)
# No difference

# Removing SITS one at a time
omgplzwork8<-lmer(trustRely ~ ABIable + ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork8)
# Slight INCREASE in Intercept and no effect on residual
# Slight decrease in predictors
# Checking model fit
anova(omgplzwork7,omgplzwork8)
# No difference

# Removing ABI one at a time
omgplzwork9<-lmer(trustRely ~ ABIbenev + ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork9)
# Slight INCREASE in Intercept and no effect on residual
# Mixed effect on predictors
# Checking model fit
anova(omgplzwork8,omgplzwork9)
# No difference

# Removing ABI one at a time
omgplzwork10<-lmer(trustRely ~ ABItegridy
                  + WeakRelC*impCat + StrRelC*impCat 
                  + rStrMean*impCat + rWeakMean*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork10)
# Slight INCREASE in Intercept and residual
# Mixed effect on predictors
# Checking model fit
anova(omgplzwork9,omgplzwork10)
# No difference

# Removing ABI one at a time
omgplzwork11<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat 
                   + rStrMean*impCat + rWeakMean*impCat
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork11)
# Slight INCREASE in Intercept and DECREASE in residual
# Mixed effect on predictors
# Checking model fit
anova(omgplzwork10,omgplzwork11)
# No difference

# Removing interaction between weak and imp
omgplzwork11<-lmer(trustRely ~ WeakRelC + StrRelC*impCat 
                   + rStrMean*impCat + rWeakMean*impCat
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork11)
# Slight DECREASE in Intercept and residual
# Slight decrease in predictors
# Checking model fit
anova(omgplzwork10,omgplzwork11)
# No difference

# Removing weakRel
omgplzwork12<-lmer(trustRely ~ StrRelC*impCat 
                   + rStrMean*impCat + rWeakMean*impCat
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork12)
# Slight INCREASE in Intercept and DECREASE in residual
# INCREASE in all predictors
# Checking model fit
anova(omgplzwork11,omgplzwork12)
# No difference

# Removing weakMean and adding back WeakRel
omgplzwork13<-lmer(trustRely ~ WeakRelC + StrRelC*impCat 
                   + rStrMean*impCat 
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork13)
# Slight INCREASE in residual and DECREASE in intercept
# Mixed effect on predictors
# Importance is now significant
# Checking model fit
anova(omgplzwork11,omgplzwork13)
# Marginal difference

# Removing interaction between importance and relStrength
omgplzwork14<-lmer(trustRely ~ WeakRelC + StrRelC+impCat 
                   + rStrMean*impCat 
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork14)
# Slight INCREASE in residual and DECREASE in intercept
# Mixed effect on predictors -- Big increase for relStrength
# Importance remains significant
# Checking model fit
anova(omgplzwork13,omgplzwork14)
# No difference

# Removing interaction between importance and relStrengthMean and returning interaction
omgplzwork15<-lmer(trustRely ~ WeakRelC + StrRelC*impCat 
                   + rStrMean
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork15)
# Slight decrease in residual and intercept
# Mixed effect on all predictors
# Large increase in Importance and Intercept
# Checking model fit
anova(omgplzwork13,omgplzwork15)
# No difference
#####################################
### Model 15 is the correct model ###
#####################################

# Adding random slope
omgplzwork16<-lmer(trustRely ~ WeakRelC + StrRelC*impCat 
                   + rStrMean
                   + (1+impCat|PIN), data=dataFinal, REML=F)
summary(omgplzwork16)
# Slight decrease in intercept and residual
# However, the cor between intercept and slope is -1.00 so the answer cannot be the model that does not work
# However very little changed 
# Checking for 
isSingular(omgplzwork16)
# It is a singular fit

# Checking interaction between relevance to strength intercept and mean
omgplzwork17<-lmer(trustRely ~ WeakRelC + StrRelC*impCat 
                   + rStrMean*StrRelC
                   + (1|PIN), data=dataFinal, REML=F)
summary(omgplzwork17)
# The intercept is not significant
anova(omgplzwork13,omgplzwork15)
# No difference

###############################
### Models for Dissertation ###
###############################
MainEffects<-lmer(trustRely ~ WeakRelC + StrRelC + impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(MainEffects)
# Only WeakRel is not significant
## Adding interaction between Strength and Importance
StrengthByImportance<-lmer(trustRely ~ WeakRelC + StrRelC*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(StrengthByImportance)
# The interaction is negative but not significant
anova(MainEffects,StrengthByImportance)
# Not a significant improvement in model fit
## Adding interaction between Weakness and Importance
weaknessByImportance<-lmer(trustRely ~ WeakRelC*impCat + StrRelC
                           + (1|PIN), data=dataFinal, REML=F)
summary(weaknessByImportance)
# The interaction is not significant
anova(MainEffects,weaknessByImportance)
# Not a significant improvement in model fit
## Adding both interactions for completeness
FullModel<-lmer(trustRely ~ WeakRelC*impCat + StrRelC*impCat
                           + (1|PIN), data=dataFinal, REML=F)
summary(FullModel)
# Neither interaction is significant
anova(StrengthByImportance,FullModel)
anova(weaknessByImportance,FullModel)
# Not a significant improvement in model fit over either singular interaction models

# Printing Model Results for Dissertation
htmlreg(list(MainEffects,StrengthByImportance,weaknessByImportance,FullModel),file = "ModelResults.doc", 
        single.row = FALSE, stars = c(0.001, 0.01, 0.05,0.1),digits=3,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)
################################
### Global Centered Analyses ###
################################
GC_Min<-lmer(trustRely ~ + WeakRel_GlobC*impCat + StrRel_GlobC*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(GC_Min)
# Relevance to strength is still predictive of trust at the global level


############################
### Exploratory Analyses ###
############################
################################################
### Looking at goal categories by importance ###
goalCatImp<-lmer(trustRely ~ Goal_Category*impCat
                  + (1|PIN), data=dataFinal, REML=F)
summary(goalCatImp)
# Whenever goalCat and importance are interacted, all effects disappear
# I know that importance varies across category
# This may suggest that category and importance are so confounded that I cannot tease them apart

# Using 2 difference crosstabs
# Crosstab 1
catImp <- xtabs(~Goal_Category+impCat, data=dataFinal)
ftable(catImp) # print table
summary(catImp) # View chi-square

# Crosstab 2
catImp2<-CrossTable(dataFinal$Goal_Category,dataFinal$impCat, expected = T)

CrossTable(dataFinal$Importance,dataFinal$impCat, expected = T)

##############################################
### Looking at Trust by Number of Projects ###
numgoalTrust<-lmer(trustRely ~ numGoals
                 + (1|PIN), data=dataFinal, REML=F)
summary(numgoalTrust)
# Trending toward lower trust - Does it predict trust variability?

# Creating a measure of trust variability
dataWerk<-dataFinal %>%
  pivot_wider(names_from = Goal,
              values_from = trustRely) %>%
  group_by(PIN) %>% 
  summarise_all(list(~na.omit(.)[1])) %>%
  rowwise() %>%
  mutate(trustVar = var(c(Goal1,Goal2,Goal3,Goal4,Goal5,Goal6))) %>%
  pivot_longer(cols = c(Goal1:Goal6),
               names_to = "Goal",
               values_to = "trustRely")

# Centering number of goals
dataWerk$numGoalsC <- scale(dataWerk$numGoals, scale = FALSE)[,]

# Using trust variability as the criterion
numgoalTrustVar<-lm(trustVar ~ numGoalsC, data=dataWerk)
summary(numgoalTrustVar)
cor.test(dataWerk$trustVar,dataWerk$numGoals)


