#Assignment 3, Empirical Methods I

#Students' names: Melcherson, Johan & Rykatkin, Oliver
#E-mails: jomelch@outlook.com, oliver.rykatkin@gmail.com
#Name of script: EmpiricalMethodsI_Assignment3 
#Data and packages: package(Ecdat), package(dplyr),
#package(ggplot2), package(BSDA), data(lnu91.txt)


#1. A random variable is a variable for which the
#observations relies on some random component. The typical
#random variable is the outcome of a dice throw. In this
#case, the outcome only depends on the random component.
#Another random variable may be the age of a university
#student. In this case, the random component is not
#comprehensive because we may find other variables that
#explains some part of the variation in the variable. The
#sample mean and the sample standard deviation for any
#random variable are also random variables because they will
#most likely vary from one sample to the other.


#2. The normal distribution is a symmetric distribution
#shaped as a bell in the middle of two tails. The single
#bell represents the unimodality of the distribution, and
#for the characteristic that both tails approach the same
#y-value as the x-value goes to plus or minus infinity, we
#say that the distribution is asymptotic to the axes. For a
#random variable to have a normal probability density
#function, it must also be continuous. The outcome of a dice
#throw is a discrete variable with a normal distribution,
#but the density function will have kinks due to the
#discrete nature of the variable. Another characteristic is
#related to the standard deviation of the distribution. For
#any normally distributed probability density function, ~68
#% of the observations will be within one standard deviation
#from the mean, ~95 % within two standard deviations from
#the mean and ~99,7 % within three standard deviations from
#the mean.


#3. The standard normal distribution has a mean of 0 and a
#standard deviation of 1. This can be represented by the
#function N(0,1).


#4. The Central Limit Theorem (CLT) states that the
#distribution of the sample means will be normally
#distributed if the samples has at least 40 observations.
#Thus, the main point with the CLT is that we can use test
#statistics based on a normal distribution even if the
#sampled variable is in itself not normally distributed.
#That is, the theorem is applied in hypothesis testing in
#that the distribution of the estimated sample statistic
#will cluster around a mean value. This implies that we can
#increase the precision of our estimates of population
#variables by increasing the number of samples.


#5. Estimate the covariance and the correlation coefficients
#between wages and years of education.

library(dplyr) #using package:dplyr

#For male and female
lnu91 %>%
  group_by(gender) %>%
  summarize(covariance = cov(wage, school), correlation = cor(wage, school))

#For public and private sector
lnu91 %>%
  group_by(sector) %>%
  summarize(covariance = cov(wage, school), correlation = cor(wage, school))

#The covariance is higher for men than for women. This
#implies that education is more profitable for men in a
#sense that their wages increase more with more schooling.
#Depending on the sector you work in, it is actually the
#case that more schooling will improve your wage more if you
#aim for a careeer in the public sector than in the private
#sector. The difference between sectors is, however, quite
#insignificant. The correlation coefficient, in turn, tells
#us that overall the strongest correlation is within the
#public sector.


#6. Standardize the wage variable for men and women
#separately, plot the variables and comment on the
#differences.

library(dplyr) #Using package:dplyr

wages_standard <- #Creating new data frame including the standardized variable
  lnu91 %>%
  group_by(gender) %>% #Scaling is done for men and women separately
  mutate(wage_s = scale(wage)) #Scale function standardizes the variable

  #Plotting the standardized variable
  ggplot(wages_standard, aes(x = wage_s, fill = gender)) + 
  geom_histogram(alpha = 1, position = "identity", breaks=seq(-5, 5, by=0.15)) +
  facet_grid(vars(gender))
  
#We see that womens' wages are more centered around the
#mean, and that more men than women have wages that are more
#than three standard deviations above the mean, that is, are
#in the 99,7th percentile.

  
#7. Estimate a 95 % confidence interval for wages of men and
#women and interpret the intervals.
  
library(dplyr) #using package:dplyr

wages_standard %>%
  group_by(gender) %>%
  summarize(mean.wage_s = mean(wage_s),
            sd.wage_s = sd(wage_s),
            n.wage_s = n()) %>%
  mutate(se.wage_s = sd.wage_s / sqrt(n.wage_s), #Computing standard errors
           lower.ci = mean.wage_s - qnorm(0.975) * se.wage_s, #qnorm gives us the z-values
           upper.ci = mean.wage_s + qnorm(0.975) * se.wage_s,
           ci_diff = upper.ci - lower.ci) #Computing the range of the ci

#The interval has a smaller range for women than for men,
#which is the case because wages for women are more
#clustered around the mean. The intervals tell us that with
#95 % certainty the true population means of the groups lie
#in the range between the upper and lower bound of the
#interval. As we have very large samples (n>1000), the
#intervals become remarkably thin.


#8. Test the hypothesis that average wages of men and women
#are equal. Compare the results with the confidence
#intervals.

library(BSDA) #using package:BSDA

z.test(wages_standard$wage_s[which(wages_standard$female=="1")], #Sorting out women for the first sample
      wages_standard$wage_s[which(wages_standard$female=="0")], #Sorting out men for the second sample
      alternative ="two.sided", mu=0, sigma.x=1, sigma.y=1,
      conf.level = 0.95)

#We have used the z-test to compare population means for our
#two standardized normally distributed variables. When we
#only compared confidence intervals, the intervals was
#clearly overlapping and it was hard to judge the
#significance of the difference in mean wages between the
#two groups. With a z-test, we can easily confirm that wages
#between men and women are significantly different with a
#remarkably low p-value.