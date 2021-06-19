rm(list=ls())
houses<-read.csv(file.choose())
fix(houses)
dim(houses)
names(houses)
# Now define variables as nominal or ordinal factors
# because Gower distance estimation needs to know
# exactly which class each is so it can handle it appropriately
# Start with house number and check in the right class afterwards
houses$HouseNumber<-factor(houses$HouseNumber)
class(houses$HouseNumber)
# Excellent, so now do for the remainder
# and check the data frame structure afterward
houses$Location<-factor(houses$Location)
houses$HouseDescription<-factor(houses$HouseDescription)
houses$SurroundingHousingDensity<-ordered(houses$SurroundingHousingDensity, levels=c("Low","Medium","High"))
houses$HouseholdHead<-factor(houses$HouseholdHead)
houses$RoofMaterial<-factor(houses$RoofMaterial)
houses$WallMaterial<-factor(houses$WallMaterial)
houses$WindowSpace<-ordered(houses$WindowSpace, levels=c("1","2","3"))
houses$OtherVentilationGapsNotes<-factor(houses$OtherVentilationGapsNotes)
str(houses)
# Looks great, so now go on to cluster analysis
library(cluster)
# Need to exclude irrelevant variables from the
# distance matrix so deselect them using subset function
gowerhouses<-daisy(subset(houses,
                          select=-c(HouseNumber,
                          HouseDescription,
                          HouseholdHead)),
                   metric = "gower")
summary(gowerhouses) 
# Note that N=nominal, O=Ordinal and I=Interval (categorical)
# So all looks right wit types listed as N,N,N,O,I,I etc
# So now figure out how many clusters optimal 
# with silhouette method
sil_width <- c(NA)
for(i in 2:10){
    pam_fit <- pam(gowerhouses,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  }
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
# Conclude that 2 clusters (k=i=2) is optimal
# So now actually do the cluster analysis
pam_fit <- pam(gowerhouses, diss = TRUE, k = 2)

# See what the outputs are from that cluster analysis
print(pam_fit)
# Now examine the most important of those results
pam_fit$medoids
# OK, the representative exemplar medoids of the two
# clusters are houses 9 and 39, which seem to differ only in their window areas
pam_fit$clustering
pam_fit$clusinfo
housesclustered <- cbind(houses, cluster = pam_fit$clustering)
head(housesclustered, n = 10)

library(dplyr)
pam_results <- houses %>%
  dplyr::select(-HouseNumber) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
# Not obvious from an initial scan that these
# clusters are very different so try to visualize
# with the Rtsne package recommended for Gower dissimilarities

# Note that for such a small dataset, the default
# perplexity of 30 is too high to fit, 
# and the plots are slightly different but 
# clearly clustered each time with perplexity of 
# 10 or 15 but not always at perplexity of 5
library(Rtsne)
tsne_obj <- Rtsne(gowerhouses, is_distance = TRUE,
                  perplexity=15)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = housesclustered$name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# Seem to clearly differ only in terms of complete 
# presence (cluster 2) or absence (cluster 1)
# of other ventilation openings: 12 above doors 
# and 1 with four built into the brickwork in the eave
# between two overlapping roofs of an extended house

# Cluster 1 also have more completely open window spaces
# plus lower window area and window gap area
# despite same median number of windows
# So look like clusters of well versus poorly ventilated
# Note also that there was only one well-ventilated house
# with a thatched roof and none with mud walls

# Story with open eave interactions with window structures
# doesn't seem to hold in this subset of houses chosen
# because they were open to mosquitoes-although the
# median length of open eaves is zero in group 2, 
# the mean is higher than for group 1

# SO NOW LOOK AT HISTOGRAMS/BAR CHARTS
# FOR ALL THE VARIABLES IN THIS CLUSTER ANALYSIS
# WITH DIFFERENT COLOURED BARS FOR THE TWO CLUSTERS

table(housesclustered$EaveGapsTotalM,housesclustered$cluster)
# need to define cluster as a factor for Wilcoxon test
housesclustered$cluster<-factor(housesclustered$cluster)
class(housesclustered$cluster)
# Now Wilcoxon Mann-Whitney U test with continuous
# correction switched off because of small sample 
wilcox.test(housesclustered$EaveGapsTotalM ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# No significant differences between length of open
# eaves between the clusters
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = EaveGapsTotalM, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 20) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = EaveGapsTotalM, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 20) + theme_classic()
# Not obviously very different in terms of open eave gaps
# Which fits with Mann-Whitney results

# So now Location, for which Chi square is appropriate
chisqLocationCluster <- chisq.test(cbind(housesclustered$Location,housesclustered$cluster))
chisqLocationCluster
# No significant differences in distribution of
# clusters across locations
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = Location, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = Location, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Not obviously different but revisit to make proper
# plots with percentage per cluster

# So now RoofMaterial, for which Chi square is appropriate
chisqRoofMaterialCluster <- chisq.test(cbind(housesclustered$RoofMaterial,housesclustered$cluster))
chisqRoofMaterialCluster
# No significant differences
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = RoofMaterial, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = RoofMaterial, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Not obviously different but revisit to make proper
# plots with percentage per cluster

# So now WallMaterial, for which Chi square is appropriate
chisqWallMaterialCluster <- chisq.test(cbind(housesclustered$WallMaterial,housesclustered$cluster))
chisqWallMaterialCluster
# No significant differences
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WallMaterial, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = WallMaterial, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Not obviously different but revisit to make proper
# plots with percentage per cluster

# So now SurroundingHousingDensity, for which Chi square is appropriate
chisqWallSurroundingHousingDensity <- chisq.test(cbind(housesclustered$SurroundingHousingDensity,housesclustered$cluster))
chisqWallSurroundingHousingDensity
# No significant differences
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = SurroundingHousingDensity, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = SurroundingHousingDensity, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Not obviously different but revisit to make proper
# plots with percentage per cluster

# So now WindowSpace, for which Chi square is appropriate
chisqWallWindowSpace <- chisq.test(cbind(housesclustered$WindowSpace,housesclustered$cluster))
chisqWallWindowSpace
# No significant differences
# Now plot out in a frequency bar graph
ggplot(housesclustered, aes(x = WindowSpace, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = WindowSpace, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Not obviously different but revisit to make proper
# plots with percentage per cluster

# So now WindowNumber 
wilcox.test(housesclustered$WindowNumber ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# No significant difference
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WindowNumber, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = WindowNumber, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# Not obviously very different in terms of open eave gaps
# Which fits with Mann-Whitney results

# So now WindowAreaAverageM2 
wilcox.test(housesclustered$WindowAreaAverageM2 ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# HIGHLY SIGNIFICANT
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WindowAreaAverageM2, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = WindowAreaAverageM2, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# GROUP 2 CLEARLY HAS A 
# HIGHER DISTRIBUTION OF AVERAGE AREA PER WINDOW
# Which fits with Mann-Whitney results

# So now WindowAreaTotalM2 
wilcox.test(housesclustered$WindowAreaTotalM2 ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# SIGNIFICANT BUT NOT AS MUCH AS FOR THE AVERAGE AREA PER WINDOW
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WindowAreaTotalM2, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = WindowAreaTotalM2, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# GROUP 2 HAS SOMEWHAT HIGHER DISTRIBUTION 
# OF TOTAL AREA OF ALL WINDOWS
# Which fits with Mann-Whitney results

# So now WindowAreaBrickedUpM2 
wilcox.test(housesclustered$WindowAreaBrickedUpM2 ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# SIGNIFICANT BUT NOT AS MUCH AS FOR THE AVERAGE AREA PER WINDOW
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WindowAreaBrickedUpM2, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = WindowAreaBrickedUpM2, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# GROUP 2 HAS SOMEWHAT HIGHER DISTRIBUTION 
# OF BRICKED UP AREA OF ALL WINDOWS
# Which fits with Mann-Whitney results

# So now WindowGapAreaTotalM2 
wilcox.test(housesclustered$WindowGapAreaTotalM2 ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# ONLY DISTANTLY APPROACHES SIGNIFICANCE
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = WindowGapAreaTotalM2, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = WindowGapAreaTotalM2, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# GROUP 2 NO OBVIOUS DIFFERENCE IN DISTRIBUTION 
# OF OPEN AREA OF ALL WINDOWS
# Which fits with Mann-Whitney results

# So now OtherVentilationGapsNumber 
wilcox.test(housesclustered$OtherVentilationGapsNumber ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# HIGHLY SIGNIFICANT
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = OtherVentilationGapsNumber, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = OtherVentilationGapsNumber, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 1) + theme_classic()
# GROUP 2 CLEAR DIFFERENCE IN DISTRIBUTION OF
# OTHER VENTILATION OPENINGS WITH ZERO OVERLAP OF
# TWO CLUSTERS IN TERMS OF NUMBERS
# Which fits with Mann-Whitney results

# So now OtherVentilationGapsNotes, for which Chi square is appropriate
chisqOtherVentilationGapsNotes <- chisq.test(cbind(housesclustered$OtherVentilationGapsNotes,housesclustered$cluster))
chisqOtherVentilationGapsNotes
# Not significant but clearly spurious for some reason to follow up on
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = OtherVentilationGapsNotes, fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# and then tried proportions but failed because
# calculates for both clusters based on the same
# common summed total
ggplot(housesclustered, aes(x = OtherVentilationGapsNotes, y = (..count..)/sum(..count..), fill = factor(cluster)))+
  geom_bar(position = position_dodge()) + theme_classic()
# Nevertheless clearly different because zero overlap in distribution

# So now OtherVentilationGapsAreaM2 
wilcox.test(housesclustered$OtherVentilationGapsAreaM2 ~ housesclustered$cluster, 
            exact=FALSE, correct=FALSE)
# HIGHLY SIGNIFICANT
# Now plot out in a frequency histogram
ggplot(housesclustered, aes(x = OtherVentilationGapsAreaM2, fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 0.1) + theme_classic()
# and then density
ggplot(housesclustered, aes(x = OtherVentilationGapsAreaM2, y=..density.., fill = factor(cluster)))+
  geom_histogram(position = position_dodge(),binwidth = 0.1) + theme_classic()
# CLUSTER 2 CLEARLY DIFFERENT DISTRIBUTION OF
# AREA OTHER VENTILATION OPENINGS WITH ZERO OVERLAP
# OF THE TWO CLUSTERS
# Which fits with Mann-Whitney results

# OVERALL CONCLUSION: THESE ARE TWO SETS OF HOUSES
# WITH DIFFERENT LEVELS OF PLANNED VENTILATION IN
# TERMS OF SUPPLEMENTARY VENTILATION OPENINGS
# ASSOCIATED WITH LARGER WINDOWS, RATHER THAN MORE
# WINDOWS. NOTE ALSO THAT THE AREA OF THE WINDOWS
# CURRENTLY LEFT OPEN DOES NOT DIFFER, DESPITE THE
# CONSIDERABLE DIFFERENCE IN MEAN WINDOW SIZE AND
# TOTAL AREA, THE BRICKED PROPORTION OF WHICH IS ALSO
# ASSOCIATED WITH CLUSTER IN THE SAME WAY