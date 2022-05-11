# Experimentelles Praktikum Elson 2022 
# The Effect of Knowledgem Map and Underlining Training on the Reading Comprehension of Scientific Texts 

#Nullhypothese1:"Die Leistung der Trainingsgruppe bei der Beantwortung der offenen Fragen unterscheidet sich nicht signifikant von denen der Kontrollgruppe."
#Alternativhypothese1:"Die Leistung der Trainingsgruppe bei der Beantwortung der offenen Fragen ist signifikant besser, als bei der Kontrollgruppe."

#Nullhypothese2:"Die Leistung der Trainingsgruppe bei der Zusammenfassung unterscheidet sich nicht signifikant von denen der Kontrollgruppe."
#Alternativhypothese2: "Die Leistung der Trainingsgruppe bei der Zusammenfassung ist signifikant besser, als bei der Kontrollgruppe."


#Unabhängige Variable "underlining": 
#0= nicht unterstreichen
#1= unterstreichen

#Unabhängige Variable "text"
#1= Text1
#2= Text2
#3= Text3

#Abhängige Variable "openquestion"
#Das Textverständins wird durch offene Fragen zum Text überprüft.

#Abhöngige Variable "textsummary"
#Das Textverständis wird  durch eine kurze Zusammenfassung des Textes überprüft.

setwd("~/Studie Textmarkieren/R-Skript")
getwd()


i <- 100

data_studie <- data.frame(id = 1:i,  complexity = rep(c("markieren", "nicht_markieren"), length = i),
                          text = rep(c("text1", "text2", "text3"), length = i),  
                          offene_frage_1 = rep(round(runif(i, min = 0, max = 2))),
                          offene_frage_2 = rep(round(runif(i, min = 0, max = 2))),
                          offene_frage_3 = rep(round(runif(i, min = 0, max = 2))),
                          offene_frage_4 = rep(round(runif(i, min = 0, max = 2))),
                          offene_frage_5 = rep(round(runif(i, min = 0, max = 2))),
                          offene_frage_6 = rep(round(runif(i, min = 0, max = 2))),
                          text_summary = rep(round(runif(i, min = 0, max = 100))))
                          
                          
lin.reg1 <- glm(openquestion ~ underlining + text ,family = gaussian, data = data_studie)
summary(log.reg1)

lin.reg2 <- glm(textsummary ~ underlining + text ,family = gaussian, data = U-Study22)
summary(log.reg2)

library(car)


#### Likelihood-Ratio-Test ####

Anova(lin.reg1)

Anova(lin.reg2)


#### Modellvergleich ####
#Überprüfung, ob beide Modelle zusammen signifikant mehr abbilden, als eines alleine.

anova(lin.reg.1, lin.reg.2, test = "Chisq")


#### Modellgüte und Effektgrößenmaße ####

library(performance)

performance_hosmer(lin.reg1)

performance_hosmer(lin.reg2)

#nicht signifikant = gute Modellgüte


#### Pseudo R2-Maß (macfadden) ####

r2_mcfadden(lin.reg1)

r2_mcfadden(lin.reg2)


#### Grafische Darstellung ####

library(visreg)


#Darstellung als bedingter Logit

visreg(lin.reg1, "openquestion" xlab = "underlining", ylab = "Logit performance", main = "Darstellung als bedingter Logit")

visreg(lin.reg2, "textsummary", xlab = "underlining", ylab = "Logit Performance", main = "Darstellung als bedingter Logit")


#Darstelung als bedingte Wahrscheinlichkeit

visreg(lin.reg1, "openquestion",
       xlab = "underlining",
       ylab = "Wahrschinlichkeit performance",
       main = "Darstellung als bedingte Wahrscheinlichkeit", scale = "response")

visreg(lin.reg2, "textsummary", 
       xlab = "underlining", 
       ylab = "Wahrscheinlichkeit performance",
       main = "Darstellung als bedingte Wahrscheinlichkeit", scale = "response")
