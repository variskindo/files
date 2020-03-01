##
## Lab 5
##

## ------------------------------------------------------------------------
head(subset(smp, prof == "sans emploi" | prof == "prof.intermediaire" |
            prof == "cadre", c(age, n.enfant, prof)))


## ------------------------------------------------------------------------
head(subset(smp, prof %in% c("sans emploi", "prof.intermediaire", "cadre"), 
            c(age, n.enfant, prof)))


## ------------------------------------------------------------------------
smpb <- subset(smp, prof %in% c("sans emploi", "prof.intermediaire", "cadre"), 
               c(age, n.enfant, prof))
summary(smpb)


## ------------------------------------------------------------------------
smpb$prof <- factor(smpb$prof, labels=c("cadre", "intermédiaire", 
                                   "sans emploi"))
table(smpb$prof)


## ------------------------------------------------------------------------
aggregate(n.enfant ~ prof, data=smpb, mean)


## ------------------------------------------------------------------------
boxplot(n.enfant ~ prof, data=smpb,
        xlab="Profession", ylab="Nombre d'enfants",
        col="cornflowerblue", border="cornflowerblue")


## ------------------------------------------------------------------------
## boxplot(n.enfant ~ prof, data=smpb, xlab="Profession", ylab="Nombre d'enfants",
##         col="cornflowerblue", border="cornflowerblue",
##         pars=list(medcol="white", whiskcol="cornflowerblue",
##             staplecol="cornflowerblue", whisklty=1), lwd=1.5)


## ------------------------------------------------------------------------
m <- lm(n.enfant ~ prof, data=smp, 
        subset=prof %in% c("sans emploi", "prof.intermediaire", "cadre"))
m


## ------------------------------------------------------------------------
drop1(m, test="F")


## ------------------------------------------------------------------------
anova(m)


## ------------------------------------------------------------------------
m <- lm(n.enfant ~ age, data=smp)
summary(m)


## ------------------------------------------------------------------------
coef(m)
coef(m)[2]
coef(m)["age"]


## ------------------------------------------------------------------------
confint(m)


## ------------------------------------------------------------------------
anova(m)


## ------------------------------------------------------------------------
head(fitted(m))
head(resid(m))


## ------------------------------------------------------------------------
smp$n.enfant[1:2] - fitted(m)[1:2]


## ------------------------------------------------------------------------
predict(m, data.frame(age=c(20, 30, 40)), interval="confidence")


## ------------------------------------------------------------------------
smp$n.enfant.bin <- factor(ifelse(smp$n.enfant > 2, 1, 0))
table(smp$n.enfant.bin)


## ------------------------------------------------------------------------
m <- glm(n.enfant.bin ~ age, data=smp, family=binomial("logit"))
summary(m)


## ------------------------------------------------------------------------
coef(m)
coef(m)[2]
exp(coef(m)[2])


## ------------------------------------------------------------------------
exp(5*coef(m)[2])


## ------------------------------------------------------------------------
head(predict(m))
head(predict(m, type="response"))
