rm(list=ls())
dat=read.csv("50_Startups.csv",head=T)

Size = numeric(nrow(dat))
Size[dat$Profit>=100000]=0
Size[dat$Profit<100000]=1


Admin = dat$Administration
Mark = dat$Marketing.Spend
states = numeric(nrow(dat))
states[dat$State=="California"]=1


fit1=glm(Size~Admin+Mark+factor(states),data=dat,family="binomial")
summary(fit1)










#============================
# Residual analysis
#============================

pred1=predict(fit1) # gives log-odds of hat(p_i)
pred2=predict(fit1,type="response") # gives hat(p_i)


res1=residuals(fit1,type="deviance")
res2=residuals(fit1,type="pearson")

# Multicollinearity
car::vif(fit1)


# diagnostics
# x- outliers
barplot(hatvalues(fit1))

# linearity
plot(Admin,pred1)
plot(Mark,pred1)


#=============================
### Variable Selection
#============================

fit1=glm(Size~Admin+Mark+Admin*Mark+states,family="binomial")

# automatic variable selection
null = glm(Size~1)
full = fit1

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = glm(aicmodel$terms,family="binomial")
summary(finalaic)


#=============================
### Confusion matrix
#============================

pred_prob=predict(finalaic,type="response") # gives hat(p_i)

yhat = numeric(length(pred_prob))
yhat[pred_prob>=0.75]=1
table(Size, yhat)


library(funModeling)
dat$Size = Size
dat$pred_prob = pred_prob
ff=gain_lift(data=dat,score="pred_prob",target="Size")



library(pROC)
rr=roc(Size,pred_prob)
plot(rr)
auc(rr)
