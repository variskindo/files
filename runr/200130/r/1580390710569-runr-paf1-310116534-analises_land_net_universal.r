
library(qpcR) # para calculo de AICc e AIC weights
library(visreg)
library(lme4)
library (bbmle)
library(mctest)
library(scatterplot3d)

#clear workspace
rm(list = ls(all = TRUE))
setwd("C:/Users/PATY/Dropbox/Meus manuscritos/Artigo Universal 2019/Analises_Redes_universal")


#upload data
Data<-read.table("rede_paisagem_universal.txt",header=TRUE, sep="\t")

attach(Data)
summary(Data)
names(Data)

lm.ex<-lm(het_1km~pland_1km)
resid(lm.ex)
plot(lm.ex)

lm.ex1<-lm(het_5km~pland_5km)
resid(lm.ex1)

plot(pland_1km,size)
plot(pland_1km,H2)
plot(pland_1km,connectance)
plot(pland_1km,compartments)
plot(pland_1km,weighted_NODF)
plot(pland_1km,web_asymmetry)
plot(pland_1km,robustness_LL)
plot(pland_1km,robustness_HL)

plot(pland_5km,size)
plot(pland_5km,H2)
plot(pland_5km,connectance)
plot(pland_5km,compartments)
plot(pland_5km,weighted_NODF)
plot(pland_5km,web_asymmetry)
plot(pland_5km,robustness_LL)
plot(pland_5km,robustness_HL)

cor.test(size,connectance)
cor.test(size,H2)
cor.test(size,weighted_NODF)
cor.test(size,robustness_LL)
cor.test(size,robustness_HL)
cor.test(size,compartments)
cor.test(size,web_asymmetry)

cor.test(resid_hetXpland_1km,pland_1km)
cor.test(resid_hetXpland_1km,pland_5km)
cor.test(resid_hetXpland_5km,pland_1km)
cor.test(resid_hetXpland_5km,pland_5km)
cor.test(pland_5km,pland_1km)
cor.test(resid_hetXpland_1km,resid_hetXpland_5km)

#Model_size
nullmodel=(glmer(size~1+(1|landscape),family=poisson,data=Data))
model1=(glmer(size~pland_1km+(1|landscape),family=poisson,data=Data))
model2=(glmer(size~pland_5km+(1|landscape),family=poisson,data=Data))
model3=(glmer(size~het_1km+(1|landscape),family=poisson,data=Data))
model4=(glmer(size~het_5km+(1|landscape),family=poisson,data=Data))

model5=(glmer(size~pland_5km+het_5km+(1|landscape),family=poisson,data=Data))
model6=(glmer(size~pland_1km+het_1km+(1|landscape),family=poisson,data=Data))
model7=(glmer(size~pland_5km+het_1km+(1|landscape),family=poisson,data=Data))
model8=(glmer(size~pland_1km+het_5km+(1|landscape),family=poisson,data=Data))
model9=(glmer(size~pland_5km+pland_1km+(1|landscape),family=poisson,data=Data))
model10=(glmer(size~het_5km+het_1km+(1|landscape),family=poisson,data=Data))

model11=(glmer(size~pland_5km*het_5km+(1|landscape),family=poisson,data=Data))
model12=(glmer(size~pland_1km*het_1km+(1|landscape),family=poisson,data=Data))
model13=(glmer(size~pland_5km*het_1km+(1|landscape),family=poisson,data=Data))
model14=(glmer(size~pland_1km*het_5km+(1|landscape),family=poisson,data=Data))
model15=(glmer(size~pland_5km*pland_1km+(1|landscape),family=poisson,data=Data))
model16=(glmer(size~het_5km*het_1km+(1|landscape),family=poisson,data=Data))

mctest(cbind(pland_1km,het_1km),size, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),size, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),size, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),size, type="i", method="VIF")

AICtab(nullmodel,model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16)

#Scatterplot_3d
grafico3d = function(x,y) {
  u<-exp(1.478743+0.027141 *x+ 1.501131*y-0.016141*x*y)
return(u)}
x1=seq(12.36,96.5,by=6)#pland
y1= seq(0.184,1.668,by=0.1)#het
z<-outer(x1,y1,grafico3d)

persp(x1,y1,z,theta = 30, phi = 15, xlab = "FLORESTA 1KM", ylab = "HETEROGENEIDADE 1KM",zlab = "SIZE",cex.lab=1.0,cex.axis=0.9,nticks = 5,
      ticktype = "detailed")->res

scatterplot3d(pland_1km, het_1km, size,type="h",angle = 30)


#Model_compartments
nullmodela=(glmer(compartments~1+(1|landscape),family=poisson,data=Data))
model1a=(glmer(compartments~pland_1km+(1|landscape),family=poisson,data=Data))
model2a=(glmer(compartments~pland_5km+(1|landscape),family=poisson,data=Data))
model3a=(glmer(compartments~het_1km+(1|landscape),family=poisson,data=Data))
model4a=(glmer(compartments~het_5km+(1|landscape),family=poisson,data=Data))

model5a=(glmer(compartments~pland_5km+het_5km+(1|landscape),family=poisson,data=Data))
model6a=(glmer(compartments~pland_1km+het_1km+(1|landscape),family=poisson,data=Data))
model7a=(glmer(compartments~pland_5km+het_1km+(1|landscape),family=poisson,data=Data))
model8a=(glmer(compartments~pland_1km+het_5km+(1|landscape),family=poisson,data=Data))
model9a=(glmer(compartments~pland_5km+pland_1km+(1|landscape),family=poisson,data=Data))
model10a=(glmer(compartments~het_5km+het_1km+(1|landscape),family=poisson,data=Data))

model11a=(glmer(compartments~pland_5km*het_5km+(1|landscape),family=poisson,data=Data))
model12a=(glmer(compartments~pland_1km*het_1km+(1|landscape),family=poisson,data=Data))
model13a=(glmer(compartments~pland_5km*het_1km+(1|landscape),family=poisson,data=Data))
model14a=(glmer(compartments~pland_1km*het_5km+(1|landscape),family=poisson,data=Data))
model15a=(glmer(compartments~pland_5km*pland_1km+(1|landscape),family=poisson,data=Data))
model16a=(glmer(compartments~het_5km*het_1km+(1|landscape),family=poisson,data=Data))

mctest(cbind(pland_1km,het_1km),compartments, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),compartments, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),compartments, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),compartments, type="i", method="VIF")

AICtab(nullmodela,model1a,model2a,model3a,model4a,model5a,model6a,model7a,model8a,model9a,model10a,model11a,model12a,model13a,model14a,model15a,model16a)

#plot_compartments_model4a
x=seq(1,2,by=0.05)
y=exp(0.1390+0.8357*x)
plot(het_5km,compartments,ylab="Networks Compartments")
points(x,y,type="l")

#Model_connectance (best null)
nullmodelb=(lmer(connectance~1+(1|landscape),data=Data))
model1b=(lmer(connectance~pland_1km+(1|landscape),data=Data))
model2b=(lmer(connectance~pland_5km+(1|landscape),data=Data))
model3b=(lmer(connectance~het_1km+(1|landscape),data=Data))
model4b=(lmer(connectance~het_5km+(1|landscape),data=Data))

model5b=(lmer(connectance~pland_5km+het_5km+(1|landscape),data=Data))
model6b=(lmer(connectance~pland_1km+het_1km+(1|landscape),data=Data))
model7b=(lmer(connectance~pland_5km+het_1km+(1|landscape),data=Data))
model8b=(lmer(connectance~pland_1km+het_5km+(1|landscape),data=Data))
model9b=(lmer(connectance~pland_5km+pland_1km+(1|landscape),data=Data))
model10b=(lmer(connectance~het_5km+het_1km+(1|landscape),data=Data))

model11b=(lmer(connectance~pland_5km*het_5km+(1|landscape),data=Data))
model12b=(lmer(connectance~pland_1km*het_1km+(1|landscape),data=Data))
model13b=(lmer(connectance~pland_5km*het_1km+(1|landscape),data=Data))
model14b=(lmer(connectance~pland_1km*het_5km+(1|landscape),data=Data))
model15b=(lmer(connectance~pland_5km*pland_1km+(1|landscape),data=Data))
model16b=(lmer(connectance~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),connectance, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),connectance, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),connectance, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),connectance, type="i", method="VIF")

AICtab(nullmodelb,model1b,model2b,model3b,model4b,model5b,model6b,model7b,model8b,model9b,model10b,model11b,model12b,model13b,model14b,model15b,model16b)


#Model_H2 (best null)
nullmodelc=(lmer(H2~1+(1|landscape),data=Data))
model1c=(lmer(H2~pland_1km+(1|landscape),data=Data))
model2c=(lmer(H2~pland_5km+(1|landscape),data=Data))
model3c=(lmer(H2~het_1km+(1|landscape),data=Data))
model4c=(lmer(H2~het_5km+(1|landscape),data=Data))

model5c=(lmer(H2~pland_5km+het_5km+(1|landscape),data=Data))
model6c=(lmer(H2~pland_1km+het_1km+(1|landscape),data=Data))
model7c=(lmer(H2~pland_5km+het_1km+(1|landscape),data=Data))
model8c=(lmer(H2~pland_1km+het_5km+(1|landscape),data=Data))
model9c=(lmer(H2~pland_5km+pland_1km+(1|landscape),data=Data))
model10c=(lmer(H2~het_5km+het_1km+(1|landscape),data=Data))

model11c=(lmer(H2~pland_5km*het_5km+(1|landscape),data=Data))
model12c=(lmer(H2~pland_1km*het_1km+(1|landscape),data=Data))
model13c=(lmer(H2~pland_5km*het_1km+(1|landscape),data=Data))
model14c=(lmer(H2~pland_1km*het_5km+(1|landscape),data=Data))
model15c=(lmer(H2~pland_5km*pland_1km+(1|landscape),data=Data))
model16c=(lmer(H2~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),H2, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),H2, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),H2, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),H2, type="i", method="VIF")

AICtab(nullmodelc,model1c,model2c,model3c,model4c,model5c,model6c,model7c,model8c,model9c,model10c,model11c,model12c,model13c,model14c,model15c,model16c)

plot(fitted(model4c), residuals(model4c))
qqnorm(resid(model4c))
qqline(resid(model4c))


#Model_weighted_NODF 
nullmodeld=(lmer(weighted_NODF~1+(1|landscape),data=Data))
model1d=(lmer(weighted_NODF~pland_1km+(1|landscape),data=Data))
model2d=(lmer(weighted_NODF~pland_5km+(1|landscape),data=Data))
model3d=(lmer(weighted_NODF~het_1km+(1|landscape),data=Data))
model4d=(lmer(weighted_NODF~het_5km+(1|landscape),data=Data))

model5d=(lmer(weighted_NODF~pland_5km+het_5km+(1|landscape),data=Data))
model6d=(lmer(weighted_NODF~pland_1km+het_1km+(1|landscape),data=Data))
model7d=(lmer(weighted_NODF~pland_5km+het_1km+(1|landscape),data=Data))
model8d=(lmer(weighted_NODF~pland_1km+het_5km+(1|landscape),data=Data))
model9d=(lmer(weighted_NODF~pland_5km+pland_1km+(1|landscape),data=Data))
model10d=(lmer(weighted_NODF~het_5km+het_1km+(1|landscape),data=Data))

model11d=(lmer(weighted_NODF~pland_5km*het_5km+(1|landscape),data=Data))
model12d=(lmer(weighted_NODF~pland_1km*het_1km+(1|landscape),data=Data))
model13d=(lmer(weighted_NODF~pland_5km*het_1km+(1|landscape),data=Data))
model14d=(lmer(weighted_NODF~pland_1km*het_5km+(1|landscape),data=Data))
model15d=(lmer(weighted_NODF~pland_5km*pland_1km+(1|landscape),data=Data))
model16d=(lmer(weighted_NODF~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),weighted_NODF, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),weighted_NODF, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),weighted_NODF, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),weighted_NODF, type="i", method="VIF")

AICtab(nullmodeld,model1d,model2d,model3d,model4d,model5d,model6d,model7d,model8d,model9d,model10d,model11d,model12d,model13d,model14d,model15d,model16d)

#Scatterplot_3d
grafico3d = function(x,y) {
  u<-exp(40.85-23.14*x-15.77*y+10.71*x*y)
  return(u)}
x1=seq(1,2,by=0.05)#het_5km
y1= seq(0.184,1.668,by=0.05)#het_1km
z<-outer(x1,y1,grafico3d)

persp(x1,y1,z,theta = 30, phi = 15, xlab = "Heterogeneity 5KM", ylab = "Heterogeneity 1KM",zlab = "W-NODF",cex.lab=1.0,cex.axis=0.9,nticks = 5,
      ticktype = "detailed")->res

scatterplot3d(het_5km, het_1km, W-NODF,type="h",angle = 30)


#Model_web_asymmetry (best null)
nullmodele=(lmer(web_asymmetry~1+(1|landscape),data=Data))
model1e=(lmer(web_asymmetry~pland_1km+(1|landscape),data=Data))
model2e=(lmer(web_asymmetry~pland_5km+(1|landscape),data=Data))
model3e=(lmer(web_asymmetry~het_1km+(1|landscape),data=Data))
model4e=(lmer(web_asymmetry~het_5km+(1|landscape),data=Data))

model5e=(lmer(web_asymmetry~pland_5km+het_5km+(1|landscape),data=Data))
model6e=(lmer(web_asymmetry~pland_1km+het_1km+(1|landscape),data=Data))
model7e=(lmer(web_asymmetry~pland_5km+het_1km+(1|landscape),data=Data))
model8e=(lmer(web_asymmetry~pland_1km+het_5km+(1|landscape),data=Data))
model9e=(lmer(web_asymmetry~pland_5km+pland_1km+(1|landscape),data=Data))
model10e=(lmer(web_asymmetry~het_5km+het_1km+(1|landscape),data=Data))

model11e=(lmer(web_asymmetry~pland_5km*het_5km+(1|landscape),data=Data))
model12e=(lmer(web_asymmetry~pland_1km*het_1km+(1|landscape),data=Data))
model13e=(lmer(web_asymmetry~pland_5km*het_1km+(1|landscape),data=Data))
model14e=(lmer(web_asymmetry~pland_1km*het_5km+(1|landscape),data=Data))
model15e=(lmer(web_asymmetry~pland_5km*pland_1km+(1|landscape),data=Data))
model16e=(lmer(web_asymmetry~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),web_asymmetry, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),web_asymmetry, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),web_asymmetry, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),web_asymmetry, type="i", method="VIF")

AICtab(nullmodele,model1e,model2e,model3e,model4e,model5e,model6e,model7e,model8e,model9e,model10e,model11e,model12e,model13e,model14e,model15e,model16e)

#Model_robustness_LL (best null)
nullmodelf=(lmer(robustness_LL~1+(1|landscape),data=Data))
model1f=(lmer(robustness_LL~pland_1km+(1|landscape),data=Data))
model2f=(lmer(robustness_LL~pland_5km+(1|landscape),data=Data))
model3f=(lmer(robustness_LL~het_1km+(1|landscape),data=Data))
model4f=(lmer(robustness_LL~het_5km+(1|landscape),data=Data))

model5f=(lmer(robustness_LL~pland_5km+het_5km+(1|landscape),data=Data))
model6f=(lmer(robustness_LL~pland_1km+het_1km+(1|landscape),data=Data))
model7f=(lmer(robustness_LL~pland_5km+het_1km+(1|landscape),data=Data))
model8f=(lmer(robustness_LL~pland_1km+het_5km+(1|landscape),data=Data))
model9f=(lmer(robustness_LL~pland_5km+pland_1km+(1|landscape),data=Data))
model10f=(lmer(robustness_LL~het_5km+het_1km+(1|landscape),data=Data))

model11f=(lmer(robustness_LL~pland_5km*het_5km+(1|landscape),data=Data))
model12f=(lmer(robustness_LL~pland_1km*het_1km+(1|landscape),data=Data))
model13f=(lmer(robustness_LL~pland_5km*het_1km+(1|landscape),data=Data))
model14f=(lmer(robustness_LL~pland_1km*het_5km+(1|landscape),data=Data))
model15f=(lmer(robustness_LL~pland_5km*pland_1km+(1|landscape),data=Data))
model16f=(lmer(robustness_LL~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),robustness_LL, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),robustness_LL, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),robustness_LL, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),robustness_LL, type="i", method="VIF")

AICtab(nullmodelf,model1f,model2f,model3f,model4f,model5f,model6f,model7f,model8f,model9f,model10f,model11f,model12f,model13f,model14f,model15f,model16f)


#Model_robustness_HL (best null)
nullmodelg=(lmer(robustness_HL~1+(1|landscape),data=Data))
model1g=(lmer(robustness_HL~pland_1km+(1|landscape),data=Data))
model2g=(lmer(robustness_HL~pland_5km+(1|landscape),data=Data))
model3g=(lmer(robustness_HL~het_1km+(1|landscape),data=Data))
model4g=(lmer(robustness_HL~het_5km+(1|landscape),data=Data))

model5g=(lmer(robustness_HL~pland_5km+het_5km+(1|landscape),data=Data))
model6g=(lmer(robustness_HL~pland_1km+het_1km+(1|landscape),data=Data))
model7g=(lmer(robustness_HL~pland_5km+het_1km+(1|landscape),data=Data))
model8g=(lmer(robustness_HL~pland_1km+het_5km+(1|landscape),data=Data))
model9g=(lmer(robustness_HL~pland_5km+pland_1km+(1|landscape),data=Data))
model10g=(lmer(robustness_HL~het_5km+het_1km+(1|landscape),data=Data))

model11g=(lmer(robustness_HL~pland_5km*het_5km+(1|landscape),data=Data))
model12g=(lmer(robustness_HL~pland_1km*het_1km+(1|landscape),data=Data))
model13g=(lmer(robustness_HL~pland_5km*het_1km+(1|landscape),data=Data))
model14g=(lmer(robustness_HL~pland_1km*het_5km+(1|landscape),data=Data))
model15g=(lmer(robustness_HL~pland_5km*pland_1km+(1|landscape),data=Data))
model16g=(lmer(robustness_HL~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),robustness_HL, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),robustness_HL, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),robustness_HL, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),robustness_HL, type="i", method="VIF")

AICtab(nullmodelg,model1g,model2g,model3g,model4g,model5g,model6g,model7g,model8g,model9g,model10g,model11g,model12g,model13g,model14g,model15g,model16g)

#Model_shannon_diversity (model16h)
nullmodelh=(lmer(shannon_diversity~1+(1|landscape),data=Data))
model1h=(lmer(shannon_diversity~pland_1km+(1|landscape),data=Data))
model2h=(lmer(shannon_diversity~pland_5km+(1|landscape),data=Data))
model3h=(lmer(shannon_diversity~het_1km+(1|landscape),data=Data))
model4h=(lmer(shannon_diversity~het_5km+(1|landscape),data=Data))

model5h=(lmer(shannon_diversity~pland_5km+het_5km+(1|landscape),data=Data))
model6h=(lmer(shannon_diversity~pland_1km+het_1km+(1|landscape),data=Data))
model7h=(lmer(shannon_diversity~pland_5km+het_1km+(1|landscape),data=Data))
model8h=(lmer(shannon_diversity~pland_1km+het_5km+(1|landscape),data=Data))
model9h=(lmer(shannon_diversity~pland_5km+pland_1km+(1|landscape),data=Data))
model10h=(lmer(shannon_diversity~het_5km+het_1km+(1|landscape),data=Data))

model11h=(lmer(shannon_diversity~pland_5km*het_5km+(1|landscape),data=Data))
model12h=(lmer(shannon_diversity~pland_1km*het_1km+(1|landscape),data=Data))
model13h=(lmer(shannon_diversity~pland_5km*het_1km+(1|landscape),data=Data))
model14h=(lmer(shannon_diversity~pland_1km*het_5km+(1|landscape),data=Data))
model15h=(lmer(shannon_diversity~pland_5km*pland_1km+(1|landscape),data=Data))
model16h=(lmer(shannon_diversity~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),shannon_diversity, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),shannon_diversity, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),shannon_diversity, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),shannon_diversity, type="i", method="VIF")

AICtab(nullmodelh,model1h,model2h,model3h,model4h,model5h,model6h,model7h,model8h,model9h,model10h,model11h,model12h,model13h,model14h,model15h,model16h)

#Scatterplot_3d
grafico3d = function(x,y) {
  u<-exp(7.319-3.163*x-4.056*y+2.934*x*y)
  return(u)}
x1=seq(1,2,by=0.05)#het_5km
y1= seq(0.184,1.668,by=0.05)#het_1km
z<-outer(x1,y1,grafico3d)

persp(x1,y1,z,theta = 30, phi = 15, xlab = "Heterogeneity 5KM", ylab = "Heterogeneity 1KM",zlab = "shannon_diversity",cex.lab=1.0,cex.axis=0.9,nticks = 5,
      ticktype = "detailed")->res

scatterplot3d(het_5km, het_1km,shannon_diversity ,type="h",angle = 30)


#Model_NODF (model16i)
nullmodeli=(lmer(NODF~1+(1|landscape),data=Data))
model1i=(lmer(NODF~pland_1km+(1|landscape),data=Data))
model2i=(lmer(NODF~pland_5km+(1|landscape),data=Data))
model3i=(lmer(NODF~het_1km+(1|landscape),data=Data))
model4i=(lmer(NODF~het_5km+(1|landscape),data=Data))

model5i=(lmer(NODF~pland_5km+het_5km+(1|landscape),data=Data))
model6i=(lmer(NODF~pland_1km+het_1km+(1|landscape),data=Data))
model7i=(lmer(NODF~pland_5km+het_1km+(1|landscape),data=Data))
model8i=(lmer(NODF~pland_1km+het_5km+(1|landscape),data=Data))
model9i=(lmer(NODF~pland_5km+pland_1km+(1|landscape),data=Data))
model10i=(lmer(NODF~het_5km+het_1km+(1|landscape),data=Data))

model11i=(lmer(NODF~pland_5km*het_5km+(1|landscape),data=Data))
model12i=(lmer(NODF~pland_1km*het_1km+(1|landscape),data=Data))
model13i=(lmer(NODF~pland_5km*het_1km+(1|landscape),data=Data))
model14i=(lmer(NODF~pland_1km*het_5km+(1|landscape),data=Data))
model15i=(lmer(NODF~pland_5km*pland_1km+(1|landscape),data=Data))
model16i=(lmer(NODF~het_5km*het_1km+(1|landscape),data=Data))

mctest(cbind(pland_1km,het_1km),NODF, type="i", method="VIF")
mctest(cbind(pland_5km,het_5km),NODF, type="i", method="VIF")
mctest(cbind(het_1km,het_5km),NODF, type="i", method="VIF")
mctest(cbind(pland_1km,pland_5km),NODF, type="i", method="VIF")

AICtab(nullmodeli,model1i,model2i,model3i,model4i,model5i,model6i,model7i,model8i,model9i,model10i,model11i,model12i,model13i,model14i,model15i,model16i)

#Scatterplot_3d
grafico3d = function(x,y) {
  u<-exp(-16.05+15.29*x+41.75*y-23.53*x*y)
  return(u)}
x1=seq(1,2,by=0.05)#het_5km
y1= seq(0.184,1.668,by=0.05)#het_1km
z<-outer(x1,y1,grafico3d)

persp(x1,y1,z,theta = 30, phi = 15, xlab = "Heterogeneity 5KM", ylab = "Heterogeneity 1KM",zlab = "NODF",cex.lab=1.0,cex.axis=0.9,nticks = 5,
      ticktype = "detailed")->res

scatterplot3d(het_5km, het_1km,NODF,type="h",angle = 30)