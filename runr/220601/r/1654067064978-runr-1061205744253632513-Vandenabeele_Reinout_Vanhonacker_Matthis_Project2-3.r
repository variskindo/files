#inladen
uitleg <- county_facts_dictionary
pendel <- UScensus[,c(1:6,29)]
colnames(pendel) <- c('fips', 'area', 'state', 'region', 'district', 'urbanization', 'pendeltijd')
attach(pendel)
attach(UScensus)
library(car)

### 2.1 Verstedelijking en ligging
## Verstedelijking
boxplot(pendeltijd~urbanization, las=2)

urbaov = aov(pendeltijd~urbanization)
summary(urbaov)               # Afhankelijkheid van urbanisatie? (verder regio etc.)
TukeyHSD(urbaov)

## Ligging
boxplot(pendeltijd~region)

regioaov = aov(pendeltijd~region)
summary(regioaov)
TukeyHSD(regioaov)

## Districten & staten
# Afzonderlijk
aovdist = aov(pendeltijd~district)
aovstate = aov(pendeltijd~state)

summary(aovdist)
summary(aovstate)

# Additief: district of state bovenop regio
model.reg=lm(pendeltijd~region)
model.reg.distr=lm(pendeltijd~region+district)
model.reg.state=lm(pendeltijd~region+state)

anova(model.reg,model.reg.distr)
anova(model.reg,model.reg.state) 

model.urb = lm(pendeltijd~urbanization)
model.urb.reg = lm(pendeltijd~urbanization + region)
anova(model.urb,model.urb.reg)
summary(model.urb.reg)

###2.2 Socio-economische factoren (numeriek)
library(MASS)
model.full =  lm(UScensus$LFE305213 ~ ., data=UScensus[,c(7:57)])
summary(model.full)
    # Meest significant als < 1 %

model.null =  lm(UScensus$LFE305213 ~ 1, data = UScensus[,c(7:57)])

#achterwaarts
model.backward = stepAIC(model.full,
                         direction="backward")
formula(model.backward)
summary(model.backward)

#beide
model.both = stepAIC(model.full,
                     direction="both")
formula(model.both) # Kleinste AIC
summary(model.both)

#voorwaarts
model.forward = stepAIC(model.null,diretion = 'forward',  scope=list(upper=model.full,lower=model.null))
formula(model.forward)
summary(model.forward)

## Multicollineariteit?
vif(model.both)
mean(vif(model.both))

## Zinvol afzonderlijke types?
model.update.reg = update(model.both, . ~ .*Region)
summary(model.update.reg)
anova(model.both, model.update.reg)

model.update.urb = update(model.both, . ~ .*Urbanization)
summary(model.update.urb)
anova(model.both, model.update.urb)
      # Beide randsignificant

### 2.3 Regularisatie
training = sample(314, 314/2)
train = UScensus[training, ]
valid = UScensus[-training,]
lambda = c(0, 10**(-10:10))
model.r = lm.ridge(LFE305213 ~., lambda = lambda, data = train[,7:57])

plot(model.r)

X = as.matrix(cbind(1, valid[,c(7:28, 30:57)]))
y.val <- X %*% t(coef(model.r))
SSEval = colSums((y.val - valid$LFE305213)**2)
#plot
plot(log10(lambda), SSEval,ylab = 'SSE',main = 'SSE van de validatiegegevens voor verschillende waarden van lambda', cex.main=1
     ,cex=1.5,col='red')
lambda_opt = which.min(SSEval); lambda_opt
  # Optimale lambda bekijken

