setwd("f:/projekty/Joachim/PQ16")

d <- read.csv("PQ16.csv", sep=",", header =T)

library(lavaan)
library(semTools)

colnames(d)[1] = 'pq1' 

d = subset(d, badanie == 2)

model1 = ' factor =~ pq1 + pq2 + pq3 + pq4 + pq5 + pq6 + pq7 + pq8 + pq9 + pq10 +
                    pq11 + pq12 + pq13 + pq14 + pq15 + pq16'
fit1 = cfa(model1, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)

summary(fit1, fit.measures=TRUE, standardized=TRUE)

fits = fitmeasures(fit1,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))


reliability(fit1)

mod_indices = modificationindices(fit1)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

model2 = ' factor =~ pq1 + pq2 + pq3 + pq4 + pq5 + pq6 + pq7 + pq8 + pq9 + pq10 +
                    pq11 + pq12 + pq13 + pq14 + pq15 + pq16
pq7 ~~ pq14'
fit2 = cfa(model2, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)

fits2 = fitmeasures(fit2,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))


summary(fit2, fit.measures=TRUE, standardized=TRUE)

reliability(fit2)

mod_indices = modificationindices(fit2)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

model3 = ' factor =~ pq1 + pq2 + pq3 + pq4 + pq5 + pq6 + pq7 + pq8 + pq9 + pq10 +
                    pq11 + pq12 + pq13 + pq14 + pq15 + pq16
pq7 ~~ pq14
pq1 ~~ pq7'
fit3 = cfa(model3, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)

fits3 = fitmeasures(fit3,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
fits3

summary(fit3, fit.measures=TRUE, standardized=TRUE)

reliability(fit3)

mod_indices = modificationindices(fit3)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

#### MEASUREMENT INVARIANCE ####

setwd("f:/projekty/Joachim/PQ16")

d <- read.csv("PQ16.csv", sep=",", header =T)

library(lavaan)
library(semTools)

colnames(d)[1] = 'pq1' 

d = subset(d, badanie == 2)

model3 = ' factor =~ pq1 + pq2 + pq3 + pq4 + pq5 + pq6 + pq7 + pq8 + pq9 + pq10 +
                    pq11 + pq12 + pq13 + pq14 + pq15 + pq16
pq7 ~~ pq14
pq1 ~~ pq7'

# Configural invariance
cfa.config <- cfa(model3, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE)

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

# metric
cfa.metric <- cfa(model3, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, group.equal = 'loadings', group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.config, cfa.metric, nested = T )
x

lavTestScore(cfa.metric)
parTable(cfa.metric)
#### IRT analysis 
library(psych)
library(ltm)
d1 = d[c('pq1','pq2','pq3','pq4','pq5','pq6','pq7','pq8','pq9','pq10','pq11','pq12',
       'pq13','pq14','pq15','pq16')]

fit3 <- gpcm(d1, IRT.param = T, start.val = "random")
summary(fit3)
coef(fit3,prob=TRUE)
plot(fit3, type = 'ICC', lwd = 2)
plot(fit3, type = 'IIC', lwd = 2)
