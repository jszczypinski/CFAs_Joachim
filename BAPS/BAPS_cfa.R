setwd("f:/projekty/Joachim/BAPS")

d <- read.csv("BAPS.csv", sep=",", header =T)

library(lavaan)
library(semTools)
colnames(d)[1] = 'BAPS1' 

#### MODEL 1 
 
model = 'survival_strategy =~ BAPS1 + BAPS2 + BAPS3 + BAPS4 + BAPS5 + BAPS6
        negative_beliefs =~ BAPS7 + BAPS8 + BAPS9 + BAPS10 + BAPS11 + BAPS12
        normalizing_beliefs =~ BAPS13 + BAPS14 + BAPS15 + BAPS16 + BAPS17 + BAPS18'

fit = cfa(model, data = d, estimator = 'WLSMV', ordered = TRUE, std.lv=TRUE, missing = "listwise",)

summary(fit, fit.measures=TRUE, standardized=TRUE)

reliability(fit)

mod_indices = modificationindices(fit)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

fits = fitmeasures(fit,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

### MODEL 2 

model2 = 'survival_strategy =~ BAPS2 + BAPS3 + BAPS4 + BAPS5 + BAPS6
        negative_beliefs =~ BAPS7 + BAPS8 + BAPS9 + BAPS10 + BAPS11 + BAPS12
        normalizing_beliefs =~ BAPS13 + BAPS14 + BAPS15 + BAPS16 + BAPS17 + BAPS18'

fit2 = cfa(model2, data = d, estimator = 'WLSMV', ordered = TRUE, std.lv=TRUE, missing = "listwise",)

summary(fit2, fit.measures=TRUE, standardized=TRUE)

reliability(fit2)

mod_indices = modificationindices(fit2)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

fits2 = fitmeasures(fit2,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

#### MODEL 3

model3 = 'survival_strategy =~ BAPS2 + BAPS3 + BAPS4 + BAPS5 + BAPS6
        negative_beliefs =~ BAPS7 + BAPS8 + BAPS9 + BAPS10 + BAPS11 + BAPS12
        normalizing_beliefs =~ BAPS13 + BAPS14 + BAPS15 + BAPS16 + BAPS17 + BAPS18
BAPS13 ~~ BAPS14'

fit3 = cfa(model3, data = d, estimator = 'WLSMV', ordered = TRUE, std.lv=TRUE, missing = "listwise",)

summary(fit3, fit.measures=TRUE, standardized=TRUE)

reliability(fit3)

mod_indices = modificationindices(fit3)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

fits3 = fitmeasures(fit3,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
### MODEL INVARIANCE
setwd("f:/projekty/Joachim/BAPS")

d <- read.csv("BAPS.csv", sep=",", header =T)

library(lavaan)
library(semTools)
colnames(d)[1] = 'BAPS1' 

model3 = 'survival_strategy =~ BAPS2 + BAPS3 + BAPS4 + BAPS5 + BAPS6
        negative_beliefs =~ BAPS7 + BAPS8 + BAPS9 + BAPS10 + BAPS11 + BAPS12
        normalizing_beliefs =~ BAPS13 + BAPS14 + BAPS15 + BAPS16 + BAPS17 + BAPS18
BAPS13 ~~ BAPS14'

#Configural invariance
cfa.config <- cfa(model3, data = d, estimator = "WLSMV", group = "diagnoza",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE)

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

# metric
cfa.metric <- cfa(model3, data = d, estimator = "WLSMV", group = "diagnoza",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, group.equal = 'loadings') 
                  #group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.config, cfa.metric, nested = T )
x

# scalar
cfa.scalar <- cfa(model3, data = d, estimator = "WLSMV", group = "diagnoza",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, group.equal = c('loadings', 'intercepts')) 
#group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.metric, cfa.scalar, nested = T )
x

# strict
cfa.strict <- cfa(model3, data = d, estimator = "WLSMV", group = "diagnoza",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, group.equal = c('loadings', 'intercepts', 'residuals')) 
#group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.scalar, cfa.strict, nested = T)
x

round(robust_dif(cfa.metric,cfa.config), digits = 3)
lavTestScore(cfa.metric)
parTable(cfa.metric)

#### CFA MODEL DIFFERENCES ####

robust_dif = function (x,y) {
    fit_x = fitmeasures(x,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor'))
    fit_y= fitmeasures(y,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor'))
    fit_x=as.list(fit_x)
    fit_y=as.list(fit_y)
    cd = (fit_x$df.scaled * fit_x$chisq.scaling.factor - fit_y$df.scaled * fit_y$chisq.scaling.factor)/
        (fit_x$df.scaled-fit_y$df.scaled)
    Test_stat = (fit_x$chisq.scaled * fit_x$chisq.scaling.factor - 
                     fit_y$chisq.scaled * fit_y$chisq.scaling.factor) /cd
    p_val = pchisq(Test_stat,df=fit_x$df.scaled-fit_y$df.scaled, lower.tail = FALSE)
    Test = c(Test_stat,p_val)
    return(Test)
}

round(robust_dif(fit,fit2), digits = 3)
round(robust_dif(fit2,fit3), digits = 3)
