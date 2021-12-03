setwd("f:/projekty/Joachim/GPTS")

d <- read.csv("GPTS.csv", sep=",", header =T)

library(lavaan)
library(lavaanPlot)
library(car)
library(psych)
library(dplyr)
library(tidyr)
library(scales)
library(coefficientalpha)
library(semTools)
library(semPlot)
library(tidySEM)
library(ggplot2)
library(RBtest)


d1 = subset(d, badanie ==1)
d2 = subset(d, badanie ==2)
d3 = subset(d, badanie ==3)

library(dplyr)

d2 = d2 %>% 
  mutate_at(vars(1:18), 
            ~factor(recode(.,
                           "1"=0, 
                           "2"=1,
                           "3"=2,
                           "4"=3,
                           "5"=4)),ordered=T) 

d3 = d3 %>% 
  mutate_at(vars(1:18), 
            ~factor(recode(.,
                           "1"=0, 
                           "2"=1,
                           "3"=2,
                           "4"=3,
                           "5"=4)),ordered=T) 

d = rbind(d1,d2)
d = rbind(d,d3)

colnames(d)[1] = 'PartA_gptsa1'     

model1 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7  + PartA_gptsa8
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10'

model_fit1 = cfa(model1, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
fitmeasures(model_fit1,c('chisq.scaled','df.scaled','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

reliability(model_fit1)

mod_indices = modificationindices(model_fit1)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

model2 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10'

model_fit2 = cfa(model2, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
fitmeasures(model_fit2,c('chisq.scaled','df.scaled','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))


model3 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10
partB_gptsb2 ~~ partB_gptsb4'

round(robust_dif(model_fit2,model_fit3), digits = 3)

model_fit3 = cfa(model3, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
fitmeasures(model_fit3,c('chisq.scaled','df.scaled',                      'cfi.scaled','tli.scaled','rmsea.scaled','srmr'))



model4 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10
partB_gptsb2 ~~ partB_gptsb4
partB_gptsb3 ~~ partB_gptsb4'



model_fit4 = cfa(model4, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
fitmeasures(model_fit4,c('chisq.scaled','df.scaled',                      'cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

round(robust_dif(model_fit3,model_fit4), digits = 3)

mod_indices = modificationindices(model_fit4)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)


model5 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10
partB_gptsb2 ~~ partB_gptsb4
partB_gptsb3 ~~ partB_gptsb4
partB_gptsb2 ~~ partB_gptsb3'



model_fit5 = cfa(model5, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
fitmeasures(model_fit5,c('chisq.scaled','df.scaled',                      'cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

round(robust_dif(model_fit4,model_fit5), digits = 3)

mod_indices = modificationindices(model_fit5)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)
#cor.test(as.numeric(d$partB_gptsb2),as.numeric(d$partB_gptsb4), method='spearman')

#MEASUREMENT INVARIANCE


model5 = 'reference =~ PartA_gptsa1 + PartA_gptsa2 +PartA_gptsa3 + PartA_gptsa4 +
          PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 
          
          persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
          partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
          partB_gpts10
partB_gptsb2 ~~ partB_gptsb4
partB_gptsb3 ~~ partB_gptsb4
partB_gptsb2 ~~ partB_gptsb3'

setwd("f:/projekty/Joachim/GPTS")

d <- read.csv("GPTS.csv", sep=",", header =T)
colnames(d)[1] = 'PartA_gptsa1'

#Configural invariance
cfa.config <- cfa(model5, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE)

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

# metric
cfa.metric <- cfa(model5, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, group.equal = 'loadings',group.partial = c('persecutory  =~ partB_gptsb7' ))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.config, cfa.metric, nested = T )
x

lavTestScore(cfa.metric)
parTable(cfa.metric)

# scalar
cfa.scalar <- cfa(model5, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, 
                  group.equal = c('loadings', 'intercepts'),
                  group.partial = c('persecutory  =~ partB_gptsb7', 'reference  =~ PartA_gptsa2' )) 
#group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.metric, cfa.scalar, nested = T )
x
lavTestScore(cfa.strict)
parTable(cfa.strict)
# strict
cfa.strict <- cfa(model5, data = d, estimator = "WLSMV", group = "diag",
                  std.lv=TRUE, missing = "listwise", ordered = TRUE, 
                  group.equal = c('loadings', 'intercepts', 'residuals'),
                  group.partial = c('persecutory  =~ partB_gptsb7', 'reference  =~ PartA_gptsa2' )) 
#group.partial = c('factor  =~ pq1', 'factor  =~ pq2', 'factor  =~ pq3'))

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

x = compareFit(cfa.scalar, cfa.strict, nested = T )
x
