setwd("f:/projekty/Joachim/IVI")

d <- read.csv("IVI.csv", sep=",", header =T)

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

colnames(d)[1] = 'IVI_1'

model = 'metaphysical =~ IVI_1 + IVI_3 + IVI_4 + IVI_5 + IVI_6 + IVI_8 + IVI_13 +
IVI_14 + IVI_15 + IVI_16 + IVI_17 + IVI_25 + IVI_26
positive =~ IVI_2 + IVI_7 + IVI_9 +IVI_18 +IVI_19 +IVI_20 + IVI_21 + IVI_24
loss_of_control =~ IVI_10 +IVI_11 +IVI_12 +IVI_22 +IVI_23'
  
model_fit = cfa(model, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
summary(model_fit,standardized=T, fit.measures = T, ordered = T)

iclust((POLYCHORIC_R(d, method = "Fox", verbose = F)))
library(psych)
library(EFA.dimensions)
omega(m = POLYCHORIC_R(d, method = "Fox", verbose = F), nfactors = 2, pc = 'pa')

new_model = 'positive =~ IVI_21 + IVI_20 + IVI_24 + IVI_19 + IVI_2 + IVI_7 + 
IVI_18 + IVI_9 + IVI_8 
negative =~ IVI_25 + IVI_16 + IVI_26 +IVI_14 +IVI_1 +IVI_6 + IVI_4 + IVI_11 + IVI_23 +IVI_22 +IVI_15 +IVI_12 + IVI_3'

model_fit = cfa(new_model, data = d, estimator = "WLSMV", std.lv=TRUE, missing = "listwise", ordered = T)
summary(model_fit,standardized=T, fit.measures = T)

positive = d[c('IVI_21','IVI_20','IVI_24','IVI_19','IVI_2','IVI_7', 'IVI_17', 'IVI_18','IVI_9','IVI_8','IVI_5','IVI_13')]

omega(m = positive, nfactors = 2, pc = 'pa')

negative = d[c('IVI_25','IVI_16','IVI_26','IVI_14','IVI_1','IVI_6', 'IVI_4', 'IVI_23','IVI_22','IVI_15','IVI_12','IVI_3')]
alpha(negative)


d2 =  d[c('IVI_2','IVI_3','IVI_4','IVI_5','IVI_6','IVI_7', 'IVI_8', 'IVI_9','IVI_10',
          'IVI_11','IVI_12','IVI_15','IVI_18','IVI_19','IVI_20',
          'IVI_21','IVI_22','IVI_23','IVI_24')]
omega(m = cor(d2), nfactors = 2, pc = 'pa')
### CORMAT 
library("dplyr")
library("corrplot")
# Correlation matrix of items
cormat <- d %>%
    select(starts_with("IVI")) %>%
    cor()# Correlation matrix plot
corrplot.mixed(cormat)

corrplot(cormat, order = "hclust", addrect = 2, method="color", tl.pos="n", 
         cl.lim=c(-1,1), col=colorRampPalette(c("blue","white","red"))(200))
