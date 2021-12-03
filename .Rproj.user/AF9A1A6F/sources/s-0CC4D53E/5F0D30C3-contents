setwd("f:/projekty/Joachim/MUSEQ")

library(lavaan)
library(semTools)
d = read.csv("MUSEQ.csv", sep=",",  header=T)

colnames(d)[1] = 'MUSEQ_1' 

model = 'auditory =~MUSEQ_1+MUSEQ_2+MUSEQ_3+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7

visual =~ MUSEQ2_1+MUSEQ2_2+MUSEQ2_3+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8

olfactory =~ MUSEQ3_1+MUSEQ3_2+MUSEQ3_3+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7

gustatory =~ MUSEQ4_1+MUSEQ4_2+MUSEQ4_3+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8

bodily =~ MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8

presence =~ MUSEQ6_1+MUSEQ6_2+MUSEQ6_3+MUSEQ6_4

##covariances ##

'
fit = cfa(model, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)

summary(fit, fit.measures=TRUE, standardized=TRUE)

reliability(fit)

mod_indices = modificationindices(fit)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)

model2 = 'auditory =~MUSEQ_1+MUSEQ_2+MUSEQ_3+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7

visual =~ MUSEQ2_1+MUSEQ2_2+MUSEQ2_3+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8

olfactory =~ MUSEQ3_1+MUSEQ3_2+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7

gustatory =~ MUSEQ4_1+MUSEQ4_2+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8

bodily =~ MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8

presence =~ MUSEQ6_1+MUSEQ6_2+MUSEQ6_3+MUSEQ6_4

##covariances ##

'
fit2 = cfa(model2, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)


fits = fitmeasures(fit,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
fits2 = fitmeasures(fit2,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))

mod_indices = modificationindices(fit2)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)


model3 = 'auditory =~MUSEQ_1+MUSEQ_2+MUSEQ_3+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7

visual =~ MUSEQ2_1+MUSEQ2_2+MUSEQ2_3+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8

olfactory =~ MUSEQ3_1+MUSEQ3_2+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7

gustatory =~ MUSEQ4_1+MUSEQ4_2+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8

bodily =~ MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8

presence =~ MUSEQ6_2+MUSEQ6_3+MUSEQ6_4

##covariances ##

'
fit3 = cfa(model3, data = d, estimator = 'WLSMV', std.lv=TRUE, missing = "listwise", ordered = TRUE)


fitmeasures(fit,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
fitmeasures(fit2,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
fitmeasures(fit3,c('chisq.scaled', 'df.scaled', 'chisq.scaling.factor','cfi.scaled','tli.scaled','rmsea.scaled','srmr'))
mod_indices = modificationindices(fit3)
head(mod_indices[order(mod_indices$mi, decreasing = T),],10)
