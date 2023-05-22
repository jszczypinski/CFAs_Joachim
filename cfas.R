#### GPTS ####
setwd('D:/Projekty/Joachim_CFA/GPTS')
library(lavaan)
library(semTools)
d = read.csv("GPTS.csv", sep=",", header=T)
d = read.csv("GPTS_v2.csv", sep=",", header=T)
colnames(d)[1] ='PartA_gptsa1'

model = 'reference =~PartA_gptsa1 + PartA_gptsa2 + PartA_gptsa3 + PartA_gptsa4 +
        PartA_gptsa5 + PartA_gptsa6 + PartA_gptsa7 + PartA_gptsa8
        persecutory =~ partB_gptsb1 + partB_gptsb2 + partB_gptsb3 + partB_gptsb4 +
        partB_gptsb5 + partB_gptsb6 + partB_gptsb7 + partB_gptsb8 + partB_gptsb9 +
        partB_gpts10
'

fit = cfa(model, data = d, estimator = 'MLR', ordered = TRUE)

summary(fit, fit.measures=TRUE, standardized=TRUE)

reliability(fit)

setwd('D:/Projekty/Joachim_CFA/MUSEQ')
library(lavaan)
library(semTools)
d = read.csv("MUSEQ.csv", sep=",", header=T)

model = 'auditory =~ ď.żMUSEQ_1+MUSEQ_2+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7

visual =~ MUSEQ2_1+MUSEQ2_2+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8

olfactory =~ MUSEQ3_1+MUSEQ3_2+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7

gustatory =~ MUSEQ4_1+MUSEQ4_2+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8

bodily =~ MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8

presence =~ MUSEQ6_2+MUSEQ6_3+MUSEQ6_4

##covariances ##

'
fit = cfa(model, data = d, estimator = 'WLSMV', ordered = TRUE)

summary(fit, fit.measures=TRUE, standardized=TRUE)

reliability(fit)

mod_indices = modificationindices(fit)
head(mod_indices[order(mod_indices$mi, decreasing = T),],20)

or_model = '
superfaktor =~ auditory + visual + olfactory + gustatory + bodily + presence

auditory =~ ď.żMUSEQ_1+MUSEQ_2+MUSEQ_3+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7

visual =~ MUSEQ2_1+MUSEQ2_2+MUSEQ2_3+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8

olfactory =~ MUSEQ3_1+MUSEQ3_2+MUSEQ3_3+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7+MUSEQ3_8

gustatory =~ MUSEQ4_1+MUSEQ4_2+MUSEQ4_3+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8

bodily =~ MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8

presence =~ MUSEQ6_1+MUSEQ6_2+MUSEQ6_3+MUSEQ6_4


'
or_fit = cfa(or_model, data = d, estimator = 'WLSMV', ordered = T, orthogonal = T)

summary(or_fit, fit.measures=TRUE, standardized=TRUE)

super_model = '
superfaktor =~ ď.żMUSEQ_1+MUSEQ_2+MUSEQ_3+MUSEQ_4+MUSEQ_5+MUSEQ_6+MUSEQ_7 +
MUSEQ2_1+MUSEQ2_2+MUSEQ2_3+MUSEQ2_4+MUSEQ2_5+MUSEQ2_6+MUSEQ2_7+MUSEQ2_8 +
MUSEQ3_1+MUSEQ3_2+MUSEQ3_3+MUSEQ3_4+MUSEQ3_5+MUSEQ3_6+MUSEQ3_7+MUSEQ3_8 +
MUSEQ4_1+MUSEQ4_2+MUSEQ4_3+MUSEQ4_4+MUSEQ4_5+MUSEQ4_6+MUSEQ4_7+MUSEQ4_8 +
MUSEQ5_1+MUSEQ5_2+MUSEQ5_3+MUSEQ5_4+MUSEQ5_5+MUSEQ5_6+MUSEQ5_7+MUSEQ5_8 +
MUSEQ6_1+MUSEQ6_2+MUSEQ6_3+MUSEQ6_4

'
super_fit = cfa(super_model, data = d, estimator = 'WLSMV', ordered = T, orthogonal = T)

summary(super_fit, fit.measures=TRUE, standardized=TRUE)

setwd('D:/Projekty/Joachim_CFA/IPASE')
library(lavaan)
library(semTools)
d = read.csv("IPASE.csv", sep=",", header=T)

model = 'transitivism =~ ď.żIPASE1+IPASE4+IPASE13
selfawareness =~ IPASE2+IPASE3+IPASE10
consciousness =~ IPASE5+IPASE8+IPASE11
cognition =~ IPASE7+IPASE14+IPASE15
somatization =~IPASE6+IPASE9+IPASE12
'
fit = cfa(model, data = d, estimator = 'WLSMV', ordered = TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

model = 'superfaktor =~ ď.żIPASE1+IPASE4+IPASE13 +IPASE2+IPASE3+IPASE10 +
IPASE5+IPASE8+IPASE11 + IPASE7+IPASE14+IPASE15 +IPASE6+IPASE9+IPASE12
'
fit = cfa(model, data = d, estimator = 'WLSMV', ordered = TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)

