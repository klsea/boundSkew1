# run models on skew data
# 7.15.19 KLS updated 10.17.19
# add follow-up t-tests 2.10.20 KLS

# load required packages
library(here)
library(lme4)
library(gdata)
library(tidyr)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),10:69)
d1 <- clean_skew(d0)

# recode response to acceptance
d1$accept <- d1$response - 1

# reorder deg_skew factor
d1$deg_skew <- factor(d1$deg_skew, levels = c('Symmetric', 'Weak', 'Moderate', 'Strong'))

# reorder valence factor
d1$valence <- factor(d1$valence, levels = c('neutral', 'gain', 'loss'))

# make magnitude a factor
d1$magnitude <- factor(d1$magnitude, levels = c('0', '0.5', '5'))

# scale and center age
d1$Age <- scale(d1$Age)

# make interaction term
d1$magval <- interaction(d1$valence, d1$magnitude)
d1$magval <- drop.levels(d1$magval)
d1$magval <- factor(d1$magval, levels = c('neutral.0', 'loss.5', 'loss.0.5', 'gain.0.5', 'gain.5'))

# baseline - only degree of skew
b1 <- glmer(accept ~ deg_skew + (1 + Age | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
               control=glmerControl(optimizer='bobyqa'))
summary(b1, correlation = FALSE)

## boundary fit - remove age from random effects
b1.1 <- glmer(accept ~ deg_skew + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(b1.1, correlation = FALSE)
saveRDS(b1.1, here('output', 'baseline.RDS'))

##follow-up t-tests
d2 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew'))
d3 <- spread(d2[,c(1,2,4)], 'deg_skew', 'accept')
b1t1 <- t.test(d3$Symmetric, d3$Weak, paired = TRUE)
b1t2 <- t.test(d3$Symmetric, d3$Moderate, paired = TRUE)
b1t3 <- t.test(d3$Symmetric, d3$Strong, paired = TRUE)
rm(d2,d3)

# model 1 - add valence
m1 <- glmer(accept ~ deg_skew * valence + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m1, correlation = FALSE)
saveRDS(m1, here('output', 'm1.RDS'))

## compare model 1 and model 2
anova(b1.1,m1)

## follow-up t-tests
d4 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'valence'))
d4$skew_valence <- interaction(d4$deg_skew, d4$valence)
d5 <- spread(d4[,c(1,9,5)], 'skew_valence', 'accept')
m1t1 <- t.test(d5$Symmetric.loss, d5$Weak.loss, paired = TRUE)
m1t2 <- t.test(d5$Symmetric.loss, d5$Moderate.loss, paired = TRUE)
m1t3 <- t.test(d5$Symmetric.loss, d5$Strong.loss, paired = TRUE)
rm(d4, d5)

# Does magnitude make a difference?

# model 2 -  magnitude instead of valence
m2 <- glmer(accept ~ deg_skew  * magnitude + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(m2, correlation = FALSE)
saveRDS(m2, here('output', 'm2.RDS'))

## compare model 2 to baseline
anova(b1.1,m2)

## follow-up t-tests
d8 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'magval'))
d8$skew_magval <- interaction(d8$deg_skew, d8$magval)
d9 <- spread(d8[,c(1,9,5)], 'skew_magval', 'accept')
m3t1 <- t.test(d9$Symmetric.loss.5, d9$Weak.loss.5, paired = TRUE)
m3t2 <- t.test(d9$Symmetric.loss.5, d9$Moderate.loss.5, paired = TRUE)
m3t3 <- t.test(d9$Symmetric.loss.5, d9$Strong.loss.5, paired = TRUE)
m3t4 <- t.test(d9$Symmetric.gain.5, d9$Moderate.gain.5, paired = TRUE)
rm(d8, d9)

# model 3 - interaction between mag and val
m3 <- glmer(accept ~ deg_skew * magval + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m3, correlation = FALSE)
saveRDS(m3, here('output', 'm3.RDS'))

## compare model 3 to baseline
anova(b1.1, m3)

## Follow-up t-tests
d6 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'magnitude'))
d6$skew_mag <- interaction(d6$deg_skew, d6$magnitude)
d7 <- spread(d6[,c(1,9,5)], 'skew_mag', 'accept')
m2t1 <- t.test(d7$Symmetric.5, d7$Weak.5, paired = TRUE)
m2t2 <- t.test(d7$Symmetric.5, d7$Moderate.5, paired = TRUE)
m2t3 <- t.test(d7$Symmetric.5, d7$Strong.5, paired = TRUE)
rm(d6, d7)

# model 4 - add Age
m4 <- glmer(accept ~ deg_skew * magval + Age + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m4, correlation = FALSE)
saveRDS(m4, here('output', 'm4.RDS'))

# compare model 5 to model 4
anova(m3,m4)




