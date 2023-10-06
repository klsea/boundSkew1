# run models on skew data
# 7.15.19 KLS updated 10.17.19
# add follow-up t-tests 2.10.20 KLS

# load required packages
library(here)
library(lme4)
library(gdata)
library(tidyr)
library(rlist)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# load source functions
source(here::here('scr', 'isolate_skew.R'))
source(here::here('scr', 'clean_skew.R'))
source(here::here('scr', 'SummarySE.R'))
source(here::here('scr', 'pairedttable.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "bound_skew1_data.csv"))

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
b1 <- glmer(accept ~ deg_skew + (1 + Age | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
               control=glmerControl(optimizer='bobyqa'))
summary(b1, correlation = FALSE)

## boundary fit - remove age from random effects
b1.1 <- glmer(accept ~ deg_skew + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(b1.1, correlation = FALSE)
saveRDS(b1.1, here::here('output', 'baseline.RDS'))

##follow-up t-tests
d2 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew'))
d3 <- spread(d2[,c(1,2,4)], 'deg_skew', 'accept')
b1_follow <- pairedttable(d3, colnames(d3[2:5]))
names(b1_follow)[3] <- 'pval'
list.save(b1_follow, here::here('output', 'b1_follow.rds'))
rm(d2,d3, b1_follow)

# model 1 - add valence
m1 <- glmer(accept ~ deg_skew * valence + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m1, correlation = FALSE)
saveRDS(m1, here::here('output', 'm1.RDS'))

## compare model 1 and model 2
anova(b1.1,m1)

## follow-up t-tests
d4 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'valence'))
d4$skew_valence <- interaction(d4$deg_skew, d4$valence)
d5 <- spread(d4[,c(1,9,5)], 'skew_valence', 'accept')
m1_follow_neutral <- pairedttable(d5[c(1,2:5)], colnames(d5[2:5]))
m1_follow_gain <- pairedttable(d5[c(1,6:9)], colnames(d5[6:9]))
m1_follow_loss <- pairedttable(d5[c(1,10:13)], colnames(d5[10:13]))
names(m1_follow_loss)[3] <- 'pval'
list.save(m1_follow_neutral, here::here('output', 'm1_follow_neutral.rds'))
list.save(m1_follow_gain, here::here('output', 'm1_follow_gain.rds'))
list.save(m1_follow_loss, here::here('output', 'm1_follow_loss.rds'))
rm(d4, d5, m1_follow_neutral, m1_follow_gain, m1_follow_loss)

# Does magnitude make a difference?

# model 2 -  magnitude instead of valence
m2 <- glmer(accept ~ deg_skew  * magnitude + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(m2, correlation = FALSE)
saveRDS(m2, here::here('output', 'm2.RDS'))

## compare model 2 to baseline
anova(b1.1,m2)

## Follow-up t-tests
d6 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'magnitude'))
d6$skew_mag <- interaction(d6$deg_skew, d6$magnitude)
d7 <- spread(d6[,c(1,9,5)], 'skew_mag', 'accept')
m2_follow_zero <- pairedttable(d7[c(1,2:5)], colnames(d7[2:5]))
m2_follow_small <- pairedttable(d7[c(1,6:9)], colnames(d7[6:9]))
m2_follow_large <- pairedttable(d7[c(1,10:13)], colnames(d7[10:13]))
names(m2_follow_large)[3] <- 'pval'
list.save(m2_follow_zero, here::here('output', 'm2_follow_zero.rds'))
list.save(m2_follow_small, here::here('output', 'm2_follow_smallmag.rds'))
list.save(m2_follow_large, here::here('output', 'm2_follow_largemag.rds'))
rm(d6, d7, m2_follow_zero, m2_follow_small, m2_follow_large)

# model 3 - interaction between mag and val
m3 <- glmer(accept ~ deg_skew * magval + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m3, correlation = FALSE)
saveRDS(m3, here::here('output', 'm3.RDS'))

## compare model 3 to baseline
anova(b1.1, m3)

## follow-up t-tests
d8 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew', 'magval'))
d8$skew_magval <- interaction(d8$deg_skew, d8$magval)
d9 <- spread(d8[,c(1,9,5)], 'skew_magval', 'accept')
#neutral
m3_follow_neutral_0 <- pairedttable(d9[c(1,2:5)], colnames(d9[2:5]))
list.save(m3_follow_neutral_0, here::here('output', 'm3_follow_neutral_0.rds'))
#losses
m3_follow_loss_5 <- pairedttable(d9[c(1,6:9)], colnames(d9[6:9]))
names(m3_follow_loss_5)[3] <- 'pval'
list.save(m3_follow_loss_5, here::here('output', 'm3_follow_loss_5.rds'))
m3_follow_loss_05 <- pairedttable(d9[c(1,10:13)], colnames(d9[10:13]))
list.save(m3_follow_loss_05, here::here('output', 'm3_follow_loss_05.rds'))
#gains
m3_follow_gain_05 <- pairedttable(d9[c(1,14:17)], colnames(d9[14:17]))
list.save(m3_follow_gain_5, here::here('output', 'm3_follow_gain_05.rds'))
m3_follow_gain_5 <- pairedttable(d9[c(1,18:21)], colnames(d9[18:21]))
names(m3_follow_gain_5)[3] <- 'pval'
list.save(m3_follow_gain_05, here::here('output', 'm3_follow_gain_5.rds'))
rm(d8, d9, m3_follow_neutral_0, m3_follow_loss_5, m3_follow_loss_05, m3_follow_gain_5, m3_follow_gain_05)

# model 4 - add Age
m4 <- glmer(accept ~ deg_skew * magval + Age + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m4, correlation = FALSE)
saveRDS(m4, here::here('output', 'm4.RDS'))

# compare model 5 to model 4
anova(m3,m4)

# Create table for manuscript
tab_model(b1, m1, m2, m3, m4, file = "study1table.doc")
write.table(t1, "t1.txt")

t1 <- matrix(nrow=15,ncol=7)
t1[1,]<- c('', '', 'Baseline', 'Model 1', 'Model 2', 'Model 3', 'Model 4')
t1[2,]<- c('Predictor', 'Comparison', '', '', '', '', '')
t1[,1]<- c('', 'Predictor', 'Degree of Skew', '', '', 'Valence', '', 'Magnitude','', 
           'Valence x Magnitude', '', '', '', 'Age', 'Fit')
t1[,2]<- c('', 'Comparison', 'Weak > Symmetric', 'Moderate > Symmetric', 'Strong > Symmetric', 
           'Gain > Neutral','Loss > Neutral', '$0.5 > $0', '$5 > $0','Lose $5 > Neutral $0', 
           '$Lose $0.5 > Neutral $0', 'Gain $0.5 > Neutral $0','Gain $5 > Neutral $0', '','')


