# Aging Follow-Up Analysis
# 4.3.20 KLS

# load required packages
library(here)
library(lme4)
library(gdata)
library(tidyr)
library(rlist)

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

# make interaction term
d1$magval <- interaction(d1$valence, d1$magnitude)
d1$magval <- drop.levels(d1$magval)
d1$magval <- factor(d1$magval, levels = c('neutral.0', 'loss.5', 'loss.0.5', 'gain.0.5', 'gain.5'))

# limit to only participants in the EV = 0 condition
d2 <- d1[which(d1$magnitude == 0),]
d2$ID <- factor(d2$ID)
ev0_m_age <- mean(d2$Age)
ev0_hist_age <- hist(d2[which(d2$deg_skew == 'Symmetric'),]$Age) # limits to one line per particiapnt

# scale and center age
d2$Age <- scale(d2$Age)

# baseline - only degree of skew
b1 <- glmer(accept ~ deg_skew + (1 + Age | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
# model doesn't converge
#summary(b1, correlation = FALSE)

## boundary fit - remove age from random effects
b1.1 <- glmer(accept ~ deg_skew + (1 | ID), data = d2, family = binomial(link = logit), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(b1.1, correlation = FALSE)
#saveRDS(b1.1, here::here('output', 'baseline.RDS'))

## boundary fit - add age 
m1 <- glmer(accept ~ deg_skew * Age + (1 | ID), data = d2, family = binomial(link = logit), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(m1, correlation = FALSE)

# compare symmetric to 25%/75% gamble
d3 <- d2[which(d2$gamble == 'X0_25_75'),]
d4 <- d2[which(d2$gamble == 'X0_50_50'),]

t.test(d3$response, d4$response, paired = TRUE)


