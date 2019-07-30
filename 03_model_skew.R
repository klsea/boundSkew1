# run models on skew data
# 7.15.19 KLS

# load required packages
library(here)
library(lme4)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(9,12),20:79)
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

# model 1 - only degree of skew
m1 <- glmer(accept ~ deg_skew + (1 + Age | ResponseId), data = d1, family = binomial(link = logit), nAGQ = 1, 
               control=glmerControl(optimizer='bobyqa'))
summary(m1, correlation = FALSE)

# model 2 - add valence
m2 <- glmer(accept ~ deg_skew + valence + (1 + Age | ResponseId), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m2, correlation = FALSE)

# model 3 - add Age
m3 <- glmer(accept ~ deg_skew + valence + Age + (1 + Age | ResponseId), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m3, correlation = FALSE)

# Does magnitude make a difference?

# model 4 - interaction between mag and val
m4 <- glmer(accept ~ deg_skew + magval + Age + (1 + Age | ResponseId), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m4, correlation = FALSE)

# model 4.1 -  magnitude instead of valence
m4.1 <- glmer(accept ~ deg_skew  + magnitude + Age + (1 + Age | ResponseId), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m4.1, correlation = FALSE)


