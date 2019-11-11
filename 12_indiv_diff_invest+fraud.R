# investment and fraud
# 11.5.19 KLS

# load required packages
library(here)
library(ggplot2)
library(Hmisc)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# identify begining and end of 
d1 <- isolate_skew(dt, 1, grep('Q173', colnames(dt)):grep('Q27', colnames(dt)))

# investment & fraud
d2 <- data.frame(d1$ID, (d1$Q173 - 2)*-1, d1$Q177, d1[11:14])
colnames(d2) <- c('ID', 'lost_invest', 'why_lost', 'detect_fraud', 'likely_fraud', 'high_pressure', 'avoid_fraud')

# separate skew
d0 <- isolate_skew(dt,c(1,2),10:69)
d3 <- clean_skew2(d0)

#recode response to acceptance
d3$accept <- d3$response - 1

# add skew count variable
d4 <-count_skew(d3)
rm(d0, d1, d3)

# merge data frames
d5 <- merge(d4, d2, by = 'ID')

# correlations
s1_corr2 <- rcorr(as.matrix(d5[c(4:5, 7:9)]))
saveRDS(s1_corr2, here('output', 's1_corr2.RDS'))

# models
d5$magval <- interaction(d5$magnitude, d5$valence)
d5$skew_count <- as.numeric(as.character(d5$skew_count))
m1 <- lm(skew_count ~ lost_invest + magval, data = d5)
summary(m1)
m2 <- lm(skew_count ~ detect_fraud + magval, data = d5)
summary(m2)
m3 <- lm(skew_count ~ likely_fraud + magval, data = d5)
summary(m3)
m4 <- lm(skew_count ~ high_pressure + magval, data = d5)
summary(m4)
