# individual difference analysis
# 10.31.19 KLS

# load required packages
library(here)
library(Hmisc)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),10:69)
d1 <- clean_skew2(d0)

#recode response to acceptance
d1$accept <- d1$response - 1

# add skew count variable
d2 <-count_skew(d1)
rm(d0, d1)

# separate strategy
d0 <- isolate_skew(dt,c(1,2),70:75)
colnames(d0) <- c('ID', 'Age', 'gut', 'math', 'win.money', 'lose.money', 'win.likely', 'lose.likely')

# merge data frames
d3 <- merge(d2,d0, by = 'ID')
d3 <- d3[c(1,5,2:4,6:11)]
d3$skew_count <- as.integer(as.character(d3$skew_count))
rm(d0,d2)

# correlations
s1_corr <- rcorr(as.matrix(d3[5:11]))
saveRDS(s1_corr, here('output', 's1_corr.RDS'))

# models
d3$magval <- interaction(d3$magnitude, d3$valence)
m1 <- lm(skew_count ~ gut + magval, data = d3)
summary(m1)
m2 <- lm(skew_count ~ win.money + magval, data = d3)
summary(m2)
m3 <- lm(skew_count ~ lose.money + magval, data = d3)
summary(m3)
m4 <- lm(skew_count ~ lose.likely + magval, data = d3)
summary(m4)
m5 <- lm(skew_count ~ math + magval, data = d3)
summary(m5)
m6 <- lm(skew_count ~ win.likely + magval, data = d3)
summary(m6)



