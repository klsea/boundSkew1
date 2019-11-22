# individual difference affective
# 11.20.19 KLS

# load required packages
library(here)
library(ggplot2)
library(Hmisc)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'isolate_measure.R'))
source(here('scr', 'SummarySE.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))
dict <- read.csv(here("data", "bound_skew1_data_dictionary.csv"))

# isolate AVI data
Sys.setlocale('LC_ALL','C')
qs <- as.character(dict$Question)
first <- grep("Enthusiastic", qs)
last <- grep("Lonely", qs)
#last <- grep("Serene", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# add labels to AVI data
d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Lonely", dict)
#d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Serene", dict)
d1 <- score_avi(d0)

# separate skew
d3 <- isolate_skew(dt,c(1,2),10:69)
d4 <- clean_skew2(d3)

#recode response to acceptance
d4$accept <- d4$response - 1

# add skew count variable
d5 <-count_skew(d4)
rm(d3, d4)

#merge data frames
d3 <- merge(d5,d1, by = 'ID')
d3 <- d3[c(1,5,2:4,6:11)]
d3$skew_count <- as.integer(as.character(d3$skew_count))
rm(d1,d5)

# correlations
s1_corr <- rcorr(as.matrix(d3[4:11]))
saveRDS(s1_corr, here('output', 's1_corr3.RDS'))

# models
d3$magval <- interaction(d3$magnitude, d3$valence)
m1 <- lm(skew_count ~ hap + magval, data = d3)
summary(m1)
m2 <- lm(skew_count ~ lap + magval, data = d3)
summary(m2)
m3 <- lm(skew_count ~ la + magval, data = d3)
summary(m3)
m4 <- lm(skew_count ~ lan + magval, data = d3)
summary(m4)
m5 <- lm(skew_count ~ han + magval, data = d3)
summary(m5)
m6 <- lm(skew_count ~ ha + magval, data = d3)
summary(m6)

