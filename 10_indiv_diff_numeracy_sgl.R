# individual differences in numeracy and graph literacy
# 11.25.19 KLS

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

# isolate graph literacy
Sys.setlocale('LC_ALL','C')
qs <- as.character(dict$Question)
first <- grep("bar charts", qs)
last <- grep("newspapers", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# graph literacy score
d1 <- score_graph_lit(d0)
rm(d0)

# isolate numeracy
first <- grep("six-sided", qs)
last <- grep("10,000 doctors", qs)
d2 <- isolate_skew(dt,c(1,2), first:last)

# score numeracy
ans_key <- create_num_answer_key()
d3 <- as.data.frame(score_num(d2, ans_key))
rm(ans_key, d2)

# separate skew
d4 <- isolate_skew(dt,c(1,2),10:69)
d5 <- clean_skew2(d4)

#recode response to acceptance
d5$accept <- d5$response - 1

# add skew count variable
d6 <-count_skew(d5)
rm(d4, d5)

# merge data frames
d7 <- merge(d6, d1, by = 'ID')
d8 <- merge(d7, d3, by = c('ID', 'Age'))
rm(d1,d3,d6,d7)

# correlations
s1_corr <- rcorr(as.matrix(d8[c(2,5:7)]))
saveRDS(s1_corr, here('output', 's1_corr4.RDS'))

# models
d8$magval <- interaction(d8$magnitude, d8$valence)
d8$skew_count <- as.integer(as.character(d8$skew_count))
m1 <- lm(skew_count ~ graph_lit + magval, data = d8)
summary(m1)
m2 <- lm(skew_count ~ Numeracy + magval, data = d8)
summary(m2)
