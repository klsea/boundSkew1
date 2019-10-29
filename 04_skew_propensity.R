# individual difference measures skew
# 10.29.19 KLS

# load required packages
library(here)
library(ggplot2)

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
d2$skew_count_f <- ordered(d2$skew_count, 
                           levels= c('0','1','2','3','4','5','6','7','8','9','10'))
d2$magval <- interaction(d2$valence, d2$magnitude)

# examine distribution
ggplot(d2, aes(skew_count_f, fill = magval)) + geom_histogram(stat='count', binwidth=.5, position="dodge")
