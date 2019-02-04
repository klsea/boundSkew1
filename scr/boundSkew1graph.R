# graph skew bound 1 data
# 2.4.19 KLS

# import libraries and set working directory
rm(list=ls())
setwd('~/R_Projects/boundSkew1/data')
library(ggplot2)

# load functions
source('~/Dropbox (Personal)/Functions/SummarySE.R')

# create summary
d0 <- na.omit(dt)
d1 <- summarySE(data=d0, measurevar = 'accept', groupvars=c('valence','deg_skew'))
d1$deg_skew
d1$deg_skew <- factor(d1$deg_skew, levels = c('Weak', 'Moderate', 'Strong'))

ggplot(d1, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9))

