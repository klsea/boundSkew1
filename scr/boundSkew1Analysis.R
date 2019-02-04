# analyze skew bound 1 data
# 2.4.19 KLS

# import libraries and set working directory
rm(list=ls())
setwd('~/R_Projects/boundSkew1/data')
library(lme4)

# load data
dt <- read.csv('boundSkew1Long.csv')

# Scale age, helps model converge because so many interactions
dt$Age = scale(dt$Age, scale=T)


