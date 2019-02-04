# clean boundary skew 1 data set
# KLS 1.11.19

# import libraries and set working directory
rm(list=ls())
setwd('~/R_Projects/boundSkew1/data')
library(reshape2)

# import data
d0 <- read.csv('Skew_boundary_raw_numeric.csv', na.strings = c('', 'NA'))

# remove first two rows (not data)
d0 <- d0[3:nrow(d0),]

# limit skew trials for now
d1 <- d0[c(grep('ResponseId', colnames(d0)), grep('Age', colnames(d0)), grep('0_95_5', colnames(d0)):grep('n5_55_45', colnames(d0)) )]

# remove incomplete records
d1 <- d1[complete.cases(d1),]

# recode age into actual age
d1$Age <- as.integer(as.character(d1$Age)) + 19

# make data long
d2 <- melt(d1, id.vars = 'ResponseId', variable.name = 'gamble')

# recode value into acceptance - 0 (for reject) and 1 (for accept)
d2$accept <- as.numeric(d2$value) - 1

# add condition names 
# valence (gain/loss/neutral)
d2$v1 <- as.factor(t(as.data.frame(strsplit(as.character(d2$gamble), '_')))[,1])
d2$valence[d2$v1 == 'X0'] <- 'neutral'
d2$valence[d2$v1 == 'X0.5'] <- 'gain'
d2$valence[d2$v1 == 'X5'] <- 'gain'
d2$valence[d2$v1 == 'n0.5'] <- 'loss'
d2$valence[d2$v1 == 'n5'] <- 'loss'

# magnitude (0, 0.5, 5)
d2$magnitude[d2$v1 == 'X0'] <- 0
d2$magnitude[d2$v1 == 'X0.5'] <- 0.5
d2$magnitude[d2$v1 == 'X5'] <- 5
d2$magnitude[d2$v1 == 'n0.5'] <- 0.5
d2$magnitude[d2$v1 == 'n5'] <- 5
d2$v1 <- NULL

# deg of skew (low, medium, high)
d2$v2 <- as.factor(t(as.data.frame(strsplit(as.character(d2$gamble), '_')))[,2])

d2$deg_skew[d2$v2 == 95 | d2$v2 == 5] <- 'Strong'
d2$deg_skew[d2$v2 == 90 | d2$v2 == 10] <- 'Strong'
d2$deg_skew[d2$v2 == 85 | d2$v2 == 15] <- 'Strong'
d2$deg_skew[d2$v2 == 80 | d2$v2 == 20] <- 'Moderate'
d2$deg_skew[d2$v2 == 75 | d2$v2 == 25] <- 'Moderate'
d2$deg_skew[d2$v2 == 70 | d2$v2 == 30] <- 'Moderate'
d2$deg_skew[d2$v2 == 65 | d2$v2 == 35] <- 'Weak'
d2$deg_skew[d2$v2 == 60 | d2$v2 == 40] <- 'Weak'
d2$deg_skew[d2$v2 == 55 | d2$v2 == 45] <- 'Weak'

d2$v2 <- NULL


#save data frame
write.csv(d2, 'boundSkew1Long.csv', row.names = FALSE)
