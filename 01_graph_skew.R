# graph skew data
# 6.10.19 KLS

# load required packages
library(here)
library(ggplot2)
library(gdata)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'SummarySE.R'))
source(here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),10:69)
d1 <- clean_skew(d0)

# recode response to acceptance
d1$accept <- d1$response - 1

# reorder deg_skew factor
d1$deg_skew <- factor(d1$deg_skew, levels = c('Symmetric', 'Weak', 'Moderate', 'Strong'))

# make magnitude a factor
d1$magnitude <- factor(d1$magnitude)

# age effects on acceptance rates
ggplot(d1, aes(Age, accept, colour = deg_skew, fill = deg_skew)) + geom_smooth(method=lm) +
  theme(legend.position = 'top') + facet_grid(magnitude ~ valence)

# create summary - main effect of degree of skewness
d2 <- summarySE(data=d1, measurevar = 'accept', groupvars='deg_skew')

p1 <- ggplot(d2, aes(deg_skew, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Acceptance Rate')  + expand_limits(y=c(-.06, 1)) + 
  theme(legend.position = 'none') + scale_fill_discrete(name = 'Skewness') +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), legend.justification = c(1,1), legend.position=c(1,1),) #+ xlab('Degree of Skewness')

# create summary - add interaction with valence of gamble
d3 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence','deg_skew'))

p4 <- ggplot(d3, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')

# create summary - add interaction with magnitude of gamble
d4 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('magnitude','deg_skew'))

p5 <- ggplot(d4, aes(magnitude, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')

# create summary - 3 way interaction 
d5 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence', 'magnitude', 'deg_skew'))

ggplot(d5, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + facet_wrap(~ magnitude)

# create summary - interaction between magnitude and valence 

# make interaction term
d1$magval <- interaction(d1$valence, d1$magnitude)
d1$magval <- drop.levels(d1$magval)
d1$magval <- factor(d1$magval, levels = c('loss.5', 'loss.0.5', 'neutral.0', 'gain.0.5', 'gain.5'))
levels(d1$magval)[1] <- '-$5.00'
levels(d1$magval)[2] <- '-$0.50'
levels(d1$magval)[3] <- '$0.00'
levels(d1$magval)[4] <- '+$0.50'
levels(d1$magval)[5] <- '+$5.00'

d6 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('magval', 'deg_skew'))

p2 <- ggplot(d6, aes(magval, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  xlab('Valence by Magnitude Interaction') +theme_minimal() + theme(axis.title.y = element_blank(), legend.position = 'none') +
  expand_limits(y=1) 

p3 <- multiplot(p1, p2, layout = matrix(c(1,2,2,2), nrow=1, byrow=TRUE))

pdf('figs/deg_skew_plot1.pdf', width = 12, height = 6)
multiplot(p1, p2, layout = matrix(c(1,2,2,2), nrow=1, byrow=TRUE))
dev.off()

# poster plots
# graph constants
lg = 26 # text size
md = 20
sm = 14

p1 <- ggplot(d2, aes(deg_skew, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Acceptance Rate')  + expand_limits(y=c(-.06, 1)) + 
  scale_fill_discrete(name = 'Skewness') +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), 
        axis.title.y = element_text(size = lg), axis.text.y = element_text(size = md), 
        legend.title = element_text(size = md), legend.text = element_text(size = sm),
        legend.justification = c(1,1), legend.position=c(1,1),) 

p2 <- ggplot(d6, aes(magval, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  xlab('Valence by Magnitude Interaction') +theme_minimal() + 
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = md), 
        legend.position = 'none', 
        axis.title.x = element_text(size = lg), axis.text.x = element_text(size = md), 
        ) + expand_limits(y=1) 

pdf('figs/deg_skew_poster.pdf', width = 12, height = 9)
multiplot(p1, p2, layout = matrix(c(1,2,2,2), nrow=1, byrow=TRUE))
dev.off()
