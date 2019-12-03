# graph strategy data
# 10.29.19 KLS

# load required packages
library(here)
library(ggplot2)
library(matrixStats)
library(tidyr)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'multiplot.R'))
source(here('scr', 'colorize_variable.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate strategy
d0 <- isolate_skew(dt,c(1,2),70:75)
colnames(d0) <- c('ID', 'Age', 'gut', 'math', 'win.money', 'lose.money', 'win.likely', 'lose.likely')

# Graph means
d1 <- cbind(colMeans(d0), apply(d0, 2, sd), apply(d0, 2, sd)/sqrt(nrow(d0)))
d1 <- d1[3:nrow(d1),]
colnames(d1) <- c('Mean', 'SD', 'SE')
names <- rownames(d1); rownames(d1) <- NULL
d1 <- cbind(names, d1)
d1 <- data.frame(d1)
d1$Mean <- as.numeric(as.character(d1$Mean))
d1$SD <- as.numeric(as.character(d1$SD))
d1$SE <- as.numeric(as.character(d1$SE))
d1$names <- factor(d1$names, levels = c('gut', 'math', 'win.money', 'lose.money', 
                                        'win.likely', 'lose.likely'))

means <- ggplot(d1, aes(names, Mean, fill = names)) + geom_bar(stat='identity') + 
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean + SE), width = .2, position=position_dodge(.9)) +
  theme_minimal() + theme(legend.position = 'none') + xlab("Strategy") + ylab('Average Rating') +
  expand_limits(y=c(1,5)) + geom_vline(aes(xintercept=2.5)) + 
  annotate("text", x = 1.2, y = 5, label = "General Strategies") +
  annotate("text", x = 4.2, y = 5, label = "Information Strategies")
#ggsave('strategy1.pdf', path = 'figs/')

# statistical tests
# compare gut vs mathematical strategy
gutvmath <- t.test(d0$gut, d0$math, paired = TRUE)
saveRDS(gutvmath, here('output', 'gutvmath1.RDS'))

# compare aspects of gamble
d2 <- gather(d0, question, rating, win.money:lose.likely, factor_key = TRUE)[,c(1:2, 5:6)]
d3 <- cbind(d2, t(as.data.frame(strsplit(as.character(d2$question), '[.]'))))
colnames(d3) <- c( "ID", "Age", "question","rating","valence", "parameter")
rownames(d3) <- NULL

valvparam <- aov(rating ~ valence * parameter + Error(ID), data = d3)
summary(valvparam)
saveRDS(valvparam, here('output', 'valvparam1.RDS'))

d0$diffstrategy <- d0$math - d0$gut



