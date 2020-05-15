# graphs for poster

# Individual differences analysis
# Create 11.25.19 KLS from prior scripts

# load required packages
library(here)
library(ggplot2)
library(tidyr)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here('output', 'individual_differences.csv'))
# graph constants
lg = 26 # text size
md = 20
sm = 14
custom_plot = list(theme(
  plot.title = element_text(size = lg, hjust = 0.5),
  axis.title.x = element_text(size = md), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = md), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = md), legend.text = element_text(size = sm), 
  legend.position='top', strip.text.x = element_text(size=md))
)

# graph affect vs. deliberative
d1 <- gather(dt[1:7], strategy, measurement, gut:math)

pdf('figs/strategy_poster.pdf', width = 7, height = 7)
ggplot(d1, aes(measurement, skew_count, color = strategy, fill = strategy)) + 
         geom_jitter(size=1, alpha=0.3, width=0.3) + 
  geom_smooth(method=lm) + 
  scale_fill_brewer(name='Strategy', palette="Set1", breaks=c('gut', 'math'), labels=c('Affective', 'Deliberative')) + 
  scale_color_brewer(name='Strategy', palette="Set1", breaks=c('gut', 'math'), labels=c('Affective', 'Deliberative')) + 
  scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + custom_plot + 
  xlab('Degree of Endorsement') + ylab('Skew Count') + ggtitle('Decision Strategy') + 
  annotate('text', x = 2, y = 1, label = 'r = 0.17 [0.04, 0.3]', color = '#CC0033', size = 8) + 
  annotate('text', x = 2, y = 8, label = 'r = -0.06 [-0.2, 0.07]', color = '#006699', size = 8)
dev.off()

# graph cognitive
d2 <- gather(dt[c(1:5, 18:19)], cognition, measurement, graph_lit:Numeracy)
labels <- c(graph_lit = "Graph Literacy", Numeracy = "Numeracy")
graph_text <- data.frame(measurement = 25, skew_count = 2, lab = 'r = -0.20, p = 0.004', 
                         cognition=factor('graph_lit', levels = c('graph_lit', 'Numeracy')))
num_text <- data.frame(measurement = 5, skew_count = 2, lab = 'r = -0.20, p = 0.003', 
                         cognition=factor('Numeracy', levels = c('graph_lit', 'Numeracy')))

pdf('figs/cognitive_poster.pdf', width = 7, height = 7)
ggplot(d2, aes(measurement, skew_count, color = cognition, fill = cognition)) + 
  geom_jitter(size=1, alpha=0.2, width=0.3) + 
  geom_smooth(method=lm) + 
  scale_fill_brewer(name='Strategy', palette="Set1") + 
  scale_color_brewer(name='Strategy', palette="Set1") + 
  scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + custom_plot +
  xlab('Cognitive Measurement') + ylab('Skew Count') + ggtitle('Cognitive Ability') +
  facet_grid(. ~ cognition, scales="free_x", labeller=labeller(cognition = labels)) + 
  geom_text(data = graph_text, label = 'r = -0.2 [-0.32, -0.06]') + 
  geom_text(data = num_text, label = 'r = -0.2 [-0.33, -0.07]') + theme(legend.position='none')
dev.off()

# graph real world
pdf('figs/realworld_poster.pdf', width = 7, height = 7)
ggplot(dt, aes(high_pressure, skew_count)) + 
  geom_jitter(size=1, alpha=0.2, width=0.3, color = '#009900') + 
  geom_smooth(method=lm, color = '#009900', fill = '#009900') + 
  scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + custom_plot +
  xlab('Degree of Endorsementt') + ylab('Skew Count') + ggtitle('Real World Relationships') +
  annotate('text', x = 3, y = 1, label = 'r = -0.16 [-0.29, -0.02]', color = '#009900', size = 8)
dev.off()

