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

custom_plot = list(theme(
  plot.title = element_text(size = 24),
  axis.title.x = element_text(size = lg), axis.text.x = element_text(size = sm),
  axis.title.y = element_text(size = lg), axis.text.y = element_text(size = sm), 
  legend.title = element_text(size = lg), legend.text = element_text(size = sm))
)

# graph affect vs. deliberative
d1 <- gather(dt[1:7], strategy, measurement, gut:math)

ggplot(d1, aes(measurement, skew_count, color = strategy, fill = strategy)) + 
         geom_jitter(size=1, alpha=0.3, width=0.3) + 
         geom_smooth(method=lm) + 
         scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + 
         xlab('Strategy') + ylab('Skew Count')


# graph cognitive
d2 <- gather(dt[c(1:5, 18:19)], cognition, measurement, graph_lit:Numeracy)

ggplot(d2, aes(measurement, skew_count, color = cognition, fill = cognition)) + 
  geom_jitter(size=1, alpha=0.2, width=0.3) + 
  geom_smooth(method=lm) + 
  scale_y_continuous(breaks = c(3, 6, 9)) + theme_minimal() + 
  xlab('Cognition') + ylab('Skew Count') + facet_grid(. ~ cognition, scales="free_x")

# graph real world
                                              