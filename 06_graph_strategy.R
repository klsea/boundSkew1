# graph strategy data
# 10.29.19 KLS

# load required packages
library(here)
library(ggplot2)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'multiplot.R'))
source(here('scr', 'colorize_variable.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate strategy
d0 <- isolate_skew(dt,c(1,2),70:75)
colnames(d0) <- c('ID', 'Age', 'gut', 'math', 'win.money', 'lose.money', 'win.likely', 'lose.likely')

gut_color <- colorize_variable(d0$gut)
p1 <- ggplot(d0, aes(Age, gut))
p2 <- ggplot(d0, aes(gut, fill = gut_color))
fancy_graph(p1,p2, gut_color, 'Gut Feeling')

math_color <- colorize_variable(d0$math)
p1 <- ggplot(d0, aes(Age, math))
p2 <- ggplot(d0, aes(math, fill = math_color))
fancy_graph(p1,p2, math_color, 'Mathematically')

win.money_color <- colorize_variable(d0$win.money)
p1 <- ggplot(d0, aes(Age, win.money))
p2 <- ggplot(d0, aes(win.money, fill = win.money_color))
fancy_graph(p1,p2, win.money_color, 'Win Money')

lose.money_color <- colorize_variable(d0$lose.money)
p1 <- ggplot(d0, aes(Age, lose.money))
p2 <- ggplot(d0, aes(lose.money, fill = lose.money_color))
fancy_graph(p1,p2, lose.money_color, 'Lose Money')

win.likely_color <- colorize_variable(d0$win.likely)
p1 <- ggplot(d0, aes(Age, win.likely))
p2 <- ggplot(d0, aes(win.likely, fill = win.likely_color))
fancy_graph(p1,p2, win.likely_color, 'Win Likely')

lose.likely_color <- colorize_variable(d0$lose.likely)
p1 <- ggplot(d0, aes(Age, lose.likely))
p2 <- ggplot(d0, aes(lose.likely, fill = lose.likely_color))
fancy_graph(p1,p2, lose.likely_color, 'Lose Likely')

#
d0$diffstrategy <- d0$math - d0$gut
ggplot(d0, aes(Age, diffstrategy)) + geom_point() + geom_smooth(method = 'lm', fill = 'blue') +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = 'red', fill = 'red')) 



