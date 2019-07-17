# score questionnaire data
# 7.5.19 KLS & SA

# load required packages
library(here)

# load source functions
source(here("scr", "isolate_measure.R"))

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))
dd <- read.csv(here("data", "bound_skew1_data_dictionary.csv"))

# AVI # Ask greg about scoring
avi <- isolate_measure(dt, "ACTUALLY", dd)
avi <- add_correct_avi_labels(avi, "ACTUALLY", dd)
avi <- score_avi(avi)

# Graph Literacy

graph_lit <- isolate_measure2(dt, "How good", "When reading", dd)
graph_lit <- add_graph_lit_labels(graph_lit, "How good", "When reading", dd)
