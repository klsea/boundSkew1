# run models on skew data
# 7.15.19 KLS updated 10.17.19

# load required packages
library(here)
library(sjPlot)

# read in model fits 
b <- readRDS(here('output', 'baseline.RDS'))
m1 <- readRDS(here('output', 'm1.RDS'))
m2 <- readRDS(here('output', 'm2.RDS'))
m3 <- readRDS(here('output', 'm3.RDS'))
m4 <- readRDS(here('output', 'm4.RDS'))

# make table
t2 <- tab_model(b,m1,m2,m3,m4, 
                dv.labels = c('Baseline', 'Model 1', 'Model 2', 'Model 3', 'Model 4'), 
                pred.labels = c('Intercept', 'Degree of Skew (Weak)', 'Degree of Skew (Moderate)', 'Degree of Skew (Strong)', 'Valence (Gain)', 'Valence (Loss)', 'Magnitude (0.5)', 'Magnitude (5)', "Magnitude x Valence (Gain x 0.5)", "Magnitude x Valence (Loss x 0.5)", "Magnitude x Valence (Gain x 5)", "Magnitude x Valence (Loss x 5)", "Age"))
t2

# create table of chi square values comparing models
chi <- c(anova(b,m1)[6][2,], anova(b,m2)[6][2,], anova(b,m3)[6][2,], anova(m3,m4)[6][2,])
df <- c(anova(b,m1)[7][2,], anova(b,m2)[7][2,], anova(b,m3)[7][2,], anova(m3,m4)[7][2,])
p <- c(anova(b,m1)[8][2,], anova(b,m2)[8][2,], anova(b,m3)[8][2,], anova(m3,m4)[8][2,])
p <- round(p, 3)
n <- rep(b@Gp[2],4)
chi <- data.frame(df, n, chi, p)
rownames(chi) <- c('b_m1', 'b_m2', 'm1_m3', 'm3_m4')
write.csv(chi, here('output', 's1_chi_squared.csv'))
