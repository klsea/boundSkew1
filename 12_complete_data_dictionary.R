# Clean skew data
# 5.14.20 KLS 

# load required packages
library(here)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))
dd <- read.csv(here("data", "bound_skew1_data_dictionary.csv"))

# add and populate variable names column
dd[,'Variable Names'] <- NA
dd$`Variable Names`[1:9] <- c(as.character(dd$Variable[1:7]), 'Practice 1', 'Practice 2')

gambles <- list(a = (10:19), b = (22:31), c = (34:43), d = (46:55), e = (58:67))

for (g in 1:length(gambles)){
  print(g)
  b <- rep(NA, length(gambles[g][[1]]))
  for (x in 1:length(gambles[g][[1]])){
    print(x)
    y <- gambles[g][[1]][x]
    z <- as.character(dd$Variable[y])
    a <- strsplit(z, '_')
    b[x] <- paste0(sub('X', '', a[[1]][1]), '_EV_', a[[1]][2], '_', a[[1]][3], '_Gamble')
  }
  dd$`Variable Names`[gambles[g][[1]]] <- b
}
rm(a, b, g, x, y, z, gambles)

dd$`Variable Names`[c(20:21, 32:33, 44:45, 56:57, 68:69)] <- paste0('Catch_', seq(1,10))
dd$`Variable Names`[c(70:75)] <- paste0('Strategy_', seq(1,6))
dd$`Variable Names`[c(76:84)] <- paste0('Investment_', seq(1,9))
dd$`Variable Names`[c(85:93)] <- paste0('Fraud_', seq(1,9))
dd$`Variable Names`[c(94:117)] <- paste0('AVI_', seq(1,24))
dd$`Variable Names`[c(118:132)] <- paste0('Numeracy_', seq(1,14))
dd$`Variable Names`[c(133:142)] <- paste0('Graph_Lit_', seq(1,10))
dd$`Variable Names`[143] <- 'DOB'

# add measurement units
dt[c(77,78, 81, 84, 91, 93, 123:128, 130:131)] <- sapply(dt[c(77,78, 81, 84, 91, 93, 123:128, 130:131)], as.character)
dd$`Measurement Units` <- sapply(dt, class)
dd$`Measurement Units`[143] <- 'mm/dd/yyyy'

# Create and populate allowed_values in data dictionary
dd[,'Allowed Values'] <- NA
dd$`Allowed Values`[1] <- '1-209'
dd$`Allowed Values`[2] <- '22-85'
dd$`Allowed Values`[c(3, 8:69, 76, 88, 129)] <- '1-2'
dd$`Allowed Values`[c(4, 83, 92, 133:142)] <- '1-6'
dd$`Allowed Values`[5] <- '1-8'
dd$`Allowed Values`[6] <- '1-16'
dd$`Allowed Values`[c(7,70:75, 79:80, 94:117)] <- '1-5'
dd$`Allowed Values`[c(77, 89, 118:119)] <- 'Positive integers'
dd$`Allowed Values`[78] <- 'Monetary amount'
dd$`Allowed Values`[c(81, 84, 91, 93, 123:128, 130:131)] <- 'Text'
dd$`Allowed Values`[c(83,121:122, 132)] <- '1-3'
dd$`Allowed Values`[c(85:87, 90)] <- '1-7'
dd$`Allowed Values`[120] <- 'decimal'
dd$`Allowed Values`[143] <- 'mm/dd/yyyy'

# Create and populate description of variable
dd[,'Description of Variable'] <- NA 
dd$`Description of Variable`[2] <- 'Age in years'
dd$`Description of Variable`[3] <- "1 = Male, 2 = Female"
dd$`Description of Variable`[4] <- "1 = Middle School, 2 = High School Diploma, 3 = Some College, 
4 = Bachelor's Degree, 5 = Master's Degree, 6 = Doctoral Degree" 
dd$`Description of Variable`[5] <- "1 = White/Caucasian, 2 = Black/African American, 3 = Asian, 4 = Hispanic/Latino, 
5 = American Indian/Alaska Native, 6 = Pacific Islander, 7 = Multiracial, 8 = Other"
dd$`Description of Variable`[6] <- '1 = less than $10,000, 2 = $10,000-$19,999, 
3 = $20,000-$29,999, 4 = $30,000-$39,999, 5 = $40,000-$49,999, 6 = $50,000-$59,999, 7 = $60,000-$69,999, 
8 = $70,000-$79,999, 9 = $80,000-$89,999, 10 = $90,000-$99,999, 11 = $100,000-$109,999, 12 = $110,000-$119,999, 
13 = $120,000-$129,999, 14 = $130,000-$139,999, 15 = $140,000-$149,999, 16 = $150,000 or more'
dd$`Description of Variable`[7] <- "1 = Not healthy at all, 5 = Very healthy"
dd$`Description of Variable`[8:69] <- "1 = certain, 2 = gamble"
dd$`Description of Variable`[70:75] <- '1 = Strongly Disagree, 2 = Somewhat Disagree, 3 = 
Neutral, 4 = Somewhat Agree, 5 = Strongly Agree'
dd$`Description of Variable`[c(76, 88)] <- '1 = Yes, 2 = No'
dd$`Description of Variable`[79] <- "1 = Less than a year ago, 2 = 1-3 years ago, 3 = 4-5 years ago, 
4 = More than 5 years ago, 5 = Don't know/can't remember"
dd$`Description of Variable`[80] <- " 1 = I just made a bad investment, 
2 = The market took a downward turn, 
3 = I didn'tknow enough about the type of investment I was making, 
4 = I was misled or defrauded by the person or organization that sold me the investment, 
5 = Other" 
dd$`Description of Variable`[81] <- 'This is the explanatory text they completed if they chose other on the previous quesiton.'
dd$`Description of Variable`[82] <- '1 = Yes, 2 = No, 3 = Not applicable'
dd$`Description of Variable`[84] <- 'This is the explanatory text they completed if they chose other on the previous quesiton.'
dd$`Description of Variable`[85] <- '1 = not at all able to detect, 7 = very able to detect'
dd$`Description of Variable`[86] <- '1 = not at all likely, 7 = very likely'
dd$`Description of Variable`[87] <- '1 = not at all able to resist, 7 = very able to resist'
dd$`Description of Variable`[89] <- 'This is the explanatory text they answered yes on the previous quesiton.'
dd$`Description of Variable`[90] <- '1 = Mail, 2 = Phone call, 3 = Email, 4 = Website, 5 = Seminar, 
6 = Other, 7 = Not applicable'
dd$`Description of Variable`[92] <- '1 = Stranger, 2 = Co-worker, 3 = Friend, 4 = Family member, 5 = Other, 6 = Not applicable'
dd$`Description of Variable`[94:117] <- '1 = Never, 2 = A small amount of time, 3 = Half of the time, 4 = Most of the time, 5 = All of the time'
dd$`Description of Variable`[121] <- '1 = 1 in 100, 2 = 1 in 1000, 3 = 1 in 10'
dd$`Description of Variable`[122] <- '1 = 1%, 2 = 10%, 3 = 5%'
dd$`Description of Variable`[129] <- '1 = 1 chance in 12, 2 = 1 chance in 37' 
dd$`Description of Variable`[132] <- '1 = The both tested positive for SARS and therefore are equally likely to have the disease, 
2 = They both tested positive for SARS and the doctor is more likely t0 have the disease, 
3 = They both tested positive for SARS and the person in the at-risk population is more likely to have the disease'
dd$`Description of Variable`[133:138] <- '1 = Not good at all, 6 = Extremely good' 
dd$`Description of Variable`[139] <- '1 = Not at all, 6 = Much easier'
dd$`Description of Variable`[140] <- '1 = Never, 6 = Very often'
dd$`Description of Variable`[141:142] <- '1 Not at all, 6 = Extremely' 

dd$`Description of Variable` <- sapply(dd$`Description of Variable`, as.character)
dd$`Description of Variable`[is.na(dd$`Description of Variable`)] <- ''

# Reorder columns
dd <- dd[c(1,3,4,5,2,6)]

write.csv(dd, here("data", "bound_skew1_data_dictionary.csv"), row.names = FALSE)

