##Trying out Logistic Regression

##Source- https://stats.oarc.ucla.edu/r/dae/logit-regression/
##Last Update- June 24, 2024

library(tidyverse)

#read csv file from web-source
data <- readr::read_csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#Description for variables:
#1. gre- GRE score
#2. gpa- GPA in under-graduation.
#3. rank- Rank of undergraduation college. (quantifier for how good the uni was)
#4. admit- 1 for should be admitted; 0- shouldn't be admitted for PG course

#general description of all variables
summary(data)

#Apply standard deviation of each column
sapply(data, sd)

## two-way contingency table of categorical outcome and predictors we want
xtabs(~admit + rank, data = data)

#Convert rank to factor- so that it gets treated as categorical
data$rank <- as.factor(data$rank)

##Using glm()- Generalized Linear Model

log_regression <- glm(admit ~ gre+gpa+rank, data = data, 
                      family = "binomial")

#Get the summary of results
summary(log_regression)

#To get the Confidence-Intervals for Coefficient-estimates.
#CI using profiled log-likelihood function
confint(log_regression)

#CIs using default Standard Errors
confint.default(log_regression)