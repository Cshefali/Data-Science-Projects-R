#Linear Regression in 1 variable- Income vs Happiness
#Last Update- Dec 5, 2023

library(tidyverse)
library(broom)
library(caTools) #for sample.split()


#working directory
wd <- getwd()

#imaginary sample of 500 people & their income (in $10,000) and happiness index
income_data <- readr::read_csv(
                paste0(wd, "/linear_regr_income_data/income.data.csv"))

#first column containing row numbers removed
income_data <- income_data[,2:3]

#summary statistics of income data
summary(income_data)


#returns logical TRUE for data points selected as train set, otherwise F
x <- caTools::sample.split(Y = income_data$income, SplitRatio = 0.8)
#create training set
train_set <- income_data[x,]
#test set
test_set <- income_data[!x,]


#Checking normality of dependent variable- happiness
#Normally distributed
ggplot(data = train_set, aes(x = happiness))+
  geom_histogram(bins = 10, color = "black", fill = "midnightblue")+
  theme_bw()

#scatter plot to see correlation between income and happiness score
ggplot(data = train_set, aes(x = income, y = happiness))+
  geom_jitter(color = "midnightblue")+
  #regression line
  geom_smooth(color = "red", se = F)+
  theme_bw()

#fitting a linear model
income_model <- stats::lm(happiness ~ income, data = train_set)

#the model output
summary(income_model)

#checking homoscedascity of the model- this creates a diagnostic plot
#creates 4 graphs
plot(income_model)

#METHOD 2 OF PLOTTING LINEAR MODEL
##first, scatter plot of data points
plot(income_data$income, income_data$happiness)
##second, abline plot of linear model
abline(income_model)

test_income <- data.frame(income = test_set$income)
target_happiness <- data.frame(happiness = test_set$happiness)

#predicting on unknown data points
predicted_happiness <- data.frame(prediction = stats::predict(income_model, test_income))

#compare the resutls of prediction with target happiness