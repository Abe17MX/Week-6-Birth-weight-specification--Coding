library(tidyverse)
library(here)
library(haven)
library(dplyr)
library(car)
library(ggplot2)

load(url("https://www.dropbox.com/s/cnwtcr096szm8im/omsba_5112_birthweight.rdata?raw=1"))
# This will load a data frame called "birthweight". You can rename it if you want

View(birthweight)

# VARIABLES DESCRIPTIONS:

# faminc = family income ($1,000s) 
# cigtax = "Cigarette tax in home state, 1988"
# cigprice = "Cigarette price in home state, 1988"
# bwght = birth weight (ounces)
# fatheduc = "Father's education (years)"
# motheduc = "Mother's education (years)"
# parity = birth order of baby (1 is first-born)
# male = 1 if male baby, 0 if female
# white = 1 if white baby, 0 else
# cigs = average daily cigarette consumption of the mother during pregnancy
# lbwght = "Natural log of birth weight"
# bwghtlbs = "Birth weight, pounds"
# packs = "Packs smoked per day while pregnant"
# lfaminc = "log(faminc)"

# Regression

# Original model
rm1 = lm(formula = bwght ~ cigs + faminc + male + white + parity, data = birthweight)
summary(rm1)

# Q1:

# transforming the family income variable to square
rm2 = lm(formula = bwght ~ cigs + faminc + I(faminc^2) + male + white + parity, data = birthweight)
summary(rm2)

# Plot between bwght and family income - linear
plot1 <- plot(birthweight$faminc, birthweight$bwght, xlab = "Family income",
              ylab = "Birth weigth in onzes", main = "Birth Weigth vs Family Income",
              xlim = c(0,100), ylim = c(10,200))

# Q2:
  
# As an alternative version, run the original equation, but with the log of birth weight as 
# the dependent outcome.

rm3 = lm(formula = log(bwght) ~ cigs + faminc + male + white + parity, data = birthweight)
summary(rm3)

# Q3

# No need for code


# Q4

rm4 = lm(formula = log(bwght) ~ cigs + lfaminc + male + white + parity, data = birthweight)
summary(rm4)


#Q5

# Say we want to know whether the effect of smoking varies by race. Run a model based on the 
# original model, and interpret your results.

rm5 = lm(formula = bwght ~ (cigs * white) + faminc + male + parity, data = birthweight)
summary(rm5)

# Q6

# Finally, we want to know if the effect of income varies by race. Run this model–again 
# based on the original model–and interpret the results.

rm6 = lm(formula = bwght ~ cigs + (faminc * white) + male + parity, data = birthweight)
summary(rm6)


#Q7

rm7 = lm(formula = bwght ~ cigs + faminc + male + white + parity, data = birthweight)
summary(rm7)







