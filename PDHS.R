#####################################################################################
# Purpose: Recoding or creating new variables.
# Data input: The model dataset women's file (IR file)
# Author: Shireen Assaf
# Date Last Modified: Jan 26, 2023 by Shireen Assaf

# Notes/instructions:
# Please have the model dataset downloaded and ready to use. We will use the IR file.
# You can download the model dataset here: https://www.dhsprogram.com/data/Model-Datasets.cfm
# For the code below to work, you must save the dataset in the same folder as your R scripts.
# For this and other examples, you can check your work using the model dataset tables.
# This do file will show simple examples for creating new variables and how you can check them with the final report tables.
# For a more comprehensive code for creating DHS indicators, please visit the DHS Program code share library on Github: https://github.com/DHSProgram/DHS-Indicators-R
# You can download the repository or copy the code for the indicators you are interested in.

#####################################################################################

# install and load the packages you need. You only need to install packages once.
# After installing a package you can load the package using the library command
#install.packages("haven") # used to open dataset with read_dta
#install.packages("here") # used for path of your project. Save your datafile in this folder path.
#install.packages("dplyr") # used for creating new variables
#install.packages("labelled") # used for haven labelled variable creation
# install.packages("pollster") # used to produce weighted estimates with topline command
# installed.packages("naniar")
library(tidyverse)
library(naniar)   # to use replace_with_na function
library(haven)
library(here)
library(dplyr)
library(labelled) 
library(pollster)
library(survey)
here() # check your path

# open your dataset
pkir <-  read_dta(here("data","PKIR71FL.dta"))

save(pkir, file = 'data/pkir.RData')

load("D:/RepTemplates/pdhs/data/pkir.RData")

# Example 1
### recode current modern contraceptive use into a binary variable ###

# check category labels
print_labels(pkir$v313)

# option 1: creating a new categorical variable using mutate and ifelse commands

pkir <- pkir |>
  mutate(modfp = 
           ifelse(v313 == 3, 1, 0)) |>   
  set_value_labels(modfp = c(yes = 1, no = 0)) |>
  set_variable_labels(modfp ="Currently used any modern method")

# option 2: creating a new categorical variable using mutate and case_when commands

pkir <- pkir |>
  mutate(modfp =
           case_when(v313 == 3 ~ 1,
                     v313 < 3 ~ 0 )) |>
  set_value_labels(modfp = c(yes = 1, no = 0)) |>
  set_variable_labels(modfp ="Currently used any modern method")

# creating the sampling weight variable. 
pkir <- pkir |> mutate(wt=v005/1000000)
#pkir$wt <- pkir$v005/1000000

# check with final report Table 7.3
topline(pkir, modfp, wt )

########################################################

# Example 2
### Number of ANC visits in 4 categories that match the table in the final report


# age of child needed for this indicator. 
# If b19_01 is not available in the data use v008 - b3_01
# pkir <- pkir |>
#   mutate(age = b19_01)

pkir <- pkir |>
  mutate(age = v008 - b3_01)


pkir <- pkir |>
  mutate(ancvisits =
           case_when(
             m14_1 == 0 ~ 0 ,
             m14_1 == 1 ~ 1 ,
             m14_1  %in% c(2,3)   ~ 2 ,
             m14_1>=4 & m14_1<=90  ~ 3 ,
             m14_1>90  ~ 9 ,
             age>=60 ~ 99 )) |>
  replace_with_na(replace = list(ancvisits = c(99))) |>
  set_value_labels(ancvisits = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) |>
  set_variable_labels(ancvisits = "Number of ANC visits")

# check with final report Table 9.2
topline(pkir, ancvisits, wt )

library(gt)

topline(pkir, ancvisits, wt ) |> 
  gt() |> fmt_number(decimals = 2)

### Setting the survey design using the svydesign command from the survey package ###
# the survey design will be saved in an object named mysurvey, you can change this name to another name
mysurvey<-svydesign(id=pkir$v021, data=pkir, strata=pkir$v022,  weight=pkir$wt, nest=T)
options(survey.lonely.psu="adjust") 

# the option (survey.lonely.psu="adjust") is used to adjust for the case if there is single PSU within a stratum 

# now that we have set the survey design, we can use the survey package commands. See below for some examples.

# attach your data
attach(pkir)

# Modern contraceptive use indicator. This indicator was also produced in the RecodingDHSVars.R
pkir <- pkir %>%
  mutate(modfp = 
           case_when(v313 == 3~ 1,
                     v313<3~ 0)) %>%   
  set_value_labels(modfp = c(yes = 1, no = 0)) %>%
  set_variable_labels(modfp ="Currently used any modern method")
attach(pkir)
# to get frequencies
svytable(~modfp, mysurvey)

# to get proportions
prop.table(svytable(~modfp, mysurvey))

#you can save the result in an object
table1 <- prop.table(svytable(~v106, mysurvey))

#then you can refer to this object later on
table1

#if you want sampling error (SE) and confidence intervals (CI), you can use svyby with by=1 for total
svyby(~modfp,by=1,  design=mysurvey, FUN=svymean, vartype=c("se", "ci"))

#if you want to do this for a categorical variable, then you should add as.factor
#below we did not need as.factor since we had a binary variable
#for instance education level, v106, has four categories. You can estimate the SE and cI for each category.
svyby(~as.factor(v106),by=1,  design=mysurvey, FUN=svymean, vartype=c("se", "ci"))

#to check the labels for v106
print_labels(v106)

# Crosstabulation of modfp (modern FP use) and place of education (v106)
table2 <- svyby(~modfp, by = ~v106, design=mysurvey, FUN=svymean, vartype=c("se", "ci"))

table2

# chi-square results 
svychisq(~modfp+v106, mysurvey)

# to see how the survey commands are used for regressions, please check the regression R script. 
# To do this for several variables at once, you can do the following

# List of variables to crosstabulate with your outcome
variables <- c("v013","v106", "v190","v025","v024") 

results <- list()  # Empty list to store the crosstabulation results

# Loop through the variables
for (var in variables) {
  # Crosstabulation using svyby
  crosstab <- svyby(~ modfp, by = as.formula(paste("~", var)), design = mysurvey, FUN = svymean, vartype = c("se", "ci"))
  
  chi_square <- svychisq(as.formula(paste("~ modfp +", var)), design = mysurvey)
  
  # Store results in list
  results[[var]] <- list(crosstab = crosstab, chi_square = chi_square)
}

# Access the crosstabulation results for each variable
for (var in variables) {
  print(paste("Crosstabulation for", var))
  print(results[[var]])
}


# Compute linear model
reg1 <- svyglm(modfp~1+v025+as.factor(v013),
               design = mysurvey,
               family = binomial(link = 'logit'))
library(broom)
tidy(reg1)
# unadjusted with place of residence 
reg1 <- svyglm(modfp~ 1+ v025 , design=mysurvey, family=binomial(link="logit"))

# to see the results
summary(reg1)

# to get ORs
ORreg1 <- exp(reg1$coefficients )
ORreg1

# multivariate models with place of residence (v025), education (v106), and current age (v013)
reg2 <- svyglm(modfp~ 1+ v025 + as.factor(v106) + as.factor(v013), design=mysurvey, family=binomial(link="logit"))

tidy(reg2)

# checking for correlations

# create a subset of data with variables you want to check

cordata <- subset(IRdata, select=c(v025, v106, v013 ,v190))

mycordata <- cor(cordata)
# we see that v025 and v190 may be highly correlated

# to produce a correlation plot
#install.packages("corrplot")
library(corrplot)

corrplot(mycordata)
