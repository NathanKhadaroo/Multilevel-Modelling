install.packages('fastDummies')
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(fastDummies)
library(broom)
library(lme4)


#loading data

mydataNAs <- read.table(file = "coursework.txt", sep = "," , header = TRUE)

mydata <- drop_na(mydataNAs)

#creating dummies and renaming

mydata <- dummy_cols(mydata, select_columns = c('hhtenure', 'hiqual3'), remove_first_dummy = TRUE)

mydata <- mydata %>% rename(rent_local_auth = hhtenure_2, rent_private = hhtenure_3,
                            school_qual = hiqual3_2, no_qual = hiqual3_3)
colnames(mydata)

#fiting models
single_null <- lm(nhood_mistrust ~ 1, data = mydata)
tidy(single_null)

twolevel_null_hid <- lmer(nhood_mistrust ~ (1|hid), data = mydata, REML = FALSE)
summary(twolevel_null_hid)

twolevel_null_region <- lmer(nhood_mistrust ~ (1|region), data = mydata, REML = FALSE)
summary(twolevel_null_region)

nullmodel_threelvl <- lmer(nhood_mistrust ~ (1|hid) + (1|region), data = mydata, REML = FALSE)
summary(nullmodel_threelvl)

#creating a table

tab_model(single_null, twolevel_null_hid,twolevel_null_region,nullmodel_threelvl, 
          dv.labels = c("Single level null", "Two levels (household)", "Two levels (region)", "Three levels"))
