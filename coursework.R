
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(fastDummies)
library(broom)
library(lme4)
library(magrittr)
library(htmlTable)

#loading data
setwd("~/Desktop/Multilevel-Modelling")

mydataNAs <- read.table(file = "coursework.txt", sep = "," , header = TRUE)

mydata <- drop_na(mydataNAs)

#creating dummies and renaming

mydata <- dummy_cols(mydata, select_columns = c('hhtenure', 'hiqual3'), remove_first_dummy = TRUE)

mydata <- mydata %>% rename(rent_local_auth = hhtenure_2, rent_private = hhtenure_3,
                            school_qual = hiqual3_2, no_qual = hiqual3_3)
colnames(mydata)

#centering age

mydata <- mydata %>% mutate(age = age - 16)

#summary statistics
a <- mydata %>%
      group_by(region) %>% 
      summarise(Number_of_households = n_distinct(hid),
            Number_of_individuals = n(),
            Mean_number_of_individuals_per_household = n()/n_distinct(hid))
        

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
          dv.labels = c("Single level null", "Two levels (household)", "Two levels (region)", "Three levels"),
          show.se = TRUE, show.ci = FALSE, show.p = FALSE)

#testing for improvement in fit 

anova(nullmodel_threelvl, twolevel_null_hid)

qchisq(0.95, 2)

#cattepillar plot

#Extract the school-level residuals and assign them to a new object u0

u0 <- ranef(twolevel_null_hid, postVar = TRUE) 

#You might get a warning message saying to use condVar instead postVar

#Save the standard error of the school-level residuals in the object u0se

u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

hid <- as.numeric(rownames(u0[[1]]))

#This section of code creates a table of the school-level residuals and gives them a rank

u0tab <- cbind(hid, u0[[1]], u0se)

colnames(u0tab) <- c("hid","u0","u0se")

u0tab <- u0tab[order(u0tab$u0), ]

u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))

colnames(u0tab)[4] <- "u0rank"

u0tab <- u0tab[order(u0tab$schoolid), ]

u0tab[1:10, ]

#Now we can create the caterpillar plot - I've added a title for the plot in the first line

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", 
     ylab = "conditional modes of r.e. for hid:_cons", 
     main = "Caterpillar plot of household level residuals")

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, 
         u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")

#calculating vpc
#P11.2.3 Calculating coverage intervals, variance partition coefficients (VPCs) and intraclass correlation coefficients (ICCs)

#Class-level variance 

VarCorr(nullmodel_threelvl)$hid

#School-level variance

VarCorr(nullmodel_threelvl)$region

#Save the residual variances as a dataframe

vars_nullmodel_threelvl <- as.data.frame(VarCorr(nullmodel_threelvl))

vars_nullmodel_threelvl

#School-level VPC - this coincides with the school-level ICC for this model

region_VPC <- vars_nullmodel_threelvl[2,4] / (vars_nullmodel_threelvl[1,4] + vars_nullmodel_threelvl[2,4] + vars_nullmodel_threelvl[3,4])

region_VPC

#Class-level VPC

hid_VPC <- vars_nullmodel_threelvl[1,4] / (vars_nullmodel_threelvl[1,4] + vars_nullmodel_threelvl[2,4] + vars_nullmodel_threelvl[3,4])

hid_VPC

#Class-level ICC

hid_ICC <- (vars_nullmodel_threelvl[1,4] + vars_nullmodel_threelvl[2,4]) / (vars_nullmodel_threelvl[1,4] + vars_nullmodel_threelvl[2,4] + vars_nullmodel_threelvl[3,4])

hid_ICC

#individual level

pupil_VPC <- vars_nullmodel_threelvl[3,4] / (vars_nullmodel_threelvl[1,4] + vars_nullmodel_threelvl[2,4] + vars_nullmodel_threelvl[3,4])

pupil_VPC


#adding variables (intercept)

model6 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual + rent_local_auth + rent_private + (1|hid) + (1|region), data = mydata, REML = FALSE)

tab_model(model6, 
          dv.labels = c("Random intercept model"),
          show.se = TRUE, show.ci = FALSE, show.p = FALSE, show.reflvl = TRUE)
summary(model6)

install.packages('texreg')
library(texreg)
htmlreg(list(model6), file = "coursework.R.doc", 
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)




