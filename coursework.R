
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(fastDummies)
library(broom)
library(lme4)
library(magrittr)
library(htmlTable)
library(RColorBrewer)
library(lattice)
library(stargazer)

#loading data
setwd("~/Desktop/Multilevel-Modelling")

mydataNAs <- read.table(file = "coursework.txt", sep = "," , header = TRUE)

mydata <- drop_na(mydataNAs)

#creating dummies and renaming

mydata <- dummy_cols(mydata, select_columns = c('hhtenure', 'hiqual3'), remove_first_dummy = TRUE)

mydata <- mydata %>% dplyr::rename(rent_local_auth = hhtenure_2, rent_private = hhtenure_3,
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



b <- mydataNAs %>%
     group_by(region) %>% 
     summarise(Number_of_households = n_distinct(hid),
            Number_of_individuals = n(),
            Mean_number_of_individuals_per_household = n()/n_distinct(hid))   

tab_df(a, col.header = c("Region", "Number of households", "Number of individuals", "Average number of individuals per household"))

tab_df(b, col.header = c("Region", "Number of households", "Number of individuals", "Average number of individuals per household"))

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

stargazer(single_null, twolevel_null_hid,twolevel_null_region,nullmodel_threelvl,
          intercept.bottom = FALSE,
          omit.table.layout = "sn",
          column.labels = c("Single level null", "Two levels (household)", "Two levels (region)","Three levels"),
          header=FALSE,
          ci = TRUE,
          type='latex')

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


stargazer(model6,
          intercept.bottom = FALSE,
          omit.table.layout = "sn",
          column.labels = c("Random Intercept Model"),
          header=FALSE,
          ci = TRUE,
          type='latex')

install.packages('texreg')
library(texreg)
?texreg
texreg(list(model6), 
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)

#adding random slopes

rsmodelhid <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual + rent_local_auth + rent_private + ( 1 + age |hid) + (1 |region), data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

rsmodelhid

anova(model6, rsmodelhid)

rsmodelregion <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual + rent_local_auth + rent_private + (1 |hid) + (1+ age |region), data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rsmodelregion)

rsmodel <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual + rent_local_auth + rent_private + (1+ age |hid) + (1+ age |region), data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

tab_model(rsmodel)

anova(model6,rsmodel)

qchisq(0.95, 1)

#We can also produce a plot that shows how the between-school variance changes by cohort, as is implied by the random slopes model using the equation var(uoj+u1j*xij)=var(u0j)+2xijcov(u0j,u1j)+x^2*var(u1j)

x <- c(min(mydata$age):max(mydata$age))

VarCorr(rsmodelhid)$hid

y <- VarCorr(rsmodelhid)$hid[1,1] + 2*x*VarCorr(rsmodelhid)$hid[1,2] + x^2*VarCorr(rsmodelhid)$hid[2,2]

plot(x, y, type = "l", xlim = c(0, 80), xlab = "age", ylab = "Total household variance", main = "Between household variance by age")

summary(mydata$age)

#or in ggplot2
palette <- brewer.pal("Greys", n=9)
color.background = palette[1]
color.grid.major = palette[5]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]


ggplot()+
  theme_grey() +
  geom_line(mapping = aes(x, y), colour = "#535353" )+
  theme(panel.border=element_blank(), axis.line=element_line(), axis.line.y=element_blank()) +  
  theme(panel.grid.major=element_line(colour=color.background,size=.75)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.line.x =element_line(colour="#535353", size=.75))+
  theme(axis.line.y =element_line(colour="#535353", size=.75))+
  theme(legend.position="none") +
  ggtitle("Between household variance by age")+ 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="black",size=20)) +
  xlab("Age - 16") +
  ylab("Total household-level variance") +
  theme(axis.text.x=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=-.5)) +
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

##Plot the predicted score lines from the random slopes model. As before, we want to plot the slopes for schools with two or more cohorts of data
library(viridis)
predscore <- fitted(rsmodelhid)

datapred <- cbind(predscore = predscore, age = mydata$age, hid = mydata$hid, region = mydata)

datapred <- data.frame(unique(datapred))

datapred <- datapred[order(datapred$hid, datapred$age), ]

datapred$multiplecohorts <- rep(0, dim(datapred)[1])

datapred$multiplecohorts[datapred$hid %in% unique(datapred$hid[duplicated(datapred$hid)])] <- 1

xyplot(predscore ~ age, data = datapred[datapred$multiplecohorts == 1, ], groups = hid, type = c("p", "l"), col = "black", main = "Predictions from random slopes model")

#in ggplot2

lol <- datapred[datapred$multiplecohorts == 1, ]

lol <- lol[!(lol$hid == 729 & lol$age == 2),]



ggplot(data = lol,
       aes(x = lol$age, 
           y = lol$predscore, group = hid, colour = as.factor(region.region)))+
  geom_point(size     = 1,
             alpha    = .8)+
  geom_line()+
  labs(title    = "Predicted scorelines from the random slopes model",
       col      = "Region") +
  xlab("Age - 16") +
  ylab("Predscore")+
  theme(axis.line.x =element_line(colour="black", size=.5))+
  theme(axis.line.y =element_line(colour="black", size=.5))+
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=-.5)) +
  ggtitle("Predicted scorelines from the random slopes model")+ 
  theme(plot.title=element_text(face="bold",colour="black",size=20))

unique(lol$hid)

#we can also create a table with the model coeffecients 


tab_model(rsmodelhid,
          dv.labels = c("Random slopes model"),
          show.se = TRUE, show.ci = FALSE, show.p = FALSE)


#fitting logistic models

lognull <- glm(worry_crime ~ 1, family = binomial("logit"), data = mydata)

summary(lognull)

lognullhid <- glmer(worry_crime ~ (1|hid), family = binomial("logit"), data = mydata) 

summary(lognullhid)

lognullreg <- glmer(worry_crime ~ (1|region), family = binomial("logit"), data = mydata) 

summary(lognullreg)

lognullthreelvl <- glmer(worry_crime ~ (1|hid) + (1|region), family = binomial("logit"), data = mydata) 

summary(lognullthreelvl)

#creating a table

tab_model(lognull, lognullhid, lognullreg, lognullthreelvl, 
          dv.labels = c("Single level null", "Two levels (household)", "Two levels (region)", "Three levels"),
          show.se = TRUE, show.ci = FALSE, show.p = TRUE)

texreg(lognull, lognullhid, lognullreg, lognullthreelvl)
#testing for improvement in fit

#reference
qchisq(0.95, 1)
qchisq(0.95, 2)

#single level null vs 2 level with hid
-2*(logLik(lognull)-logLik(lognullhid))

#single level null vs 2 level with region
-2*(logLik(lognull)-logLik(lognullreg))

#single level null vs 3 level
-2*(logLik(lognull)-logLik(lognullthreelvl))

#2 level with hid vs 3 level 

-2*(logLik(lognullhid)-logLik(lognullthreelvl))

#2 level with reg vs 3 level 

-2*(logLik(lognullreg)-logLik(lognullthreelvl))

#adding explanatory variables

logmodelreg <- glmer(worry_crime ~ age + sclfsato + urban + school_qual + no_qual 
                     + rent_local_auth + rent_private  + ageXurban +
                       ageXrent_local_auth + ageXrent_private + (1 |region), family = binomial("logit"), data = mydata)

logmodelfinal <- glmer(worry_crime ~ sclfsato + urban + school_qual + no_qual 
                       + (1 |region), family = binomial("logit"), data = mydata)

tab_model(logmodelfinal, 
          dv.labels = c('Logistic model'),
          show.se = TRUE, show.ci = FALSE, show.p = TRUE)

#testing for improvement in fit

-2*(logLik(lognullreg)-logLik(logmodelreg))

#creating table

tab_model(logmodelreg,rsmodelint2level, 
          dv.labels = c('Logistic model', 'Linear model' ),
          show.se = TRUE, show.ci = FALSE, show.p = TRUE)


#creating interaction terms for age
mydata$ageXsclfsato <- mydata$age*mydata$sclfsato
mydata$ageXurban <- mydata$age*mydata$urban
mydata$ageXfemale <- mydata$age*mydata$female
mydata$ageXrent_local_auth <- mydata$age*mydata$rent_local_auth
mydata$ageXrent_private <- mydata$age*mydata$rent_private
mydata$ageXschool_qual <- mydata$age*mydata$school_qual
mydata$ageXno_qual <- mydata$age*mydata$no_qual
mydata$ageXworry_crime <- mydata$age*mydata$worry_crime

colnames(mydata)
#begin with sclfsato

rsmodelhidint1 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                   + rent_local_auth + rent_private + ageXsclfsato + ( 1 + age |hid) + (1 |region),
                   data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

rimodelhidint1 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                    + rent_local_auth + rent_private + ageXsclfsato + ( 1|hid) + (1 |region),
                    data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rimodelhidint1)

-2*(logLik(model6)-logLik(rimodelhidint1))

#then urban

rsmodelhidint2 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXurban + ( 1 + age |hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

rimodelhidint2 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXurban + ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(rsmodelhid, rsmodelhidint2)

anova(model6, rimodelhidint2)

-2*(logLik(rsmodelhid)-logLik(rsmodelhidint2))

#then female

rimodelhidint3 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXfemale + ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rimodelhidint3)

#then hhtenure

rimodelhidint4 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXrent_local_auth + ageXrent_private + ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rimodelhidint4)

#then education

rimodelhidint5 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXschool_qual + ageXno_qual+ ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rimodelhidint5)

#and finally worry crime

rimodelhidint6 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXworry_crime + ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(model6, rimodelhidint6)

#final model

rimodelhidint7 <-  lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                        + rent_local_auth + rent_private + ageXworry_crime + ageXurban +
                          ageXrent_local_auth + ageXrent_private + ( 1|hid) + (1 |region),
                        data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

#testing for approriateness of rs

rsmodelhidint7 <-  lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                        + rent_local_auth + rent_private + ageXworry_crime + ageXurban +
                          ageXrent_local_auth + ageXrent_private + ( 1 + age|hid) + (1 |region),
                        data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

summary(rsmodelhidint7)

anova(rimodelhidint7, rsmodelhidint7)

#testing for appropriateness of three level model

rsmodelint2level <-  lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                        + rent_local_auth + rent_private  + ageXurban +
                          ageXrent_local_auth + ageXrent_private + ( 1 + age|hid),
                        data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

anova(rsmodelint2level, rsmodelhidint7)


tab_model(rsmodelint2level)

#plotting interaction effects
mydata2 <- mydata

#first household model (rimodelhidint4)
mydata2$urban <- mean(mydata$urban)
mydata2$sclfsato <- mean(mydata$sclfsato)
mydata2$school_qual <- mean(mydata$school_qual)
mydata2$no_qual <- mean(mydata$no_qual)

rimodelhidint4 <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual 
                       + rent_local_auth + rent_private + ageXrent_local_auth + ageXrent_private + ( 1|hid) + (1 |region),
                       data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))


mydata2$predprob <- predict(rimodelhidint4, mydata2,  type = "response")

mydata2 <- mydata2[order(mydata2$age), ]

xyplot(mydata2$predprob ~ mydata2$age, groups = mydata2$hhtenure, type = 
         c("r"), col = c("green", "blue", "red"), lty = c("solid", "dashed", "dotted"), 
       xlab = "Age - 16", ylab = "Pr(nhood_mistrust)", ylim = c(1, 5), 
       aspect = 0.75, key = list(space = "bottom", columns = 2, text = list(c("Owner/mortgaged", 
      "Local authority/housing association rent", "Private rent")), lines = list(col = c("green", "blue","red"), lty = c("solid", "dashed", "dotted"))))
