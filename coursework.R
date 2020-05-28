
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

#adding random slopes

rsmodelhid <- lmer(nhood_mistrust ~ age + sclfsato + urban + school_qual + no_qual + rent_local_auth + rent_private + ( 1+ age |hid) + (1 |region), data = mydata, REML = FALSE, control = lmerControl(check.nobs.vs.nRE = "ignore"))

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


