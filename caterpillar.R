
lapply(c("R2MLwiN", "arm","ggplot2","RColorBrewer","grid"),library, character.only=TRUE)

# Set working directory and load data
setwd("~/Desktop/Multilevel-Modelling")

mydataNAs <- read.table(file = "coursework.txt", sep = "," , header = TRUE)

mydata <- drop_na(mydataNAs)

# Fit a linear varying intercept multilevel model with group level predictors of normexam ~ sex + standlrt + vrband + schgend + avslrt + schav + (1|school)
fit <- lmer(nhood_mistrust ~ (1|hid), data = mydata, REML = FALSE)
display(fit)

# Display "Random"/"Varying" i.e. school effects
ranef(fit)

# Extract higher level residuals
ranef <-ranef(fit)
ranef.se<-se.ranef(fit)
u0mn <-ranef$hid
u0sd <-ranef.se$hid
# Rank residuals

u0rankmn <- rank(u0mn)
u0hi <- u0mn + (1.96*u0sd)
u0lo <- u0mn  - (1.96*u0sd)

u0 <- ranef(fit, postVar = TRUE) 

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

u0tab <- u0tab[order(u0tab$hid), ]

u0tab[1:10, ]

# Combine into data.frame
d <-data.frame(u0tab$u0rank, u0tab$u0, u0hi, u0lo) 

# Set colors
palette <- brewer.pal("Greys", n=9)
color.background = palette[1]
color.grid.major = palette[5]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]

# Plot

ggplot()+
  geom_pointrange(data=u0tab,mapping=aes(x=u0tab$u0rank , y=u0tab$u0,ymin=d$X.Intercept..1,ymax=d$X.Intercept.), position="identity", size=0.1, color="dodgerblue4", alpha = 0.5)+
  geom_point(data=u0tab,mapping=aes(x=u0tab$u0rank , y=u0tab$u0))+
  theme_bw() +
  theme(panel.background=element_rect(fill=color.background, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_blank(), axis.line=element_line(), axis.line.y=element_blank()) +  
  theme(panel.grid.major=element_line(colour=color.background,size=.75)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.line.x =element_line(colour="#535353", size=.75))+
  theme(axis.line.y =element_line(colour="#535353", size=.75))+
  theme(legend.position="none") +
  ggtitle("Caterpillar plot of household level residuals")+ 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="black",size=20)) +
  xlab("Rank of residuals") +
  ylab("Conditional modes of r.e. for hid:_cons") +
  theme(axis.text.x=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=-.5)) +
  geom_hline(yintercept=0,size=1.2, alpha=0.8,colour="red", linetype="twodash")+
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

