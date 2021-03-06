# install these packages 
install.packages("ggplot2")
install.packages("viridis")
install.packages("devtools")
install.packages("Hmisc")
install.packages("effects")
devtools::install_github("ropensci/plotly")
install.packages("car")
install.packages("texreg")
install.packages("splines")
install.packages("effects")

# set your working directory
getwd()
setwd("C:/Users/nellim/Dropbox/Fels") 

library(ggplot2)
library(viridis)
library(readstata13) # to read stata data file saved in version 13


# Class Objectives: In this lesson you will learn how to: 
# 1. non-linear regression models 

##########################
# Non-linear regressions #
##########################

## Logistic Regression

# get summary statistics
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

# create binary outcome variable
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=binomial())
summary(fit.full)
# rating = numeric self-rating of their marriage (1=very unhappy to 5=very happy)

# fit reduced model
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                     rating, data=Affairs, family=binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

library(effects)
plot(effect("rating",fit.reduced))


# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating),
                       age = seq(17, 57, 10), 
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

library(effects)
plot(effect("age",fit.reduced))


# bring in the data 
acs <- read.dta13("C:/Users/nellim/Dropbox/Fels/acsphillylaborforce.dta")
names(acs)

###################
# Data management #
###################

# rename variables for better graphic and summary tables
acs$Race <- acs$raceth
acs$Sex <- acs$sex
acs$Education <- acs$educ_year
acs$Degree <- acs$education
acs$Occupation <- acs$gen_occ
acs$Industry <- acs$ind_cat5
acs$Income <- acs$incwage
acs$Managers <- acs$leader_cat
acs$Age <- as.numeric(acs$age)

# label your variable using Hmisc package
library(Hmisc)
label(acs$Income) <- "Income from wages"
label(acs$Education) <- "Years of schooling"
label(acs$Managers) <- "Managerial Positions"
label(acs$Occupation) <- "Major Occupational Categories"
label(acs$Industry) <- "Major Industrial Categories"

# create a indicator for top executives and managers
table(acs$Managers)
acs$Managers <- as.character(acs$Managers)
acs$topmanagers <- 0
acs$topmanagers[acs$Managers == "Chief Executives" | acs$Managers == "Managers"] <- 1
acs$topmanagers <- factor(acs$topmanagers, 
                           levels=c(0,1),
                           labels=c("No","Yes"))

table(acs$Managers, acs$topmanagers)

## Logistic Regression

library(splines)
# fit full model
fit.full <- glm(topmanagers ~ Sex + Race + ns(Education, df=3) + poly(Age, 2),
                data=acs,family=binomial())
summary(fit.full)
# rating = numeric self-rating of their marriage (1=very unhappy to 5=very happy)

# fit reduced model
fit.reduced1 <- glm(topmanagers ~ Sex + Race + Education + poly(Age, 2),
                   data=acs, family=binomial())
summary(fit.reduced1)

# compare models
anova(fit.reduced1, fit.full, test="Chisq")


# fit reduced model
fit.reduced2 <- glm(topmanagers ~ + Race + ns(Education, df=3) + poly(Age, 2),
                    data=acs, family=binomial())
summary(fit.reduced2)

# compare models
anova(fit.reduced2, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced2)
exp(coef(fit.reduced2))

# recode race
acs$Race <- as.character(acs$Race)
table(acs$Race)
acs$Race2 <- 0
acs$Race2[acs$Race == "B-NH"] <- 1
table(acs$Race2,acs$Race)
acs$Race2 <- factor(acs$Race2, 
                          levels=c(0,1),
                          labels=c("Other","B-NH"))


fit.reduced3 <- glm(topmanagers ~ + Race2 + ns(Education, df=3) + poly(Age, 2),
                    data=acs, family=binomial())
summary(fit.reduced3)

# compare models
anova(fit.reduced3, fit.reduced2, test="Chisq")


# fit reduced model
fit.interactive <- glm(topmanagers ~ + Race2*poly(Age,2) + ns(Education, df=3),
                    data=acs, family=binomial())
summary(fit.interactive)

plot(effect("Race2*poly(Age,2)",fit.interactive))

