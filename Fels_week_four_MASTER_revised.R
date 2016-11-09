# install these packages 
install.packages("ggplot2")
install.packages("viridis")
install.packages("devtools")
install.packages("readstata13")
install.packages("gmodels")
install.packages("pander")
install.packages("dplyr")
install.packages("vcd")
install.packages("plyr")

devtools::install_github("ropensci/plotly")

# set your working directory
getwd()
setwd("C:/Users/nellim/Dropbox/Fels") 

#load the packages 
library(ggplot2)
library(plotly)
library(readstata13) # to read stata data file saved in version 13

# bring the data
gss <- read.dta13("C:/Users/nellim/Dropbox/Fels/gss_conlegis.dta")


#######################################
# Some housekeeping and review items #
#######################################

####################
# saving your work #
####################
## saving Script is not the same as saving data
## to save data you need to run the following codes

## there are several ways. One way is to use save() to save the exact object. e.g. for data frame "foo":

# save(foo,file="data.Rda")

## Then load it with:

# load("data.Rda")


####################
# subset your data #
####################

## select variables v1, v2, v3
# myvars <- c("v1", "v2", "v3")
# newdata <- mydata[myvars]

## another method
# myvars <- paste("v", 1:3, sep="")
# newdata <- mydata[myvars]

## select 1st and 5th thru 10th variables
# newdata <- mydata[c(1,5:10)]

## first 5 observations
# newdata <- mydata[1:5,]

## based on variable values
# newdata <- mydata[ which(mydata$gender=='F' 
#                         & mydata$age > 65), ]

## using subset function 
# newdata <- subset(mydata, age >= 20 | age < 10, 
#                  select=c(ID, Weight))

## using subset function (part 2)
# newdata <- subset(mydata, sex=="m" & age > 25,
#                  select=weight:income)



###################
# Data management #
###################

# convert factor variables into character variables
# to make it easier to manage and remove extra value labels
gss$race <- as.character(gss$race)
gss$sex <- as.character(gss$sex)
gss$degree <- as.character(gss$degree)
gss$partyid <- as.character(gss$partyid)
gss$conlegis <- as.character(gss$conlegis)

# we need to reorder the categories to be ordinal
table(gss$conlegis) # categories are out of order
gss$conlegis2 <- NA
gss$conlegis2[gss$conlegis=="a great deal"] <- 1
gss$conlegis2[gss$conlegis=="only some"] <- 2
gss$conlegis2[gss$conlegis=="hardly any"] <- 3
gss$conlegis2 <- factor(gss$conlegis2, 
                        labels = c("Great Deal","Only Some", "Hardly Any"))

table(gss$conlegis2,gss$conlegis)

View(gss)


# Class Objectives: In this lesson you will learn how to: 
# 1. How to get descriptive statistics for qualitative variables
# 2. Use ggplot2 to make graphs from your data 

# frequency tables

# one way table
mytable <- with(gss, table(conlegis2))
mytable  # frequencies

prop.table(mytable) # proportions
prop.table(mytable)*100 # percentages

# two way table
mytable <- xtabs(~ year+conlegis2, data=gss)
mytable
prop.table(mytable, 1) # row proportions

gss2014 <- gss[gss$year==2014, ] # subset GSS data for year == 2014
mytable <- xtabs(~ race+conlegis2, data=gss2014)
mytable # frequencies
margin.table(mytable,1) #row sums
margin.table(mytable, 2) # column sums
prop.table(mytable) # cell proportions
prop.table(mytable, 1) # row proportions
prop.table(mytable, 2) # column proportions
addmargins(mytable) # add row and column sums to table

mytable <- xtabs(~ year+conlegis2, data=gss)
mytable # frequencies
margin.table(mytable,1) #row sums
margin.table(mytable, 2) # column sums
prop.table(mytable, 1) # row proportions
prop.table(mytable, 2) # column proportions
addmargins(mytable) # add row and column sums to table


# more complex tables
addmargins(prop.table(mytable))
addmargins(prop.table(mytable, 1), 2)
addmargins(prop.table(mytable, 2), 1)


# Listing 7.10 - Two way table using CrossTable
library(gmodels)

?CrossTable
#CrossTable(x, y, digits=3, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
#           prop.t=TRUE, prop.chisq=TRUE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
#           resid=FALSE, sresid=FALSE, asresid=FALSE,
#           missing.include=FALSE,
#           format=c("SAS","SPSS"), dnn = NULL, ...)

CrossTable(gss2014$race, gss2014$conlegis2)
CrossTable(gss2014$race, gss2014$conlegis2, prop.t=FALSE, prop.chisq=FALSE, format="SPSS")
CrossTable(gss2014$race, gss2014$conlegis2, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, format="SPSS")

CrossTable(gss$year, gss$conlegis2, prop.t=FALSE, prop.chisq=FALSE, format="SPSS")

library(pander)
#make a pretty table of the above crosstab


# Listing 7.11 - Three way table
mytable <- xtabs(~ race+sex+conlegis2, data=gss2014)
mytable
ftable(mytable) 
ftable(prop.table(mytable, c(1,2)))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))

# Now we learn how to display these results using R
# We will start with looking at the trends for all Americans

# first we need to compute the percentages for each category of
# the outcome variable: confidence in congress
mytable <- xtabs(~ year+conlegis2, data=gss) # two-way table
mytable <- prop.table(mytable, 1) # row proportions
mydata <- data.frame(mytable) # transform the matrix into a dataframe

head(mydata)

mydata$Percent <- mydata$Freq # preparing % for Y-axis
mydata$Percent <- mydata$Percent*100
mydata$year <- as.character(mydata$year)
mydata$year <- as.numeric(mydata$year)

# basic line graph
plot1 <- ggplot(data = mydata, aes(x=year, y=Percent, linetype=conlegis2)) +
              geom_line()
plot1
# take control of x and y 
plot1 <- plot1 + scale_x_continuous(breaks=seq(1973, 2014, 4)) +  
        scale_y_continuous(breaks=seq(0, 70, 5)) 
plot1
# Add labels
plot1 <- plot1 + labs(title="Confidence in Congress", x="Year", y="%", 
                      fill = "Levels")
plot1
# Add annotations
plot1 <- plot1 + annotate("text", x=1975, y=35, label="Watergate", colour="red") +
                 annotate("text", x=1998, y=20, label="Clinton Impeachment", colour="blue") +
                 annotate("text", x=2001, y=47, label="Republican Control", colour="red")
plot1
# Shaded rectangular box
plot1 <- plot1 + annotate("rect", xmin=1995, xmax=2007, ymin=0, ymax=70, alpha=.1,
                          fill="red")
plot1
# remove background using theme
plot1 <- plot1 + theme_bw() 
plot1
# remove grid lines
plot1 <- plot1 + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot1
# Change Legend title
plot1 + labs(linetype='Levels')
plot1

# Now we will look at the outcomes by racial groups
# computing percentages to plot
mytable2 <- xtabs(~ year+race+conlegis2, data=gss)
mytable2 <- ftable(prop.table(mytable2, c(1,2)))
mydata2 <- data.frame(mytable2)
library(dplyr) # make sure you install the package
# subset only those who have "hardly any confidence" in congress
mydata2 <- data.frame(mydata2 %>% filter(conlegis2 == "Hardly Any"))
mydata2$Percent <- mydata2$Freq
mydata2$Percent <- mydata2$Percent*100
mydata2$year <- as.character(mydata2$year)
mydata2$year <- as.numeric(mydata2$year)
View(mydata2)
plot2 <- ggplot(data = mydata2, aes(x=year, y=Percent, linetype=race)) +
  geom_line()
plot2
plot2 <- plot2 + scale_x_continuous(breaks=seq(1973, 2014, 4)) +  
  scale_y_continuous(breaks=seq(0, 70, 5)) 
plot2
plot2 <- plot2 + labs(title="Percent of Americans Who Had Hardly Any Confidence \nin Congress by Race", x="Year", y="%", 
                      fill = "Race")
plot2
plot2 <- plot2 + annotate("text", x=1975, y=35, label="Watergate", colour="red") +
  annotate("text", x=1998, y=20, label="Clinton Impeachment", colour="blue") +
  annotate("text", x=2001, y=47, label="Republican Control", colour="red")
plot2
plot2 <- plot2 + annotate("rect", xmin=1995, xmax=2007, ymin=0, ymax=70, alpha=.1,
                          fill="red")
plot2
plot2 <- plot2 + theme_bw() 
plot2
plot2 <- plot2 + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot2
plot2 + labs(linetype='Race')
plot2
# now we look at outcomes in 2014 by racial groups
# mosaic plots are useful to see the relationships among categorical variables
library(vcd)
mosaic( ~ race + conlegis2, data=gss2014, highlighting="conlegis2",
        highlighting_fill=c("lightblue","pink","red"))
gss$conlegis2 <- factor(gss$conlegis2, 
                        labels = c("Great Deal","Only Some", "Hardly Any"))

gss2014$race <- factor(gss2014$race, 
                       labels = c("B", "O", "W")) 
mosaic( ~ sex + race + conlegis2, data=gss2014, highlighting="conlegis2",
        highlighting_fill=c("lightblue","pink","red"),
        direction=c("h","h","v"))


# bargraphs
mytable3 <- xtabs(~ race+conlegis2, data=gss2014)
mytable3
mytable3 <- prop.table(mytable3, 1) # row proportions
mydata3 <- data.frame(mytable3)
mydata3$race <- as.character(mydata3$race)
mydata3$Percent <- mydata3$Freq
mydata3$Percent <- round(mydata3$Percent*100, digits=1)
library(plyr)
mydata3 <- ddply(mydata3, "race", transform, label_y=cumsum(Percent)) 
#ddply() to divide the data into racial groups, then calculate a cumulative sum of percentages

plot3 <- ggplot(mydata3, aes(x=race, y=Percent, fill=conlegis2)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y=label_y,label=paste(Percent,"%"), vjust=1.5))
plot3 <- plot3 + theme_bw() + labs(fill="Levels")

plot3 

# dot plots
gss2014$race_sex <- paste(gss2014$race, gss2014$sex)
mytable4 <- xtabs(~ race_sex+conlegis2, data=gss2014)
mytable4
mytable4 <- prop.table(mytable4, 1) # row proportions
mydata4 <- data.frame(mytable4)
mydata4$Levels <- mydata4$conlegis2
mydata4$race_sex <- as.character(mydata4$race_sex)
mydata4$Percent <- mydata4$Freq
mydata4$Percent <- round(mydata4$Percent*100, digits=1)
ggplot(mydata4, aes(x=Percent, y=race_sex)) + 
   labs(title="Confidence in Congress by Race and Sex", x="%", y="Race and Sex") +
   scale_x_continuous(breaks=seq(0, 70, 10)) +
   geom_point(size=3, aes(colour=Levels)) 


# Class Objectives: In this lesson you will learn how to: 
# 1. How to get descriptive statistics for qualitative variables
# 2. Use ggplot2 to make graphs from your data 
