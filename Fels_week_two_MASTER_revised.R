# Class Objectives:
# In this lesson you will learn how to:
#   1. Apply math to objects
#   2. Edit objects
#   3. Recode values and variable names
#   4. Change value's format type
#   5. Add new variables to objects
#   6. Order data by variables
#   7. Subset data
#   8. More exercises on sampling



getwd()
setwd("C:/Users/nellim/Dropbox/Fels") # place your working directory into the folder which has youre
                               # data. I work with data that I put on the desktop so my directory
                               # is my desktop.

# Make sure your working directory is in the same place as the excel file
# If you are confused about a function type ?function_name into the command line
# and it will bring up a help menu. For example,
?mean

# The command read.csv allows you to read a .csv file into R and you can place the data into
# an object. 
alumni <- read.csv("Alumni_cleaned.csv")
View(alumni) # note capital V

# The package foreign allows you to read Stata files (.dta) into R
library(foreign)  # This runs the package foreign. You will need to run it each time you
                  # open R
gss <- read.dta("gss_week_one.dta") # This says to read the file into the object "gss"
View(gss)

# Managing data, though it may seem simple and tedious, is among the most important parts of
# being able to analyze data. Think of programming like baking a cake. Managing the data is 
# getting all your ingredients ready to bake. Without every ingredient, you can't have your cake.
# When you manage your data (the ingredients), you are getting it ready to analyze (bake). 
# The final result (the cake) is only as good as the ingredients (cleaned data) that you have. 
# During this lesson you will learn how to begin cleaning, and looking at your data, for future
# analysis. 

###################
# Class Exercises #
###################
# 1. Find the mean year in alumni data




mean(alumni$Year)

# 3. Add 10 years to every year




alumni$Year + 10

# 4. Create a new variable which has 10 years added to every year




new_object <- alumni$Year + 10 # bad example. Now there is an object floating around in the memory


################# New topic ###############
# You can use the edit command to manually change values in your data. This can work for
# quick, small changes, but will be very inefficient for large changes. We will learn how to
# make mass changes to your data. 
edit(alumni)

# Recoding values means making new values based on some condition of the old value. For example
# if you have data on President Obama's public opinion poll numbers, you can change the actual
# percentages of approval into categories. You can change any number below 50% to say "Bad"
# and any number above 50% to say "good". The following example will show how to do this.

alumni$Year[alumni$Year < 1990] <- "This is an example"
# What the above code said is look at every row in the Year column, if the value is under 1990,
# change that value to the words in the quotations. In more programming language, every value
# less than 1990 gets "This is an example"

# reload the alumni data because we played with variables
alumni <- read.csv("Alumni_cleaned.csv")

# You can use "logical operators" (see page 75 in your textbook) to recode
# some important logical operators follow
# <         Less than
# >         Greater Than
# <=        Less than or equal to
# >=        Greater than or equal to
# ==        Equal to
# !=        Not equal to
# x | y     Condition that meats either x or y condition

alumni$First <- as.character(alumni$First)
alumni$Location <- as.character(alumni$Location)                             
# This changes the format of this column to "character" which just means text. We will get to 
# this command soon.

##################
# Class exercise #
##################

# 1. Recode all years equal to 1990 to year 2000 in alumni data




alumni$Year[alumni$Year == 1990] <- 2000

# 2. Change the name in row 1 to your name, and the location in row 1, to your home location.





edit(alumni) # One option
alumni$First[1] <- "My name" # Better option
alumni$Location[1] <- "My Home"
View(alumni)

# 3. Recode your name to the name "Harry Houdini"




alumni$First[1] <- "Harry Houdini" # one option
alumni$First[alumni$First == "My name"] <- "Harry Houdini" # Better option
View(alumni)

# 4. In gss, recode pres08 to a binary response (vote for Obama, yes or no)





gss$pres08 <- as.character(gss$pres08) # Need to make it characters
gss$pres08_yn[gss$pres08 == "obama"] <- 1 
gss$pres08_yn[gss$pres08 != "obama"] <- 0

gss$pres08_yn <- factor(gss$pres08_yn, labels = c("No","Yes"))
View(gss)
table(gss$pres08_yn,gss$pres08) #always check your work

#alternative recode
gss <- within(gss,{
              voted_for_obama <- NA
              voted_for_obama[pres08 == "obama"] <- 1
              voted_for_obama[pres08 != "obama"] <- 0})

gss$voted_for_obama <- factor(gss$voted_for_obama, labels = c("No","Yes"))

table(gss$pres08_yn,gss$voted_for_obama) #two approaches are the same.

# 5. In gss, recode race to white/nonwhite







table(gss$race)

gss$white <- as.character(gss$race)
gss$white[gss$race == "white"] <- 1
gss$white[gss$race != "white"] <- 0

gss$white <- factor(gss$white, labels = c("No","Yes"))

table(gss$white,gss$race)

# 6. In gss, create a variable that combines race and sex






gss$race_sex <- paste(gss$race, gss$sex) # This is a sample command to paste the value
                                         # in gss$race, and then the value in gss$sex 
                                         # next to each other in a new variable called 
                                         # combined
View(gss)
table(gss$sex,gss$race_sex)
table(gss$race,gss$race_sex)


################# New topic ###############
# You can use the names() function to see what the column names are called. You can also use this
# function to change the column names

names(alumni) # This says all column names in the object alumni
names(alumni)[1] # This says the name for the first column in the object alumni
names(alumni)[2:4] # This says the name for columns 2 to 4 in the object alumni

###################
# Class exercises #
###################

# 1. Change the first column to say "Name" in alumni data






names(alumni)[1] <- "Name"

# 2. Show the names of columns 1, 4, 5






names(alumni)[c(1,4,5)]


################# New topic ###############
# R places values in many different formats (e.g. numeric, character, etc.). Sometimes you must
# change the format of the values to the one that you desire. For example, to do math you 
# must have numeric data. Changing formats is quite easy. See page 82 in your book for a full
# list of formats.

is.numeric(alumni$Year) # This asks whether the data in the column Year in the object alumni
                        # is numeric. It says "TRUE" which means that it is numeric
mean(alumni$Year)

is.character(alumni$Year) # This asks if the data is character. Character format means text
                 
# The commands for changing format type is the same as testing them except they start with as.
# istead of is.

as.character(alumni$Year)
alumni$Year <- as.character(alumni$Year) # Remember that to save your changes you must tell R
                                         # to do the command into your object

mean(alumni$Year) # Notice how when you changed the type to character you are unable to do math
                  # on the data.

alumni[order(alumni$Year)[1:2],]

# The order() function says to order the data. By default the order is ascending (smallest to 
# largest, or alphabetically). You can specify what you want to order by. The previous
# command said to order the first 10 items object alumni by the Year. This means that it
# rearranged the data in so it is chronoligical, earliest year first, most recent year last. 

alumni[order(alumni$Year, alumni$Name)[1:10],] # This orders it by both the year and the name.

# you got an error message. what is going on?
view(alumni)
# another error! what is going on?


View(alumni)
alumni[order(alumni$Year, alumni$First)[1:10],] # This orders it by both the year and the name.

###################
# Class exercises #
###################

# 1. Change all Year data to numeric format in alumni data






alumni$Year <- as.character(alumni$Year)

# 2. Make a new object that contains only the first 10 years, and then make them character format







new_alumni <- alumni$Year[order(alumni$Year)[1:10]]
new_alumni <- as.character(new_alumni)


# 3. Make a new object of the first 100 rows, ordered by year.










new_alumni <- alumni[order(alumni$Year)[1:100],]

# 4. Reorder the data from question 3 to be in descending order (most recent year first)








new_alumni <- new_alumni[order(new_alumni$Year, decreasing = TRUE),]



################# New topic ###############
# The nrow() function tells you how many rows are in your object
nrow(alumni)

# You can create a new column in the object by saying that this new column gets your desired
# data

nrow(alumni)
alumni$count <- 1:nrow(alumni)
View(alumni)


##################
# Class Exercise #
##################

# 1. Explain in words what each line in the above code does. 









# The above code first finds out how many rows are in the object alumni - the answer is 144
# It then makes a new column called count that has a list of numbers from 1 to 144 (which is 
# how many rows there are). Finally it views the new object.


################# New topic ###############
# Oftentimes it is easier to create new objects containing only the data you are interested in,
# this is called subsetting - Making new objects from old objects. Think of the old object
# as your kitchen pantry. When you subset, you are taking only the ingredients you need
# and leaving the rest. 
# You have already been subsetting when you made the new ordered object
# Lets work with that subsetted object and subset it some more.

new_alumni$Date <- NULL # To "drop", or delete" a column say that the column gets NULL. Null
                        # will remove the column

new_alumni_2 <- data.frame(new_alumni$Name)
# Using data.frame() makes the new object into a data frame. 
# Data frames contain columns and rows

variables <- c("Name", "Location")
new_alumni_2 <- alumni[variables]
View(new_alumni_2)
# error what is happening
# look at the data
View(new_alumni_2)
View(alumni)
variables <- c("First", "Location")
new_alumni_2 <- alumni[variables]
# error? why?






variables <- c("Name", "Location")
View(new_alumni_2)

# This says to make an object with the values "Name" and "Location" (remember, those are our
# column names). The next line says to make a new object with columns that match the values
# in the object variables

new_alumni_2 <- alumni[c(alumni$Name, alumni$Location),] 
# This does the same as the last command but is written out more

variables <- names(alumni) %in% c("Names", "Location")
new_alumni_2 <- alumni[!variables]
View(new_alumni_2)
# The above code says to make an object, called variables, that looks if columns have names matching
# "Names" and Locations". The next line says to create a new object with all columns except
# those matching the names in "variables". Remember that the ! symbol means not in R. 
# != means not equal to

# You can use the subset() function to subset.

subset_alumni <- subset(alumni, Year < 1975 & Year > 1955,
       select = c(Name, Location, Year))

subset_alumni <- subset(alumni, Year == 2000 | Year == 1975)

###################
# Class exercises #
###################
subset_alumni <- subset(alumni, Year < 1975 & Year > 1955,
                        select = c(Name, Location, Year))

# 1. Explain in words what the first command does




# This says to make a new object using only rows in which the year is less then 1975 and
# more than 1955. The only data that will be used is in the columns Name, Location, and Year

subset_alumni <- subset(alumni, Year == 2000 | Year == 1975)
# 2. Explain in words what the second command does




# This says make a new object using years that equal either 2000 or 1975. It uses all the data
# in alumni


################# New topic ###############
# The function sample() lets you take samples from the data
?sample

sample(1:100, size = 5, replace = FALSE)
sample(1:100, size = 5, replace = FALSE)
sample(1:100, size = 5, replace = FALSE)
sample(1:100, size = 5, replace = FALSE)
sample(1:100, size = 5, replace = FALSE)

# sample() takes a random sample of the data and will produce a new result each time. It is 
# R's version of pulling names out of a hat.

sample(alumni$First, size = 5, replace = TRUE)
#error?





sample(alumni$Name, size = 5, replace = TRUE)

presidents <- c("Washington", "Lincoln", "Clinton", "Eisenhower", "Reagan", "Bush", 
                "George W. Bush", "Obama", "Grant", "Kennedy", "Johnson")

sample(presidents, size = 10000, replace = TRUE)
table(sample(presidents, size = 10000, replace = TRUE))

###################
# Class exercises #
###################

# 1. Take a sample, without replacement, of presidents 5 times






sample(presidents, size = 5, replace = FALSE)

# 2. Take a sample, with replacement, of presidents 10000 times






sample(presidents, size = 10000, replace = TRUE)

# 3. Summarize how many time each president was selected in question 2






table(sample(presidents, size = 10000, replace = TRUE))

# In this lesson you learned how to:
#   1. Apply math to objects
#   2. Edit objects
#   3. Recode values and variable names
#   4. Change value's format type
#   5. Add new variables to objects
#   6. Order data by variables
#   7. Subset data
#   8. More exercises on sampling