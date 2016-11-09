 
# grep - 1973 {g}lobally search a {r}egular {e}xpression and {p}rint

data.text <- c("suspect",
               "three suspects",
               "3 suspects",
               "9mm firearm",
               "483 McNeil Building",
               "3718 Locust Walk",
               "4 Privet Drive",
               "10880 Malibu Point",
               "Philadelphia, PA",
               "Philly",
               "Phila",
               "Phil.",
               "Phil Dunphy",
               "19104-6286",
               "20015",
               "90291",
               "90210",
               "(215) 573-9097",
               "215-573-9097",
               "2155739097")

# find the letter "a"
grep("a",data.text)
grep("a",data.text, value = TRUE)

# find the number "1"
grep("1",data.text,value=TRUE)

# find numbers
#   square brackets contain lists of characters
grep("[0123456789]",data.text,value=TRUE)

# find odd numbers
grep("[13579]",data.text,value=TRUE)

# find four numbers together
grep("[0-9][0-9][0-9][0-9]",data.text,value=TRUE)
grep("[0-9]{4}",data.text,value=TRUE)

# find 2 to 3 numbers together
grep("[0-9]{2,3}",data.text,value=TRUE)

# find capital letters
grep("[A-Z]",data.text,value=TRUE)

#Classroom exercises
#1. Write a regular expression that will match these
   c("123","567","314")
 #  but not these
   c("1234","5678","3141")

   grep("^[0-9]{3}$", c("123","567","314", "1234","5678","3141"), value = TRUE)
   
   grep("^...$", c("123","567","314", "1234","5678","3141"), value = TRUE)
   
   
   #2. Write a regular expression that will match these
   c("A1","B1","C1")
 #  but not these
   c("D1","E1","F1")

   grep("[ABC]", c("A1","B1","C1", "D1","E1","F1"), value = TRUE)
   
   
   
# starts with a letter
grep("^[A-Za-z]",data.text,value=TRUE)

# ends with a letter
grep("[A-Za-z]$",data.text,value=TRUE)

# find something that's not a letter next to a letter
grep("[^A-Za-z ][A-Za-z]",data.text,value=TRUE)

# strings with numbers followed by letters
#   + means at least one of the previous
grep("[0-9]+[A-Za-z]+",data.text,value=TRUE)

# 3. Write a regular expression that will match these
   c("123ABC","234BCDEF","435C")
  # but not these
   c("1ABC23","2468BC","1234C5")

# phone numbers
grep("[0-9]{3}-[0-9]{3}-[0-9]{4}",data.text,value=TRUE)

# regular expressions have several "special characters"
#   \ ^ $ {} [] () <> . * + ? | - &
#   if you want to search for these "protect" them with \

#   parentheses are special need to "protect" them with \\
#   if it's special to R use one \, special to grep use \\
grep("\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}",data.text,value=TRUE)
grep("[0-9]{9}",data.text,value=TRUE)

# use | as "or" to put them together
grep("[0-9]{3}-[0-9]{3}-[0-9]{4}|\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}|[0-9]{9}", 
     data.text,value=TRUE)

# () group together characters as words
# get those with three suspects
grep("(three|3) suspects",data.text,value=TRUE)
# get DC and LA ZIP codes
grep("(902|200)[0-9]{2}",data.text,value=TRUE)

# The ? indicates optional text
grep("Phil(adelphia)",data.text,value=TRUE)
grep("Phil(adelphia)?",data.text,value=TRUE)

# find a word
#   \b looks for boundaries (not letters or numbers)
grep("\\b[A-Za-z]{4}\\b",data.text,value=TRUE)
# alternatively
#   | represents "or"
grep("( |^)[A-Za-z]{4}( |$)",data.text,value=TRUE)


# Class exercises
# 1. Find commas
# 2. Find ZIP codes
# 3. Find those with six letter word
# 4. Find mentions of Philadelphia
# 5. Find capitalized words
# 6. Find the addresses


# gsub - global substitution

# remove numbers from text
gsub("[0-9]","",data.text)

# remove anything that is not a number from text
gsub("[^0-9]","",data.text)

Class exercise
1. Remove the commas from these numbers
   

  as.numeric(c("9,453","23,432","4,334,645","1,234"))

  as.numeric( gsub(",", "", c("9,453","23,432","4,334,645","1,234")))
      
# Delete the "plus four" part of the ZIP code
#   Use parentheses to save parts, saved as \\1, \\2, etc.
gsub("([0-9]{5})-[0-9]{4}","\\1",data.text)

# If more than two words, keep the first two
gsub("^([^ ]+ [^ ]+) .*$","\\1",data.text)

Class exercise
1. Add commas to these numbers
   c("9453","2332","4645","1234")


# standardize phone numbers
phone.nums <- grep("^\\(?[0-9]{3}(\\) |-| )?[0-9]{3}(-| )?[0-9]{4}",
                   data.text,value=TRUE)
phone.nums <- gsub("^\\(([0-9]{3})\\) ([0-9]{3}-[0-9]{4})","\\1-\\2",phone.nums)
phone.nums <- gsub("([0-9]{3})([0-9]{3})([0-9]{4})","\\1-\\2-\\3",phone.nums)

# 
# Class exercises
# 1. Spell out PA
# 2. Spell out Philadelphia
# 3. Change Phil to Phillip
# 4. Keep just the first word
# 5. Keep only area codes
