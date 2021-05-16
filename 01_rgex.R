################################################################################
# Dealing with Regular Expressions aka regex
# By Anahita Sanandaji
# Copyright 2020 may not be shared or posted without permission. 
################################################################################


# Please set your working directory now: you can also go to session -> Set working directory
getwd()
#setwd("?")


#===============================================================================
# 1) Intro
#===============================================================================
# A regular expression (aka regex) is a sequence of characters that define a search pattern, 
# mainly for use in pattern matching with text strings. 

## I KNOW! regex syntax can appear a bit confusing.
# We will just touch the basic in our class.
# feel free to explore more: http://www.regular-expressions.info/
# This is also a great resource for Strings in R that covers regex as well: https://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
# I also love these two references:
# 1: http://uc-r.github.io/regex
# 2: https://homerhanumat.github.io/r-notes/entering-a-regex-in-r.html

help(regex)
# or
help(regexp)

#===============================================================================
# 2) Substitution: Use of sub() and gsub() with Metacharcters
#===============================================================================
# Metacharacters consist of non-alphanumeric symbols such as:
# .    \\    |    (    )    [    {    $    *    +   ?

# To match metacharacters in R you need to escape them with a double backslash "\\"
# examples $ -> \\$  * -> \\*
#-------------------------------------------------------------------------------

### 2.1 sub(): To replace the first matching occurrence of a pattern use sub():

sub(pattern = "OLD", replacement = "NEW", "OLD")
sub(pattern = "x", replacement = "e", "This is a txxt")

# substitute $ with ?: these are metacharcters and we need to use \\
sub(pattern = "\\$", replacement = "\\?", "How are you$")

#This will not work as expected if we do not put \\
sub(pattern = "$", replacement = "?", "How are you$")

# substitute ^ with Pizza
sub("\\^", "Pizza", "I love ^^.")

myText <- c("one ^", "two ^s", "I love ^", "Peperoni ^")
sub("\\^", "Pizza", myText)

#-------------------------------------------------------------------------------
### 2.2 gsub(): To replace all matching occurrences of a pattern use gsub():
sub(pattern = "OLD", replacement = "NEW", "OLD OLD OLD")
gsub(pattern = "OLD", replacement = "NEW", "OLD OLD OLD")

# substitute all ^ with Pizza
gsub("\\^", "Pizza", "I love ^^.")

myText <- c("one ^^", "two ^s", "I love ^^", "Peperoni ^")
gsub("\\^", "Pizza", myText)


#===============================================================================
# 3) Character classes
#===============================================================================

### 3.1 Normal Character classes
### To match one of several characters in a set we can enclose the characters of 
### concern with square brackets [ ]. ### Check my lecture notes for the table 

x <- "2020Sky 1234 is987 b567lue, YES."

# remove numbers
gsub(pattern = "[0-9]", replacement = "", x)

#remove lowercase
gsub(pattern = "[a-z]", replacement = "", x)

#remove all characters
gsub(pattern = "[a-zA-Z]", replacement = "", x)


#==============================================================================
### 3.2 POSIX character classes
#Closely related to regex character classes are POSIX character
#classes which are expressed in double brackets [[ ]].
### Check my lecture notes for the table 

y <- "2020Cloud 1234 is987 White1111."
# remove numbers
gsub(pattern = "[[:digit:]]", replacement = "", y)

#remove lowercase
gsub(pattern = "[[:lower:]]", replacement = "", y)

#remove all characters
gsub(pattern = "[[:alpha:]]", replacement = "", y)

#===============================================================================
# 4) How we use it in our class for text analysis
#===============================================================================
myData <- "<br /><br />!!!Now we are 123456     **        engaged in a great    civil war!!!??,   222 testing?*!."

#Remove HTML codes
myCleanedData <- gsub("<.*?>", "", myData)
myData
myCleanedData

#Remove all punctuation marks
myCleanedData <- gsub("[[:punct:]]","", myCleanedData)
myData
myCleanedData

#Remove digits
myCleanedData <- gsub("\\d+", "", myCleanedData)
myData
myCleanedData

#Remove all extra white space characters
# \\s+ will match any space character (space, tab etc), or repeats of space characters, 
# and will replace it with a single space " "
myCleanedData <- gsub("  ", " ", myCleanedData)

#better
myCleanedData <- gsub("\\s+", " ", myCleanedData)

myData
myCleanedData





