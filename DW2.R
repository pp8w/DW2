library(dplyr)
library(tidyr)

#Reading file from CSV
rawcsv <- read.csv("titanic_original.csv")

#Changing from DF to TBL
csvtbl <- tbl_df(rawcsv)


#Embarcation
#Finding missing values and fill with S, watch for empty
vemb <- as.character(csvtbl$embarked)

embreplace <- function(item) {
  if (item =="") {item = "S"} 
      else {item = item}
  }

vemb <- vapply(vemb, embreplace, character(1))
csvtbl$embarked <- vemb

#Age - Find mean age of existing values, populate empties with that value

vage <- as.numeric(csvtbl$age)
meanage <- mean (vage, na.rm = TRUE)
agereplace <- function(item, age) {
  if (is.na(item)) {item = age } 
  else {item = item}
  }

vage <- sapply(vage, agereplace, meanage)
csvtbl$age <- vage


#Lifeboat - Fill empty boats column with NA

vlife <- as.character(csvtbl$boat)

lifereplace <- function(item) {
   if (item=="") {item="NA"}
  else {item=item}
  }

vlife <- sapply(vlife, lifereplace)
csvtbl$boat=vlife


#Cabin
#If cabin value, has_cabin_number column = 1

vcabin <- as.character(csvtbl$cabin)

cabinreplace <- function(item) {
  if (item!="") {item=1}
  else {item=0}
}

vcabin <- vapply(vcabin, cabinreplace, numeric(1))
print(vcabin)
csvtbl <- cbind(csvtbl, vcabin)
glimpse(csvtbl)

#Output
write.csv(csvtbl, file = "titanic_clean.csv", row.names=FALSE)


