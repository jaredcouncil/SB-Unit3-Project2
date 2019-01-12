library(dplyr)
library(tidyverse)

#0: Upload csv file as data frame
titanic0 <- read_csv("titanic_original.csv")
View(titanic0)

#1: find missing values in "embarked" column, replace them with "S"
#determine how many NAs
titanic0 %>%
  group_by(embarked) %>% 
  count()
#determine which rows have NAs at "embarked"
titanic0 %>%
  filter(is.na(embarked))
#I discovered one row with all missing values. Attempted to isolate it
titanic0 %>%
  filter(is.na(pclass))
#realized that it's the only entry with "pclass" as NA, so created new data set (titanic1) omitting that row
titanic1 <- titanic0 %>% 
  filter(!is.na(pclass))
titanic1
#double-checked that that entry with all NAs is gone
titanic1 %>% 
  filter(is.na(embarked))
#now replacing all "embarked" NAs with "S" (with replace_na(). Tried str_replace() at first with no luck)
#Assigned to new variable
titanic2 <- titanic1 %>% 
  mutate(embarked = replace_na(embarked, "S"))
#tested that those entries were converted to "S" (by filtering unique ticket number from two entries above)
titanic2 %>% 
  filter(ticket == 113572)

#2: fill out missing ages
#see how many ages missing
titanic2 %>% 
  group_by(age) %>% 
  count() %>% 
  filter(is.na(age))
#Answer: 263 NAs
#Sampling of 13 entries with missing ages (so I can get a feel for data)
titanic2 %>% 
  filter(is.na(age)) %>% 
  sample_n(13)
#calculate the mean of the age column (excluding NAs)
titanic2 %>% 
  summarise(avg_age = mean(age,na.rm = TRUE))
#Answer: 29.9 years old
#use that to populate missing "age" values. (using mutate)
#Could plug 29.9 into mutate function manually, but I'm opting to incorporate previous code. Assigning new variable
titanic3 <- titanic2 %>% 
  mutate(age = replace_na(age, mean(age, na.rm = TRUE)))
#Ran two tests to make sure every entry had an age value
titanic3 %>% sample_n(13)
titanic3 %>% 
  filter(is.na(age))

#3: replace "boat" column NAs with strings
#assess
titanic3 %>% 
  group_by(boat) %>% 
  count() %>% 
  filter(is.na(boat))
#Answer: 823 missing values for boat
#replacing those NAs with string "None". Assign to new variable
titanic4 <- titanic3 %>% 
  mutate(boat = replace_na(boat, "None"))
#test
titanic4 %>% 
  filter(is.na(boat))

#4: create dummy column for missing cabins
#the other day I looked up automated function for this. Today I'm going to try and give it my own shot
#basically need to mutate a new column that has a one or zero based on NA in cabin column
#attempting to create function
#cabin_check <- function(x) {
#  cnt <- 0
#  for(y in x) {
#    count <- cnt + 1
#  }
#  if(is.na(x) == TRUE) {
#    return(0)
#  } else {
#    return(1)
#  }
#}
#samp_vec <- sample(titanic3$body, 10)
#cabin_check(samp_vec)

#attempting with ifelse() function
#titanic4 %>% 
#  mutate(has_cabin_number = ifelse(is.na == TRUE, 0, 1))
#doesn't work

#attempting with spread() 
#titanic4 %>% 
#  mutate(has_cabin_number = 1) %>% 
#  spread(cabin, has_cabin_number, fill = 0)
#doesn't work

#trying ifelse() again
#titanic4 %>% 
#  mutate(has_cabin_number = ifelse(is.na, 0, 1))
#Error in mutate_impl(.data, dots) : 
#  Evaluation error: cannot coerce type 'builtin' to vector of type 'logical'.
#titanic4 %>% 
#  mutate(has_cabin_number = ifelse(is.na(), 0, 1))
#Error in mutate_impl(.data, dots) : 
#  Evaluation error: 0 arguments passed to 'is.na' which requires 1.
#doesn't work
#voila! Assigned as new variable
titanic5 <- titanic4 %>% 
  mutate(has_cabin_number = ifelse(is.na(cabin), 0, 1))
#test (compare "cabin" and "has_cabin_number" columns)
titanic5 %>% 
  select(contains("cabin")) %>% 
  print(n = 25)

#export as csv
write.csv(titanic5, "titanic_clean.csv")
