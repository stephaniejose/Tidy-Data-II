#27/02/2024
#Workshop 6
#Tidy data 2

library(tidyr)
library(dplyr)

#this is the beetles data that we were looking at in the last session (workshop 5)
beetles <- read.table("dung_beetles.csv", sep=",",header=T)
#in this dataset there is a lot of columns that have been created that we don't need, so in order to get rid of those we will need to use the select() function 

?select
#this selects variables in a data frame, it will choose variables that you want 

#Selection features:
#':' is for selecting a range of consecutive variables
#'!' is for taking the complement of a set of variables
#'&' and '|' is for selecting the intersection for the union of two sets of variables
#'c()' is for combining selections 

beetles %>% select(1:68)
#this will pick out the species columns by number (from 1 to 68)
#this isn't very useful as it means that we need to count all the columns 

#Using helper functions
#they are used in conjunction with the main verbs to make specific tasks and calculations a bit easier 

beetles %>% pivot_longer(beetles, cols = start_with("X"), names_to='Extra')

beetles %>% select(Month, Site, contains('_'))
#it chooses the data with a an '_' in it, therefore, it won't include the column names that start with X as they are automatically made by R and do not contain an '_'

beetles %>% select(!Month)
#this is another way to remove the extra columns
#this is called the negation operator '!'
#by putting this in front of any tidy-select command it will select everything except that, so it will selet every column except month

#Look at your data and figure out another way to select those columns. You'll have to use a helper function and the negation operator
beetles <- beetles %>% select(!starts_with('X'))
#in this case it tells R to negate (ignore) all the columns that start with 'X'

#FILTER
#filter provides a way to select subsets of rows based on specific criteria
beetles <- beetles %>% 
  filter(Onthophagus_sideki > 10) #so for the column of "Onthaphagus_sideki" it will only display the values in this column that are greater than 10

beetles <- beetles %>% 
  filter(Onthophagus_sideki & Ochicanthon_woroae > 10)
#the number of rows decreases (in comparison to when Onthopagus sideki was used alone)

#This will write a script that selects only the row(s) for which Ochicanthon woroae has greater than 15 samples in the month of July
beetles <- beetles %>%
  filter(Ochicanthon_woroae > 15, Month == "July")

#RENAME
#this will allow us to individually rename columns in the file, however, in the following case this will be very long
beetles %>% rename(c(Copris_agnus=Copis_agnus,
                     Copris_ramosiceps=Copis_ramosiceps,
                     ...)) 

?rename
#rename() changes the names of individual variables using new_name = old_name 
#rename_with() renames columns using a function














