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

fixcopris <- function(x) 
{gsub("opis","opris",x)}
#the first line defines a function named fixedcopris that takes a single argument x. The argument is expected to be a string
#the first argument is gsub is the pattern to be searched for which is "opis" and the second argument is the replacement string "opris". The third argument is the input string x where the substitution will be applied

beetles <- rename_with(beetles, fixcopris, .col = contains('_'))
#we used the rename_with that is used with other function to incorporate our fixcopris function in order to change more than one column at a time
#the first argument in the rename_with is the dataframe we want to modify which is "beetles". The second argument is "fixcopris: which replaces occurrences of "opis" with "opris". 
#'.col = contains('-') tells rename_with to apply fixcopris function only to those columns whose names contain the underscore character ('_')

#Pivot the data into a table with a species name and counts using pivot_longer
beetles <- beetles %>% pivot_longer(cols = contains('_'), 
                         names_to ='Spp', 
                         values_to ='Counts')

?pivot_longer

#Pipe the 'select' and 'rename' commands together, then adds your pivots
beetles <- beetles %>% 
  select(!starts_with('X')) %>% 
  rename_with(~ gsub("opis", "opris", .x), .cols = contains ('_'))%>%
  pivot_longer(cols = contains('_'), names_to ='Spp', values_to = 'Counts')
#to pipe them together it required a bit of modification to my previous code when i had used them separately
#'~' is used to create a formula, which, when used in functions like 'rename_with' it represents a shorthand for writing anonymous functions. It is a compact way to define an anonymous function that will be applied to each selected column name

beetles <- rename_with(beetles, tolower)

#SEPARATE
#(Extra) How would you calculate the total number of Copris genus beetles caught without separating the columns? How many are there?

#In order to calculate the total number of Copris genus beetles it would be easier to split it into two columns. 'separate_wider_delim' should be used

?separate_longer_delim #splits by a delimiter 
#separate_longer_position() splits by a fixed width

#separate_longer_delim(data, cols, delim,...)
#separate_longer_position(data, cols, width, ..., keep_empty = FALSE)

beetles <- beetles %>% separate_wider_delim("spp",
                                 names = c('genus','species'), 
                                 delim = "_")
#here i named the column that i would like to separate into two columns and called the new columns 'genus' and 'species. i kept getting error messages in regards to delim but i had just made delim='_' which is the space in between the species and genus names in order to show the separation and therefore translate them into different columns
#'delim = '_'' the function interprets this to mean that it should split each value in the specified column wherever it finds an underscore character, and then create new columns based on these splits

#MUTATE
#Mutate will make an new column with the output of a function that we apply to a column 
#Mutate can make things easier to read and make it looks more presentable e.g. remove the '_'
#it can be used with any function that takes a vector, and returns a vector of the same length 
#can be used to fix typos, capitalisations and other inconsistencies using functions like gsub / toupper / tolower

#Example - to create a new column 
mydf %>% mutate("new_column" = do_some_stuff(old_column) )

#Example - fix typos, capitalisations etc
mydf %>% mutate("old_column" = do_some_stuff(old_column) )

#this will replace the underscores in our dataframe to empty space
beetles %>% mutate("spp" = gsub('_',' ',spp))
#this replaces all the underscores in the column spp using gsub. it subs out the '_' with empty space to make it look nicer

casesdf <- read.table("WMR2022_reported_cases_3.txt",
                      sep="\t",
                      header=T,
                      na.strings=c("")) %>% 
  fill(country) %>% 
  pivot_longer(cols=c(3:14),
               names_to="year",
               values_to="cases") %>%
  pivot_wider(names_from = method,
              values_from = cases)

?rename






