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

casesdf <- casesdf %>% rename('suspected' = 'Suspected cases',
                  'examined' = 'Microscopy examined',
                  'positive' = 'Microscopy positive')
#rename() was used in order to replace the names of multiple column names. before '=' is the new name and after the '=' is the old name

str(casesdf,vec.len=2) 
#this shows the first two columns (vec.len=4 will show 4 columns)
#year is currently a character, this is wrong as it only contains numerical values. this happened because of the way it was converted from the columns it has an X in front of it(R does not like numerical column names, since this would easily confused with column positions, so it adds an 'X' before them)

#Use mutate and gsub ti remove the 'X' from every value in the years column 

casesdf <- casesdf %>% mutate(year = gsub('X','',year))

?mutate

#6.2 Changing format with mute

str(casesdf)
#it still comes up as a character so in order to change it the function 'as.numeric' will take the character vector and convert each one to a numerical value

casesdf <- casesdf %>% 
  mutate(year = as.numeric(gsub('X','',year)))

class(casesdf$year) #unable to complete, still comes up as 

#6.3 Remove numbers from character columns
#Use mutate and gsub to remove all the numbers from the country columns

casesdf <- casesdf %>% mutate(country = gsub('[2:4]','',country))
#this deleted the unwanted numbers in the country column. the numbers were 2 and 4: 'united republic of `. tanzania2' 'south sudan4'

#6.4 Remove characters from number columns
#Use mutate and gsub to remove all the non-number characters from the suspected column

casesdf <- casesdf %>% mutate(suspected = as.numeric(gsub('[^0-9.-]','',suspected)))

class(casesdf$suspected)

#Make a function which cleans numbers and casts them to a numerical value. Call it 'clean_number'

clean_number <- function(x) {
  as.numeric(gsub('^[0-9]','',x))
}

casesdf %>% clean_number(positive) #unsure

#6.5 Mutate across
#using the across() function do the same operation for examined and positive at the same time

?across
#across(.cols, .fns, ..., .names)
#.cols is the columns to change
#.fns the function that needs to be used in each of the columns

casesdf <- casesdf %>% 
  mutate(across(c(suspected,examined,positive),clean_number))
#the selected columns were suspected, examined and positive. the function that was to be used is clean_number which cleans the column numbers and converts them to numeric 

class(casesdf$examined)

#What is an alternative tidy_select way to select everything except 'country'?

?tidy

casesdf %>% mutate(across(!country,clean_number))
#this chooses all the columns apart from the country column

#6.6 Calculations with mutate
#Using mutate you can make new columns that are derived from the ones that are already there

casesdf %>% mutate(new_col = do_some_stuff(old_col))

#'test_positivity' can be used as a metric of how well your disease surveillance is going. You have two columns of the number of tests performed 'examined' and the number of tests that were positive 'positive'. You can divide one by the other to give you the proportion of positive tests for each year and country

#Make a new column for 'test_positivity' rounded to two significant digits and add it to your table 

casesdf <- casesdf %>%
  mutate(test_positivity = round(examined / positive, 2))
#round() is a function that is used to round off values to a specific number of decimal value

?mutate

#7 FACTORS

#It is inefficient to have 'country' as a character array and is better to convert it to a factor
#factors are degined for categorical data, effectively as set of integers with names attached

#Use as.fcator and mutate to convert country to a factor

casesdf <- casesdf %>%
  mutate(country = as.factor(country))

class(casesdf$country)

#We can look at all the categories for any factor like so:
levels(casesdf$country)
#one of the counties is called Eritrae instead of Eritrea so this must be changed

#Use mutate and gsub to replace 'Eritrae' with 'Eritrea'; then convert it to a factor

casesdf %>%
  mutate(country = gsub('Eritrae','Eritrea',country)) %>%
  mutate(country = as.factor(country))

#8 WRITE TO FILE

#Once you have a nice table, you want to output the file. 'write_table' works in a similar way to 'read.table'

#using write.table to output your clean table with the following format:
#plain text
#tab delimited
#with a header line 
#no quotation marks
#no row names

write.table(casesdf, 'WMR2022_reported_cases_clean.txt', 
            sep = '\t',
            col.names = T,
            row.names = F,
            quote = F)

#9 The big challenge

# Sample data creation
df <- data.frame(
  character_column = rep(c("A", "B", "C"), each = 4),
  numeric_values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)
df

# Applying the transformation
df <- df %>%
  group_by(character_column) %>%
  mutate(average_value = mean(numeric_values, na.rm = TRUE)) %>%
  ungroup()

# View the modified dataframe
print(df)


