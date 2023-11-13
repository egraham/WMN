# Shiny app to pull data from "Who is Making News" (https://www.whoismakingnews.com/), 
#   add a best-guess gender of name column, and display data for interaction with Shiny.

#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("diffdf")

############################################################
##### To find probable gender of names, use Genderize: #####
############################################################
# https://genderize.io/
# up to 1000 names per day free
#https://api.genderize.io?name=peter
# The request will render a response like the following:
# {
#   "name": "peter",
#   "gender": "male",
#   "probability": 0.99,
#   "count": 165452
# }
############################################################

#install.packages("rjson")
#install.packages("jsonlite")

library(jsonlite)
library(tidyverse)
library(diffdf)
library(rdrop2)

# This function will be used to get Genderized names
readUrl <- function(JSON_query) {
  tryCatch({
    next_entry <- data.frame(fromJSON(JSON_query))
    message(paste("Success:", JSON_query))
    return(next_entry)
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", JSON_query))
    message("Here's the original error message:")
    message(cond)
    return(data.frame(gender=NA, count=NA, probability=NA))
  },
  warning=function(cond) {
    message(paste("URL caused a warning:", JSON_query))
    message("Here's the original warning message:")
    message(cond)
    return(data.frame(gender=NA, count=NA, probability=NA))
  },
  finally={
    message(paste("Processed URL:", JSON_query))
  }
  )
}

# Get the previously saved data sets and run from desktop
setwd("C:/Users/erkso/Desktop/WMN")

wmn_data_previous <- readRDS("wmn_data_previous.RDS")
wmn_names_genderized <- readRDS("wmn_names_genderized.RDS") |> 
  mutate(NumChildren = as.character(NumChildren))

# Regularly download new WMN dataset and compare to old (wmn_data.RData) to find new and changed
#   data entries.  Update the names_genderized.RData
#   and the wmn_data.RData data files for use with visualizations.


# Download the new data from Kristen Browde's website that accumulates
#   data on "Who is really committing crimes against children".
# These data change with additions, revisions, possibly subtractions as data is updated.

# Download newest data set:
#Old URL - wmn_data_current <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRsWWugIo1pp0Xc1WmMmvawFzQslpUqlIMCjw3JhwOrW2sS6gOvXv3C_TV9eHAD46wjiaqzPNvLbRUT/pub?gid=1732993794&single=true&output=csv')
#not direct - wmn_data_current <- read_csv('https://docs.google.com/spreadsheets/d/1t6I-j30Nf7pTwl2i1snMbFWcTbWkYMtnk192JL1Og9k/edit#gid=1882457294')
wmn_data_current <- read_csv('News Coverage Database - TikTok Series data  - Main.csv') |> 
  filter(!Deleted) |>  # remove deleted names
  filter(!is.na(Name))  # Remove rows that do not have a Name value (output can include lots of blank lines)
summary(wmn_data_current)
  
wmn_data_current_temp <- wmn_data_current
#wmn_data_current <- wmn_data_current_temp  # recover wihtout having to download again

# Replace column names that have spaces with no-space alternatives (but keep capitalization, just because)
#   also, don't change NumChildren into a value, since "hundreds" exist in the column
#   and the column is not very populated anyway, so may not be used.  
wmn_data_current <- wmn_data_current |>
  rename(Political = 'Political affiliation', 
         DQueen = 'Drag Queen',
         NumChildren = 'Number of children affected') |>
  mutate(Category = factor(Category)) |>
  mutate(Political = factor(Political)) |>
  mutate(NumChildren = as.character(NumChildren)) |>
  mutate(Trans = as.logical(Trans)) |>
  mutate(Notes = as.character(Notes)) |>
  mutate(Sentence = as.character(Sentence)) |>  # mixed numbers and strings, predict makes it numeric
  mutate(Name = str_to_title(Name)) # put into upper-case, both names, all names
summary(wmn_data_current)

# Change "Cop" or "cop" into "Police", correct typos, fix inconsistent plurals
wmn_data_current <- wmn_data_current |>
  mutate(Category = str_replace(Category,'Cop|cop', 'Police')) |> 
  mutate(Relation = str_replace(Relation,'Cop|cop', 'Police')) |>
  # correct typos in server data set:
  mutate(Category = str_replace(Category,'Day Care/babsyitter', 'Day Care/babysitter')) |>
  mutate(State = str_replace(State,'Wa', 'WA')) |>
  mutate(State = str_replace(State,'wV', 'WV')) |> 
  mutate(State = str_replace(State,'IS', 'IA')) |> 
  filter(State != "IT") |>  # remove the Pope in Italy - this is US only?
  # correct plurals and capitalizations (all capitalized, all plural)
  mutate(Category = str_replace(Category,"Priests/brothers", "Priests/Brothers")) |>
  mutate(Category = str_replace(Category,"Mormon leaders", "Mormon Leaders")) |>
  mutate(Category = str_replace(Category,"Missionary", "Missionaries")) |>
  mutate(Category = str_replace(Category,"Family Member", "Family Members")) |>
  mutate(Category = str_replace(Category,"Family Friend/neighbor", "Family Friends/Neighbors")) |>
  mutate(Category = str_replace(Category,"Teachers/aides", "Teachers/Aides")) |>
  mutate(Category = str_replace(Category,"Day Care/babysitter", "Day Care/Babysitters")) |>
  mutate(Category = str_replace(Category,"Coach", "Coaches")) |>
  mutate(Category = str_replace(Category,"Doctor", "Doctors"))
summary(wmn_data_current)

# # Add "first names" column, assuming one space between first and last name
# #   and that everyone has a first name (not true).  Entries such as might occur for first names
# #   like "FL Sting Pasco", "18 men", or "unnamed parent" will return first names that will
# #   not return a result in the Genderize database  when filtering for >100 entries
# #   (to remove noise). Actual names like "La Luz" will also not result in usable data.

wmn_data_current <- wmn_data_current |>
  mutate(  # now extract first names and put into column
    First_name = str_to_title(sapply(strsplit(Name, " "), `[`, 1)),
    .after = Name
  ) |>
  #filter(nchar(First_name) >2) |>  # Remove 1- and 2-character names (to limit Genderize calls?)
  mutate(Date = mdy(Date)) |>  # change character string to actual dates
  arrange(Date, Name)  # sort by date then name
summary(wmn_data_current)

# remove *some* items that don't make sense for genderized names
wmn_data_current_for_names <- wmn_data_current |> 
  filter(nchar(First_name) >2) |>   # Remove 1- and 2-character names (to limit Genderize calls?)
  distinct(.keep_all = TRUE)  # remove redundant identical rows
  # this will NOT remove redundant names when entries are different (for example, when
  # updates to a case or other info is provided - we want to keep this info)
summary(wmn_data_current_for_names)
  
# Do a left-join, keeping all columns in wmn_data_current, adding previously 
#   determined gender categories from wmn_names_genderized (Gender, Gender_probability, Gender_count),
#   adding as NA to new entries (or changed)
wmn_names_genderized <- left_join(wmn_data_current_for_names, wmn_names_genderized, 
                                  by = c("Date", "Name", "First_name", 
                                         "State", "Category", "Relation", 
                                         "Political", "Deleted", "Trans", 
                                         "DQueen", "Status", "NumChildren", 
                                         "Charges", "Sentence", "URL", "Notes"))

# remove redundant entries (this happens, not sure why)
wmn_names_genderized <- wmn_names_genderized |> 
  distinct(Date, Name, State, Category, .keep_all = TRUE)

# clean up Genderized names, remove those with numbers or punctuation
wmn_names_genderized <- wmn_names_genderized |>
  filter(!str_detect(Name, "Unnamed")) |>
  filter(!str_detect(Name, "unnamed")) |>
  filter(!str_detect(Name, "non")) |>
  filter(!str_detect(Name, "Non")) |>
  filter(!str_detect(Name, "not")) |>
  filter(!str_detect(Name, "Not")) 
wmn_names_genderized <- wmn_names_genderized[!grepl("[0-9.]", wmn_names_genderized$Name),]

wmn_names_genderized_temp <- wmn_names_genderized

# Note that some rows have the same person but with different charges.  
#   This requires a new df for counting names, since charges and 
#   number of children is difficult to pull out or is not complete.
# Also, do not remove rows based on Deleted flag because comments and other info can still
#   be relevant in duplicate entries.

# PUll out the rows in the "new" wmn_data_current that don't occur in the "old" wmn_data_previous file,
#   since the new one on the server is considered "truth".  
#   This is based on date and full name and will use the "anti_join" method.
#   "anti_join" shows the rows from the first dataset data1 where there are not matching values
#   from the second dataset data2: not_in_common <- anti_join(data1, data2, join_by(a, c))
## description from: https://datascienceplus.com/merging-datasets-with-tidyverse/

# Below, we create lists for counts, eliminating multiple entries for the same individual

# This will have redundant names (multiple charges) and will be further changed to
#   allow for more accurate counting of names (but not number of children affected).
#   We do not remove all multiples here, although this will slow the process (more api calls)

# # Send the NA Genders to the Genderize URL to get an updated corresponding list.
# Go row-by-row, not sure how else to do this without subsetting and being confusing to join again
# Returned JSON looks like this: {"count":2653329,"name":"David","gender":"male","probability":1.0}

# How many to process?
count_gender_NAs <- wmn_names_genderized |>
  filter(is.na(Gender)) |>
  nrow()
print(paste(count_gender_NAs, "names to genderize", sep = " "))

for (rowindex in 1:dim(wmn_names_genderized)[1]) {  # march through the data set!
  if(is.na(wmn_names_genderized$Gender[rowindex])) {  # if there is an NA where a Gender should be
    # construct query
    JSON_query <-
      paste("https://api.genderize.io?name=",
            wmn_names_genderized$First_name[rowindex],
            sep = "")
    print(paste("Trying: #", rowindex, ": ", JSON_query, sep=""))
    
    # use tryCatch() (at the top, inside the function readUrl) for reading external URLs
    next_entry <-
      readUrl(JSON_query)  # get Genderized name list from webserver
    print(paste("line", 305, next_entry, sep=" "))
    
    if (is.na(next_entry$gender)) {  
      # if no gender is returned, e.g., $gender=null, could not find name
      wmn_names_genderized$Gender[rowindex] = "atypical"
      wmn_names_genderized$Gender_count[rowindex] = 0
      wmn_names_genderized$Gender_probability[rowindex] = 0
      print("Attempt at Genderizing name failed, adding dummy, continuing...")
      
    } else {  # it worked!
      wmn_names_genderized$Gender[rowindex] = next_entry$gender
      wmn_names_genderized$Gender_count[rowindex] = next_entry$count
      wmn_names_genderized$Gender_probability[rowindex] = next_entry$probability
      print(paste("Row: ", rowindex, " Success in finding gender of name", sep=""))
    }
  } else {
    # already an entry for gender, skip to next row
  }
}

summary(wmn_names_genderized)

# Save the updated data sets back to DropBox
wmn_data_previous <- wmn_data_current

# drop_upload('wmn_data_previous.RDS', path = "WMN_shiny", dtoken = token)
# drop_upload('wmn_names_genderized.RDS', path = "WMN_shiny", dtoken = token)

# save locally also
saveRDS(wmn_data_previous, file = "wmn_data_previous.RDS")
saveRDS(wmn_names_genderized, file = "wmn_names_genderized.RDS")


# For other manipulations
write_csv(wmn_data_previous, file="wmn_data_previous.csv")
write_csv(wmn_names_genderized, file="wmn_names_genderized.csv")

###########################################################################################
############################# End updating data sets ######################################
###########################################################################################

# # How many atypical names?
# count_gender_atypicals <- wmn_names_genderized |>
#   filter(Gender == "atypical") |>
#   nrow()
# print(paste(count_gender_atypicals, "atypical names to look at again", sep = " "))

############################################################################################
########### transform KFF download data ###################################################

# KFF_data_pca <- KFF_data
# KFF_data_pca <- read_csv('KFF_data.csv', na = c("na", "NA", "N/A"))
# #load("wmn_data_previous.RDS")
# 
# 
# 
# statecodes <- KFF_data$Location 
# statecodes <- state.abb[match(statecodes,state.name)]
# # Add DC and remove whole US (first line)
# KFF_data <- KFF_data |>
#   mutate(ST = statecodes)
# KFF_data <- KFF_data[-1,]
# KFF_data[is.na(KFF_data$ST),]$ST <- "DC"
# 
# # use the whole wmn data set, not the subset genderized data (from the previous tab)!
# # get State, crime count, and per-state crime frequency
# state_counts <- wmn_data_previous |>
#   group_by(State) |>
#   summarize(State_n = n())# |>
# #mutate(crime_freq = State_n / sum(State_n))
# 
# # Combine data set to include the proportions per state and subtract to get positives and negatives
# KFF_data <- inner_join(state_counts, KFF_data, by = c("State" = "ST"))
# 
# # Subtract frequency of perps from frequency of total residents to get positives (more crimes) and negatives.
# KFF_data <- KFF_data |>
#   mutate(crime_freq = State_n / sum(State_n), .after = State_n) |> 
#   mutate(expected_crime = Total_Residents * sum(State_n), .after = crime_freq) |> 
#   mutate(crime_percent_from_expected = (State_n - expected_crime) / expected_crime * 100, .after = expected_crime) |> 
#   mutate(mycolor = ifelse(crime_percent_from_expected > 0, "type1", "type2"), .after = crime_percent_from_expected)# |> 
# #mutate(State=factor(State, State))
# 
# # add an index for slope sorted by location
# KFF_data <- sorting(KFF_data, "Location", reverse=TRUE)
# KFF_data$state_index <- 1:nrow(KFF_data)
# 
# 
# 
# 
# # # Turn state names into two-letter codes to compare with WMN data
# statecodes <- state_crime$State 
# statecodes <- state.abb[match(statecodes,state.name)]
# state_crime <- state_crime |>
#   mutate(ST = statecodes)
# state_punishment <- state_punishment |>
#   mutate(ST = statecodes)
# # 
# # Turn state names into two-letter codes to compare with WMN data
# statecodes <- KFF_data_pca$Location
# statecodes <- state.abb[match(statecodes,state.name)]
# # Add DC and remove whole US (first line)
# KFF_data_pca <- KFF_data_pca |>
#   mutate(ST = statecodes)
# KFF_data_pca <- KFF_data_pca[-1,] # remove USA total
# KFF_data_pca <- KFF_data_pca[!is.na(KFF_data_pca$ST),] # remove DC
# 
# # use the whole wmn data set, not the subset genderized data (from the previous tab)!
# # get State, crime count, and per-state crime frequency
# state_counts <- wmn_data_previous |>
#   group_by(State) |>
#   summarize(State_n = n())# |>
# #mutate(crime_freq = State_n / sum(State_n))
# 
# # Combine data set to include the proportions per state and subtract to get positives and negatives
# wmn_data <- inner_join(state_counts, KFF_data_pca, by = c("State" = "ST"))
# 
# # Subtract frequency of perps from frequency of total residents to get positives (more crimes) and negatives.
# wmn_data <- wmn_data |>
#   mutate(crime_freq = State_n / sum(State_n), .after = State_n) |>
#   mutate(expected_crime = Total_Residents * sum(State_n), .after = crime_freq) |>
#   mutate(crime_percent_from_expected = (State_n - expected_crime) / expected_crime * 100, .after = expected_crime) |> 
#   select(1:5,)
