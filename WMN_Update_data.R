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

# https://docs.google.com/spreadsheets/d/1t6I-j30Nf7pTwl2i1snMbFWcTbWkYMtnk192JL1Og9k/edit#gid=1882457294

#install.packages("rjson")
#install.packages("jsonlite")

library(jsonlite)
library(tidyverse)
library(diffdf)
library(rdrop2)
library(tsibble)
library(fable)

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
  mutate(NumChildren = as.character(NumChildren)) |>
  mutate(Sentence = as.character(Sentence))

# wmn_names_genderized <- read_csv("wmn_names_genderized.csv") |>
#   mutate(NumChildren = as.character(NumChildren)) |>
#   mutate(Date = mdy(Date)) |>
#   mutate(Sentence = as.character(Sentence))

# Regularly download new WMN dataset and compare to old (wmn_data.RData) to find new and changed
#   data entries.  Update the names_genderized.RData
#   and the wmn_data.RData data files for use with visualizations.


# Download the new data from Kristen Browde's website that accumulates
#   data on "Who is really committing crimes against children".
# These data change with additions, revisions, possibly subtractions as data is updated.

# Download newest data set:
#Old URL - wmn_data_current <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRsWWugIo1pp0Xc1WmMmvawFzQslpUqlIMCjw3JhwOrW2sS6gOvXv3C_TV9eHAD46wjiaqzPNvLbRUT/pub?gid=1732993794&single=true&output=csv')
#not direct - wmn_data_current <- read_csv('https://docs.google.com/spreadsheets/d/1t6I-j30Nf7pTwl2i1snMbFWcTbWkYMtnk192JL1Og9k/edit#gid=1882457294')

#https://docs.google.com/spreadsheets/d/1t6I-j30Nf7pTwl2i1snMbFWcTbWkYMtnk192JL1Og9k/edit#gid=1882457294

wmn_data_current <- read_csv('News Coverage Database - TikTok Series data  - Main.csv') |>
  filter(!Deleted) |>  # remove deleted names
  filter(!is.na(Name))  # Remove rows that do not have a Name value (output can include lots of blank lines)
summary(wmn_data_current)

#wmn_data_current_temp <- wmn_data_current
#wmn_data_current <- wmn_data_current_temp  # recover wihtout having to download again

# # some weirdness - someone renamed column in data
# wmn_data_current <- wmn_data_current |>
#   rename(Date = 'gma')

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
#summary(wmn_data_current)

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
#summary(wmn_data_current)

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
#summary(wmn_data_current)

# remove *some* items that don't make sense for genderized names
wmn_data_current_for_names <- wmn_data_current |>
  filter(nchar(First_name) >2) |>   # Remove 1- and 2-character names (to limit Genderize calls?)
  distinct(.keep_all = TRUE)  # remove redundant identical rows
  # this will NOT remove redundant names when entries are different (for example, when
  # updates to a case or other info is provided - we want to keep this info)
#summary(wmn_data_current_for_names)

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

# look at existing list of gendered names and use those before going online.
wmn_genderized_list <- read_csv("GenderedNamesList.csv")

# this could be done much faster
for (rowindex in 1:dim(wmn_names_genderized)[1]) {  # march through the data set!
  if(is.na(wmn_names_genderized$Gender[rowindex])) {  # if there is an NA where a Gender should be
    # loop through gendered list
    namefound <- FALSE
    for (rowindex2 in 1:dim(wmn_genderized_list)[1]) {
      if(wmn_names_genderized$First_name[rowindex] == wmn_genderized_list$First_name[rowindex2]){
        wmn_names_genderized$Gender[rowindex] = wmn_genderized_list$Gender[rowindex2]
        wmn_names_genderized$Gender_count[rowindex] = wmn_genderized_list$Gender_count[rowindex2]
        wmn_names_genderized$Gender_probability[rowindex] = wmn_genderized_list$Gender_probability[rowindex2]
        print(paste("Success in finding gender of name: ", wmn_names_genderized$First_name[rowindex], sep=""))
        namefound <- TRUE
      }
    }
    if(!namefound){
      print(paste("Could not find: ", wmn_names_genderized$First_name[rowindex], sep=""))
    }
  }
}

# How many to process online?
count_gender_NAs <- wmn_names_genderized |>
  filter(is.na(Gender)) |>
  nrow()
print(paste(count_gender_NAs, "names to genderize", sep = " "))

# note, after 100: {"error":"Request limit reached"}
for (rowindex in 1:dim(wmn_names_genderized)[1]) {  # march through the data set!
  if(is.na(wmn_names_genderized$Gender[rowindex])) {  # if there is an NA where a Gender should be
    # construct query
    JSON_query <-
      paste("https://api.genderize.io?name=",
            wmn_names_genderized$First_name[rowindex],
            sep = "")
    print("")
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
      print("Attempt at Genderizing name failed, adding dummy, continuing after pause...")
      Sys.sleep(1)

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

# create updated generized name list after the call to the web service
wmn_genderized_list <- wmn_names_genderized |>
  distinct(First_name, Gender, Gender_count, Gender_probability) |> # get all genders that are unique
  group_by(First_name) |> # remove the ones with the smallest Gender_count (older entries)
  top_n(1, Gender_count) |>
  arrange(First_name)

write_csv(wmn_genderized_list, file="GenderedNamesList.csv")

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

## Update of Accuracy of predictions ##
## Use daily data from the second week in July, when sampling changed, to one month ago
#daterange_start <- seq(ymd("2023-07-14"), as.Date(now())-days(30), by=1)
daterange_start <- seq(ymd("2023-07-14"), ymd("2024-05-23"), by=1)

# fill in zeros for days with no crimes, create a df ready for time series
eventsperday <- wmn_data_previous |>
  group_by(Date) |>
  summarize(perday = n()) |>
  complete(Date = seq.Date(min(Date), max(Date), by = "days"),
           fill = list(perday = 0)) |>
  mutate(cum_sum = cumsum(perday)) |>
  arrange(Date)

############################################################################
# Linear model
##############
# daily data from the second week in July, when sampling changed
sumduration = 30
linear_error <- tibble(date=as.Date(x = integer(0), origin = "2000-01-01"), actual=numeric(), lin_predicted=numeric(), lin_diffcrimes=numeric())

for(i in daterange_start){
  numEventsperday_reg <- eventsperday |>
    filter(Date > ymd("2023-07-14"))
    #filter(Date >= as.Date(i) & Date <= as.Date(i) + days(sumduration))

  cumsum30.lm = lm(cum_sum ~ Date, data=numEventsperday_reg)
  # slope is crimes per day the prediction
  cumsum_m <- round(cumsum30.lm$coefficients[2],0)
  # actual crimes
  num_crimes_day <- numEventsperday_reg$perday[numEventsperday_reg$Date == as.Date(i) + days(sumduration)]
  # numeric zero if no crimes that day
  if(length(num_crimes_day)>0){
    # predicted minus actual, so if + then over-predicting
    linear_error <- linear_error |>
      add_row(date = as.Date(i) + days(sumduration), actual = num_crimes_day, lin_predicted = cumsum_m, lin_diffcrimes = cumsum_m - num_crimes_day)
  }
  #print(paste(as.Date(i) + days(30), "predicted:", cumsum_m, "actual:", numEventsperday_reg$perday[numEventsperday_reg$Date == as.Date(i) + days(30)], sep=" "))
}

###### as example only, below ###########
# for plotting box plot
linear_over_predict <- linear_error |>
  filter(lin_diffcrimes > 0)
linear_under_predict <- linear_error |>
  filter(lin_diffcrimes < 0)
linear_well_predict <- linear_error |>
  filter(lin_diffcrimes == 0)
linear_t <- t.test(x = linear_error$lin_predicted, y = linear_error$actual,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

if(linear_t$estimate > 0){
  error_sign <- paste0(" more crimes per day predicted than actual\n(S.D. = ", round(sd(linear_error$lin_diffcrimes),1),"; n = ",length(linear_error$lin_predicted),")")
} else if(linear_t$estimate < 0){
  error_sign <- paste0(" fewer crimes predicted than actual\n(S.D. = ", round(sd(linear_error$lin_diffcrimes),1),"; n = ",length(linear_error$lin_predicted),")")
} else if(linear_t$estimate == 0){
  error_sign <- paste0(", an impossibly perfect estimate\n(S.D. = ", round(sd(linear_error$lin_diffcrimes),1),"; n = ",length(linear_error$lin_predicted),")")
}

linear_subt <- paste("Mean error is: ",round(linear_t$estimate,3), error_sign, sep="")

# Basic jitter box plot
# lin_err_plot <- ggplot(linear_error, aes(x = "", y = lin_diffcrimes)) +
#   geom_boxplot(outlier.shape = NA) +
#   #stat_summary(fun = mean, geom = "errorbar", aes(xmax = 5, xmin = -5), width = .75, linetype = "dashed") +
#   #stat_summary(fun=mean, geom='point', shape=20, size=8, col="green") +
#   geom_jitter(data=linear_over_predict, col="red") +
#   geom_jitter(data=linear_under_predict, col="blue") +
#   geom_jitter(data=linear_well_predict, col="black") +
#
#   #
# theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# # add titles
# lin_err_plot <- lin_err_plot +
#   labs(
#     title = "Errors of Linear Model Estimates",
#     subtitle = linear_subt,
#     y = "Predicted minus Actual",
#     x = ""
#   )
#
# # make theme prettier
# lin_err_plot <- lin_err_plot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# lin_err_plot

###### as example only, below ###########
# for plotting line plot

# lin_err_lineplot <- ggplot(linear_error, aes(x = date, y = lin_diffcrimes)) +
#   geom_line() +
#   geom_point(data=linear_over_predict, aes(x = date, y = lin_diffcrimes), col="red") +
#   geom_point(data=linear_under_predict, aes(x = date, y = lin_diffcrimes), col="blue") +
#   geom_point(data=linear_well_predict, aes(x = date, y = lin_diffcrimes), col="black") +
#   geom_hline(yintercept = 0) +
#   #coord_flip() +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# # add titles
# lin_err_lineplot <- lin_err_lineplot +
#   labs(
#     title = "Errors of Linear Model Estimates",
#     subtitle = "Error per date",
#     y = "Predicted minus Actual",
#     x = "Date"
#   )
#
# # make theme prettier
# lin_err_lineplot <- lin_err_lineplot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# lin_err_lineplot


####################################################################
# ETS model - last month of data
###########

##
eventsperday_ets <- eventsperday |>
  select(date=Date, value=perday) |>
  as_tsibble(index=date)

sumduration = 30
ets_error <- tibble(date=as.Date(x = integer(0), origin = "2000-01-01"), actual=numeric(), ets_predicted=numeric(), ets_diffcrimes=numeric())

# to test
#i <- "2023-10-16"#daterange_start[3]

# this is WILDLY inefficient, to do this for the whole set every time!

for(i in daterange_start){
  numEventsperday_ets_error <- eventsperday_ets |>
    filter(date >= as.Date(i) & date < as.Date(i) + days(sumduration))

  # ETS: Exponential smoothing state space model
  fit <- numEventsperday_ets_error |>
    model(
      ets = ETS(value)
    )

  # use model to forecast 1 days into future
  fc <- fit |>
    forecast(h = 1)

  futuredata <- tibble(date=fc$date, crimes=round(fc$.mean,0))

  num_crimes_day <- eventsperday$perday[eventsperday$Date == as.Date(i) + days(sumduration)]

  print(paste(futuredata$date, "future data:", futuredata$crimes, "num_crimes_day:", num_crimes_day, sep=" "))

  # numeric zero if no crimes that day
  if(length(futuredata$crimes)>0 & length(num_crimes_day)>0){
    # predicted minus actual, so if + then over-predicting
    ets_error <- ets_error |>
      add_row(date = as.Date(i) + days(sumduration), actual = num_crimes_day, ets_predicted = futuredata$crimes,
              ets_diffcrimes = futuredata$crimes - num_crimes_day)
  } else {
    print("missing data")
  }
  #print(paste(as.Date(i) + days(30), "predicted:", cumsum_m, "actual:", numEventsperday_reg$perday[numEventsperday_reg$Date == as.Date(i) + days(30)], sep=" "))
}

ets_error

###### as example only, below ###########
# for plotting
ets_over_predict <- ets_error |>
  filter(ets_diffcrimes > 0)
ets_under_predict <- ets_error |>
  filter(ets_diffcrimes < 0)
ets_well_predict <- ets_error |>
  filter(ets_diffcrimes == 0)

ets_t <- t.test(x = ets_error$ets_predicted, y = ets_error$actual,
                   alternative = c("two.sided"),
                   mu = 0, paired = TRUE, var.equal = FALSE,
                   conf.level = 0.95)

if(ets_t$estimate > 0){
  ets_error_sign <- paste0(" more crimes predicted per day than actual\nNot significantly different (p=", round(ets_t$p.value,3),"; n=",length(ets_error$ets_predicted),")")
} else if(ets_t$estimate < 0){
  ets_error_sign <- paste0(" fewer crimes predicted than actual\nNot significantly different (p=", round(ets_t$p.value,3),"; n=",length(ets_error$ets_predicted),")")
} else if(ets_t$estimate == 0){
  ets_error_sign <- paste0(", an impossibly perfect estimate\nNot significantly different (p=", round(ets_t$p.value,3),"; n=",length(ets_error$ets_predicted),")")
}

ets_subt <- paste("Mean error is: ",round(ets_t$estimate,3), ets_error_sign, sep="")

#Basic jitter box plot
ets_err_plot <- ggplot(ets_error, aes(x = "", y = ets_diffcrimes)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data=ets_over_predict, col="red") +
  geom_jitter(data=ets_under_predict, col="blue") +
  geom_jitter(data=ets_well_predict, col="black") +
  #coord_flip() +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  )

# add titles
ets_err_plot <- ets_err_plot +
  labs(
    title = "Errors of ETS Model Estimates",
    subtitle = ets_subt,
    y = "Predicted minus Actual",
    x = ""
  )

# make theme prettier
ets_err_plot <- ets_err_plot + theme(
  legend.position="none",  # remove legend because tooltips will suffice
  panel.background = element_rect(fill = "white", colour = "white"),
  panel.grid = element_line(colour = "grey92"),
  panel.grid.minor = element_line(linewidth = rel(1)),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16),
  axis.title.y = element_text(size=17),
  axis.title.x = element_text(size=17),
  plot.title = element_text( # font size "large"
    size = 20,
    hjust = 0, vjust = 1,
    margin = margin(b = 15/2)
  ),
  plot.subtitle = element_text( # font size "regular"
    size = 15,
    hjust = 0, vjust = 1,
    margin = margin(b = 15/2)
  )
)

# show the figure
ets_err_plot

###### as example only, below ###########
# for plotting line plot

ets_err_lineplot <- ggplot(ets_error, aes(x = date, y = ets_diffcrimes)) +
  geom_line() +
  geom_point(data=ets_over_predict, aes(x = date, y = ets_diffcrimes), col="red") +
  geom_point(data=ets_under_predict, aes(x = date, y = ets_diffcrimes), col="blue") +
  geom_point(data=ets_well_predict, aes(x = date, y = ets_diffcrimes), col="black") +
  geom_hline(yintercept = 0) +
  #coord_flip() +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  )

# add titles
ets_err_lineplot <- ets_err_lineplot +
  labs(
    title = "Errors of ETS Model Estimates",
    subtitle = "Error per date",
    y = "Predicted minus Actual",
    x = "Date"
  )

# make theme prettier
ets_err_lineplot <- ets_err_lineplot + theme(
  legend.position="none",  # remove legend because tooltips will suffice
  panel.background = element_rect(fill = "white", colour = "white"),
  panel.grid = element_line(colour = "grey92"),
  panel.grid.minor = element_line(linewidth = rel(1)),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16),
  axis.title.y = element_text(size=17),
  axis.title.x = element_text(size=17),
  plot.title = element_text( # font size "large"
    size = 20,
    hjust = 0, vjust = 1,
    margin = margin(b = 15/2)
  ),
  plot.subtitle = element_text( # font size "regular"
    size = 15,
    hjust = 0, vjust = 1,
    margin = margin(b = 15/2)
  )
)

# show the figure
ets_err_lineplot

####################################################################
# decomp model
##############
eventsperday_d <- eventsperday
#  filter(Date >= now()-(days(30) + (8 - wday(now())))) # last 30 days + some for next filter

# start the time series on Sunday so we don't have to calculate what day the model spits out
# day of the week, starting on Sunday, day 7
startindex <- 1
for(i in 1:8){
  if(wday(eventsperday_d$Date[i], week_start=1) == 7){
    startindex <- i
  }
}

sumduration = 30
d_error <- tibble(date=as.Date(x = integer(0), origin = "2000-01-01"), actual=numeric(), d_predicted=numeric(), d_diffcrimes=numeric())

# to test
#i <- daterange_start[1]
for(i in daterange_start){
  numEventsperday_d_error <- eventsperday_d |>
    filter(Date >= as.Date(i) & Date < as.Date(i) + days(sumduration))
  if(dim(numEventsperday_d_error)[1]>25){




  tsdata = ts(numEventsperday_d_error$perday[startindex:length(numEventsperday_d_error$perday)], freq=7) ## “seasonal” window of 7 days
  #stl_data <- stl(tsdata, s.window=30)
  decomp_data <- decompose(tsdata, "multiplicative")

  # get today's crime by multiplying today's seasonal position by the current trend
  todaycrime_d <- decomp_data$seasonal[wday(now())] *
    mean(decomp_data$trend[!is.na(decomp_data$trend)])

  todaycrime_d <- round(todaycrime_d,0)

  num_crimes_day <- eventsperday$perday[eventsperday$Date == as.Date(i) + days(sumduration)]

  print(paste("todaycrime_d:", todaycrime_d, "num_crimes_day:", num_crimes_day, sep=" "))

  # numeric zero if no crimes that day
  if(length(todaycrime_d)>0 & length(num_crimes_day)>0){
    # predicted minus actual, so if + then over-predicting
    d_error <- d_error |>
      add_row(date = as.Date(i) + days(sumduration), actual = num_crimes_day, d_predicted = todaycrime_d, d_diffcrimes = todaycrime_d - num_crimes_day)
    }} else {
    print("missing data")
  }

}

###### as example only, below ###########
# for plotting
d_over_predict <- d_error |>
  filter(d_diffcrimes > 0)
d_under_predict <- d_error |>
  filter(d_diffcrimes < 0)
d_well_predict <- d_error |>
  filter(d_diffcrimes == 0)

d_t <- t.test(x = d_error$d_predicted, y = d_error$actual,
                alternative = c("two.sided"),
                mu = 0, paired = TRUE, var.equal = FALSE,
                conf.level = 0.95)

if(d_t$estimate > 0){
  d_error_sign <- paste0(" more crimes predicted per day than actual\nNot significantly different (p=", round(d_t$p.value,3),"; n=",length(d_error$d_predicted),")")
} else if(d_t$estimate < 0){
  d_error_sign <- paste0(" fewer crimes predicted than actual\nNot significantly different (p=", round(d_t$p.value,3),"; n=",length(d_error$d_predicted),")")
} else if(ds_t$estimate == 0){
  d_error_sign <- paste0(", an impossibly perfect estimate\nNot significantly different (p=", round(d_t$p.value,3),"; n=",length(d_error$d_predicted),")")
}

d_subt <- paste("Mean error is: ",round(d_t$estimate,3), d_error_sign, sep="")

# Basic jitter box plot
# d_err_plot <- ggplot(d_error, aes(x = "", y = d_diffcrimes)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(data=d_over_predict, col="red") +
#   geom_jitter(data=d_under_predict, col="blue") +
#   geom_jitter(data=d_well_predict, col="black") +
#   #coord_flip() +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# # add titles
# d_err_plot <- d_err_plot +
#   labs(
#     title = "Errors of Decomposition Model Estimates",
#     subtitle = d_error_sign,
#     y = "Predicted minus Actual",
#     x = ""
#   )
#
# # make theme prettier
# d_err_plot <- d_err_plot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# d_err_plot
#
# ###### as example only, below ###########
# # for plotting line plot
#
# d_err_lineplot <- ggplot(d_error, aes(x = date, y = d_diffcrimes)) +
#   geom_line() +
#   geom_point(data=d_over_predict, aes(x = date, y = d_diffcrimes), col="red") +
#   geom_point(data=d_under_predict, aes(x = date, y = d_diffcrimes), col="blue") +
#   geom_point(data=d_well_predict, aes(x = date, y = d_diffcrimes), col="black") +
#   geom_hline(yintercept = 0) +
#   #coord_flip() +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# # add titles
# d_err_lineplot <- d_err_lineplot +
#   labs(
#     title = "Errors of Decomposition Model Estimates",
#     subtitle = "Error per date",
#     y = "Predicted minus Actual",
#     x = "Date"
#   )
#
# # make theme prettier
# d_err_lineplot <- d_err_lineplot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# d_err_lineplot

############################################
# Put all errors together for box plot groups

linear_error_a <- linear_error |>
  select(!lin_predicted)
ets_error_a <- ets_error |>
  select(!ets_predicted)
d_error_a <- d_error |>
  select(!d_predicted)

all_errors <- inner_join(linear_error_a, d_error_a, by = c("date" = "date", "actual" = "actual")) |>
  inner_join(ets_error_a, by = c("date" = "date", "actual" = "actual"))

# pivot longer
all_errors_longer <- all_errors |>
  pivot_longer(
    cols = -c(date, actual),
    names_to = "model"
  ) |>
  mutate(model = factor(model, levels = c("lin_predicted","lin_diffcrimes","d_predicted","d_diffcrimes","ets_predicted","ets_diffcrimes")))
# Show the structure of the new, longer data set

saveRDS(all_errors_longer, file = "all_errors_longer.RDS")

# allthree_plot <- ggplot(all_errors_longer, aes(x=model, y=value)) +
#   geom_boxplot()+
#   theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# allthree_plot <- allthree_plot +
#   scale_x_discrete(drop=FALSE, na.translate = FALSE,
#                    breaks = c(
#                      "lin_diffcrimes",
#                      "d_diffcrimes",
#                      "ets_diffcrimes"
#                    ),
#                    labels = c(
#                      "Linear",
#                      "Decomposition",
#                      "ETS"
#                    )
#   )
#
# # add titles
# allthree_plot <- allthree_plot +
#   labs(
#     title = "Errors of Model Estimates",
#     y = "Predicted minus Actual",
#     x = "Models"
#   )
#
# # make theme prettier
# allthree_plot <- allthree_plot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# allthree_plot

# #### linear line plot ####
#
# linear_error <- all_errors_longer |>
#   filter(model == "lin_diffcrimes")
#
# linear_over_predict <- linear_error |>
#   filter(value > 0)
# linear_under_predict <- linear_error |>
#   filter(value < 0)
# linear_well_predict <- linear_error |>
#   filter(value == 0)
#
# lin_err_lineplot <- ggplot(linear_error, aes(x = date, y = value)) +
#   geom_line() +
#   geom_point(data=linear_over_predict, aes(x = date, y = value), col="red") +
#   geom_point(data=linear_under_predict, aes(x = date, y = value), col="blue") +
#   geom_point(data=linear_well_predict, aes(x = date, y = value), col="black") +
#   geom_hline(yintercept = 0) +
#   #coord_flip() +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#   )
#
# # add titles
# lin_err_lineplot <- lin_err_lineplot +
#   labs(
#     title = "Errors of Linear Model Estimates",
#     subtitle = "Error per date",
#     y = "Predicted minus Actual",
#     x = "Date"
#   )
#
# # make theme prettier
# lin_err_lineplot <- lin_err_lineplot + theme(
#   legend.position="none",  # remove legend because tooltips will suffice
#   panel.background = element_rect(fill = "white", colour = "white"),
#   panel.grid = element_line(colour = "grey92"),
#   panel.grid.minor = element_line(linewidth = rel(1)),
#   axis.text.x = element_text(size=16),
#   axis.text.y = element_text(size=16),
#   axis.title.y = element_text(size=17),
#   axis.title.x = element_text(size=17),
#   plot.title = element_text( # font size "large"
#     size = 20,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   ),
#   plot.subtitle = element_text( # font size "regular"
#     size = 15,
#     hjust = 0, vjust = 1,
#     margin = margin(b = 15/2)
#   )
# )
#
# # show the figure
# lin_err_lineplot

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
