##################
### User input ###
##################
# Policing data file
datafilename <- "durhamdata.csv"   # Note: eventually, we will need to build a way for
#       users to specify which column of their data
#       is which variable (annoying)


# Race                                           # This is sort of how we can do it in the app
racecolumn <- "race"
americanindianalaskanativecode <- "I"
asiancode <- "A"
blackcode <- "B"
hispaniclatinocode <- NA
nativehawaiianpacificislandercode <- NA
whitecode <- "W"
multiracialcode <- NA
notlistedcode <- NA

# Gender
gendercolumn <- "sex"
womancode <- "F"
mancode <- "M"

# Charge
chargecolumns <- paste0("charge",1:25)

# Type of arrest
typeofarrestcolumn <- "typeofarrest"
actualarresttypes <- c("Taken Into Custody", "Summoned/Cited")

# Bond amount column
bondamountcolumn <- "bondamount"

# Patrolling geo unit
patrolcolumn <- "LAWDIST"

# Officer
officercolumn <- "arrestingofficer"

# Date and time
datetimecolumn <- "datetimeofarrest"
timezone <- "US/Eastern"

######################
### Load libraries ###
######################

library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(labelled)
library(openxlsx)
library(lubridate)
library(hms)

#################################
### Tidy up policing data set ###
#################################
# Set columns to keep
keeps <- c(racecolumn, gendercolumn, chargecolumns, typeofarrestcolumn, bondamountcolumn, patrolcolumn, officercolumn, datetimecolumn)

# Load data
policingdata <- datafilename %>%
  read_csv(col_select = all_of(keeps))

# Recode race data
policingdata <- policingdata %>%
  rename(Race = !!racecolumn) %>%
  mutate(Race = dplyr::case_match(Race,
                                  americanindianalaskanativecode ~ "AIAN",
                                  asiancode ~ "Asian",
                                  blackcode ~ "Black",
                                  hispaniclatinocode ~ "Hispanic/Latino",
                                  nativehawaiianpacificislandercode ~ "NHPI",
                                  whitecode ~ "White",
                                  multiracialcode ~ "Multiracial",
                                  notlistedcode ~ "A race not listed above",
                                  .default = "Missing race data")) %>%
  mutate(Race = factor(Race, levels = c("AIAN",
                                        "Asian",
                                        "Black",
                                        "Hispanic/Latino",
                                        "NHPI",
                                        "White",
                                        "Multiracial",
                                        "A race not listed above",
                                        "Missing race data")))

# Recode gender data
policingdata <- policingdata %>%
  rename(Gender = !!gendercolumn) %>%
  mutate(Gender = case_match(Gender,
                             mancode ~ "Man",
                             womancode ~ "Woman",
                             .default = "Missing gender data")) %>%
  mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data")))

# # Make list of all possible charges
# chargelist <- policingdata %>%
#   select(starts_with("charge")) %>%
#   unlist %>%
#   unname %>%
#   unique %>%
#   sort %>%
#   data.frame(charge = .)

# Type of arrest
policingdata <- policingdata %>%
  rename(typeofarrest = !!typeofarrestcolumn) %>%
  mutate(typeofarrest = str_to_title(typeofarrest)) %>%
  mutate(typeofarrest = replace(typeofarrest, is.na(typeofarrest), "Missing arrest type data")) %>%
  mutate(typeofarrest = factor(typeofarrest, levels = c(unique(typeofarrest), "Missing arrest type data")))

# Bond amount
policingdata <- policingdata %>%
  rename(bondamount = !!bondamountcolumn) %>%
  mutate(bondamount = str_replace_all(bondamount, "(\\$|\\,)", "")) %>%
  mutate(bondamount = as.numeric(bondamount))

# Organize patrol geo units
policingdata <- policingdata %>%
  rename(Patrol = !!patrolcolumn) %>%
  mutate(Patrol = factor(Patrol))

# Organize officers
policingdata <- policingdata %>%
  rename(Officer = !!officercolumn) %>%
  mutate(Officer = factor(Officer))

# Put in nice order
policingdata <- policingdata %>%
  relocate(Race, Gender, typeofarrest, bondamount, Patrol, Officer, datetimeofarrest, paste0("charge",1:25)) %>%
  as.data.frame %>%
  remove_attributes("spec")

write.csv(policingdata, "DurhamCleaned.csv")
