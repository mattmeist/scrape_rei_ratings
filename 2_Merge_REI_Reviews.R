rm(list = ls())

# Check if groundhog is already installed, and if not, install it
if (!requireNamespace("groundhog", quietly = TRUE)) {
  install.packages("groundhog", dependencies = TRUE)
}

#Load it
library(groundhog)

# Load packages with groundhog
ghog.date <- "2023-12-01"
pkgs <- c("data.table", "dplyr", "ggplot2", "readr", "stringr")
groundhog.library(pkgs, ghog.date)
rm(ghog.date, pkgs)

multmerge = function(mypath){
  filenames = list.files(path = mypath, full.names = TRUE)
  rbindlist(lapply(filenames,function(i) tryCatch(fread(i,colClasses = 'character'),
                                                  error = function(e) {
                                                    cat("\nError reading in file:",i,"\t") #Identifies problem files by name
                                                    message(e) #Prints error message without stopping loop
                                                    list(ERROR=i) #Adds a placeholder column so rbindlist will execute
                                                  })), #End of tryCatch and lapply
            fill = TRUE) #rbindlist arguments
} #End of function

# Product Overalls
path <- "OVRS"
ovrs <- multmerge(path)
ovrs <- ovrs[!duplicated(ovrs)]
fwrite(ovrs, 'REI_ovr.csv')

# Product Prices
path <- "PRICES"
prices <- multmerge(path)
prices <- prices[!duplicated(prices)]
fwrite(prices, 'REI_prices.csv')

# Product Reviews
path <- "REVIEWS"
reviews <- multmerge(path)
reviews <- reviews[!duplicated(reviews$Id)]
fwrite(reviews, 'REI_Reviews_All.csv')

########################################################
##### Remove duplicated reviews and fix locations ######
########################################################

### Fixing Locations in REI data ###

d <- fread('REI_Reviews_All.csv')
d$reviewId <- d$Id
d$pid <- d$ProductId
d <- d[!is.na(reviewId)]

nrow(d)

#########################################
#### Now! Let's find some locations #####
#########################################

### The problem:
# We want to have the location of each review. 
## However, those locations are not in standard format. Below are
## the options

# 1. City, State (2I), Country (3I)
# 2. City, State (2I), Country (word)
# 3. City, State (2I)
# 4. City, State (word), Country (3I)
# 5. City, State (word), Country (word)
# 6. City, State (word)
# 7. City, Country (3I)
# 8. City, Country (word)
# 9. City
# 10. State (2I)
# 11. State (word)
# 12. Nickname (e.g. 'Philly')
# 13. Nickname including city (e.g. 'Windy chicago')
# 14. Nickname including state (e.g. 'Rainy Oregon')
# 15. Abbreviations (e.g. 'PDX')
# 16. Areas (e.g. 'Socal' and 'Rockies')

### STRATEGY ###
# I hope we can get enough usable data from #1-#11, 13-15. #12 and #16 seem fuzzy

## Data Needed ##
# 1. State codes
# 2. City, state

## Assumption ##
# When I have a state but no city, I will put the reviewer in the population center of that state
# But I will identify this assumption.

d$city <- rep(NA, times = nrow(d))
d$state <- rep(NA, times = nrow(d))
d$city_from_state <- rep(FALSE, times = nrow(d))

## US City Data 
#DATA IS FROM https://simplemaps.com/data/us-cities
cities <- fread('uscities.csv')

## And state codes
states <- fread('states.csv')

cities <- left_join(cities, states, by = c('state_name' = 'State', 'state_id' = 'Code')) |> 
  as.data.table()

cities$citystate <- paste(cities$city, cities$state_name)
cities$citystate_abb <- paste(cities$city, cities$state_id)

# Remove stuff we don't need
cities <- cities |>
  select(-c(id, military, source, ranking, zips, county_fips, county_name, city_ascii)) |> 
  as.data.table()

# Write all these to uppercase
d$UserLocation <- toupper(d$UserLocation)
cities$City <- toupper(cities$city)
cities$State <- toupper(cities$state_name)
cities$citystate <- toupper(cities$citystate)
cities$citystate_abb <- toupper(cities$citystate_abb)

## Save as csv to my own folder in case this person takes the data off
#fwrite(cities,'cities.csv')


### Doin' stuff ###
# If there are two commas
## If the thing between commas is 2 letters
### Call the city what's before, the state what's after
## If it's more
### Call the city what's before, the state what's after
# If there is one comma
## If the thing after the comma is 2 letters
## If it's more
# If there are no commas
## Is the word in the list of states?
## Is the word in the list of cities?
## Is the last word in the list of states?
### Is whatever is before that in the list of cities?
## Is any part of the text in the list of states?
## Is any part in the list of cities?

# Split out data that has locations from data without
d |> 
  group_by(UserLocation) |>
  summarise(num = n()) |>
  arrange(desc(num))

no_locations <- d[d$UserLocation == "" | d$UserLocation == "UNDISCLOSED"]
d <- d[d$UserLocation != "" & d$UserLocation != "UNDISCLOSED"]

d |> 
  group_by(UserLocation) |>
  summarise(num = n()) |>
  arrange(desc(num))

## REMOVE WORDS THAT MIGHT MESS THIS UP ##
removeWords <- function(str, stopwords) {
  stop_words_pattern <- paste0(stopwords, collapse = "|")
  return(str_replace_all(str, stop_words_pattern, ""))
}

cutwords <- c('METRO', 'COUNTY', 'SOUTHERN', 'CENTRAL', 'NORTHERN', 'EASTERN',
              ' WESTERN', 'RAINY', 'SNOWY')

d$UserLocation <- trimws(removeWords(d$UserLocation, cutwords))

# Remove the different ways to say america
americapattern <- ",\\s*US$|,\\s*USA$|,\\s*U\\.S\\.A\\.$|,\\s*UNITED STATES$|,\\s*AMERICA$|,\\s*UNITED STATES OF AMERICA$|\\sUS$|\\sUSA$"

d$UserLocation <- trimws(gsub(americapattern, "", d$UserLocation, ignore.case = TRUE))

# Remove "STATE"
d$UserLocation <- trimws(removeWords(d$UserLocation, "STATE"))

# See if place is just a state name or just a city name
d$state_only <- ifelse(d$UserLocation %in% cities$State | 
                         d$UserLocation %in% cities$state_id, 1, 0)

d$state <- ifelse(d$UserLocation %in% cities$State, d$UserLocation,
                  ifelse(d$UserLocation %in% cities$state_id,
                         cities$State[match(d$UserLocation, cities$state_id)],
                         NA_character_))

d$state_id <- ifelse(d$UserLocation %in% cities$state_id, d$UserLocation,
                  ifelse(d$UserLocation %in% cities$State,
                         cities$state_id[match(d$UserLocation, cities$State)],
                         NA_character_))

d$city_only <- ifelse(d$state_only == 0 &
                        d$UserLocation %in% cities$City, 1, 0)

d$city <- ifelse(d$city_only == 0, NA_character_, d$UserLocation)

burned <- d[state_only == 1 | city_only == 1]$Id

# Start Finding more complex ones
d$commas <- str_count(d$UserLocation, ",")

# Places with one comma
d$pre_comma_1 <- trimws(str_match(d$UserLocation, "([A-Za-z\\s]+),\\s*([A-Za-z]+(?:\\s+[A-Za-z]+)?)$")[, 2])
d$pre_comma_2 <- trimws(str_match(d$UserLocation, "([A-Za-z\\s]+),\\s*([A-Za-z]+(?:\\s+[A-Za-z]+)?)$")[, 3])
d$citystate_fromcomma <- paste(d$pre_comma_1, d$pre_comma_2)

# Match to citystate
d$state <- ifelse(d$Id %in% burned, d$state,
       ifelse(d$citystate_fromcomma %in% cities$citystate,
       cities$State[match(d$citystate_fromcomma, cities$citystate)],
       ifelse(d$citystate_fromcomma %in% cities$citystate_abb,
              cities$State[match(d$citystate_fromcomma, cities$citystate_abb)],
       NA_character_)))

d$state_id <- ifelse(d$Id %in% burned, d$state_id,
                  ifelse(d$citystate_fromcomma %in% cities$citystate,
                         cities$state_id[match(d$citystate_fromcomma, cities$citystate)],
                         ifelse(d$citystate_fromcomma %in% cities$citystate_abb,
                                cities$state_id[match(d$citystate_fromcomma, cities$citystate_abb)],
                                NA_character_)))

d$city <- ifelse(d$Id %in% burned, d$city,
                 ifelse(d$citystate_fromcomma %in% cities$citystate,
                        cities$City[match(d$citystate_fromcomma, cities$citystate)],
                        ifelse(d$citystate_fromcomma %in% cities$citystate_abb,
                               cities$City[match(d$citystate_fromcomma, cities$citystate_abb)],
                               NA_character_)))

burned <- c(d[(!is.na(state) & !is.na(city)) | state_only == 1 | city_only == 1]$Id)

#Let's check in
d[!(Id %in% burned)] |>
  group_by(UserLocation) |>
  summarise(num = n()) |>
  arrange(desc(num))

# Places with spaces
d$pre_space <- str_trim(gsub("[[:punct:]]", "", str_match(d$UserLocation, "^(.+)\\s+([A-Za-z]+(?:\\s+[A-Za-z]+)?)$")[, 2]))
d$post_space <- str_trim(gsub("[[:punct:]]", "", str_match(d$UserLocation, "^(.+)\\s+([A-Za-z]+(?:\\s+[A-Za-z]+)?)$")[, 3]))
d$citystate_fromspace <- paste(d$pre_space, d$post_space)

# Match to citystate
d$state <- ifelse(d$Id %in% burned, d$state,
                  ifelse(d$citystate_fromspace %in% cities$citystate,
                         cities$State[match(d$citystate_fromspace, cities$citystate)],
                         ifelse(d$citystate_fromspace %in% cities$citystate_abb,
                                cities$State[match(d$citystate_fromspace, cities$citystate_abb)],
                                NA_character_)))


d$state_id <- ifelse(d$Id %in% burned, d$state_id,
                     ifelse(d$citystate_fromspace %in% cities$citystate,
                            cities$state_id[match(d$citystate_fromspace, cities$citystate)],
                            ifelse(d$citystate_fromspace %in% cities$citystate_abb,
                                   cities$state_id[match(d$citystate_fromspace, cities$citystate_abb)],
                                   NA_character_)))

d$city <- ifelse(d$Id %in% burned, d$city,
                 ifelse(d$citystate_fromspace %in% cities$citystate,
                        cities$City[match(d$citystate_fromspace, cities$citystate)],
                        ifelse(d$citystate_fromspace %in% cities$citystate_abb,
                               cities$City[match(d$citystate_fromspace, cities$citystate_abb)],
                               NA_character_)))

burned <- c(d[(!is.na(state) & !is.na(city)) | state_only == 1 | city_only == 1]$Id)

#Let's check in
d[!(Id %in% burned)] |>
  group_by(UserLocation) |>
  summarise(num = n()) |>
  arrange(desc(num)) |>
  head(40)

#### Some manual ones
#NYC
d$city <- ifelse(d$UserLocation == "NYC" | d$UserLocation == "NY, NY", "NEW YORK", d$city)
d$state <- ifelse(d$UserLocation == "NYC" | d$UserLocation == "NY, NY", "NEW YORK", d$state)
d$state_id <- ifelse(d$UserLocation == "NYC" | d$UserLocation == "NY, NY", "NY", d$state_id)

#NEW YORK CITY
d$city <- ifelse(d$UserLocation == "NEW YORK CITY", "NEW YORK", d$city)
d$state <- ifelse(d$UserLocation == "NEW YORK CITY", "NEW YORK", d$state)
d$state_id <- ifelse(d$UserLocation == "NEW YORK CITY", "NY", d$state_id)

#LONG ISLAND, NY
d$city <- ifelse(d$UserLocation == "LONG ISLAND, NY", "BRENTWOOD", d$city)
d$state <- ifelse(d$UserLocation == "LONG ISLAND, NY", "BRENTWOOD", d$state)
d$state_id <- ifelse(d$UserLocation == "LONG ISLAND, NY", "NY", d$state_id)

#SLC, UT
d$city <- ifelse(d$UserLocation == "SLC, UT"|
                   d$UserLocation == "SLC", "SALT LAKE CITY", d$city)
d$state <- ifelse(d$UserLocation == "SLC, UT"|
                    d$UserLocation == "SLC", "UTAH", d$state)
d$state_id <- ifelse(d$UserLocation == "SLC, UT" |
                       d$UserLocation == "SLC", "UT", d$state_id)

#ST. LOUIS, MO
d$city <- ifelse(d$UserLocation == "ST. LOUIS, MO" |
                   d$UserLocation == "ST. LOUIS"|
                   d$UserLocation == "ST LOUIS, MO" |
                   d$UserLocation == "ST LOUIS"|
                   d$UserLocation == "SAINT LOUIS"|
                   d$UserLocation == "SAINT LOUIS, MO", "ST. LOUIS", d$city)
d$state <- ifelse(d$UserLocation == "ST. LOUIS, MO" |
                    d$UserLocation == "ST. LOUIS"|
                    d$UserLocation == "ST LOUIS, MO" |
                    d$UserLocation == "ST LOUIS"|
                    d$UserLocation == "SAINT LOUIS"|
                    d$UserLocation == "SAINT LOUIS, MO", "MISSOURI", d$state)
d$state_id <- ifelse(d$UserLocation == "ST. LOUIS, MO" |
                       d$UserLocation == "ST. LOUIS"|
                       d$UserLocation == "ST LOUIS, MO" |
                       d$UserLocation == "ST LOUIS"|
                       d$UserLocation == "SAINT LOUIS"|
                       d$UserLocation == "SAINT LOUIS, MO", "MO", d$state_id)

#ST. PAUL, MN
d$city <- ifelse(d$UserLocation == "ST. PAUL, MN" |
                   d$UserLocation == "ST. PAUL"|
                   d$UserLocation == "ST PAUL, MN" |
                   d$UserLocation == "ST PAUL"|
                   d$UserLocation == "SAINT PAUL"|
                   d$UserLocation == "SAINT PAUL, MN"|
                   d$UserLocation == "TWIN CITIES, MN", "ST. PAUL", d$city)
d$state <- ifelse(d$UserLocation == "ST. PAUL, MN" |
                    d$UserLocation == "ST. PAUL"|
                    d$UserLocation == "ST PAUL, MN" |
                    d$UserLocation == "ST PAUL"|
                    d$UserLocation == "SAINT PAUL"|
                    d$UserLocation == "SAINT PAUL, MN"|
                    d$UserLocation == "TWIN CITIES, MN", "MINNESOTA", d$state)
d$state_id <- ifelse(d$UserLocation == "ST. PAUL, MN" |
                       d$UserLocation == "ST. PAUL"|
                       d$UserLocation == "ST PAUL, MN" |
                       d$UserLocation == "ST PAUL"|
                       d$UserLocation == "SAINT PAUL"|
                       d$UserLocation == "SAINT PAUL, MN"|
                       d$UserLocation == "TWIN CITIES, MN", "MN", d$state_id)

#VENTURA, CA
d$city <- ifelse(d$UserLocation == "VENTURA, CA", "OXNARD", d$city)
d$state <- ifelse(d$UserLocation == "VENTURA, CA", "CALIFORNIA", d$state)
d$state_id <- ifelse(d$UserLocation == "VENTURA, CA", "CA", d$state_id)

#PDX
d$city <- ifelse(d$UserLocation == "PDX", "PORTLAND", d$city)
d$state <- ifelse(d$UserLocation == "PDX", "OREGON", d$state)
d$state_id <- ifelse(d$UserLocation == "PDX", "OR", d$state_id)

#WASHINGTON, D.C.
d$city <- ifelse(d$UserLocation == "WASHINGTON, D.C.", "WASHINGTON", d$city)
d$state <- ifelse(d$UserLocation == "WASHINGTON, D.C.", "DISTRICT OF COLUMBIA", d$state)
d$state_id <- ifelse(d$UserLocation == "WASHINGTON, D.C.", "DC", d$state_id)

#LA
d$city <- ifelse(d$UserLocation == "LA" |
                   d$UserLocation == "LA, CA", "LOS ANGELES", d$city)
d$state <- ifelse(d$UserLocation == "LA"|
                    d$UserLocation == "LA, CA", "CALIFORNIA", d$state)
d$state_id <- ifelse(d$UserLocation == "LA"|
                       d$UserLocation == "LA, CA", "CA", d$state_id)

#BAY AREA, CA
#SF BAY AREA
# I am going to give these people to SF. This should be conservative, as sf has less variance in weather than surrounding areas
d$city <- ifelse(d$UserLocation == "BAY AREA, CA" | 
                   d$UserLocation == "SF BAY AREA" |
                   d$UserLocation == "SAN FRANCISCO BAY AREA" |
                   d$UserLocation == "BAY AREA"|
                   d$UserLocation == "SF, CA"|
                   d$UserLocation == "SF", "SAN FRANCISCO", d$city)
d$state <- ifelse(d$UserLocation == "BAY AREA, CA" | 
                    d$UserLocation == "SF BAY AREA" |
                    d$UserLocation == "SAN FRANCISCO BAY AREA" |
                    d$UserLocation == "BAY AREA"|
                    d$UserLocation == "SF, CA"|
                    d$UserLocation == "SF", "CALIFORNIA", d$state)
d$state_id <- ifelse(d$UserLocation == "BAY AREA, CA" | 
                       d$UserLocation == "SF BAY AREA" |
                       d$UserLocation == "SAN FRANCISCO BAY AREA" |
                       d$UserLocation == "BAY AREA"|
                       d$UserLocation == "SF, CA"|
                       d$UserLocation == "SF", "CA", d$state_id)

#SOCAL
# I am going to give these people to LA.
d$city <- ifelse(d$UserLocation == "SOCAL" |
                   d$UserLocation == "SO CAL", "LOS ANGELES", d$city)
d$state <- ifelse(d$UserLocation == "SOCAL"|
                    d$UserLocation == "SO CAL", "CALIFORNIA", d$state)
d$state_id <- ifelse(d$UserLocation == "SOCAL"|
                       d$UserLocation == "SO CAL", "CA", d$state_id)

#PNW
# I am going to give these people to SEATTLE
d$city <- ifelse(d$UserLocation == "PNW" | d$UserLocation == "PACIFIC NORTHWEST", "SEATTLE", d$city)
d$state <- ifelse(d$UserLocation == "PNW" | d$UserLocation == "PACIFIC NORTHWEST", "WASHINGTON", d$state)
d$state_id <- ifelse(d$UserLocation == "PNW" | d$UserLocation == "PACIFIC NORTHWEST", "WA", d$state_id)

burned <- c(d[(!is.na(state) & !is.na(city)) | state_only == 1 | city_only == 1]$Id)

#FILL IN STATE IF USER LOCATION = CITY ONLY
# Assuming 'cities' is a data frame with columns 'City', 'State', 'State_id', and 'population'
# Assuming 'd' is your data frame with columns 'city_only', 'city', 'State', and 'State_id'

# Arrange cities by population in descending order
cities <- cities[order(-cities$population), ]
cities <- cities[!duplicated(cities$citystate)]

# Identify rows where city_only is 1
city_only_rows <- d$city_only == 1

# Find indices for matching cities
matching_indices <- match(d$city[city_only_rows], cities$City)

# Update State and State_id columns using vectorized indexing
d$state[city_only_rows] <- cities$State[matching_indices]
d$state_id[city_only_rows] <- cities$state_id[matching_indices]

d$citystate <- paste(d$city, d$state)

d <- left_join(d, cities, 
               by = c('citystate' = 'citystate')) |>
  as.data.table()

# How many people have locations?
nrow(d[is.na(lat)==F])

# Here is the US census doc for each state's population center
#statemids <- read_delim(getURL("https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt"), delim = ',')
statemids <- read_delim('Data - Raw/statemids.txt', delim = ',') |> as.data.frame()

# I will create two new columns, which set lat and long to the middle of the state
# IF city was imputed from state
# Initialize lat_mid and lng_mid with NAs
d$lat_mid <- ifelse(!is.na(d$lat), d$lat,
                    ifelse(!is.na(d$state),
                           statemids$LATITUDE[match(toupper(d$state), toupper(statemids$STNAME))],
                           NA))

d$lng_mid <- ifelse(!is.na(d$lng), d$lng,
                    ifelse(!is.na(d$state),
                           statemids$LONGITUDE[match(toupper(d$state), toupper(statemids$STNAME))],
                           NA))

# Clean up columns
d <- d[,c(1:57,65, 69:75,79,80)]
setnames(d, old = "city.x", new = "city")
setnames(d, old = "state_id.x", new = "state_id")

# Join back in places with missing locations
d <- rbindlist(list(d, no_locations), use.names = TRUE, fill = TRUE)

nrow(d[is.na(lat_mid)==F])/ nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])

fwrite(d, 'REI_reviews_located.csv')
