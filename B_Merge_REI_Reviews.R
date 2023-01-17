library(data.table)
library(dplyr)
library(readr)

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
reviews <- reviews[!duplicated(reviews)]
fwrite(reviews, 'REI_reviews.csv')


########################################################
##### Remove duplicated reviews and fix locations ######
########################################################

library(tidyverse)
library(data.table)
library(dtplyr)
### Fixing Locations in REI data ###

d <- fread('REI_reviews.csv')
d$reviewId <- d$Id
d$pid <- d$ProductId
d <- subset(d, is.na(reviewId) == F) %>% as.data.table()

nrow(d)
length(unique(d$pid))

### First, remove duplicates ###
# What number should we get to?
length(unique(d[, reviewId]))

## Spoiler: The issue is products listed in two cats. We're going to remove the second review for such cases
d$keep <- rep(NA, times = nrow(d))
d <- as.data.frame(d)
for(i in 1:nrow(d)){
  RID <- d$reviewId[i]
  d$keep[i] <- ifelse(nrow(d[d$reviewId == RID,]) == 1, 1,
                      ifelse(sum(d[d$reviewId == RID, ]$keep, na.rm=T)==1,0,1))
}

d <- d %>%
  subset(keep == 1)

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
library(RCurl)
#DATA IS FROM https://simplemaps.com/data/us-cities
cities <- fread('~/Dropbox/Research/REI/Data - Raw/uscities.csv')

## And state codes
states <- fread('~/Dropbox/Research/REI/Data - Raw/states.csv')

cities <- left_join(cities, states, by = c('state_name' = 'State', 'state_id' = 'Code')) %>% 
  as.data.table()

cities$citystate <- paste(cities$city, cities$state_name)

# Remove stuff we don't need
cities <- cities %>%
  select(-c(id, military, source, ranking, zips, county_fips, county_name, city_ascii)) %>% 
  as.data.table()

# Write all these to uppercase
d$UserLocation <- toupper(d$UserLocation)
cities$City <- toupper(cities$city)
cities$State <- toupper(cities$state_name)
cities$citystate <- toupper(cities$citystate)

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
library(rematch2)

## REMOVE WORDS THAT MIGHT MESS THIS UP ##
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

cutwords <- c('METRO', 'COUNTY', 'SOUTHERN', 'CENTRAL', 'NORTHERN', 'EASTERN',
              ' WESTERN', 'RAINY', 'SNOWY')

for(i in 1:nrow(d)){
  d$UserLocation[i] <- removeWords(d$UserLocation[i], cutwords)
}

# Start Finding!
d <- as.data.table(d)
d$commas <- str_count(d$UserLocation, ",")

d$state_only <- ifelse(d$UserLocation %in% cities$State,1,
                       ifelse(d$UserLocation %in% cities$state_id,1, 0))
d$city_only <- ifelse(d$UserLocation %in% cities$City, 1, 0)

# What is the city?
d$city <- ifelse(d$state_only == 1, NA_character_, #If no city, it's NA
                 ifelse(d$city_only == 1, d$UserLocation, #If only city, it's city
                        ifelse(d$commas >= 1, str_match(d$UserLocation, '(\\w*\\s?\\w*\\s?\\w*),')[,2],
                               #If one or more commas, it's what's before the comma
                               ifelse(str_match(d$UserLocation, '(\\w*\\s?\\w*\\s?\\w*)')[,2] %in% cities$City, #If no commas, three word
                                      str_match(d$UserLocation, '(\\w*\\s?\\w*\\s?\\w*)')[,2], 
                                      ifelse(str_match(d$UserLocation, '(\\w*\\s?\\w*)\\s?\\w*')[,2] %in% cities$City, #If two word
                                             str_match(d$UserLocation, '(\\w*\\s?\\w*)\\s?\\w*')[,2], 
                                             ifelse(str_match(d$UserLocation, '(\\w*)\\s?\\w*\\s?\\w*')[,2] %in% cities$City, #If one word
                                                    str_match(d$UserLocation, '(\\w*)\\s?\\w*\\s?\\w*')[,2], NA_character_))))))
# Fix Cities with State names
d$city <- str_remove(d$city,'^\\s')
d$city <- str_remove(d$city,'\\s+$')
d[city == 'WASHINGTON', UserLocation] #eg
d$city <- ifelse(is.na(d$city)==F & str_detect(d$UserLocation, 'STATE$'), NA_character_, d$city )

# State?
d$state <- ifelse(d$state_only == 1, 
                  ifelse(str_match(d$UserLocation, '\\W([A-z]{2})$')[,2] %in% cities$state_id, NA_character_, d$UserLocation),#If only state, it's UserLocation
                  ifelse(d$city_only == 1, NA_character_,#If no state, it's NA
                         ifelse(d$commas >= 1, 
                                ifelse(str_match(d$UserLocation, ',\\s+([A-z]{2}$)')[,2] %in% cities$state_id, NA_character_,
                                       ifelse(str_match(d$UserLocation, ',\\s+(\\w*)\\s?')[,2] %in% cities$State, 
                                              str_match(d$UserLocation, ',\\s+(\\w*)\\s?')[,2], NA_character_)),
                                ifelse(str_match(d$UserLocation, '\\W([A-z]{2})$')[,2] %in% cities$state_id, NA_character_,
                                       ifelse(str_match(d$UserLocation, '(\\w+)$')[,2] %in% cities$State, str_match(d$UserLocation, '(\\w+)$')[,2],
                                              ifelse(str_match(d$UserLocation, '(\\w+)\\s\\w+$')[,2] %in% cities$State, str_match(d$UserLocation, '(\\w+)\\s\\w+$')[,2],
                                                     ifelse(str_match(d$UserLocation, '(\\w+)\\s\\w+\\s\\w+$')[,2] %in% cities$State, str_match(d$UserLocation, '(\\w+)\\s\\w+\\s\\w+$')[,2],
                                                            NA_character_)))))))

d$state_id <- ifelse(d$state_only == 1, 
                     ifelse(str_match(d$UserLocation, '\\W([A-z]{2})$')[,2] %in% cities$state_id, str_match(d$UserLocation, '\\W([A-z]{2})$')[,2], NA_character_),
                     ifelse(d$commas >= 1, 
                            ifelse(str_match(d$UserLocation, ',\\s+([A-z]{2})$')[,2] %in% cities$state_id, str_match(d$UserLocation, ',\\s+([A-z]{2})$')[,2],
                                   ifelse(str_match(d$UserLocation, ',\\s+([A-z]{2}),')[,2] %in% cities$state_id, str_match(d$UserLocation, ',\\s+([A-z]{2}),')[,2],
                                          NA_character_)),
                            ifelse(str_match(d$UserLocation, '\\W([A-z]{2})$')[,2] %in% cities$state_id, str_match(d$UserLocation, '\\W([A-z]{2})$')[,2],
                                   NA_character_)))

# Fill in where we have ID or name only
forstate <- NA
forid <- NA

d <- as.data.frame(d)
cities <- as.data.frame(cities)
states <- states %>% 
  mutate(State = toupper(State)) %>% 
  as.data.frame()

for(i in 1:nrow(d)){
  forstate <- d$state[i]
  forid <- d$state_id[i]
  d$state[i] <- ifelse(is.na(forstate),
                       ifelse(is.na(forid), NA_character_, states[states$Code == forid, 1]),
                       d$state[i])
  d$state_id[i] <- ifelse(is.na(forid),
                          ifelse(is.na(forstate), NA_character_, states[states$State == forstate, 3]),
                          d$state_id[i])
}

#FILL IN STATE IF USER LOCATION = CITY ONLY
cities <- cities %>% 
  arrange(desc(population))
d$State <- rep(NA, times = nrow(d))
d$State_id <- rep(NA, times = nrow(d))

for(i in 1:nrow(d)){
  d$State[i] <- ifelse(d$city_only[i] == 1, 
                       cities[cities$City == d$city[i], 13][1], 
                       d$state[i])
  d$State_id[i] <- ifelse(d$city_only[i] == 1, 
                          cities[cities$City == d$city[i], 2][1], 
                          d$state_id[i])
  thestate = d$State[i]
}

d$citystate <- paste(d$city, d$State)

d <- left_join(d, cities, 
               by = c('citystate' = 'citystate')) %>%
  as.data.table()

# How many people have locations?
nrow(d[is.na(lat)==F])
nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])

nrow(d[is.na(lat)==F]) / nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])
# How many people have cities in their location?
nrow(d[is.na(city.x)==F])
nrow(d[is.na(city.x)==F])  / nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])

### Fill in common missing ones
# This is sort of manual... This ain't my #1 job
d[is.na(city.x)==F & is.na(lat)] %>%
  group_by(UserLocation) %>%
  summarise(num = n()) %>% 
  arrange(desc(num)) %>% 
  as.data.table() %>% 
  head(20)

# Lotta Canadians...
d$city.x <- ifelse(str_detect(d$UserLocation, '\\sLOUIS') & is.na(d$lat),
                   'ST. LOUIS', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '\\sLOUIS') & is.na(d$lat),
                  'MISSOURI', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '\\sLOUIS') & is.na(d$lat),
                cities[cities$City=='ST. LOUIS',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '\\sLOUIS') & is.na(d$lng),
                cities[cities$City=='ST. LOUIS',][1,5], d$lng)

# Orange County <- I'm putting these folks in anaheim
d$city.x <- ifelse(str_detect(d$UserLocation, '^ORANGE COUNTY') & is.na(d$lat),
                   'ANAHEIM', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^ORANGE COUNTY') & is.na(d$lat),
                  'CALIFORNIA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^ORANGE COUNTY') & is.na(d$lat),
                cities[cities$City=='ANAHEIM',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^ORANGE COUNTY') & is.na(d$lng),
                cities[cities$City=='ANAHEIM',][1,5], d$lng)

# Bay Area <- I'm putting these folks in San Jose
d$city.x <- ifelse(str_detect(d$UserLocation, '^BAY AREA')  & is.na(d$lat),
                   'SAN JOSE', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^BAY AREA') & is.na(d$lat),
                  'CALIFORNIA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^BAY AREA') & is.na(d$lat),
                cities[cities$City=='SAN JOSE',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^BAY AREA') & is.na(d$lng),
                cities[cities$City=='SAN JOSE',][1,5], d$lng)

# NYC 
d$city.x <- ifelse(str_detect(d$UserLocation, '^NYC$')  & is.na(d$lat),
                   'NEW YORK', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^NYC$') & is.na(d$lat),
                  'NEW YORK', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^NYC$') & is.na(d$lat),
                cities[cities$City=='NEW YORK',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^NYC$') & is.na(d$lng),
                cities[cities$City=='NEW YORK',][1,5], d$lng)


# NEW YORK
d$city.x <- ifelse(str_detect(d$UserLocation, '^NEW YORK')  & is.na(d$lat),
                   'NEW YORK', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^NEW YORK') & is.na(d$lat),
                  'NEW YORK', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^NEW YORK') & is.na(d$lat),
                cities[cities$City=='NEW YORK',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^NEW YORK') & is.na(d$lng),
                cities[cities$City=='NEW YORK',][1,5], d$lng)

# SLC 
d$city.x <- ifelse(str_detect(d$UserLocation, '^SLC')  & is.na(d$lat),
                   'SALT LAKE CITY', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^SLC') & is.na(d$lat),
                  'UTAH', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^SLC') & is.na(d$lat),
                cities[cities$City=='SALT LAKE CITY',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^SLC') & is.na(d$lng),
                cities[cities$City=='SALT LAKE CITY',][1,5], d$lng)

# Saint Paul
d$city.x <- ifelse(str_detect(d$UserLocation, '^ST. PAUL')  & is.na(d$lat),
                   'ST. PAUL', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^ST. PAUL') & is.na(d$lat),
                  'MINNESOTA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^ST. PAUL') & is.na(d$lat),
                cities[cities$City=='ST. PAUL',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^ST. PAUL') & is.na(d$lng),
                cities[cities$City=='ST. PAUL',][1,5], d$lng)

d$city.x <- ifelse(str_detect(d$UserLocation, '^ST PAUL')  & is.na(d$lat),
                   'ST. PAUL', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^ST PAUL') & is.na(d$lat),
                  'MINNESOTA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^ST PAUL') & is.na(d$lat),
                cities[cities$City=='ST. PAUL',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^ST PAUL') & is.na(d$lng),
                cities[cities$City=='ST. PAUL',][1,5], d$lng)

d$city.x <- ifelse(str_detect(d$UserLocation, '^SAINT PAUL')  & is.na(d$lat),
                   'ST. PAUL', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^SAINT PAUL') & is.na(d$lat),
                  'MINNESOTA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^SAINT PAUL') & is.na(d$lat),
                cities[cities$City=='ST. PAUL',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^SAINT PAUL') & is.na(d$lng),
                cities[cities$City=='ST. PAUL',][1,5], d$lng)

d$city.x <- ifelse(str_detect(d$UserLocation, '^TWIN CITIES')  & is.na(d$lat),
                   'ST. PAUL', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^TWIN CITIES') & is.na(d$lat),
                  'MINNESOTA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^TWIN CITIES') & is.na(d$lat),
                cities[cities$City=='ST. PAUL',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^TWIN CITIES') & is.na(d$lng),
                cities[cities$City=='ST. PAUL',][1,5], d$lng)

# MINNEAPOLIS
d$city.x <- ifelse(str_detect(d$UserLocation, '^MINNEAPOLIS')  & is.na(d$lat),
                   'MINNEAPOLIS', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^MINNEAPOLIS') & is.na(d$lat),
                  'MINNESOTA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^MINNEAPOLIS') & is.na(d$lat),
                cities[cities$City=='MINNEAPOLIS',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^MINNEAPOLIS') & is.na(d$lng),
                cities[cities$City=='MINNEAPOLIS',][1,5], d$lng)

# SEATTLE
d$city.x <- ifelse(str_detect(d$UserLocation, '^SEATTLE')  & is.na(d$lat),
                   'SEATTLE', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^SEATTLE') & is.na(d$lat),
                  'WASHINGTON', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^SEATTLE') & is.na(d$lat),
                cities[cities$City=='SEATTLE',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^SEATTLE') & is.na(d$lng),
                cities[cities$City=='SEATTLE',][1,5], d$lng)

# Long Island
d$city.x <- ifelse(str_detect(d$UserLocation, '^LONG ISLAND')  & is.na(d$lat),
                   'WESTBURY', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^LONG ISLAND') & is.na(d$lat),
                  'NEW YORK', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^LONG ISLAND') & is.na(d$lat),
                cities[cities$City=='WESTBURY',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^LONG ISLAND') & is.na(d$lng),
                cities[cities$City=='WESTBURY',][1,5], d$lng)

# SF
d$city.x <- ifelse(str_detect(d$UserLocation, '^SF')  & is.na(d$lat),
                   'SAN FRANCISCO', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^SF') & is.na(d$lat),
                  'CALIFORNIA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^SF') & is.na(d$lat),
                cities[cities$City=='SAN FRANCISCO',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^SF') & is.na(d$lng),
                cities[cities$City=='SAN FRANCISCO',][1,5], d$lng)

# NY
d$city.x <- ifelse(str_detect(d$UserLocation, '^NY')  & is.na(d$lat),
                   'NEW YORK', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^NY') & is.na(d$lat),
                  'NEW YORK', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^NY') & is.na(d$lat),
                cities[cities$City=='NEW YORK',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^NY') & is.na(d$lng),
                cities[cities$City=='NEW YORK',][1,5], d$lng)

# LA
d$city.x <- ifelse(str_detect(d$UserLocation, '^LA')  & is.na(d$lat),
                   'LOS ANGELES', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^LA') & is.na(d$lat),
                  'CALIFORNIA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^LA') & is.na(d$lat),
                cities[cities$City=='LOS ANGELES',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^LA') & is.na(d$lng),
                cities[cities$City=='LOS ANGELES',][1,5], d$lng)

# VENTURA <- DOES NOT EXIST ON HERE!
d$city.x <- ifelse(str_detect(d$UserLocation, '^VENTURA')  & is.na(d$lat),
                   'VENTURA', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^VENTURA') & is.na(d$lat),
                  'CALIFORNIA', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^VENTURA') & is.na(d$lat),
                cities[cities$City=='OXNARD',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^VENTURA') & is.na(d$lng),
                cities[cities$City=='OXNARD',][1,5], d$lng)

# WASHINGTON
d$city.x <- ifelse(str_detect(d$UserLocation, '^WASHINGTON, D.C.')  & is.na(d$lat),
                   'WASHINGTON', d$city.x)
d$state <- ifelse(str_detect(d$UserLocation, '^WASHINGTON, D.C.') & is.na(d$lat),
                  'WASHINGTON', d$state)
d$lat <- ifelse(str_detect(d$UserLocation, '^WASHINGTON, D.C.') & is.na(d$lat),
                cities[cities$City=='WASHINGTON',][1,4], d$lat)
d$lng <- ifelse(str_detect(d$UserLocation, '^WASHINGTON, D.C.') & is.na(d$lng),
                cities[cities$City=='WASHINGTON',][1,5], d$lng)

# How many people have locations?
nrow(d[is.na(lat)==F])
nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])

nrow(d[is.na(lat)==F]) / nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])
# How many people have cities in their location?
nrow(d[is.na(city.x)==F])
nrow(d[is.na(city.x)==F])  / nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])

# Here is the US census doc for each state's population center
#statemids <- read_delim(getURL("https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt"), delim = ',')
statemids <- read_delim('statemids.txt', delim = ',') %>% as.data.frame()

# I will create two new columns, which set lat and long to the middle of the state
# IF city was imputed from state

d <- as.data.table(d)
d$lat_mid <- rep(NA, times = nrow(d))
d$lng_mid <- rep(NA, times = nrow(d))
thestate <- NA

for( i in 1:nrow(d)){
  thestate = d$State.x[i]
  d$lat_mid[i] <- ifelse(is.na(d$lat[i])==F, d$lat[i],
                         ifelse(is.na(d$State.x[i]), NA,
                                statemids[toupper(statemids$STNAME) == thestate,]$LATITUDE[1]))
  d$lng_mid[i] <- ifelse(is.na(d$lng[i])==F, d$lng[i],
                         ifelse(is.na(d$State.x[i]), NA,
                                statemids[toupper(statemids$STNAME) == thestate,]$LONGITUDE[1]))
}



nrow(as.data.table(d)[is.na(lat_mid)==F])/ nrow(d[UserLocation != '' & UserLocation != 'UNDISCLOSED'])
fwrite(d, 'REI_reviews_located.csv')

## GENERATE CLEAN FILE

d <- d %>% select(c(reviewId, UserLocation, AuthorId, 
                    Rating, Helpfulness, TotalFeedbackCount, TotalNegativeFeedbackCount, 
                    SubmissionTime, ReviewText, Title, 
                    pid, city.x, State.x, State_id, city_from_state,
                    lat, lng, lat_mid, lng_mid))

fwrite(as.data.table(d), 'REI_reviews_located_clean.csv')