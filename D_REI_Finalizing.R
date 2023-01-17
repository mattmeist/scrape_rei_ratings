library(tidyverse)
library(data.table)
d <- fread('REI_reviews_located_clean.csv')
d_noaa <- fread('REI_placebo.csv')
d_noaa <- d_noaa %>%
  select(c(reviewId, STATION, ELEVATION, TEMP,
           MAX, MIN, PRCP, PRCP_ATTRIBUTES, avg7MAX, avg7MIN, avg7TEMP, avg7PRCP,
           avg3MAX, avg3MIN, avg3TEMP, avg3PRCP)) %>%
  as.data.table()
d_noaa$reviewId <- as.numeric(d_noaa$reviewId)

d <- d %>% left_join(d_noaa, by = c('reviewId')) %>% as.data.table()

prices <- fread('REI_prices.csv')
products <- fread('REI_ovr.csv')

prices <- subset(prices, pid %in% unique(d$pid))
prices <- prices[!duplicated(prices$pid), ]
products <- subset(products, pid %in% unique(prices$pid))
products <- products[!duplicated(products$pid), ]
prices <- left_join(prices, products)

d <- left_join(d, prices) %>% as.data.table()

rm(products, d_noaa, prices)

# Now add categories:
cold_weather <- c("womens-insulated-jackets", "womens-snow-jackets",
                  "mens-insulated-jackets", "mens-snow-jackets", 
                  "gloves-and-mittens")
d$cold_weather <- ifelse(d$category %in% cold_weather, 1, 0 )

camping <- c("insect-repellent","portable-power-devices","sleeping-bags-and-accessories",
             "sleeping-pads", "solar-chargers")
d$camping <- ifelse(d$category %in% camping, 1, 0 )

bike_gear <- c("bike-computers","bike-helmets","bike-trainers","bikes", "cycling-shoes")
d$bike_gear <- ifelse(d$category %in% bike_gear, 1, 0 )

tech <- c("solar-chargers", "fitness-trackers", "headphones", "portable-power-devices")
d$tech <- ifelse(d$category %in% tech, 1, 0 )

jackets <- c("womens-insulated-jackets", "womens-rain-jackets","womens-snow-jackets",
             "mens-insulated-jackets", "mens-rain-jackets","mens-snow-jackets")
d$jackets <- ifelse(d$category %in% jackets, 1, 0 )

mens <- c("mens-running-shoes", "mens-insulated-jackets", "mens-rain-jackets","mens-snow-jackets")
d$mens <- ifelse(d$category %in% mens, 1, 0 )

womens <- c("womens-running-shoes", "womens-insulated-jackets", "womens-rain-jackets","womens-snow-jackets")
d$womens <- ifelse(d$category %in% womens, 1, 0 )

shoes <- c("mens-running-shoes", "womens-running-shoes", "climbing-shoes", "cycling-shoes")
d$shoes <- ifelse(d$category %in% shoes, 1, 0 )

d$Category <- ifelse(d$shoes == 1, 'shoes',
                     ifelse(d$cold_weather == 1, 'cold',
                            ifelse(d$tech == 1, 'tech',
                                   ifelse(d$bike_gear == 1, 'bike',
                                          ifelse(d$camping == 1, 'camping',
                                                 ifelse(d$jackets == 1, 'jacket',
                                                        'other'))))))

fwrite(d, 'REI_Reviews_Placebo.csv')
