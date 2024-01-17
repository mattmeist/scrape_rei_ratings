rm(list = ls()); gc() #Clear working directory

# Check if groundhog is already installed, and if not, install it
if (!requireNamespace("groundhog", quietly = TRUE)) {
  install.packages("groundhog", dependencies = TRUE)
}

#Load it
library(groundhog)

# Load packages with groundhog
ghog.date <- "2023-12-01"
pkgs <- c('data.table', 'tidyverse')
groundhog.library(pkgs, ghog.date)
rm(ghog.date, pkgs)

d <- fread('REI_Analysis.csv')
prices <- fread('REI_prices.csv')
products <- fread('REI_ovr.csv')

prices <- subset(prices, pid %in% unique(d$pid))
prices <- prices[!duplicated(prices$pid), ]
products <- subset(products, pid %in% unique(prices$pid))
products <- products[!duplicated(products$pid), ]
prices <- left_join(prices, products, by = 'pid')

d <- left_join(d, prices)
unique(d$category)

rm(products, prices)

# Now add categories:
cold_weather <- c("womens-jackets", "snow-clothing","mens-jackets",
                  "gloves-and-mittens", "mens-insulated-jackets", "womens-insulated-jackets",
                  "mens-snow-jackets", "womens-snow-jackets", "snowboard-clothing",
                  "ski-clothing")
d$cold_weather <- ifelse(d$category %in% cold_weather, 1, 0 )

d$ski <- ifelse(str_detect(d$category, "ski") | str_detect(d$category, "snowbo"), 1, 0)

camping <- c("insect-repellent","portable-power-devices","sleeping-bags-and-accessories",
             "sleeping-pads", "solar-chargers")
d$camping <- ifelse(d$category %in% camping | str_detect(d$category, "camp"), 1, 0 )

bike_gear <- c("bike-computers","bike-helmets","bike-trainers","bikes", "cycling-shoes")
d$bike_gear <- ifelse(d$category %in% bike_gear, 1, 0 )

tech <- c("solar-chargers", "fitness-trackers", "headphones", "portable-power-devices")
d$tech <- ifelse(d$category %in% tech, 1, 0 )

jackets <- c("womens-insulated-jackets", "womens-rain-jackets","womens-snow-jackets",
             "mens-insulated-jackets", "mens-rain-jackets","mens-snow-jackets",
             "womens-jackets", "mens-jackets")
d$jackets <- ifelse(d$category %in% jackets, 1, 0 )

d$mens <- ifelse(str_detect(d$category, "^men"), 1, 0 )
d$womens <- ifelse(str_detect(d$category, "^women"), 1, 0 )
d$shoes <- ifelse(str_detect(d$category, "shoes"), 1, 0)

d$Category <- ifelse(d$shoes == 1, 'shoes',
                     ifelse(d$cold_weather == 1, 'cold',
                            ifelse(d$tech == 1, 'tech',
                                   ifelse(d$bike_gear == 1, 'bike',
                                          ifelse(d$camping == 1, 'camping',
                                                 ifelse(d$jackets == 1, 'jacket',
                                                        'other'))))))

fwrite(d, 'REI_Final.csv')
