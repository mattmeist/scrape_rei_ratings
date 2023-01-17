# scrape_rei_ratings
This code uses Python primarily, but R for some data wrangling. 
This code and data were used in the paper https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4155522.
There are four scripts in this repository as is. They should be run in alphabetical order, which is why there is an an A-D before each script.

## Script A (A_Scrape_REI_Reviews.ipynb):
This jupyter notebook is where all of the scraping happens.
Here, I go to REI and pull product information and reviews for every product I can find on the website.
This should be the most useful to future folks, so let me know if there are issues with it.

## Script B (B_Merge_REI_Reviews.R):
This could probably be in Python, but I'm an R guy, so it's there.
The code is borrowed from Tony Cookson (https://www.r-bloggers.com/2011/04/merging-multiple-data-files-into-one-data-frame/)

## Script C (C_Merge_Weather_Data.ipynb):
Here we are, back to jupyter notebooks...
This code might not be useful for everyone, as you may not care about weather. 
However, it's actually not a trivial thing to do, so I wanted to make the code available for finding the closest set of coordinates to another.

## Script D (D_REI_Finalizing.R):
This is probably the least useful.
It just cleans the merged data into something I could use for analysis. Use it if you need!

I hope all of these scripts can be helpful in some way to you. But more so, I hope they're not wrong and harmful... 
If you have any questions, or spot any errors, please let me know!
