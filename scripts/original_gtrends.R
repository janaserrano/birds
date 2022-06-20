#original https://www.linkedin.com/pulse/how-download-google-trends-search-interest-data-bulk-using-rhodes/
# prep data + environment -------------------------------------------------

# Install and load the readr gtrendsR & purrr packages
library(readr)
library(gtrendsR)
library(purrr)
library(dplyr)
library(readxl)

# load data & extract keywords list
#sp_list <- read_excel(PrimeDayKeywords$Keyword)
# manual inspection of dataframe
View(sp_list)


# bulk trend function -----------------------------------------------------

# The function wrap all the arguments of the gtrendR::trends function and return only the interest_over_time (you can change that)
googleTrendsData <- function (keywords) {
  # Set the geographic region, time span, Google product,...
  # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
  country <- c('US')
  time <- ("2016-09-04 2021-08-21")
  channel <- 'web'
  
  trends <- gtrends(keywords,
                    gprop = channel,
                    geo = country,
                    time = time)
  
  results <- trends$interest_over_time
}


# loop for all keywords and data export -----------------------------------

# for loop to try googleTrendsData function
# and return error message in console when encountered
output <- data.frame()
# change 1 to whichever number when daily quota has been reached
for (i in c(1:length(sp_list))) {
  try({
    output_new = map_dfr(.x = sp_list[i],
                         .f = googleTrendsData) %>%
      data.frame()
    output <- rbind(output, output_new)
  })
  # export dataframe as csv file in the working directory
  write.csv(output, 'PrimeDay KWs Trends Past 5 Years.csv')
}

######another method
# Last Update: 2020-08-22 
# Install and load the readr gtrendsR & purrr packages 
installed.packages("readr","gtrendsR", "purrr") 
library(readr) 
library(gtrendsR) 
library(purrr) 

# Load your keywords list (.csv file) 
kwlist <- readLines("Your-keywords-list-path.csv")

# The function wrap all the arguments of the gtrendR::trends function and return only the interest_over_time (you can change that)
googleTrendsData <- function (keywords) { 
  
  # Set the geographic region, time span, Google product,... 
  # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf 
  country <- c('IT') 
  time <- ("2018-08-01 2018-08-27") 
  channel <- 'web' 
  
  trends <- gtrends(keywords, 
                    gprop = channel,
                    geo = country,
                    time = time ) 
  
  results <- trends$interest_over_time 
} 

# googleTrendsData function is executed over the kwlist
output <- map_dfr(.x = sp_list,
                  .f = googleTrendsData ) 

# Download the dataframe "output" as a .csv file 
write.csv(output, "download.csv")