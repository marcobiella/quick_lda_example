library(tidyverse)
library(academictwitteR)

#notes
#https://developer.twitter.com/en/use-cases/do-research/academic-research/resources
#https://cran.r-project.org/web/packages/academictwitteR/vignettes/academictwitteR-tidy.html
#https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/build-a-query#list
#https://towardsdatascience.com/short-text-topic-modelling-lda-vs-gsdmm-20f1db742e14
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7861298/

#read API credentials
oautCred <- read.delim(file = "oautCred.txt", header = TRUE, sep = ",") %>% unlist()
#set bearer toke in environment
Sys.setenv(TWITTER_BEARER = unname(oautCred["bearer"]))

#retrieve tweets based on query (academic researcher developer account required)
get_all_tweets(
  #tweet search query (contains "#lamda" or "lamda", is in english and not a retweet)
  query = '(#lamda OR lamda OR #sentient OR sentient) lang:en -is:retweet', #tweet search query
               n = Inf, #max number of tweets to retrieve
               start_tweets = "2022-08-02T00:00:00Z", #starting date
               end_tweets = "2022-08-02T23:59:59Z", #end date
               file = "lamda_tweets", #file to store the data
               data_path = "lamda_data/", #path to store the data
               bind_tweets = TRUE, #format as table
               )

#remove credentials
rm(oautCred)
