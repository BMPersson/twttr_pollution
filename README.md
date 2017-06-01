# twttr_pollution
Simple script to scrape and clean tweets from Plume (here using @PlumeInLondon) accounts.
Plume tweets regular updates on the pollution levels across a range of cities. Use this script to scrape data from
any of Plume's accounts to create your own plots an analysis of pollution levels in your favourite city.

This script requires a number of libraries to be installed:
  twitteR
  ROAuth
  plyr
  dplyr
  ggplot2

Collecting data from Twitter also requires the registration of of an application to get the necessary Twitter:
  API Key
  API Secret
  Token
  Token Secret

Registering an application can be done here: https://apps.twitter.com/

Running the script from start to finish will:
  Get Tweets from the specified Plume timeline
  Extract the time and that of each tweet
  Get the reported pollution level from each tweet
  Get the time of day for each pollution report
  Get the pollution category (low, moderate, high, etc) for each report
  Find the day of week for each tweet
  
Plots:
  Pollution level at each time of day at one-hour intervals
  Pollution levels each day of the week
  Pollution levels associated with each pollution 'grade' (low, moderate, high level of pollution, etc)
  Pollution level every hour of the day, split by day of the week
