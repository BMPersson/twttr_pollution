#Load the necessary libraries
library("twitteR")
library("ROAuth")
library("plyr")
library("dplyr")
library("ggplot2")


### - SET UP TWITTER - ###


# Declare Twitter API Credentials
api_key <- "tZq1yysVyDg1oiFtsal3kughH" # From dev.twitter.com
api_secret <- "AzJwVluUxCAEon9pNaPWStw7sPVvD8kDHihFTREsXwS828Rmei" # From dev.twitter.com
token <- "551112455-tu1Lf5U0q1jYXol69sGmxqPu7Fz5lqqh1JehRJRi" # From dev.twitter.com
token_secret <- "MoMEAj1K2BF9lUQ9MTt8MRolimXL9p4QwKTD6GdPNStnc" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)


### - IMPORT TWEETS - ###


#Get tweets from Plume timeline
tweets <- userTimeline("PlumeInLondon", n = 2000)

#Convert list of tweets to data frame
tweets.df <- twListToDF(tweets)


### - DATA CLEANING - ###


#Change the time and date format to put the time of each tweet in its own column - Also converts times to factors
#Creates data frame where data from tweets will be added
data.df = data.frame(
  date=tweets.df$created,
  timetweeted=format(tweets.df$created, "%H:%M")
)

#Converts times back to POSIXct - sets all dates to the same, allowing for plotting times, NOT across different dates
data.df$timetweeted <- as.POSIXct(data.df$timetweeted,format='%H:%M')

#Extracts the pollution level from the tweets
pollution <- regmatches(tweets.df$text, gregexpr("(?<=\\().*?(?=\\))", tweets.df$text, perl=T))
  pollution <- as.numeric(pollution)

#Removes any NAs from the pollution variable
pollution[sapply(pollution, is.null)] <- NA

#Add the pollution level to the data frame
data.df$pollution <- pollution

#Extract the time of day for the pollution level from the tweets
m <- gregexpr("([0-9]+AM|[0-9]+PM)", tweets.df$text)
  timeDay <- regmatches(tweets.df$text,m)

#Convert time of day to character
timeDay <- as.character(timeDay)

#Add time of day to the data frame
data.df$time <- timeDay

#Extract Plume's pollution category from the tweets
lev <- gregexpr("(^Extreme|^Very high|^High|Moderate|Low)", tweets.df$text)
  level <- regmatches(tweets.df$text,lev)

#Change pollution category to character
level <- as.character(level)

#Add pollution category to data frame
data.df$level <- level

#Converts dates to days of the week
data.df$day <- weekdays(as.Date(data.df$date))

#data.df <- data.df[complete.cases(data.df),]
data.df <- na.omit(data.df) 

#Set the correct order of the time-of-day from 12AM to 11PM
t.order <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM', '6AM', '7AM', '8AM', '9AM', '10AM', '11AM', '12PM', 
             '1PM', '2PM', '3PM', '4PM', '5PM', '6PM', '7PM', '8PM', '9PM', '10PM', '11PM')

#Removes any rows where the time has been extracted incorrectly - ensures that all values in time column matches any value in
#the t.order variable
data.df <- data.df[(data.df$time %in% t.order),]

#Summarise and group data by time of day
data.df.time <- data.frame(data.df %>% group_by(time) %>% summarise(avg_pollution = mean(pollution)))
  data.df.time$time <- factor(data.df.time$time, levels = t.order)

#Summarise and group data by day of week
data.df.day <- data.frame(data.df %>% group_by(day) %>% summarise(avg_pollution = mean(pollution)))

#Set the correct order of day of week from Monday to Sunday
d.order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
  data.df.day$day <- factor(data.df.day$day, levels = d.order)

#Summarise and group data by pollution level category
data.df.level <- data.frame(data.df %>% group_by(level) %>% summarise(avg_pollution = mean(pollution)))

#Create tables to see how many measures per hour of the day and day per week the data set has
no_measures_time <- table(data.df$time)
no_measures_day <- table(data.df$day)


### - PLOTS - ###


#Time of day plot
timePlot <- ggplot(data.df.time, aes(data.df.time$time,data.df.time$avg_pollution, group=1)) + 
  geom_line(size = 2.2, color = 'royalblue4') +
  labs(y = "Average pollution level", x = "Time of day") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, b = 5), face="bold", size = 24),
        axis.title.y = element_text(margin = margin(r = 15, l = 5), face="bold", size = 24), 
        axis.text.x = element_text(colour="black", size = 14),
        axis.text.y = element_text(colour="black", size = 20),
        axis.line = element_line(size=1.5, colour = "black"))


#Day of week plot
dayPlot <- ggplot(data.df.day, aes(data.df.day$day, data.df.day$avg_pollution, group=1)) + 
  geom_line(size = 2.2, color = 'royalblue4') +
  geom_point(shape = 21, colour = "royalblue4", fill = "white", size = 2.5, stroke = 2.5) +
  labs(y = "Pollution amount", x = "Day of week") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, b = 5), face="bold", size = 24),
        axis.title.y = element_text(margin = margin(r = 15, l = 5), face="bold", size = 24), 
        axis.text.x = element_text(colour="black", size = 20),
        axis.text.y = element_text(colour="black", size = 20),
        axis.line = element_line(size=1.5, colour = "black"))


#Box plot with range of the levlels of pollution - low, medium, high, very high, and extreme
l.order <- c('Low', 'Moderate', 'High', 'Very high', 'Extreme')
  data.df$level <- factor(data.df$level, levels = l.order)

#Convert Level, time of day, and day of week to numericals for plotting purposes
data.df <- data.df %>% mutate(levelHolder = ifelse(level == "Low", 1, 
  ifelse(level == "Moderate", 2,
  ifelse(level == "High", 3,
  ifelse(level == "Very high", 4,
  ifelse(level == "Extreme", 5, NA))))))

boxPlot <- ggplot(data.df, aes(x=level, y=pollution)) + geom_boxplot(colour = "#1F3552", fill = "#4279AE", size = 1)+
  labs(y = "Pollution amount", x = "Level of pollution") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, b = 5), face="bold", size = 24),
        axis.title.y = element_text(margin = margin(r = 15, l = 5), face="bold", size = 24), 
        axis.text.x = element_text(colour="black", size = 20),
        axis.text.y = element_text(colour="black", size = 20),
        axis.line = element_line(size=1.5, colour = "black"))


#Group by time of day, and day of week
data.df.group <- data.frame(data.df %>% group_by(time, day) %>% summarise(avg_pollution = mean(pollution)))
  data.df.group$time <- factor(data.df.group$time, levels = t.order)
  data.df.group$day <- factor(data.df.group$day, levels = d.order)

#Plot pollution level over time of day for each day of week individually
ind_dayPlot <- ggplot(data.df.group, aes(time, avg_pollution, group=day)) + 
  labs(y = "Pollution amount", x = "Time of day") +
  geom_line(aes(colour=day), size = 2.2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, b = 5), face="bold", size = 24),
        axis.title.y = element_text(margin = margin(r = 15, l = 5), face="bold", size = 24), 
        axis.text.x = element_text(colour="black", size = 13),
        axis.text.y = element_text(colour="black", size = 22),
        axis.line = element_line(size=1.5, colour = "black"))
