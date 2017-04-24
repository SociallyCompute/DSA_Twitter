###this erases all variables stored in memory
rm(list=ls(all=TRUE))

#import twitteR and stringr for API access and text manipulation
library(twitteR)
library(stringr)
library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(lubridate)
library(SnowballC)

#search twitter API for boolean string
#search twitter for boolean phrase
#setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)

setup_twitter_oauth("zCYCpLFMIi3UXHr6lfxJAGxO3", "hy3ucjEwwSIyDs6JP84CIfchGI6fJAlS964a1qNCxTZUAmqz7J",
                    "2756479056-YqYNKJbjcNwNxx8VKGq54maYYP0JnwcxJH2qUdo", "tyW0Qnh9mZ0csDO7KpAKpEC3tlp8Ui6n4ZB8MEhp9hAhr")

#Extract tweets from a single user at a time   

clinton_tweets <- userTimeline(user = "@HillaryClinton",
                               n = 200, includeRts = FALSE, retryOnRateLimit = 2000)
trump_tweets <- userTimeline(user = "@realDonaldTrump",
                             n = 200, includeRts = FALSE, retryOnRateLimit = 2000)  

# clinton_tweets$text <- sapply(clinton_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# trump_tweets$text <- sapply(trump_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Put the tweets downloaded into a data.frame
clinton_tweets <- twListToDF(clinton_tweets)
trump_tweets <- twListToDF(trump_tweets)

 Encoding(clinton_tweets$text) <- "UTF-8"
 Encoding(trump_tweets$text) <- "UTF-8"


# clinton_tweets$text <- sapply(clinton_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# trump_tweets$text <- sapply(trump_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Remove punctuation, numbers, html-links and unecessary spaces:
# textScrubber <- function(dataframe) {
#   
#   dataframe$text <-  gsub("—", " ", dataframe$text)
#   dataframe$text <-  gsub("&", " ", dataframe$text)
#   dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
#   dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
#   dataframe$text <-  gsub("http\\w+", "", dataframe$text)
#   dataframe$text <-  gsub("\n", " ", dataframe$text)
#   dataframe$text <-  gsub("[ \t]{2,}", "", dataframe$text)
#   dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
#   dataframe$text <-  tolower(dataframe$text)
#   
#   return(dataframe)
# }

# clinton_tweets <- textScrubber(clinton_tweets)
# trump_tweets <- textScrubber(trump_tweets)

# The tweets are now easier to work with. 
# The next step is removing all the so-called “stopwords”(words that do not add meaning to the topic), 
# and convert the text into a Term Document Matrix. 
# The TDM is then summed up so we get a data.frame of words arranged by how often they are used:

tdmCreator <- function(dataframe, stemDoc = T, rmStopwords = T){
  
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, stopwords())
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(wordLengths = c(4, Inf)))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}

clinton_tweets <- tdmCreator(clinton_tweets)
trump_tweets <- tdmCreator(trump_tweets)



#Selects the 15 most used words.
trump_tweets <- trump_tweets[1:15,]
clinton_tweets <- clinton_tweets[1:15,]

#Create bar graph with appropriate colours
#and use coord_flip() to help the labels look nicer.
trump_plot <- ggplot(trump_tweets, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold"))

clinton_plot <- ggplot(clinton_tweets, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold"))

#There are other ways to get these plots
#side-by-side, but this is easy.
grid.arrange(trump_plot, clinton_plot, ncol=2)


