#Require the necessary packages
library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
#Connecting to Twitter
consumerKey <- "7VxbNvz9c9wErMNRAhXmj7wPm"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "eUL07mhqIUTd0sn2xTcPkNVG2vX239pkcQVEUlgZI9i6C7H9HC"
accessToken <- "156183279Qn3XxwRBxOXscfPkStemCzHNprBE4LioSVAhKwE6"
accessTokenSecret <- "q6zsRpY51k5vIs4rmRTYlEysaGUbjv82cQB5xopOhjFPF"
Cred <- OAuthFactory$new(consumerKey="7VxbNvz9c9wErMNRAhXmj7wPm",consumerSecret="eUL07mhqIUTd0sn2xTcPkNVG2vX239pkcQVEUlgZI9i6C7H9HC",
      requestURL="https://api.twitter.com/oauth/request_token",
      accessURL= "https://api.twitter.com/access_token",
      authURL="https://api.twitter.com/oauth/authorize")
Cred$handshake()
setup_twitter_oauth("7VxbNvz9c9wErMNRAhXmj7wPm","eUL07mhqIUTd0sn2xTcPkNVG2vX239pkcQVEUlgZI9i6C7H9HC","156183279-Qn3XxwRBxOXscfPkStemCzHNprBE4LioSVAhKwE6","q6zsRpY51k5vIs4rmRTYlEysaGUbjv82cQB5xopOhjFPF")
Asaba2018<-searchTwitter("#Asaba", n=200, lang = NULL)
Asabagames<- ldply(Asaba2018, function(t) t$toDataFrame())
##Cleaning the tweet
library(stringr)
library(tidytext)
library(tidyverse)
Asabagamess<-strip_retweets(Asaba2018)
text <- sapply(Asabagamess, function(x) x$getText())
text1 <- gsub("&amp", "", text)
text2 <- gsub("http\\w+", "", text1)
text3 <- gsub("@\\w+", "", text2)
text4 <- gsub('[[:punct:]]', '', text3)
text5 <- gsub('[[:cntrl:]]', '', text4)
text6 <- gsub("[[:digit:]]", "", text5)
text7 <- gsub("[ \t]{2,}", "", text6)
text8 <- gsub("^\\s+|\\s+$", "", text7)
#Using Try Error Function to convert to lower case.
try.error= function(x)  {
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try.error, "error"))
    y=tolower(x)
  return(y)
}
textdat=sapply(text8,try.error)

textdata=textdat[!is.na(textdat)]
#Assign spaces and not missing data in spaces and not missing values
names(textdata)=NULL
#Getting the positive and Negative emotions
View(textdata)
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(textdata, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}  
pos.words = scan('C:/Users/OLUWATOBI/Desktop/RClassCovenLabs/positive-words.txt',what='character',comment.char=';')
neg.words = scan('C:/Users/OLUWATOBI/Desktop/RClassCovenLabs/negative-words.txt',what='character',comment.char=';')
AsabaScore <- score.sentiment(textdata,pos.words,neg.words,.progress='text')
hist(AsabaScore$score)
View(AsabaScore)
#The emotional classification
library(syuzhet)
library(ggplot2)
mysentiment <- get_nrc_sentiment(textdat)
SentimentScores<-data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores<-cbind("sentiment"= row.names(SentimentScores), SentimentScores)
rownames(SentimentScores)<-NULL
View(SentimentScores)
library(ggplot2)
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") + xlab("Sentiment") + ylab("Score") + ggtitle("Asaba 2018")
