#Libraries to extract the tweets
library(twitteR)
library(RCurl)
library(ROAuth)

#Authentication keys from twitters
api_key <- "W491M4OGSvK49mjSEDaX4u1cb"
api_secret <- "k9NEZIkhtuUOlym4yUB3biiGxa5q3GJXTYEoA0eTdr6RSVLK6V"
access_token <-	"4088399893-HBGxmI1TL9VeptB2uDXI2VILaa1S97VbbivjVDr"
access_token_Secret <-	"qZQE8EJKB2RLP5x22IuhmTXS6OMJTG0wRFkuwztlAYlh1"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_Secret)


tweets <- searchTwitter('#IPL2018', n=300, lang="en")
tweets


#saving the extracted tweets as data frame
framed_tw <- twListToDF(tweets)
framed_tw[,2:16] <- NULL
View(framed_tw)


#--------PRE-PROCESSING DATA-----------#

library(stringr)
library(tm)
library(wordcloud)

#converting to ASCII tweets to avoid confusion
iconv(framed_tw$text, from = "UTF-8", to = "ASCII", sub = "")

#Clean text by removing graphic characters  
framed_tw$text=str_replace_all(framed_tw$text,"[^[:graph:]]", " ")

#Remove Junk Values and replacement words like fffd which appear 
#because of encoding differences
framed_tw$text <- gsub("[^[:alnum:]///' ]", "", framed_tw$text)

#Convert all text to lower case
framed_tw$text <- tolower(framed_tw$text)

#Remove retweet keyword
framed_tw$text <- gsub("rt", "", framed_tw$text)

#Remove Punctuations
framed_tw$text <- gsub("[[:punct:]]", "", framed_tw$text)

#Remove links
framed_tw$text <- gsub("http\\w+", "", framed_tw$text)

#Remove tabs
framed_tw$text <- gsub("[ |\t]{2,}", "", framed_tw$text)

#Remove blankspaces at begining
framed_tw$text <- gsub("^ ", "", framed_tw$text)

#Remove blankspaces at the end
framed_tw$text <- gsub(" $", "", framed_tw$text)

#Remove usernames
framed_tw$text <- gsub("@\\w+", "", framed_tw$text)

#convert comments into corpus
frame_corpus <- Corpus(VectorSource(framed_tw))
frame_corpus

frame_corpus = tm_map(frame_corpus, removeWords, stopwords('english'))
frame_corpus = tm_map(frame_corpus, removeNumbers)

#wordCloud
wordcloud(frame_corpus,scale=c(6,.5),min.freq=3,max.words=150,
          random.order= FALSE, random.color=FALSE,
          colors=brewer.pal(8,"Dark2"))

#Build document term matrix
tdm = TermDocumentMatrix(frame_corpus)

#calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq = as.matrix(words_freq)
words_freq = data.frame(words_freq)
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)

