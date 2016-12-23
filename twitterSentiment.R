
library(ggplot2)
library(RTextTools)
library(tm)
library(e1071)
library(wordcloud)
library(randomForest)

tweets <- read.csv("Tweets.csv")#read csv
table(tweets$airline_sentiment)#breakdown of sentiment

#bar plot of sentiment by airline
ggplot(data = tweets) + geom_bar(aes(x = airline, fill = airline_sentiment), stat = "count") +
  labs(x = "Airline", y = "Count", title = "Sentiment By Airline") 

neg_tweets <- tweets[tweets$airline_sentiment == "negative",]#negative tweets
pos_tweets <- tweets[tweets$airline_sentiment == "positive",]#positive tweets
neut_tweets <- tweets[tweets$airline_sentiment == "neutral",]#neutral tweets

#bar plot of negative reasons across all airlines
ggplot(data = neg_tweets) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

#bar plot of negative reasons for American 
ggplot(data = subset(neg_tweets, neg_tweets$airline == "American")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for American") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

#bar plot of negative reasons for Delta
ggplot(data = subset(neg_tweets, neg_tweets$airline == "Delta")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for Delta") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

#bar plot of negative reasons for Southwest
ggplot(data = subset(neg_tweets, neg_tweets$airline == "Southwest")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for Southwest") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE)

#bar plot of negative reasons for United
ggplot(data = subset(neg_tweets, neg_tweets$airline == "United")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for United") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

#bar plot of negative reasons for US Airways
ggplot(data = subset(neg_tweets, neg_tweets$airline == "US Airways")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for US Airways") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

#bar plot of negative reasons for Virgin America
ggplot(data = subset(neg_tweets, neg_tweets$airline == "Virgin America")) + geom_bar(aes(x = negativereason, fill = negativereason), stat = "count") + 
  labs(x = "Negative Reason", y = "Count", title = "Reason for Negative Tweets for Virgin America") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill=FALSE) 

tweets_only <- subset(tweets, select = c(airline_sentiment, text))#data frame of only sentiment and tweet text

tweets_only2 <- tweets_only
tweets_only2$text <- gsub("@\\w+", "", tweets_only$text)#remove twitter handles from tweet text 
rmWords <- c("the", "you", "for", "and", "get", "can", "now", "just")#list of words to remove

#function to create document term matrix
dtm = function(rawTweets){
  #rawTweets: list of tweets to be transformed
  createCorpus <- Corpus(VectorSource(rawTweets))#create corpus out of rawTweets
  createCorpus <- tm_map(createCorpus, content_transformer(tolower)) #transform all text to lowercase
  createCorpus <- tm_map(createCorpus, removePunctuation) #remove puntcuation from tweets
  createCorpus <- tm_map(createCorpus, removeWords, stopwords("english"))#remove stopwords from corpus
  createCorpus <- tm_map(createCorpus, removeWords, rmWords)#remove additional words from corpus
  createCorpus <- DocumentTermMatrix(createCorpus)#create document term matrix
  createCorpus <- removeSparseTerms(createCorpus, 0.99)#remove sparce terms from matrix
  
  return(createCorpus)#return document term matrix
}

par(mfrow=c(1,2))
pos_matrix <- dtm(tweets_only2$text[tweets_only2$airline_sentiment == "positive"])#document term matrix of positive tweets
pos_matrix <- as.data.frame(as.matrix(pos_matrix))#transform matrix to data frame

pos_freq_words <- colSums(pos_matrix)#count of word frequency

pos_freq_words <- pos_freq_words[order(pos_freq_words, decreasing = T)]#order words by frequency 
head(pos_freq_words)#6 most frequent words
#create word cloud of words from positive tweets
wordcloud(freq = as.vector(pos_freq_words), words = names(pos_freq_words),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Reds')[4:8])

neg_matrix <- dtm(tweets_only2$text[tweets_only2$airline_sentiment == "negative"])#document term matrix of negative tweets
neg_matrix <- as.data.frame(as.matrix(neg_matrix))#transform matrix to data frame

neg_freq_words <- colSums(neg_matrix)#count of word frequency

neg_freq_words <- neg_freq_words[order(neg_freq_words, decreasing = T)]#order words by frequency 
head(neg_freq_words)#6 most frequent words
#create word cloud of words from negative tweets
wordcloud(freq = as.vector(neg_freq_words), words = names(neg_freq_words),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Blues')[4:8])


par(mfrow=c(1,1))
#function to create document term matrix without the words in rmWords removed
dtmFull = function(rawTweets){
  #rawTweets: list of tweets to be transformed
  createCorpus <- Corpus(VectorSource(rawTweets))#create corpus out of rawTweets
  createCorpus <- tm_map(createCorpus, content_transformer(tolower)) #transform all text to lowercase
  createCorpus <- tm_map(createCorpus, removePunctuation) #remove puntcuation from tweets
  createCorpus <- tm_map(createCorpus, removeWords, stopwords("english"))
  createCorpus <- DocumentTermMatrix(createCorpus)#create document term matrix
  createCorpus <- removeSparseTerms(createCorpus, 0.99)#remove sparce terms from matrix
  
  return(createCorpus)#return document term matrix
}

matrix <- dtmFull(tweets_only2$text)#create document term matrix for all tweets

mat <- as.matrix(matrix)#transform to matrix

set.seed(123)#make the train set selection repeatable
train_index <- sample(seq_len(nrow(tweets_only2)), size = floor(0.75 * nrow(tweets_only2)))#generate random indexes
train_set <- mat[train_index,]#create train set
train.y <- tweets_only2$airline_sentiment[train_index]#response variable for train set
test_set <- mat[-train_index,]#create test set
test.y <- tweets_only2$airline_sentiment[-train_index]#response variable for test set

nbClassifier = naiveBayes(train_set, train.y)#naive bayes classifier

container <- create_container(train_set, train.y, trainSize = 1:length(train_set[,1]), virgin = F)#create container
rfClassifier <- randomForest(train_set, train.y, ntree = 500)#random forest classifier
rfcv <- cross_validate(container, nfold = 5, "RF")#5- fold cross validation of random forest classifier


#svm
svmClassifier <- svm(train_set, train.y, cross = 5)#svm classifier with 5-fold cross validation
svmcv <- svmClassifier$accuracies#cross validation results


nbpredict <- predict(nbClassifier, test_set)#predict test set classification using naive bayes classifier
rfpredict <- predict(rfClassifier, test_set)#predict test set classification using random forest classifier
svmpredict <- predict(svmClassifier, test_set)#predict test set classification using svm classifier

confusionMatrix(nbpredict, test.y)#confusion matrix for naive bayes classifier
confusionMatrix(rfpredict, test.y)#confusion matrix for random forest classifier
confusionMatrix(svmpredict, test.y)#confusion matrix for svm classifier

