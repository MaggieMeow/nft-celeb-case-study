##Sentiment Analysis

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(dplyr)
library(lexicon)
library(ggplot2)


community1 <- readtext('data_discord.csv', text_field = 'content', encoding = "UTF-8")
community_corpus  <-  corpus(community1)
View(community1)
View(community_corpus)
ndoc(community_corpus)

##Example community post
as.character(community_corpus)[50]


##Preliminary analysis to get a sense of the themes
##removing punctuation, numbers, seperators, urls and english stopwords
##finding the top 50 frequent singluar words
community_tokens  <- community_corpus %>% tokens( remove_punct = TRUE, remove_numbers=TRUE, remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=1:3)
community_dfm  <-  dfm(community_tokens)
topfeatures(community_dfm, 50)
##finding the top 50 frequent phrases of length 2-3 words
community_tokens  <- community_corpus %>% tokens( remove_punct = TRUE, remove_numbers=TRUE, remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
community_dfm  <-  dfm(community_tokens)
topfeatures(community_dfm, 50)


##Calculating Sentiment scores of each comment
data_dictionary_LSD2015

# convert corpus to tokens and apply sentiment dictionary to code tokens
sentiment_tokens  <- community_corpus%>% tokens() %>% tokens_lookup(dictionary = data_dictionary_LSD2015)

# create document-feature matrix of sentiment
dfm_sentiment  <-  dfm(sentiment_tokens)

# convert to a data.frame to allow further calculations 
sentiment_df  <- convert(dfm_sentiment, to = 'data.frame')

#viewing the sentiments of the first few comments
head(sentiment_df)


##Plotting the Sentiment scores

sentiment_df$sent_score <- log(sentiment_df$positive + sentiment_df$neg_negative + 0.5)  -  log(sentiment_df$negative + sentiment_df$neg_positive + 0.5)
#turning the author names into factors, so i can apply colour to them
sentiment_df$author <- as.factor(community1$author_username)
#changing the reaction emoji count to numeric, and summing the values in a single list
sentiment_df$reaction_size <- sapply(strsplit(community1$reaction_emoji_count_list, ","), function(x) sum(as.numeric(x)))


#plotting sentiment score by post
plot(sentiment_df$sent_score, xlab = 'Post number', ylab= 'Sentiment Score', main = 'Sentiment Score by post',  col = sentiment_df$author)
#the legend for the plots
legend(10,3.5,unique(sentiment_df$author),col=1:length(sentiment_df$author),pch=1)

##adding timestamp to dataframe
sentiment_df$timestamp <- community1$timestamp
#converting timestamp
sentiment_df$timestamp <- strptime(sentiment_df$timestamp, "%Y-%m-%d %H:%M:%S")
#plotting sentiment score by timestamp
plot(sentiment_df$timestamp, sentiment_df$sent_score,  xlab = 'Timestamp', ylab= 'Sentiment Score', main = 'Sentiment Score over Time')
legend(5,4,unique(sentiment_df$author),col=1:length(sentiment_df$author),pch=1)


##plot again, but in ggplot
ggplot(sentiment_df, aes(x = as.Date(timestamp), y = sent_score, colour = author)) +
  geom_point(aes(size = reaction_size)) + 
  geom_smooth(method = "lm")+
  labs(x = "Time", y = "Estimated sentiment")
  #theme(legend.position = 'none')

## Who posts the most
barplot(table(sentiment_df$author), xlab='Author Usernames', ylab= 'Frequency of Post', main = 'Frequency of Posts by User')


##Emoji reaction analysis --- ATTEMPT
##https://search.r-project.org/CRAN/refmans/lexicon/html/hash_sentiment_emojis.html

hash_sentiment_emojis
emojis_sentiment


communityr <- readtext('data_discord.csv', text_field = 'reaction_list', encoding = "UTF-8")
community_corpusr  <-  corpus(communityr)
View(communityr)
View(community_corpusr)
ndoc(community_corpusr)

##Example community post
as.character(community_corpusr)[50]


##most used reactions
community_tokens  <- community_corpusr %>% tokens( remove_punct = TRUE) %>%  tokens_tolower() %>% tokens_ngrams(n=1:1)
community_dfm  <-  dfm(community_tokens)
topfeatures(community_dfm, 50)



# convert corpus to tokens and apply sentiment dictionary to code tokens
sentiment_tokens  <- community_corpusr%>% tokens() %>% tokens_lookup(dictionary = hash_sentiment_emojis)

# create document-feature matrix of sentiment
dfm_sentiment  <-  dfm(sentiment_tokens)

# convert to a data.frame to allow further calculations 
sentiment_df  <- convert(dfm_sentiment, to = 'data.frame')

#viewing the sentiments of the first few comments
head(sentiment_df)


##Plotting the Sentiment scores

sentiment_df$sent_score <- log(sentiment_df$positive + sentiment_df$neg_negative + 0.5)  -  log(sentiment_df$negative + sentiment_df$neg_positive + 0.5)
sentiment_df$author <- as.factor(community1$author_username)

#plotting sentiment score by post
plot(sentiment_df$sent_score, xlab = 'Post number', ylab= 'Sentiment Score', main = 'Sentiment Score by post',  col = sentiment_df$author)
#the legend for the plots
legend(10,3.5,unique(sentiment_df$author),col=1:length(sentiment_df$author),pch=1)

##adding timestamp to dataframe
sentiment_df$timestamp <- community1$timestamp
#converting timestamp
sentiment_df$timestamp <- strptime(sentiment_df$timestamp, "%Y-%m-%d %H:%M:%S")
#plotting sentiment score by timestamp
plot(sentiment_df$timestamp, sentiment_df$sent_score,  xlab = 'Timestamp', ylab= 'Sentiment Score', main = 'Sentiment Score over Time',  col = sentiment_df$author)
legend(5,4,unique(sentiment_df$author),col=1:length(sentiment_df$author),pch=1)
