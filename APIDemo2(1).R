library(twitteR)
library(ROAuth)
#setup.  Get keys from https://developer.twitter.com/
api_key <- "Your Key Here"
api_secret <- "Your Key Here"
access_token <- "Your Key Here"
access_token_secret <- "Your Key Here"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Get Tweets
tweets <- userTimeline("elonmusk", n = 3200, includeRts=TRUE)
tweets_df <- twListToDF(trump_tweets)
stock_tweets_list <- searchTwitter("tesla+stock market", resultType="recent",n = 500)
stock_tweets <- twListToDF(stock_tweets_list)
stock_tweets_no_retweets <- twListToDF(strip_retweets(stock_tweets_list))

#Write to file
write.csv(tweets_df,file = "C:/R Files/data/tweets_test.csv")


