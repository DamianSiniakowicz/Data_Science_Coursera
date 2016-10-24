# getting stuff off of twitter

library(ROAuth)
library(streamR)

credential <- OAuthFactory$new(consumerKey='**CONSUMER KEY**',
                               consumerSecret='**CONSUMER SECRETY KEY**',
                               requestURL='https://api.twitter.com/oauth/request_token',
                               accessURL='https://api.twitter.com/oauth/access_token',
                               authURL='https://api.twitter.com/oauth/authorize')

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")


#function to actually scrape Twitter
filterStream( file.name="tweets_test.json",
              track="twitter", tweets=1000, oauth=cred, timeout=10, lang='en' )

#Parses the tweets
tweet_df <- parseTweets(tweets='tweets_test.json')

#userStream
install.packages("twitteR")

userTimeline('HillaryClinton',n=10)



