library(twitteR)
library(ROAuth)

#Diambil dari twitter developer
access_secret<-"MivFNO8rsUlqnown3D5yYBgHqlSZOhssVHIhZL7rFHoDT"
access_token<-"1471672230-7TKNN3cuuFG1G8JlyRO0bwy3schnnxu50z4GuCB"
consumer_key<-"q4Yccg2KIno1Pff0dOsKbA2RB"
consumer_secret<-"0PJgaCurPorQXkSclIQcFmcE0bBNprGzwqo4W4e6EuIzQf4CV7"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Mencari tweet
jokowi_tweets = searchTwitter("jokowisilahkanmundur", n=1000, lang="id")

#cleaning text
jokowiTweets <- sapply(jokowi_tweets, function(x) x$getText())

catch.error = function(x)
{
  y = NA
  #test untuk mengecek error
  catch_error = tryCatch(tolower(x), error=function(e) e)
  #Jika tidak error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  
  return(y)
}

#Membersihkan tweet untuk analisis sentimen
cleanTweets <- function(tweet) {
  #remove html links:
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  #remove retweet entities:
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  #remove #hashtags:
  tweet = gsub("#\\w+", " ", tweet)
  #remove all "@people":
  tweet = gsub("@\\w+", " ", tweet)
  #remove all punctuations:
  tweet = gsub("[[:punct:]]", " ", tweet)
  #remove numbers, kita hanya butuh teks untuk analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  #remove unnecessary spaces (white spaces, tabs, etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  #remove amp
  tweet = gsub("&amp", " ", tweet)
  #remove crazy character
  tweet = gsub("[^a-zA-Z0-9]", " ", tweet)
  #remove alphanumeric
  tweet = gsub("[^[:alnum:]]", " ", tweet)
  #jika ada lagi yang dirasa ingin dihilangkan, bisa. Contohnya, slang words/bahasa gaul dapat dihilangkan dengan cara serupa di atas.
  #ubah semua kata menjadi lowercase:
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs <- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  #remove "NA" tweets:
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  #remove repetitive tweets:
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

#Proses pembersihan tweet
tjokowiCleaned = cleanTweetsAndRemoveNAs(jokowiTweets)

#Export data ke csv
write.csv(tjokowiCleaned, file = "tjokowiCleaned.csv")

#Import lexicon
opinion.lexicon.pos = scan("s-pos.txt", what = "character", comment.char = ";")
opinion.lexicon.neg = scan("s-neg.txt", what = "character", comment.char = ";")

pos.words = c(opinion.lexicon.pos)
neg.words = c(opinion.lexicon.neg)

#Membuat fungsi score.sentiment() 
#untuk menghitung hasil sentimen mentah berdasarkan algoritma pencocokan sederhana:
getSentimentScore = function(sentences, pos.words, neg.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    #remove digit, punctuation, dan special/control character:
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    #convert semua teks menjadi lowercase:
    sentence = tolower(sentence)
    #pisahkan setiap kalimat menggunakan spasi (space delimiter):
    words = unlist(str_split(sentence, "\\s+"))
    #lakukan boolean match dari setiap kata-kata menggunakan pos &amp;amp;amp; neg opinion-lexicon:
    pos.matches = !is.na(match(words, pos.words))
    neg.matches = !is.na(match(words, neg.words))
    #score sentimen = total positive sentiment - total negative:
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  #return data frame berisi kalimat beserta sentimennya:
  return(data.frame(text = sentences, score = scores))
}

#Terapkan ke data tweet yang telah kita bersihkan:
jokowiResult = getSentimentScore(tjokowiCleaned, pos.words, neg.words)

#Export ke csv:
write.csv(jokowiResult, file = "jokowiResult.csv")