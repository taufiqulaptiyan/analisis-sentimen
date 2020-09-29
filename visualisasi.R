library(Rstem)
library(sentiment)
library(ggplot2)
library(tm)
library(wordcloud)

#Classify_emotion function returns an object of class data frame with 7 columns 
#(anger, disgust, fear, joy, sadness, surprise, best_fit) and one row for each document:
tjokowiclassemo = classify_emotion(tjokowiCleaned, algorithm="bayes", prior=1.0)

#Ganti NA dengan unknown
#pilih kolom ke-7 atau best_fit:
jokowiEmotion = tjokowiclassemo[,7]
jokowiEmotion[is.na(jokowiEmotion)] = "unknown"

#Simpan data ke csv, jika sudah ada tidak perlu lagi
write.csv(jokowiEmotion, file = "jokowiEmotion_en.csv")

#Measure polarity (pos, neg, neutral):
tjokowiClassPol = classify_polarity(tjokowiCleaned, algorithm = "bayes")

#Fetch polarity category best_fit for our analysis purposes:
jokowiPol = tjokowiClassPol[,4]

#lihat hasilnya:
head(jokowiPol, 20)

#buat data frame dari hasil di atas:
jokowiSentimenDF = data.frame(text=tjokowiCleaned, emotion=jokowiEmotion, polarity=jokowiPol, stringAsFactors=FALSE)

#lihat hasilnya:
head(jokowiSentimenDF, 20)

#function for plotting
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}
#plotting tweets emotions
plotSentiments1(jokowiSentimenDF, "Analisis Sentimen Dari Tweet Tentang #JokowiSilahkanMundur")

#tinjau polaritas sentimen (positif vs negatif), untuk melihat tingkat dukungan:
#plot distribusi polaritas tweet:
plotSentiments2 <- function(sentiment_dataframe, title)
{
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}


#plotting tweets polarity
plotSentiments2(jokowiSentimenDF, "Analisis Polaritas Dari Tweet Tentang #JokowiSilahkanMundur")

#pembershian ulang data
removeCustomWords <- function(TweetsCleaned)
{
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i], 
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn", "guy", "booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}

#pembuatan fungsi wordcloud
getWordCloud <- function(sentiment_dataframe, TweetsCleaned, Emotion)
{
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomWords(TweetsCleaned)
  
  for(i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[Emotion == emos[i]], collapse=" ")
  }
  corpus <- Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  #require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5))
}
getWordCloud(jokowiSentimenDF, tjokowiCleaned, jokowiEmotion)

#export ke csv jika sudah ada tidak perlu lagi
write.csv(jokowiSentimenDF, file = "jokowisentimen.csv")
