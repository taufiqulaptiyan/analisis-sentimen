library(shiny)
library(dplyr)
library(rvest)
library(leaflet)
library(vroom)
library(here)
library(DT)
library(tm)
library(ggplot2)
library(wordcloud)
library(mapdata)

# Shiny UI
ui <- fluidPage(
  theme = "bootstrap.css",
  includeCSS("www/styles.css"),
  
  navbarPage(
    "Analisis Sentimen Pengguna Twitter Tentang #JokowiSilahkanMundur",
    id = "main_navbar",
    tabPanel(
      "Result Lexicon",
      titlePanel("Analisis sentimen #JokowiSilahkanMundur Dengan Metode Word Matching Sederhana (Lexicon)"),
      mainPanel(
        tabPanel("Tweet", DT::dataTableOutput("data1")), 
      )
    ),

    tabPanel(
      "Statistik Lexicon",
      titlePanel("Statistik Sentiment Score"),
      mainPanel(
        tabsetPanel(
          tabPanel("Histogram #JokowiSilahkannMundur", plotOutput("histo1")), 
          tabPanel("Summary #JokowiSilahkannMundur", verbatimTextOutput("statistik1")) 
        )
      )
    ),
    tabPanel(
      "Result Naive Bayes",
      titlePanel("Analisis Sentimen #JokowiSilahkannMundur Dengan Metode Naive Bayes"),
      mainPanel(
        tabsetPanel(
          tabPanel("Tweet", DT::dataTableOutput("data2"))
        )
      )
    ),
   
    tabPanel(
      "Statistik Naive Bayes",
      titlePanel("Statistik Sentiment Dengan Naive Bayes"),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot Tweet Emotions #JokowiSilahkannMundur", plotOutput("histo2")), 
          tabPanel("Plot Tweet Polarity #JokowiSilahkannMundur", plotOutput("histo3")), 
          tabPanel("Wordcloud Tweet #JokowiSilahkannMundur", plotOutput("histo4"))
        )
      )
    )
  )
)


# Shiny Server
server <- function(input, output, session) 
{
  #Inisialisasi file csv 
  jokowi <- vroom(here("jokowiResult.csv"))
  jokowi <- data.frame(jokowi)
  
  #Ambil data tweet bersih 
  tjokowiCleaned <- vroom(here("tjokowiCleaned.csv"))
  tjokowiCleaned <- data.frame(tjokowiCleaned)
  
  #Ambil data emotion  
  jokowiEmotion <- vroom(here("jokowiEmotion.csv"))
  jokowiEmotion <- data.frame(jokowiEmotion)
  
  #Ambil data hasil sentiment analysis 
  jokowi_bayes <- vroom(here("jokowisentimen.csv"))
  jokowi_bayes <- data.frame(jokowi_bayes)
  
  #Pengubahan tipe data untuk kolom score 
  jokowi$score <-  as.numeric(jokowi$score)
  
  #Membuat Tabel Menu Result Lexicon 
  output$data1 <-DT::renderDataTable(datatable(
    jokowi[,c(-1,-23,-24,-25,-28:-35)],filter = 'top',
    colnames = c("Text", "Score")
  ))
  
  
  #Membuat Tabel Menu Result Bayes
  output$data2 <-DT::renderDataTable(datatable(
    jokowi_bayes[,c(-1,-23,-24,-25,-28:-35)],filter = 'top',
    colnames = c("Text", "Emotion", "Polarity", "String As Factors")
  ))
  
  
  #Membuat histogram dan summary menu statistik Lexicon
  output$histo1 <- renderPlot({
    hist(jokowi$score)
  })
  
  
  output$statistik1 <- renderPrint({
    summary(jokowi$score)
  })
  
  #fungsi untuk plotting sentiment analysis dengan ggplot2 
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
  
  #Plot sentiment 
  output$histo2 <- renderPlot({
    plotSentiments1(jokowi_bayes, "Sentimen Analisis Dari Tweet #JokowiSilahkannMundur")
  })
  
  
  #Tinjau polaritas sentimen (positif vs negatif), untuk melihat tingkat dukungan:
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
  
  #Plot polaritas
  output$histo3 <- renderPlot({
    plotSentiments2(jokowi_bayes, "Analisis Polaritas Dari Tweet #JokowiSilahkannMundur")
  })
  
  
  #Fungsi pembershian ulang data
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
  
  #Pembuatan fungsi wordcloud
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

    suppressWarnings(comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5))
  }
  
  #Pemanggilan fungsi wordcloud
  output$histo4 <- renderPlot({
    getWordCloud(jokowi_bayes, tjokowiCleaned, jokowiEmotion)
  })
  
}

#Fungsi untuk Run
shinyApp(ui = ui, server = server)