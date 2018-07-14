### Load all packages
list.of.packages <- c("httr", "magrittr","RCurl","RSelenium","seleniumPipes","plyr","XML","raster","readr","tm","wordcloud","png","stringr","dplyr","SnowballC")
new.packages <- list.of.packages[(!list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)#
lapply(list.of.packages, require, character.only  =TRUE)
rm(list.of.packages,new.package)


### Fire up Selenium Server
driver<- rsDriver(browser=c("chrome"), port=4444L) # instantiate a new remote driver to connect to Selenium Server
remDr <- driver[["client"]] # start chrome brower
remDr$open(silent= T) # open web brower
cat("   Selenium Server connect at", remDr$getStatus()$build$time,"\n")



###  Execute 90 loop of API data fetching to get all 1800 maximum records

n <- 90 # number of pages to scrape.

df_reviews = data.frame()

for (i in 1:n){
  if (i==1){
    cat("Processing page ",i,"\n")
    url<-'https://www.yelp.com/biz/pai-northern-thai-kitchen-toronto-5'    
  } else {
    cat("Processing page ",i,"\n")
    url<-paste0('https://www.yelp.com/biz/pai-northern-thai-kitchen-toronto-5?start=',(i-1)*20)
  }
  
  remDr$navigate(url) # navigate to webpage
  gsite<-getURL(url) # download url using RCurl
  sParse <- htmlParse(gsite) # parse downloaded url
  
  reviewer_id_str<-xpathSApply(sParse, "//div[@class='review-sidebar-content']/div/div/ul/li/a[@id='dropdown_user-name']", xmlGetAttr, "href") # reviewer id string
  reviewer_id<-str_replace(reviewer_id_str,"/user_details\\?userid=","") # retrieve id number only
  
  title_str<-xpathSApply(sParse, "//div[@class='review-content']/div/div/div[contains(@class,'i-stars')]", xmlGetAttr, "title") # reveiw rating string
  title<-trimws(str_replace(title_str,"star rating","")) # retrieve rating number only
  
  
  date_str<-xpathSApply(sParse, "//span[@class='rating-qualifier']",xmlValue) # date string
  date_str<-sapply(date_str, tolower)
  date<-date_str[!grepl("previous",date_str)]
  date1<-str_remove_all(date,"\n")
  date2<-str_remove_all(date1," ")
  date3<-str_remove_all(date2,"[A-z]")
  
  
  review_text_str<-xpathSApply(sParse, "//p[@lang='en']",xmlValue)
  review_text<-trimws(review_text_str)
  
  if (length(reviewer_id)==length(title) & length(title)==length(date) & length(date3)==length(review_text)) {
    temp_table=cbind.data.frame(reviewer_id, title, date3, review_text)
    df_reviews = rbind(df_reviews,temp_table)
    
  } else {
    cat("   The length of review table columns does not match...\n")
  }
  
  
}

textvector <- as.character(subset(df_reviews, df_reviews$title=="5.0")[["review_text"]])

corpus <- Corpus(VectorSource(textvector))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, c("thai","pai","curry","green","chicken","beef"))
corpus <- tm_map(corpus, stemDocument)

wordcloud(corpus, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))


cat("Read data ...\n")
reviews <- read_csv("../input/Reviews.csv")

par(mar=c(1,1,1,1))

# Many products have multiple productIds and their reviews have multiple copies
uniqReviews <- unique(reviews[, 3:10])

### define a function that takes a vector of texts and returns word frequency matrix
getWordFreq <- function(textVector) {
  # remove "not" from stopwords, as "not" is very important negative word
  stopWords <- stopwords("en")
  stopWords <- stopWords[stopWords != "not"]
  
  corpus = Corpus(VectorSource(textVector))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("thai","pai","curry","green","chicken","beef", stopWords))  
  df
  dtm = DocumentTermMatrix(corpus)
  removeSparse <- removeSparseTerms(dtm, 0.997)
  return(as.data.frame(as.matrix(removeSparse)))
}

### define a function that shows the top 100 frequent words in clound
displayWordcloud <- function(wordFrequency) {
  wordcloud(colnames(wordFrequency), colSums(wordFrequency),
            max.words = 100, random.order = FALSE, scale = c(4, 1),
            rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}

### word frequencies of the column "summary" for score = 1, 2, 3, 4, 5
cat("Calculate word frequency of reviews with score = 1 ...\n")
wordFreq1 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==1])

cat("Calculate word frequency of reviews with score = 2 ...\n")
wordFreq2 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==2])

cat("Calculate word frequency of reviews with score = 3 ...\n")
wordFreq3 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==3])

cat("Calculate word frequency of reviews with score = 4 ...\n")
wordFreq4 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==4])

cat("Calculate word frequency of reviews with score = 5, be patient ...\n")
wordFreq5 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==5])


### save plot as png file
cat("make plot ...\n")
png(file = "wordcloud_score.png", width = 1100, height = 270)

layout(matrix(1:5, nrow=1))

displayWordcloud(wordFreq1)
text(x = 0.5, y = 1.1, "score = 1", cex = 2)
displayWordcloud(wordFreq2)
text(x = 0.5, y = 1.1, "score = 2", cex = 2)
displayWordcloud(wordFreq3)
text(x = 0.5, y = 1.1, "score = 3", cex = 2)
displayWordcloud(wordFreq4)
text(x = 0.5, y = 1.1, "score = 4", cex = 2)
displayWordcloud(wordFreq5)
text(x = 0.5, y = 1.1, "score = 5", cex = 2)

dev.off()


### clear memory
remove(list = ls())
gc()
