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

