#Setup--------------------------
options(java.parameters = "- Xmx10G")
setwd("~/R/Coursera/Capstone")
if(length(list.files(pattern="Coursera-SwiftKey.zip"))==0){
  list.filesDownloadURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  Zipfile <- "./Coursera-SwiftKey.zip"
}
DDir <- "data_full"

set.seed(12345)
library(data.table)
library(tm)
library(SnowballC)
library(RWeka)

# Functions ------------------
  CleanData <- function(InputData){
    b <- VCorpus(VectorSource(InputData), readerControl=list(language="english"))
    b <- tm_map(b, content_transformer(function(x) iconv(x, from = "UTF-8", to="ASCII", sub="")))
    b <- tm_map(b, content_transformer(function(x) gsub("[Hh][Tt][Tt][Pp][[:alnum:][:punct:]]*", "", x)))
    b <- tm_map(b, content_transformer(function(x) gsub("'","", x)))
    b <- tm_map(b, content_transformer(function(x) gsub("[[:punct:]]"," ", x)))
   #  b <- tm_map(b, content_transformer(function(x) gsub("[[:punct:]]* *(\\w+[']\\w+)|[[:punct:]]+ *| {2,}", " \\1", x)))
    b <- tm_map(b, content_transformer(tolower))
    b <- tm_map(b, content_transformer(removeNumbers))
   #   b <- tm_map(b, content_transformer(function(x) grepl("([a-z])\\1\\1","", x)))
    #b <- tm_map(b, removeWords, stopwords("english")) 
    b <- tm_map(b, content_transformer(stripWhitespace))
    b <- tm_map(b, removeWords, BanWords) 
    b <- tm_map(b, content_transformer(function(x) gsub("^[[:space:]]","", x)))
    b <- tm_map(b, content_transformer(function(x) gsub("[[:space:]]$","", x)))
   # b <- tm_map(b, stemDocument, language = "english")
    b <- tm_map(b, PlainTextDocument)
  }
  

  TDMtoDT <- function(inputtdm){
  m <- as.matrix(inputtdm)
  m <- data.table(cbind("Word"=rownames(m),"Freq"=as.numeric(m)), keep.rownames = FALSE)
  m$Freq <- as.numeric(m$Freq)
  m <- m[order(-Word,decreasing=TRUE),]
}


  Create_nGram <- function(FType, nGram){
    FName <- paste(FType,"_b",sep="")
    if (length(ls(pattern=FName)) == 0) load(paste("./", DDir,"/", FName,".RData",sep="")) 
    if (nGram == 1){
      tdm <- TermDocumentMatrix(get(FName), control = list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = nGram, max = nGram)), wordLengths = c(1, Inf)))
    } else {
      tdm <- TermDocumentMatrix(get(FName), control = list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = nGram, max = nGram))))
    }
    saveRDS(tdm, paste("./", DDir, "/", FType,"_tdm", nGram, ".rds",sep=""))
    D1 <- TDMtoDT(tdm)
    saveRDS(D1, paste("./", DDir, "/", FType,"_D", nGram, ".rds",sep=""))
    rm(tdm,D1)
    gc()
    return("Complete!!")
  }
  
  

# DL & Load Data ------------------
if(length(list.files(pattern="Coursera-SwiftKey.zip"))==0){
  download.file(DownloadURL,Zipfile, cacheOK = TRUE)
  unzip(Zipfile, files = NULL, list = FALSE, overwrite = TRUE,junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)
  rm(list.filesDownloadURL, Zipfile)
}

blog <- fread("./final/en_US/en_US.blogs.txt", header = FALSE, sep="\n", stringsAsFactors = FALSE, encoding="UTF-8")
news <- fread("./final/en_US/en_US.news.txt", header = FALSE, sep="\n", stringsAsFactors = FALSE, encoding="UTF-8")

twitter <- read.table("./final/en_US/en_US.twitter.txt", header = FALSE, sep="\n", quote = "", stringsAsFactors = FALSE,  encoding="UTF-8", skipNul = TRUE)
twitter <- data.table:::as.data.table.list(twitter)

# Create Sample ------------------
blog_SM <- as.data.table(sample(blog$V1, size = nrow(blog) * 1))
news_SM <- as.data.table(sample(news$V1, size = nrow(news) * 1))
twitter_SM <- as.data.table(sample(twitter$V1, size = nrow(twitter) * 1))
rm(blog, twitter, news)
SampleData <- rbind(blog_SM, news_SM, twitter_SM)
rm(blog_SM, twitter_SM, news_SM)
save(SampleData, file = paste("./", DDir,"/SampleData.RData",sep=""))

save(blog_SM, file = paste("./", DDir,"/blog_SM.RData",sep=""))
save(news_SM, file = paste("./", DDir,"/news_SM.RData",sep=""))
save(twitter_SM, file = paste("./", DDir,"/twitter_SM.RData",sep=""))


gc()

#1. blogs-------------------
  
  load(paste("./", DDir,"/blog_SM.RData",sep=""))
  BanWords <- readLines("BanWords.txt", encoding="UTF-8", warn=FALSE, skipNul=TRUE)
  blog_b <- CleanData(blog_SM)
  save(blog_b, file = paste("./", DDir,"/blog_b.RData",sep=""))
  rm(blog_SM)
  gc()
  load(paste("./", DDir,"/blog_b.RData",sep=""))
    Create_nGram ("blog", 1)
    Create_nGram ("blog", 2)
    Create_nGram ("blog", 3)
  rm(blog_b)
  gc()

#2. news ================
  load(paste("./", DDir,"/news_SM.RData",sep=""))
  BanWords <- readLines("BanWords.txt", encoding="UTF-8", warn=FALSE, skipNul=TRUE)
  news_b <- CleanData(news_SM)
  save(news_b, file = paste("./", DDir,"/news_b.RData",sep=""))
  rm(news_SM)
  gc()
  load(paste("./", DDir,"/news_b.RData",sep=""))
    Create_nGram ("news", 1)
    Create_nGram ("news", 2)
    Create_nGram ("news", 3)
  rm(news_b)
  gc()
  
#3. twitter ================
  load(paste("./", DDir,"/twitter_SM.RData",sep=""))
  BanWords <- readLines("BanWords.txt", encoding="UTF-8", warn=FALSE, skipNul=TRUE)
  twitter_b <- CleanData(twitter_SM)
  save(twitter_b, file = paste("./", DDir,"/twitter_b.RData",sep=""))
  rm(twitter_SM)
  gc()
  load(paste("./", DDir,"/twitter_b.RData",sep=""))
    Create_nGram ("twitter", 1)
    Create_nGram ("twitter", 2)
    Create_nGram ("twitter", 3)
  rm(twitter_b)
  gc()
