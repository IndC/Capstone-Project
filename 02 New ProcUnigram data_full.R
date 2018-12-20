#Setup--------------------------
options(java.parameters = "- Xmx10G")
setwd("~/R/Coursera/Capstone")
DDir <- "data_full"

set.seed(12345)
library(data.table)
library(tm)
library(SnowballC)
library(RWeka)

# Initial Processing----

##Unigram Processing ====
  blog_D1 <- readRDS(paste("./", DDir,"/blog_D1.rds",sep=""))
  news_D1 <- readRDS(paste("./", DDir,"/news_D1.rds",sep=""))
  twitter_D1 <- readRDS(paste("./", DDir,"/twitter_D1.rds",sep=""))

  setkey(blog_D1, "Word")
  setkey(news_D1, "Word")
  setkey(twitter_D1, "Word")
  
  D1 <- merge(blog_D1, news_D1, all=TRUE)
  D1 <- merge(D1, twitter_D1, all=TRUE)
  D1[, Freq.n := rowSums(.SD, na.rm = TRUE), .SDcols = c("Freq", "Freq.x", "Freq.y")]
  D1[,c("Freq", "Freq.x", "Freq.y"):= NULL]
  colnames(D1) <- c("Word","Freq")
  saveRDS(D1, paste("./", DDir,"/D1A.rds",sep=""))
    TmpD <- D1[, lapply(.SD, function(x) grepl("([a-z])\\1\\1", x, ignore.case=TRUE))]
    D1 <- D1[TmpD$Word == 0]
    setkey(D1, "Word")
  saveRDS(D1, paste("./", DDir,"/D1.rds",sep=""))
  rm(blog_D1, news_D1, twitter_D1, D1)
  gc()
  
##Bigram Processing ====
  blog_D2 <- readRDS(paste("./", DDir,"/blog_D2.rds",sep=""))
  news_D2 <- readRDS(paste("./", DDir,"/news_D2.rds",sep=""))
  twitter_D2 <- readRDS(paste("./", DDir,"/twitter_D2.rds",sep=""))
  
  setkey(blog_D2, "Word")
  setkey(news_D2, "Word")
  setkey(twitter_D2, "Word")
  
  D2 <- merge(blog_D2, news_D2, all=TRUE)
  D2 <- merge(D2, twitter_D2, all=TRUE)
  D2[, Freq.n := rowSums(.SD, na.rm = TRUE), .SDcols = c("Freq", "Freq.x", "Freq.y")]
  D2[,c("Freq", "Freq.x", "Freq.y"):= NULL]
  colnames(D2) <- c("Word","Freq")
  saveRDS(D2, paste("./", DDir,"/D2.rds",sep=""))
  rm(blog_D2, news_D2, twitter_D2, D2)
  gc()
  
##Trigram Processing ====
  blog_D3 <- readRDS(paste("./", DDir,"/blog_D3.rds",sep=""))
  news_D3 <- readRDS(paste("./", DDir,"/news_D3.rds",sep=""))
  twitter_D3 <- readRDS(paste("./", DDir,"/twitter_D3.rds",sep=""))
  
  setkey(blog_D3, "Word")
  setkey(news_D3, "Word")
  setkey(twitter_D3, "Word")
  
  D3 <- merge(blog_D3, news_D3, all=TRUE)
  D3 <- merge(D3, twitter_D3, all=TRUE)
  D3[, Freq.n := rowSums(.SD, na.rm = TRUE), .SDcols = c("Freq", "Freq.x", "Freq.y")]
  D3[,c("Freq", "Freq.x", "Freq.y"):= NULL]
  colnames(D3) <- c("Word","Freq")
  saveRDS(D3, paste("./", DDir,"/D3.rds",sep=""))
  rm(blog_D3, news_D3, twitter_D3, D3)
  gc()
  
#Additional Processing ----
  

D1 <- readRDS(paste("./", DDir,"/D1.rds",sep=""))
TmpD <- D1[, lapply(.SD, function(x) grepl("([a-z])\\1\\1", x, ignore.case=TRUE))]
D1A <- D1[TmpD$Word == 0]
setkey(D1A, "Word")
saveRDS(D1A, paste("./", DDir,"/D1A.rds",sep=""))

-----------------------
Tmp <- grep("[a-z]", D1, value = TRUE)

  grep("[a-zA-Z]{3,}", a, value = TRUE)
!is.character(grep("[a-zA-Z]{3,}", a, value = TRUE))
a <- c("a", "bb", "ccc", "dddd", "eeee", "fsfsdffffff", "wesardtgggggg")
grep("([a-z])\\1\\1", a, ignore.case = TRUE, value = TRUE)


# Combining ----

D1A <- subset(D1, !is.character(grep("[a-zA-Z]{3,}", a, value = TRUE)))

#===========================================


#Unigram Processing Further --------------
D1 <- readRDS(paste("./", DDir,"/D1.rds",sep=""))
D1 <- D1[,"RNum" := .I]
saveRDS(D1, paste("./", DDir,"/D1.rds",sep=""))


D1S <- data.table(Word = D1$Word, Stem = wordStem(D1$Word, language = "english"), Freq = D1$Freq)
setkey(D1S, "Stem")

D1U <- D1S[,sum(Freq), by=Stem]

D1U <- D1U[,"RNum" := .I]
setnames(D1U,  c("Word", "Freq", "RNum"))
saveRDS(D1U, paste("./", DDir,"/D1Unq.rds",sep=""))

D1U <- D1U[,c("Freq") := NULL]

D1S <- D1S[D1U]
D1S <- D1S[order(Stem, -Freq)]
setkey(D1S, "Stem")
saveRDS(D1S, paste("./", DDir,"/D1Full.rds",sep=""))
D1S <- D1S[,c("Stem") := NULL]
setkey(D1S, "Word")
D1S <- D1S[1:522390]

saveRDS(D1S, paste("./", DDir,"/D1.rds",sep=""))
####================================####




