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





###################################
#          NOT REQUIRED           #
###################################

#Bigram Processing Further --------------

D1 <- readRDS(paste("./", DDir,"/D1.rds",sep=""))
D2 <- readRDS(paste("./", DDir,"/D2Org.rds",sep=""))
setkey(D2, 'Word')
D2[, c("W1", "W2") := tstrsplit(Word, " ", fixed=TRUE)]

setkey(D2, 'W1')
D2 <- D2[D1]

setkey(D2, 'W2')
D2 <- D2[D1]

D2 <- D2[complete.cases(D2),]
setnames(D2, c("Word", "Freq",  "W1", "W2", "W1Fq", "W1Row", "W2Fq", "W2Row", "Prob"))

D2 <- D2[,"Prob" := D2$Freq / D2$W1Fq]
D2R <- data.table("W1Row" = D2$W1Row, "W2Row" = D2$W2Row, "Prob" = D2$Prob)
setkey(D2R, 'W1Row')

saveRDS(D2, paste("./", DDir,"/D2F.rds",sep=""))
saveRDS(D2R, paste("./", DDir,"/D2R.rds",sep=""))
rm(D1, D2, D2R)
gc()

#Trigram Processing Further --------------
load(paste("./", DDir,"/D1R.RData",sep=""))
load(paste("./", DDir,"/D2.RData",sep=""))
load(paste("./", DDir,"/D3.RData",sep=""))

#D1 <- data.table(D1, key='Word')

#D2 <- data.table(D2, key='Word')
D2 <- D2[,"RNum" := .I]
colnames(D2) <- c("W1W2","B_Freq","B_RNum")
D2 <- data.table(D2, key='W1W2')
View(D2)


D3 <- data.table(D3, key='Word')
D3[, c("W1", "W2", "W3") := tstrsplit(Word, " ", fixed=TRUE)]

  tables()

D3 <- data.table(D3, key='W1')
D3 <- D3[D1]

D3 <- data.table(D3, key='W2')
D3 <- D3[D1]

D3 <- data.table(D3, key='W3')
D3 <- D3[D1]

D3 <- D3[complete.cases(D3),]

D3[, "W1W2" := paste(W1, W2, sep = " ")]
D3 <- data.table(D3, key='W1W2')

D3 <- D3[D2]
D3 <- D3[complete.cases(D3),]

D3 <- D3[,"Prob" := D3$Freq / D3$B_Freq]
D3R <- data.table("W1Row" = D3$RNum, "W2Row" = D3$i.RNum, "W3Row" = D3$i.RNum.1, "W1W2Row" = D3$B_RNum, "Prob" = D3$Prob, key="W1W2Row")
View(D3)

save(D3, file = paste("./", DDir,"/D3F.RData",sep=""))
save(D3R, file = paste("./", DDir,"/D3R.RData",sep=""))

#======================================
    saveRDS(D3R, "mymodel.rds")
    mod2 <- readRDS("mymodel.rds")
#======================================
    
T <- D3R[1:10]
T2 <- T[, "WWW" := W1Row*1000000 + W2Row + W3Row/1000000]

(T2[3,WWW]-as.integer(T2[3,WWW]))*1000000

#================================
#D3RR <- D3R[, "WWW" := W1Row*1000000 + W2Row + W3Row/1000000]
#D3RR[,c("W1Row", "W2Row", "W3Row"):=NULL]

#D3RRR <- D3RR[,c("W1W2Row"):=NULL]


load(paste("./", DDir,"/D3R.RData",sep=""))
D3RR <- D3R[, "WordCt" := W1W2Row*10^8 + W3Row + Prob]
D3RR[,c("W1Row", "W2Row", "W3Row", "W1W2Row", "Prob"):=NULL]
setkey(D3RR, "WordCt")
saveRDS(D3RR, paste("./", DDir,"/D3RR.rds",sep=""))


load(paste("./", DDir,"/D2R.RData",sep=""))
D2RR <- D2R[, "WordCt" := W1Row*10^8 + W2Row + Prob]
D2RR[,c("W1Row", "W2Row", "Prob"):=NULL]
setkey(D2RR, "WordCt")
saveRDS(D2RR, paste("./", DDir,"/D2RR.rds",sep=""))

saveRDS(D1, paste("./", DDir,"/D1R.rds",sep=""))

#================================

D1 <- readRDS(paste("./", DDir,"/D1R.rds",sep=""))
D2RR <- readRDS(paste("./", DDir,"/D2RR.rds",sep=""))
D3RR <- readRDS(paste("./", DDir,"/D3RR.rds",sep=""))

