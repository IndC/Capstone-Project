#Setup--------------------------
options(java.parameters = "- Xmx10G")
setwd("~/R/Coursera/Capstone")
DDir <- "data_full"

set.seed(12345)
library(data.table)
library(tm)
library(SnowballC)
library(RWeka)

#FUnctions=============
cleantxt <- function(x){
  x <- tolower(gsub("[^[:alpha:] ]","",x))
  x <- gsub("  ", " ", x, fixed = TRUE)
  x <- gsub("^\\s+|\\s+$", "", x)
}

TxtoRn <- function(str, nCr = 4){
  str1 <- cleantxt(str)
  str1 <- unlist(strsplit(str1, split = " "))
  str1 <- rev(str1)
  str1 <- str1[1:nCr]
  str1 <- str1[!is.na(str1)]
  str1 <- wordStem(str1, language = "english")
  str2 <- ""
  for (i in 1:length(str1)){
    str2[i] <- Grm1[Stem == str1[i]]$RNum[1]
    #if (is.na(str2[i])){
    #  Grm2[W1Row == str2[i-1]]$W2Row
    #}
    if (is.na(str2[i])){
      str2[i] <- setorder(Grm1[Stem %like% paste("^",substr(str1[i],1,nchar(str1[i])-1),sep="")], -Freq)[1]$RNum
    }
  }
  str2 <- as.numeric(str2[!is.na(str2)])
}



#Model---------------

Grm1 <- readRDS(paste("./", DDir,"/Grm1.rds",sep=""))
Grm2 <- readRDS(paste("./", DDir,"/Grm2.rds",sep=""))
Grm3 <- readRDS(paste("./", DDir,"/Grm3.rds",sep=""))

Srn <- TxtoRn("and helps reduce  your stress")
Srn <- TxtoRn("Thanks so much for pointing me in   ")
Srn <- TxtoRn("WE are going   ")

Stg <- "nice to meet you on Tuesday"
Txtx <- VWord (Stg,3)
Txtx
Txtx <- VWord (Stg,2)
Txtx



VWord <- function(Input, nGram = 3){
  Srn <- TxtoRn(Input)
  if (length(Srn) <2) nGram <- 2
    if (nGram > 2) {
      x <- setorder(Grm3[PreWord == Srn[2]*10^8 + Srn[1]], -Prob)
    } else {
      x <- setorder(Grm2[PreWord == Srn[1]], -Prob)
    }
  setkey(x, "WordRow")
  x <- Grm1[x]
  setkey(x, "Word")
  x <- x[ , list(Prob = sum(Prob), Freq = sum(Freq)), by=Word]
  #x <- x[,Srt:=Freq*Prob]
  setorder(x, -Prob)
  x[1:5]$Word
}







#---------
z <- setorder(Grm1[x], -Prob, -Freq)
setkey(z, "Word")
z <- unique(z)
z

c <- 1




# http://nlp.stanford.edu/~wcmac/papers/20050421-smoothing-tutorial.pdf
















VWord <- function (str, AddW = c(""), LastW=0){
  Opt <- data.table()
  str <- unlist(strsplit(str, split = " "))
  str <- wordStem(str, language = "english")
  AddW <- unlist(AddW)
  SL <- list(Lw = length(str) - LastW, La = length(AddW), Fg = 0, mP = 1) 
  
        #Lw: No of words in string
        #La: No of additional words to check
        #Fg: 
        #mP: Probability Multiplication factor
  
  if (AddW[1] !=""){
    SL$Fg <- 1
    SL$mP <- 0.25
  } 
  
  for (i in 1:SL$La){
    if(SL$Fg == 1) str[SL$Lw] <- AddW[i]
    x <- setorder(Grm3[W1Row == Grm1[Word == str[SL$Lw-1]]$RNum & W2Row == Grm1[Word == str[SL$Lw]]$RNum], -Prob)
    x <- x[complete.cases(x),]
    if (nrow(x) > 0) Opt <- rbind(Opt, data.table(Grm1[x$W3Row], "Prob" = x$Prob * SL$mP, "From" = paste("T",SL$Lw,SL$Lp,SL$Fg, sep = " "))[1:5])
    x <- setorder(Grm2[W1Row==Grm1[Word==str[SL$Lw]]$RNum], -Prob)
    x <- x[complete.cases(x),]
    if (nrow(x) > 0) Opt <- rbind(Opt, data.table(Grm1[x$W2Row], "Prob" = x$Prob *0.3 * SL$mP, "From" = paste("B",SL$Lw,SL$Lp,SL$Fg, sep = " "))[1:5])
  }
  
  NewList <- setorder(Opt[, list(Prob=sum(Prob)), by = list(Word = Word)], -Prob)
  rm(Opt)
  NewList <- NewList[complete.cases(NewList),]
}



char <- "see arctic monkeys this"

test <- cleantxt(char)

a <- VWord(test)
a$Word

b <- VWord(test,unlist(list(VWord(test," ")$Word)))
b$Word


c <- VWord(test," ")
c$Word




b <- adist(itext,a)


str <- "we will never find another"
AddWd <- NULL
AddWd <-" "
AddWd <- c("a", "an", "he")
length(AddWd)



  
  AddWd <- unlist(list(Opt$Word))
  AddWd <- unlist(list(NewList$Word[1:5]))
  Op <- Opt
  NL <- NewList
  NL1 <- NewList
  
  
  
  
    
  AddWd
  Lw = length(str)
  str[Lw]
  x <- setorder(D3R[W1Row == D1[Word == str[Lw-1]]$RNum & W2Row == D1[Word == str[Lw]]$RNum], -Prob)
  Opt <- data.table(D1[x$W3Row], "Prob" = x$Prob)[1:5]
  if(nrow(x) ==0){
    x <- setorder(D2R[W1Row==D1[Word==str[Lw]]$RNum], -Prob)
    Opt2 <- data.table(D1[x$W2Row], "Prob" = x$Prob)[1:5]
  }
  Opt
  

  
  
VWord <- c("in","exchange")
x <- setorder(D3R[W1Row == D1[Word == VWord[1]]$RNum & W2Row == D1[Word == VWord[2]]$RNum], -Prob)
D1[x$W3Row[1:5]]

