#Setup--------------------------
options(java.parameters = "- Xmx10G")
setwd("~/R/Coursera/Capstone")
DDir <- "data_full"

set.seed(12345)
library(data.table)
library(tm)
library(SnowballC)
library(RWeka)


#Bigram Processing Further --------------
D1 <- readRDS(paste("./", DDir,"/D1.rds",sep=""))
D2 <- readRDS(paste("./", DDir,"/D2Org.rds",sep=""))

setkey(D2, "Word")
D2[, c("W1", "W2") := tstrsplit(Word, " ", fixed=TRUE)]

setkey(D2, "W1")
D2 <- D2[D1]

setkey(D2, "W2")
D2 <- D2[D1]

D2 <- D2[complete.cases(D2),]

saveRDS(D2, paste("./", DDir,"/D2Proc.rds",sep=""))

#-------------
D2 <- readRDS(paste("./", DDir,"/D2Proc.rds",sep=""))

setkey(D2, "RNum", "i.RNum")
D2 <- D2[, "CRow" := as.character(RNum*10^8 + i.RNum)]

setkey(D2, "CRow")
D2U <- D2[ , sum(Freq), by=CRow]

setkey(D2U, "CRow")
D2F <- unique(D2)
D2F <- D2U[D2F]

D2F <- D2F[complete.cases(D2F),]
D2F <- D2F[, "Freq" :=NULL]

setnames(D2F, c("CRow", "Freq", "Word", "W1", "W2", "W1Freq", "W1Row", "W2Freq", "W2Row"))

D2F <- D2F[,"Prob" := (D2F$Freq +1) / (D2F$W1Freq+1)]
D2F <- D2F[,"RNum" := .I]
saveRDS(D2F, paste("./", DDir,"/D2Full.rds",sep=""))

D2R <- D2F[, c("CRow", "Freq", "Word", "W1", "W2", "W1Freq", "W2Freq" ):=NULL]
saveRDS(D2R, paste("./", DDir,"/D2.rds",sep=""))



#Trigram Processing Further --------------
D1 <- readRDS(paste("./", DDir,"/D1.rds",sep=""))
D2 <- readRDS(paste("./", DDir,"/D2.rds",sep=""))
D3 <- readRDS(paste("./", DDir,"/D3Org.rds",sep=""))

setkey(D3, "Word")
D3[, c("W1", "W2", "W3") := tstrsplit(Word, " ", fixed=TRUE)]

setkey(D3, "W1")
D3 <- D3[D1]

setkey(D3, "W2")
D3 <- D3[D1]

setkey(D3, "W3")
D3 <- D3[D1]

D3 <- D3[complete.cases(D3),]

saveRDS(D3, paste("./", DDir,"/D3Proc.rds",sep=""))

#------------------------4/24
D3 <- readRDS(paste("./", DDir,"/D3Proc.rds",sep=""))

D3 <- D3[,"Word" :=NULL]

D3 <- D3[,"CRow" := paste(RNum, i.RNum, i.RNum.1, sep = " ")]

setkey(D3, "CRow")
D3U <- D3[ , sum(Freq), by=CRow]

setkey(D3U, "CRow")
D3F <- unique(D3)
D3F <- D3U[D3F]

saveRDS(D3F, paste("./", DDir,"/D3Full.rds",sep=""))

#------------------------
D2 <- readRDS(paste("./", DDir,"/D2Full.rds",sep=""))
D3R <- readRDS(paste("./", DDir,"/D3Full.rds",sep=""))

#D2: "CRow", "Freq", "Word", "W1", "W2", "W1Freq", "W1Row", "W2Freq", "W2Row", "Prob", "RNum"
#D3R:  "CRow", "V1", "Freq", "W1", "W2", "W3", "i.Freq", "RNum", "i.Freq.1", "i.RNum", "i.Freq.2", "i.RNum.1"

D2[,c("CRow", "W1", "W2", "W1Freq", "W1Row",  "W2Freq", "W2Row",  "Prob"):=NULL]
setkey(D2, "Word")

D3R[, c("CRow", "Freq"):=NULL]
D3R[, "W1W2" := paste(W1, W2, sep = " ")]
setkey(D3R, "W1W2")

D3R <- D3R[D2]
D3R <- D3R[complete.cases(D3R),]

#D3R:  "V1", "W1", "W2", "W3", "i.Freq", "RNum", "i.Freq.1", "i.RNum", "i.Freq.2", "i.RNum.1", "W1W2", "Freq", "i.RNum.2"

saveRDS(D3R, paste("./", DDir,"/D3Proc2.rds",sep=""))

setorder(D3R, W1W2, -i.Freq.2)
ZA <- D3R[ ,head(.SD, 5), by = W1W2]
setkey(ZA, "W1W2")

saveRDS(ZA, paste("./", DDir,"/D3ZA.rds",sep=""))
#ZA: "W1W2","V1","W1","W2","W3","i.Freq","RNum","i.Freq.1","i.RNum","i.Freq.2","i.RNum.1","Freq","i.RNum.2"
# "W1W2", "Freq", "W1", "W2", "W3" , "W1Freq", "W1Row", "W2Freq", "W2Row", "W3Freq", "W3Row", "W1W2Freq", "W1W2Row"

#----------------------------------

D3ZA <- readRDS(paste("./", DDir,"/D3ZA.rds",sep=""))
D3ZA <- D3ZA[,"Prob" := NULL]
setnames(D3ZA, c("W1W2", "Freq", "W1", "W2", "W3" , "W1Freq", "W1Row", "W2Freq", "W2Row", "W3Freq", "W3Row", "W1W2Freq", "W1W2Row"))
D3ZA <- D3ZA[,"Prob" := (D3ZA$Freq+1) / (D3ZA$W1W2Freq+1)]
saveRDS(D3ZA, paste("./", DDir,"/D3ZA.rds",sep=""))

D3ZA <- D3ZA[,c("W1W2", "Freq", "W1", "W2", "W3" , "W1Freq", "W2Freq", "W3Freq", "W1W2Freq"):=NULL]
D3ZA <- D3ZA[,c("W1W2Row"):=NULL]
D3ZA <- D3ZA[,"W1W2" := W1Row*10^8 + W2Row]
D3ZA <- D3ZA[,c("W1Row", "W2Row"):=NULL]
setkey(D3ZA, "W1W2")

    setcolorder(D3ZA, c("W1W2", "W3Row", "Prob"))
    setnames(D3ZA, c("PreWord", "WordRow", "Prob"))
    setkey(D3ZA, "PreWord")
saveRDS(D3ZA, paste("./", DDir,"/Grm3.rds",sep=""))

# Grm3 <- readRDS(paste("./", DDir,"/Grm3.rds",sep=""))
# Grm3 <- Grm3[,"W1W2" := W1Row*10^8 + W2Row]
# Grm3 <- Grm3[,c("W1Row", "W2Row"):=NULL]
# setkey(Grm3, "W1W2")
# saveRDS(Grm3, paste("./", DDir,"/Grm3.rds",sep=""))

D2 <- readRDS(paste("./", DDir,"/D2.rds",sep=""))
D2 <- D2[,"RNum":=NULL]
setkey(D2, "W1Row")
saveRDS(D2, paste("./", DDir,"/Grm2.rds",sep=""))


D1 <- readRDS(paste("./", DDir,"/D1Full.rds",sep=""))
setkey(D1, "Stem")
saveRDS(D1, paste("./", DDir,"/Grm1.rds",sep=""))

  setkey(Grm1, "RNum")
  saveRDS(Grm1, paste("./", DDir,"/Grm1.rds",sep=""))
  
  #------
  setnames(Grm2, c("PreWord", "WordRow", "Prob"))
  setkey(Grm2, "PreWord")
  saveRDS(Grm2, paste("./", DDir,"/Grm2.rds",sep=""))

  # setcolorder(Grm3, c("W1W2", "W3Row", "Prob"))
  # setnames(Grm3, c("PreWord", "WordRow", "Prob"))
  # setkey(Grm3, "PreWord")
  # saveRDS(Grm3, paste("./", DDir,"/Grm3.rds",sep=""))
  #---------------
  
  Grm1 <- readRDS(paste("./", DDir,"/Grm1.rds",sep=""))
  D1U <- readRDS(paste("./", DDir,"/D1Unq.rds",sep=""))
  
  setorder(Grm1, RNum, -Freq)
  G1 <- Grm1[ ,head(.SD, 1), by = RNum]
  setkey(G1, "RNum")
  saveRDS(G1, paste("./", DDir,"/Grm1.rds",sep=""))
  