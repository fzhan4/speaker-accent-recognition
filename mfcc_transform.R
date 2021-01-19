library(tidyverse)
library(tuneR)

######### setting #########
nc = 13 # numcep
###########################

setwd("~/STAT479/archive")
data = read.csv("speakers_all.csv")

# initialize empty data frame
df.mfcc = data.frame(language=character(),
                     X1.mean=double(), X2.mean=double(), X3.mean=double(),
                     X4.mean=double(), X5.mean=double(), X6.mean=double(),
                     X7.mean=double(), X8.mean=double(), X9.mean=double(),
                     X10.mean=double(), X11.mean=double(), X12.mean=double(),
                     X13.mean=double(),
                     X1.sd=double(), X2.sd=double(), X3.sd=double(),
                     X4.sd=double(), X5.sd=double(), X6.sd=double(),
                     X7.sd=double(), X8.sd=double(), X9.sd=double(),
                     X10.sd=double(), X11.sd=double(), X12.sd=double(), 
                     X13.sd=double())

setwd("~/STAT479/archive/recordings")
file_list = list.files(path="~/STAT479/archive/recordings")

for (i in 1:length(file_list)){
  # read .wav file and use melfcc() to transform data
  w = readWave(file_list[i])
  m = melfcc(w, numcep = nc)
  
  ##
  dim(m) 
  ##
  
  # compute mean and sd for each MFCC
  mean.vec = colMeans(m)
  sd.vec = apply(m, 2, sd)
  
  
  # format language name, MFCC mean and sd, add to data frame
  # lang.name = gsub('[0-9]+.wav', '', file_list[i]) #FIXME
  file = gsub('.wav', '', file_list[i])
  lang.name = data$country[data$filename == file]
  
  name.mat = matrix(lang.name)
  colnames(name.mat) = "language"
  
  val.mat = matrix(c(mean.vec, sd.vec), nrow=1)
  colnames(val.mat) = colnames(df.mfcc)[2:dim(df.mfcc)[2]]
  
  row = merge(name.mat, val.mat)
  df.mfcc = rbind(df.mfcc, row) 
}

##
dim(df.mfcc)
##

df.mfcc.no = na.omit(df.mfcc)

setwd("~/STAT479/archive")
write.csv(df.mfcc.no, "mfcc_13.csv", row.names = F)