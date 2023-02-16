#1: Get country dataset

#import africa dataset 
setwd("C:/Users/Documents/CTech/Project/AfricaRep")  # set your working dir
nigdata = foreign::read.spss(paste("C:/Users
/Documents/CTech/Project/dhs/nigeria/NGKR6AFL.SAV", sep = ""), 
                             to.data.frame = T)  # read data
