setwd("/Users/bim/Documents/Personal/Coursera/Johns Hopkins Data Science 2014/Practical Machine Learning WI14/Project")
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)

# Only use columns with good data (many blanks and NAs in others)
keep <- c(8:11,37:49,60:68,84:86,102,113:124,140,151:160)
trainImport <- read.csv("pml-training.csv")[keep]
testImport <- read.csv("pml-testing.csv")[keep]
