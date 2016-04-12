## Set up
setwd("~/Data Science/project4-team6/data")
library(dplyr)
library(stringr)
library(tm)

## Read in data, source func
load("matData.RData")
movies = levels(matData$name)
moviesW = sapply(movies,toWord)
source("../lib/find.R")
source("../lib/toWord.R")
source("../lib/vIn.R")

## Try out to find a movie
find("great",moviesW,movies)
system.time(find("happy",moviesW,movies))
system.time(find("mind beautiful",moviesW,movies))
system.time(find("world places living", moviesW,movies))