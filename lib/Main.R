## Set up
setwd("~/Data Science/project4-team6/data")
library(dplyr)
library(stringr)
library(tm)
library(seqinr)
library(Biostrings)


## Read in data, source func
load("matData.RData")
source("../lib/find.R")
source("../lib/toWord.R")
source("../lib/vIn.R")

## Try out to find a movie
find("great")
system.time(find("happy"))
system.time(find("mind beautiful"))
system.time(find("world places living"))

## Try out to find a movie with typo
findWithTypo("", moviesW, movies, dic)