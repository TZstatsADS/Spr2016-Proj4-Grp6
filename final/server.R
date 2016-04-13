# Import all library
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(seqinr)
library(Biostrings)

#Setwd
setwd("C:/Users/ouwen/Downloads/Project4/")
source("find.R")
source("findWithTypo.R")
source("toWord.R")
source("vIn.R")
source("align.R")
source("toCharacter.R")
source("spellCheck.R")

## Read in data, source func
load("matData.RData")
movies = levels(matData$name)
moviesW = sapply(movies,toWord)


# Begin server code
shinyServer(function(input, output){
  
  movietosearchfor <- reactive({
    movietosearchfor <- as.character(input$moviename)
    })
  
  type <- reactive({
    type <- as.character(input$Type)
  })
  

  
  #find movie
  results <- reactive({
    if (type() == "Yes"){
      results <- findWithTypo(movietosearchfor())
    }
    else{
          results <- find(movietosearchfor())
    }
  })

#output information 
  
  output$summary <- renderPrint({
    summary <- results()
    return(summary)
  })
  
})