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
load("name.RData")

names1 = names[1:20]
names2 = names[21:40]
names3 = names[41:60]
names4 = names[61:length(names)]

names1 = paste("Kung Fu", names1, sep =" ")
names2 = paste("Superman VS", names2, sep =" ")
names3 = paste("Breakfast at Tiffany with", names3, sep=" ")
names4 = paste("Star Wars: The", names4, "Awakens", sep=" ")
names5 = c("Go Team 6!", "Superman VS Yuting", "Sign Up for Applied Data Science", "Kung Fu Tian Zheng")
eggs = c(names1,names2,names3,names4,names5)

movies = levels(matData$name)
movies = c(movies, eggs)
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