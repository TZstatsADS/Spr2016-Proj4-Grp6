# Set up 
source("../lib/toWord.R")
source("../lib/vIn.R")
# Plug in easter eggs
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
## Public func
find = function(key){
  findP(key,moviesW,movies)
}
## Private func
findP = function(key,moviesW,movies){
  keyW = toWord(key)
  score = lapply(moviesW,vIn,keyW)
  score = sapply(score,sum)
  if(max(score)==0){
    return()
  }
  thresh = sort(score, decreasing=TRUE)[5]
  ind = score >= max(0, thresh)
  return(movies[ind])
}