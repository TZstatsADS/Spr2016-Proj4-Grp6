#read all the movie data
data<-read.csv("./project4-team6/data/datawithnameandgenre.csv")

get_similarity <- function(movie1){
  #the first function gives the users who rate both movies
  common_reviewers_by_id <- function(movie1,movie2) {
    reviews1 <- subset(data, V1.1==movie1)
    reviews2 <- subset(data, V1.1==movie2)
    reviewers_sameset <- intersect(reviews1[,'review_userid'],
                                   reviews2[,'review_userid'])
    if (length(reviewers_sameset)<2) {
      NA
    }
    else{
      reviewers_sameset
    }
  }
  #the second function gives the ratings of these common users
  get_review_metrics <- function(movie,id){
    metrics<-subset(data,V1.1== movie & review_userid %in% id)
    metrics<-metrics[order(unique(metrics$review_userid)),]
    metrics
  }
  
  id <- common_reviewers_by_id(movie1, insert)
  if (any(is.na(id))) {
    return (NA)
  }else{
    metrics1 <- get_review_metrics(movie1,id)
    metrics2 <- get_review_metrics(insert,id)
    #the helpfulness is multiplied by the corresponding rating score:
    product <- function(metrics){
      names <- names(metrics)
      temp1 <- sapply(as.matrix(metrics[names[1]]), function(x) eval(parse(text=x)))
      temp2 <- sapply(as.matrix(metrics[names[2]]), function(x) eval(parse(text=x)))
      temp1*temp2
    }
    similarity <- product(metrics1)%*%product(metrics2)/(product(metrics1)%*%product(metrics1)*product(metrics2)%*%product(metrics2))^0.5
    if (similarity == 1){
      similarity = NA
    }
    similarity
  }
}


#how to use the function:
#inert a movie name, and then run the following function, 
#the output is the most similar movie suggested by the algorithm
insert<-"The Last Samurai"
index<-which(data[,"V1.1"]==insert)[1]
data2<-data[which(data[,"V2"]==data[index,"V2"]),]
system.time(similarity<-sapply(as.matrix(unique(data2[,"V1.1"])),get_similarity))
similarity <- na.omit(similarity)
names(similarity[which.max(similarity)])