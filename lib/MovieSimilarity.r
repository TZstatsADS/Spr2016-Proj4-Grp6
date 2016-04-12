#the basic idea of this method is to find the common reviewers of two movies, 
#and then vactorize the scores they give, using helpness as the weight of every score
#finally get correlation of these two movies by calculating the cosine of the angle between these two vctors

#the followings are three functions
#use movie2 <- "Minority Report"
# movie1<-"The Last Samurai"
# to test the function: get_similarity(movie1,movie2) 
#
#          [,1]
#[1,] 0.9479966

data<-read.csv("datawithnameandgenre.csv")
#Currently you must enter the name of the moviw with a quotation mark
#otherwise you will get a warning message: object undefined
#insert two movies, find the common reviewers:
common_reviewers_by_id <- function(movie1,movie2) {
  reviews1 <- subset(data, V1.1==movie1)
  reviews2 <- subset(data, V1.1==movie2)
  reviewers_sameset <- intersect(reviews1[,'review_userid'],
                                 reviews2[,'review_userid'])
  if (length(reviewers_sameset)==0) {
    NA
    }
  else{
    reviewers_sameset
  }
  }

#get review results given the reviewe ID:
get_review_metrics <- function(movie,id){
  metrics<-subset(data,V1.1== movie & review_userid %in% id)
  #reorder the reviewers
  metrics<-metrics[order(metrics$review_userid),]
  metrics<-metrics[,c("review_helpfulness","review_score")]
  metrics
}

#finally we calculate the similarity of two given movies:
get_similarity <- function(movie1,movie2){
  #get the common reviewer of the two movies:
  id <- common_reviewers_by_id(movie1, movie2)
  #in case the two movies have no common reviewers???
  for (j in 1:length(id)) {
   if (is.na(id[j])) {
    return (NA)
   }
  }
  #get the review scores of the two movies of these common users:
  metrics1 <- get_review_metrics(movie1,id)
  metrics2 <- get_review_metrics(movie2,id)
  #regard helpfulness as the weight of every user's score
  #muliple every score by its helpfulness:
  product <- function(metrics){
    names <- names(metrics)
    temp1 <- sapply(as.matrix(metrics[names[1]]), function(x) eval(parse(text=x)))
    temp2 <- sapply(as.matrix(metrics[names[2]]), function(x) eval(parse(text=x)))
    temp1*temp2
  }
  #similarity is defined as the cosine of angle of two vectors that we get,
  #The greater the cosine is, the greater the correlation is.
  #the following equation if the cross product of the two vectors devided by
  #the product of the vector norms
  similarity <- product(metrics1)%*%product(metrics2)/(product(metrics1)%*%product(metrics1)*product(metrics2)%*%product(metrics2))^0.5
  similarity
  }
  
  
