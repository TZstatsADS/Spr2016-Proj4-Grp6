find = function(key,moviesW,movies){
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