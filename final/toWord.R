toWord = function(string){
  cVector = string %>%
    tolower() %>%
    removePunctuation() %>%
    strsplit(split=" ") %>%
    #gsub(pattern="'",replacement="") %>%
    unlist()
  return(cVector)
}