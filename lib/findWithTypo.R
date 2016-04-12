## Build dictionary
dic = do.call(c,moviesW) %>%
  as.factor() %>%
  levels()
dic = dic[-(1:140)]
dicW = dic
dicC = sapply(dic, toCharacter)
## Public func
findWithTypo = function(key){
  findWithTypoP(key,moviesW,movies,dicW,dicC)
}
## Private func
findWithTypoP = function(key,moviesW,movies,dicW,dicC){
  keyW = toWord(key)
  keyT = spellCheck(keyW,dicW,dicC)
  print(paste("Do you mean ",keyT,"?",sep=""))
  return(find(keyT))
}





