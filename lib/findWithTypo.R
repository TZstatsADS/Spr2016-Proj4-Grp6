## Set up
source("../lib/align.R")
source("../lib/toCharacter.R")
source("../lib/spellCheck.R")
## Build dictionary
dic = do.call(c,moviesW) %>%
  as.factor() %>%
  levels()
dic = dic[-(1:140)]
dicW = dic
dicC = lapply(dicW, toCharacter)
## Public func
findWithTypo = function(key){
  findWithTypoP(key,moviesW,movies,dicW,dicC)
}
## Private func
findWithTypoP = function(key,moviesW,movies,dicW,dicC){
  keyW = toWord(key)
  keyT = keyW
  for(i in 1:length(keyW)){
    keyT[i] = spellCheck(keyW[i],dicW,dicC)
  }
  keyT = paste(keyT, collapse = " ")
  print(paste("Do you mean '",keyT,"'?",sep=""))
  return(find(keyT))
}





