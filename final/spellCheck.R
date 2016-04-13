spellCheck = function(keyW,dicW,dicC){
  keyC = lapply(keyW, toCharacter)
  keyPre = vector()
  for(i in 1:length(keyC)){
    score = lapply(dicC,vIn,keyC[[i]])
    score = sapply(score,sum)
    if(max(score)==0){
      return()
    }
    thresh = sort(score, decreasing=TRUE)[5]
    ind = (score >= max(0, thresh))
    dicA = dicW[ind]
    score=lapply(dicA,align,string2=keyW)
    #print(score)
    pre = dicA[which.max(score)]
    keyPre = c(keyPre, pre)
  }
  key = paste(keyPre, sep=" ")
  return(key)
}