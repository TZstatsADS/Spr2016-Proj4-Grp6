spellCheck = function(keyW,dicW,dicC){
  keyC = lapply(keyW, strsplit, split="")
  keyPre = vector()
  for(i in 1:length(keyC)){
    score = lapply(dicC,vIn,keyC)
    score = sapply(score,sum)
    print(max(score))
    if(max(score)==0){
      return()
    }
    thresh = sort(score, decreasing=TRUE)[10]
    ind = (score >= max(0, thresh))
    dicA = dicC[ind]
    print(dicA)
    align=lapply(dicA,pairwiseAlignment,subject=keyC,type="local",gapOpening=0,gapExtension=0)
    score=lapply(align,score)
    print(score)
    pre = which.max(score)
    pre = dicW[ind[pre]]
    keyPre = c(keyPre, pre)
  }
  key = paste(keyPre, sep=" ")
  return(key)
}