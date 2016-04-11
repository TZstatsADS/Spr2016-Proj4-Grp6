align = function(string1,string2){
  Align = pairwiseAlignment(string1, string2,gapOpening=0,gapExtension=0,type="local",scoreOnly = FALSE)
  return(score(Align))
}