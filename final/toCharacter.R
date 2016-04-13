toCharacter = function(word){
  character = word %>%
    strsplit(split="") %>%
    unlist()
  return(character)
}