require(tm)
sourcename = "news"; lines = 500
text<-readLines(paste0("F:/btp/text_prediction-shinyApp-master/project","/final/en_US/en_US.",sourcename,".txt"),n = lines)
Clean_String <- function(string){
  # Lowercase
  #temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(string,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}
cleanedCorpus <- Corpus(VectorSource(Clean_String(Corpus(VectorSource(text)))))
require(tau)
tau_ngrams <- function(x, ngrams) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=ngrams)))))
clean.tdm <- DocumentTermMatrix(cleanedCorpus,control = list(tokenize = function(x) tau_ngrams(x, ngrams),wordLengths = c(1, Inf)))
