text_to_sentiment <- function(text){
  require(stringr)
  
  # removing @, # and internet links, RT, \n, digits
  text <- gsub("@|#|(?=https:).+?(?=\\s|$)|^RT|\n|\\d", "", text, perl = TRUE)
  # converte to lowercase
  text <- tolower(text)
  # remove punctuation
  text <- gsub("[[:punct:]]", "", text)
  # create tokens
  tokens <- str_split(text, " ")
  # remove empty tokens and stopwords. Afterwards remove all the empty tweets with no left information.
  to_rm <- vector()
  for (i in 1:length(tokens)){
    tokens[[i]] <- tokens[[i]][!(tokens[[i]] == "")]
    tokens[[i]] <- tokens[[i]][!(tokens[[i]] %in% stopwords("german"))]
    if(is.null(tokens[[i]])){
      to_rm <- c(to_rm, i)
    } 
  }
  if(length(to_rm) > 0){
    tokens <- tokens[[-to_rm]]
  }
  ## load sentiment dictionary
  if(!("SentiWS_v2.0_Positive.txt" %in% list.files("data"))){
    download.file("https://downloads.wortschatz-leipzig.de/etc/SentiWS/SentiWS_v2.0.zip", destfile = "data/SentiWS_v2.0.zip")
    setwd("data")
    unzip("SentiWS_v2.0.zip", overwrite = FALSE)
    setwd("..")
  }
  neg_dic <- read.table("data/SentiWS_v2.0_Negative.txt", header = FALSE, sep = "\t") 
  pos_dic <- read.table("data/SentiWS_v2.0_Positive.txt", header = FALSE, sep ="\t")
  dic <- rbind.data.frame(neg_dic, pos_dic)
  # now extract exact words with the score
  # remember that some cells in V3 may be empty than you have to extract the word from V1
  dic_data <- data.frame(words = character(),
                         score = numeric())
  for(row in 1:nrow(neg_dic)){
    # 1. move word stem to V3
    word_v1 <- str_extract(dic[row, "V1"], ".+?(?=\\W)")
    dic[row,"V3"] <- str_c(word_v1, neg_dic[row,"V3"], sep = ",")
    words <- tolower(unlist(str_split(dic[row,"V3"], ",")))
    score <- dic[row,"V2"]
    temp <- cbind.data.frame(words = words, score = score)
    dic_data <- rbind.data.frame(dic_data, temp)
  }
  dic_data <- dic_data[-which(dic_data$words == ""),]
  
  # assign sentiment score to each word in tokens
  sentiment_score <- 0
  tokens_vec <- unlist(tokens)
  for (token in tokens_vec){
    # sum the sentiment score for all the tokens in my list stuff gedöns.
    if(!(token %in% dic_data[, "words"])){
      score <- 0
    } else {
      score <- dic_data[which(dic_data[,"words"] == token),"score"]
    }
    sentiment_score <- sum(sentiment_score, score) 
  }
  return(sentiment_score)
}