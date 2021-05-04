library(BTM)
library(textstem)
library(stopwords)
library(LDAvis)

# Preprocessing -----------------------------------------------------------

# preproccessing included:
  # lowercasing all characters
  # removing all punctuation (including # and @)
  # lemmatizing 
  # remove stop words

raw <- read.csv('data/trump_insult_tweets_2014_to_2021.csv')
raw <- raw[,c("tweet")]
raw <- raw[!duplicated(raw)]
raw <- tolower(raw)
raw <- gsub('[[:punct:]]+','',raw)


data_for_model <- data.frame()

for (i in 1:length(raw)){
  split_post <- strsplit(raw[i], " ")
  data_for_model <- rbind(data_for_model, cbind(i,unlist(split_post)))
  if(i %% 1000 == 0){
    print(i)
  }
}

data_for_model$lemma <- lemmatize_strings(data_for_model$token)
data_for_model <- data_for_model[,-2]

stopwords <- stopwords()

data_for_model <- subset(data_for_model, data_for_model$lemma %in% stopwords == FALSE)
data_for_model <- subset(data_for_model, data_for_model$lemma != "")


# write.csv(data_for_model, "data/data_for_btm_model.csv", row.names = FALSE)




# Build BTM model ---------------------------------------------------------

# reading in the saved preprocessed df
# data_for_model <- read.csv("data/data_for_btm_model.csv", stringsAsFactors = FALSE)
View(data_for_model[c(1:100), ])



set.seed(44)
topics <- 6
model <- BTM(data_for_model, 
             k = topics,
             alpha = 50/topics,
             beta = 0.01, 
             iter = 100, 
             window = 30, 
             background = FALSE,  
             detailed = TRUE) # needed for LDA





# LDAvis ------------------------------------------------------------------

docsize <- table(data_for_model$id)
phi <- t(model$phi)

scores <- predict(model, data_for_model)
scores <- scores[names(docsize), ]

json <- createJSON(
  phi = phi,
  theta = scores,
  doc.length = as.vector(docsize),
  vocab = model$vocabulary$token,
  term.frequency = model$vocabulary$freq)
serVis(json)

     