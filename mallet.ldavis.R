# Author: Thomas Keller. This is the work used for the "Open Data Visualizations and Analytics as Tools for Policy-Making" published by Government Information Quarterly

library(dplyr)
library(readr)
library(stringr)
library(text2vec)
library(tm)
library(qdapRegex)
library(SnowballC)
library(mallet)
#> setwd("SET YOUR DIRECTORY HERE: C:/Users/lonihagen/TopicModelingTutorial/LDAvis")
df<-read_csv('over150.2.csv')
####################################1. data cleaning and munging######################################################################
#large character vector
texts=c()
for(sid in 1:nrow(df)){
  texts=c(texts,paste(c(df$title[sid],df$body[sid])," ",collapse=" "))
}
#transform to a dataframe, title is used as IDs for later purpose
jtext=data.frame(sid=df$title,text=texts)
#lower case
jtext$text=tolower(jtext$text)

#delete punctuation
jtext$text=str_replace_all(jtext$text, "[[:punct:]]", " ")

#delete any alphabetic or numeric character and other letters produced online but are not useful for our analysis such as "amp" "https"..
jtext$text=str_replace_all(jtext$text, "https://t.co/[A-Za-z\\d]+|https://[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp|@[A-Za-z\\d]+|&amp",'')

#drop stopwords (added our own stopwords to a stopwords dictionary)
stopwords_regex = paste(stopwords('en'), "amp", collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')


#drop all stopwords contained in the jtext data frame
jtext$text = str_replace_all(jtext$text, stopwords_regex, '')

#drop words length 1 or 2
jtext$text=sapply(jtext$text, function(x) rm_nchar_words(x, "1,2"))
ntext=1:nrow(jtext) #jtext$sid

#train using mallet
#mallet.import: This function takes an array of document IDs and text files (as character strings) and converts them into a Mallet instance list.
mall.instance <- mallet.import(
  as.character(ntext),
  jtext$text,
  "mallet.en.txt",
  FALSE,
  token.regexp="[\\p{L}]+")

#set number of topic
topic.model=MalletLDA(num.topics=20)
topic.model$loadDocuments(mall.instance)
vocab=topic.model$getVocabulary()
word.freqs=mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(40,80)
topic.model$train(400)

topic.words.m<-mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)

dim(topic.words.m)

vocabulary <- topic.model$getVocabulary() 
colnames(topic.words.m) <- vocabulary 

doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T,
                                  normalized=T)


doc.topics.df <- as.data.frame(doc.topics.m)
doc.topics.df <- cbind(ntext, doc.topics.df)

doc.topic.means.df <- aggregate(doc.topics.df[,2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),mean)



library(LDAvis)
phi <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)
phi.count <- t(mallet.topic.words(topic.model, smoothed = TRUE, normalized = FALSE))

topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
topic.counts <- rowSums(topic.words.m)

topic.proportions <- topic.counts/sum(topic.counts)

vocab <- topic.model$getVocabulary() 

doc.tokens <- data.frame(id=c(1:nrow(doc.topics.m)), tokens=0)
for(i in vocab){
  # Find word if word in text
  matched <- grepl(i, jtext$text)
  doc.tokens[matched,2] =doc.tokens[matched,2] +  1
}

# this process takes long time. just use a small sample first. Commenting this for now because I've ran this to create a json file.
# ldajson=createJSON(phi = phi, 
#                    theta = doc.topics.m, 
#                    doc.length = doc.tokens$tokens, 
#                    vocab = vocab, 
#                    term.frequency = apply(phi.count, 1, sum))
# 
# write(ldajson,'mallet_petitions_1119.json')
library(readr)# just run this and the following scripts next time
serVis(ldajson)

save.image("lda.vis.RData")
