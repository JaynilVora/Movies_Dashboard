library(proxy)
library(recommenderlab)
library(reshape2)
library("tm")
library("wordcloud")
library("twitteR")
library("httr")
library("lattice")
library("openssl")
library("httpuv")

#getwd()
#setwd("/Users/jaynilvora/Documents/git_r_projects/MovieDashboard")

#install.packages(c('ROAuth','RCurl'))
movies2 <- read.csv("data/movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("data/ratings.csv", header = TRUE)
#movies4 <- read.csv("/Users/jaynilvora/Desktop/moviesfinal.csv", header = TRUE)
movies2016 <- read.csv("data/newimdb2016.csv", header = TRUE, stringsAsFactors=FALSE)
movies2015 <- read.csv("data/newimdb2015.csv", header = TRUE, stringsAsFactors=FALSE)
movies2014 <- read.csv("data/newimdb2014.csv", header = TRUE, stringsAsFactors=FALSE)
movies2013 <- read.csv("data/newimdb2013.csv", header = TRUE, stringsAsFactors=FALSE)
movies2012 <- read.csv("data/newimdb2012.csv", header = TRUE, stringsAsFactors=FALSE)
movies2011 <- read.csv("data/newimdb2011.csv", header = TRUE, stringsAsFactors=FALSE)
movies2010 <- read.csv("data/newimdb2010.csv", header = TRUE, stringsAsFactors=FALSE)


wordcloud1 <- function(input){
  require('ROAuth')
  require('RCurl')
  key <- "vZSxLplTfbaXI8gVyThJ4JqvR"
  secret <- "c4LKqTMoia49wAup3lEAEGr3z0OfaMxgyZniPioOWjSTwZPTpu"
  secrettk <- "3FYoesz1EOeB4CbwUHy5cHv38zH6B0RTsE1MpyWxlBFxQ"
  mytoken <- "915707499172556801-TAAl4VV55IELsToq3M2rMD6yt0MZm8g"
  setup_twitter_oauth(key, secret, mytoken, secrettk)
  
  udemytweets = searchTwitter(input , n=500)
  udemylist <- sapply(udemytweets, function(x) x$getText())
  udemycorpus <- Corpus(VectorSource(udemylist))
  udemycorpus <- tm_map(udemycorpus,function(x)removeWords(x,stopwords()))
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
  udemycorpus<- tm_map(udemycorpus,toSpace,"[^[:graph:]]")
  udemycorpus <- tm_map(udemycorpus, content_transformer(tolower))
  udemycorpus <- tm_map(udemycorpus,removePunctuation)
  
  ?getTransformations
  
  #udemycorpus <- tm_map(udemycorpus, PlainTextDocument)
  
  #udemycorpus = tm_map(udemycorpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
  # UTF-8-MAC b/c I'm on macOS
  return(wordcloud(udemycorpus, min.freq = 4, scale = c(5,1), random.color = F, max.word = 50, random.order = F))
  
}

histogram1 <- function(input){
  require('ROAuth')
  require('RCurl')
  key <- "vZSxLplTfbaXI8gVyThJ4JqvR"
  secret <- "c4LKqTMoia49wAup3lEAEGr3z0OfaMxgyZniPioOWjSTwZPTpu"
  secrettk <- "3FYoesz1EOeB4CbwUHy5cHv38zH6B0RTsE1MpyWxlBFxQ"
  mytoken <- "915707499172556801-TAAl4VV55IELsToq3M2rMD6yt0MZm8g"
  setup_twitter_oauth(key, secret, mytoken, secrettk)
  
  pos = readLines("data/Positive-Words.txt")
  neg = readLines("data/Negative-Words.txt")
  
  sentences = c("I am not happy sad", "you are bad", "awesome experience")
  
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      # sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  
  udemytweets = searchTwitter(input , n=500)
  usalist <- sapply(udemytweets, function(x) x$getText())
  nd = c(length(usalist))
  country = c(usalist)
  scores = score.sentiment(country,pos,neg,.progress = 'text')
  scores$country = factor(rep(c(""), nd))
  scores$very.pos = as.numeric(scores$score >= 2)
  scores$very.neg = as.numeric(scores$score <= -2)
  
  numpos = sum(scores$very.pos)
  numneg = sum(scores$very.neg)
  
  global_score = round(100*numpos / (numpos + numneg))
  return( histogram(data=scores, ~score|country, main="", xlab="", sub="Sentiment Score"))
  
  
}



movie_new <- function(input){
  row_numnew <- which(movies2016[,1] == input)
  
  display <- movies2016[row_numnew,]
  displaymarix = as.data.frame(display);
  displaymarix <- t(displaymarix)
  colnames(displaymarix) <- "Information"
  displaymarix <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display)))
  return(displaymarix)
}

movie_new2015 <- function(input){
  row_numnew <- which(movies2015[,1] == input)
  
  display1 <- movies2015[row_numnew,]
  displaymarix1 = as.data.frame(display1);
  displaymarix1 <- t(displaymarix1)
  colnames(displaymarix1) <- "Information"
  displaymarix1 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display1)))
  return(displaymarix1)
}

movie_new2014 <- function(input){
  row_numnew <- which(movies2014[,1] == input)
  
  display2 <- movies2014[row_numnew,]
  displaymarix2 = as.data.frame(display2);
  displaymarix2 <- t(displaymarix2)
  colnames(displaymarix2) <- "Information"
  displaymarix2 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display2)))
  return(displaymarix2)
}

movie_new2013 <- function(input){
  row_numnew <- which(movies2013[,1] == input)
  
  display3 <- movies2013[row_numnew,]
  displaymarix3 = as.data.frame(display3);
  displaymarix3 <- t(displaymarix3)
  colnames(displaymarix3) <- "Information"
  displaymarix3 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display3)))
  return(displaymarix3)
}

movie_new2012 <- function(input){
  row_numnew <- which(movies2012[,1] == input)
  
  display4 <- movies2012[row_numnew,]
  displaymarix4 = as.data.frame(display4);
  displaymarix4 <- t(displaymarix4)
  colnames(displaymarix4) <- "Information"
  displaymarix4 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display4)))
  return(displaymarix4)
}

movie_new2011 <- function(input){
  row_numnew <- which(movies2011[,1] == input)
  
  display5 <- movies2011[row_numnew,]
  displaymarix5 = as.data.frame(display5);
  displaymarix5 <- t(displaymarix5)
  colnames(displaymarix5) <- "Information"
  displaymarix5 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display5)))
  return(displaymarix5)
}

movie_new2010 <- function(input){
  row_numnew <- which(movies2010[,1] == input)
  
  display6 <- movies2010[row_numnew,]
  displaymarix6 = as.data.frame(display6);
  displaymarix6 <- t(displaymarix6)
  colnames(displaymarix6) <- "Information"
  displaymarix6 <- data.frame(Movie=c("Name","Genre","Description","Director","Actors","Rating","Revenue in Millions"),Information=c(t(display6)))
  return(displaymarix6)
}


movie_recommendation <- function(input,input2,input3) {
  row_num <- which(movies2[,2] == input)
  row_num2 <- which(movies2[,2] == input2)
  row_num3 <- which(movies2[,2] == input3)
  userSelect <- matrix(NA,9066)
  userSelect[row_num] <- 5 #hard code first selection to rating 5
  userSelect[row_num2] <- 4 #hard code second selection to rating 4
  userSelect[row_num3] <- 3 #hard code third selection to rating 3
  userSelect <- t(userSelect)
  
  ratingmat <- dcast(ratings,userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingmat <- ratingmat[,-1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect,ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)
  
  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")
  
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, ratingmat2[1], n=10)
  recom_list <- as(recom, "list")
  recom_result <- data.frame(matrix(NA,10))
  for (i in c(1:10)){
    recom_result[i,1] <- movies2[as.integer(recom_list[[1]][i]),2]
  }
  colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
  return(recom_result)
}

