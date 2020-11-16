#remove cache
rm(list=ls())
#import package
library(tidyverse)
library(tidytext)
library(textcat)
library(forcats)
library(pacman)
library(tm)
library(SnowballC)
library(caret)

# Read the data
reviews <- read_csv('review_city.csv')
business <- read_csv('business_city.csv')

# Get all the reviews of one restaurant
get_restaurant_review <- function(restaurant, reviews) {
  restaurant_name <- restaurant$name[1]
  restaurant_review <- reviews %>%
    filter(business_id %in% restaurant$business_id) %>%
    select("stars", "text") %>%
    mutate(name = restaurant_name) %>%
    relocate(name)
  return(restaurant_review)
}

# Reviews of McDonald's
(McDonalds <- business %>% 
    filter(name == "McDonald's")) 

McDonalds_reviews <- get_restaurant_review(McDonalds, reviews)

# Reviews of Burger King
(Burger_king <- business %>%
    filter(name == "Burger King"))

Burger_king_reviews <- get_restaurant_review(Burger_king, reviews)

# Reviews of Five Guys
Five_guys <- business %>%
  filter(name == "Five Guys")

Five_guys_reviews <- get_restaurant_review(Five_guys, reviews)

# Reviews of Wendy's
Wendys <- business %>%
  filter(name == "Wendy's")

Wendys_reviews <- get_restaurant_review(Wendys, reviews)

# Reviews of Shake Shack
Shake_shack <- business %>%
  filter(name == "Shake Shack")

Shake_shack_reviews <- get_restaurant_review(Shake_shack, reviews)

# Combine the reviews of these five fast food restaurants
fast_food_reviews <- rbind(McDonalds_reviews,Burger_king_reviews,Five_guys_reviews,Wendys_reviews,Shake_shack_reviews)
#calculate tf
fast_food_reviews1 <- fast_food_reviews%>%unnest_tokens(word,text)%>%count(name,word,sort=TRUE)%>%bind_tf_idf(word,name,n)


logistic_regression <- function(fast_food_reviews){
  #set newstopwords
  newstopwords <- c("bk", "mcdonald", "thru",
                    "morning", "breakfast",
                    "burgerking", "soul",
                    "xt", "snow", "passenger",
                    "q", "concrete", "satisfrie", "cle")
  # Transform the target reviews to corpus
  fast_food_reviews_corpus <- Corpus(VectorSource(fast_food_reviews$text))
  ### cleaning data
  # tolower the words
  fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                     content_transformer(tolower))
  #remove numbers
  fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                     removeNumbers)
  #remove stopwords
  fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                     removeWords,
                                     c("the", "and", stopwords("english"),newstopwords))
  fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                     stripWhitespace)
  fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                     removePunctuation)
  #check data
  inspect(fast_food_reviews_corpus[1])
  
  # Change to Document-Term Matrix (DTM) representation:
  # documents as the rows, terms/words as the columns
  
  reviews_dtm <- DocumentTermMatrix(fast_food_reviews_corpus)
  reviews_dtm
  
  inspect(reviews_dtm[1:10, 1:10])
  
  # Reduce the dimension of DTM
  reviews_dtm <- removeSparseTerms(reviews_dtm, 0.995) # speedy but delete some words with lowest fequency
  reviews_dtm
  
  findFreqTerms(reviews_dtm, 1000)
  
  # Calculate tf-idf
  reviews_dtm_tfidf <- DocumentTermMatrix(fast_food_reviews_corpus,
                                          control = list(weighting = weightTfIdf))
  reviews_dtm_tfidf <- removeSparseTerms(reviews_dtm_tfidf, 0.99)
  reviews_dtm_tfidf
  
  inspect(reviews_dtm_tfidf[2, 1:20])
  
  # Obtain binary variable
  fast_food_reviews <- fast_food_reviews %>% 
    mutate(label = as.factor(ifelse(stars > 2, 1, 0)))
  
  # Change tibble to dataframe
  data <- as.data.frame(fast_food_reviews)
  data <- data[-c(1,2,3)]
  data <- cbind(data, as.matrix(reviews_dtm_tfidf))
  
  # Split training and test data
  set.seed(628)
  id_train <- createDataPartition(data$label, p=0.8,list=FALSE) # we need to train data as positive and negative 
  
  data_train <- data[id_train,]
  data_test <- data[-id_train,]
  
  # Final data
  write.csv(McDonalds,"McDonalds.csv")
  write.csv(Burger_king,"Burger_king.csv")
  write.csv(Five_guys,"Five_guys.csv")
  write.csv(Wendys,"Wendys.csv")
  write.csv(Shake_shack,"Shake_shack.csv")
  
  # Statistic model: Logistic regression
  
  # Train on training data
  start.time <- Sys.time()
  fast_food_reviews.glm <- glm(label~., family = "binomial",
                               data = data_train, maxit = 100)
  end.time <- Sys.time()
  (time.taken <- end.time - start.time)
  
  
  
  # Predict on test data
  pred.glm <- as.numeric(predict(fast_food_reviews.glm, 
                                 data_test, type="response") > 0.5)
  table(data_test$label, pred.glm, dnn = c("Obs", "Pred"))
  mean(ifelse(data_test$label != pred.glm, 1, 0))
  
  
  
  print(summary(fast_food_reviews.glm))
  
  # Obtain the Coefficient of Logistic regression
  coefficient <- data.frame("word"=names(coefficients(fast_food_reviews.glm)),"coef" = coefficients(fast_food_reviews.glm))
  
  
  return(coefficient)}

#five models
McDonalds_reviews_regression <- logistic_regression(McDonalds_reviews)
Burger_king_reviews_regression <- logistic_regression(Burger_king_reviews)
Five_guys_reviews_regression <- logistic_regression(Five_guys_reviews)
Wendys_reviews_regression <- logistic_regression(Wendys_reviews)
Shake_shack_reviews_regression <- logistic_regression(Shake_shack_reviews)

#caculate the score
McDonalds <- inner_join(McDonalds_reviews_regression,fast_food_reviews1%>%filter(name=="McDonald's"))%>%mutate(score=coef*tf)
Burger_king <- inner_join(Burger_king_reviews_regression,fast_food_reviews1%>%filter(name=="Burger King"))%>%mutate(score=coef*tf)
Five_guys <- inner_join(Five_guys_reviews_regression,fast_food_reviews1%>%filter(name=="Five Guys"))%>%mutate(score=coef*tf)
Wendys <- inner_join(Wendys_reviews_regression,fast_food_reviews1%>%filter(name=="Wendy's"))%>%mutate(score=coef*tf)
Shake_shack <- inner_join(Shake_shack_reviews_regression,fast_food_reviews1%>%filter(name=="Shake Shack"))%>%mutate(score=coef*tf)


#Draw plots
#McDonalds
ggplot(data=McDonalds%>%arrange(desc(score))%>%head(20),aes(x=reorder(word,score),y=score,fill="red"))+geom_col(show.legend = FALSE)  + labs(y = "score", x = "word")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
#Burger_king
ggplot(data=Burger_king%>%arrange(desc(score))%>%head(20),aes(x=reorder(word,score),y=score,fill="green"))+geom_col(show.legend = FALSE)  + labs(y = "score", x = "word")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
#Five_guys
ggplot(data=Five_guys%>%arrange(desc(score))%>%head(20),aes(x=reorder(word,score),y=score,fill="blue"))+geom_col(show.legend = FALSE)  + labs(y = "score", x = "word")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
#Wendys
ggplot(data=Wendys%>%arrange(desc(score))%>%head(20),aes(x=reorder(word,score),y=score,fill="cyan"))+geom_col(show.legend = FALSE)  + labs(y = "score", x = "word")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
#Shake_shack
ggplot(data=Shake_shack%>%arrange(desc(score))%>%head(20),aes(x=reorder(word,score),y=score,fill="purple"))+geom_col(show.legend = FALSE)  + labs(y = "score", x = "word")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()


