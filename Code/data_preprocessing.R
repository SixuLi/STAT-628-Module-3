library(dplyr)
library(ggplot2)
library(stringr)
#library(lubridate)
library(tidytext)
library(DT)
#library(leaflet)
library(textcat)
library(forcats)
library(pacman)
library(tm)
library(SnowballC)
library(glmnet)
library(jsonlite)
library(caret)
#p_load_gh('hrbrmstr/pluralize')
#p_load(quanteda)

rm(list = ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

# Read the data
# reviews <- read_csv('Data/review_city.csv')
# business <- read_csv('Data/business_city.csv')
reviews <- stream_in(file("Data/review_city.json"), pagesize = 272024)
business <- stream_in(file("Data/business_city.json"), pagesize = 36327)

business <- business %>%
  select(business_id, name, address, city, state, postal_code,
         stars, review_count, is_open, categories)

head(business)

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
    filter(name == "McDonald's") %>%
    arrange(desc(review_count, stars)))

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
fast_food_reviews <- McDonalds_reviews %>%
  full_join(Burger_king_reviews) %>%
  full_join(Five_guys_reviews) %>%
  full_join(Wendys_reviews) %>%
  full_join(Shake_shack_reviews)

# Change plural to single
fast_food_reviews$text <- gsub("?s\\b", "", fast_food_reviews$text)

# Tokenize the reviews
reviews_words <- fast_food_reviews %>%
  # Tokenize
  unnest_tokens(word, text) %>%
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # reomve stop words
  anti_join(stop_words) %>%
  #anti_join(mystopwords) %>%
  # stem the words
  mutate(word = SnowballC::wordStem(word)) %>%
  # get count of each token in each document
  count(name, word, sort = TRUE)

# Count the total words for each restaurant
total_words <- reviews_words %>%
  group_by(name) %>%
  summarise(total = sum(n))

reviews_words <- left_join(reviews_words, total_words)

# Calculate the tf-idf of each word and visualize it
review_tf_idf <- reviews_words %>%
  bind_tf_idf(word, name, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 
  
review_tf_idf %>%
  group_by(name) %>%
  slice_max(tf_idf, n=15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Remove some not useful words such as "mcdonald", "bk"
mystopwords <- tibble(word = c("bk", "mcdonald", "thru",
                               "morning", "breakfast",
                               "burgerking", "soul",
                               "xt", "snow", "passenger",
                               "q", "concrete", "satisfrie", "cle",
                               "nice", "burger", "love", "perfect", "lot",
                               "super", "fine", "pleasant", "food", "worst", "thi",
                               "king", "bad", "eat", "told", "reivew", "regular",
                               "add", "talk", "heard", "suck", "lunch", "shame",
                               "guy", "wendy"))

newstopwords <- c("bk", "mcdonald", "thru",
                  "morning", "breakfast",
                  "burgerking", "soul",
                  "xt", "snow", "passenger",
                  "q", "concrete", "satisfrie", "cle",
                  "nice", "burger", "love", "perfect", "lot",
                  "super", "fine", "pleasant", "food", "worst", "thi",
                  "king", "bad", "eat", "told", "reivew", "regular",
                  "add", "talk", "heard", "suck", "lunch", "shame", "guy", "wendy")

reviews_words <- anti_join(reviews_words, mystopwords,
                           by = "word")

review_tf_idf <- reviews_words %>%
  bind_tf_idf(word, name, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

review_tf_idf %>%
  group_by(name) %>%
  slice_max(tf_idf, n=15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Transform the target reviews to corpus
fast_food_reviews_corpus <- Corpus(VectorSource(fast_food_reviews$text))
fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                   content_transformer(tolower))
fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                   removeNumbers)
fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                   removeWords,
                                   c("the", "and", stopwords("english"), newstopwords))
fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                   stripWhitespace)
fast_food_reviews_corpus <- tm_map(fast_food_reviews_corpus,
                                   removePunctuation)

inspect(fast_food_reviews_corpus[1])

# Change to Document-Term Matrix (DTM) representation:
# documents as the rows, terms/words as the columns

reviews_dtm <- DocumentTermMatrix(fast_food_reviews_corpus)
reviews_dtm

inspect(reviews_dtm[1:10, 1:10])

# Reduce the dimension of DTM
reviews_dtm <- removeSparseTerms(reviews_dtm, 0.995)
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
split_data <- function(data) {
  set.seed(628)
  id_train <- createDataPartition(data$label, p=0.8,list=FALSE)
  
  data_train <- data[id_train,]
  data_test <- data[-id_train,]
  
  return(list(data_train, data_test))
}

data_total <- split_data(data)
data_train <- data_total[[1]]
data_test <- data_total[[2]]









