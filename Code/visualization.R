source("data_preprocessing.R")
source("logistic_reg.R")
source("lasso_logistic_red.R")

# Visualization

visualize <- function(restaurant_name, coefficients) {
  
  # Obtain the coefficients corresponding to specific restaurant
  restaurant_coefficients <- inner_join(coefficients,review_tf_idf %>% 
                                        filter(name==restaurant_name)) %>% 
                                        mutate(score=coef*tf)
  
  # Top 20 positive words
  plot(restaurant_coefficients %>% 
    arrange(desc(score)) %>% 
    head(20) %>%
    ggplot(aes(x=reorder(word,score),y=score,fill="red")) + 
    geom_col(show.legend = FALSE) + 
    labs(y = "score", x = "word", title = restaurant_name) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    coord_flip())
  
  
  # Top 30 positive and negative words
  plot(restaurant_coefficients %>%
    top_n(30, abs(score)) %>%
    mutate(word = reorder(word, score)) %>%
    head(30) %>%
    ggplot(aes(word, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    labs(y = "score", x = "word", title = restaurant_name) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    coord_flip())
}

# McDonald's 

# Coefficients from logistic regression
visualize("McDonald's", coefficients)

# Coefficients from regularized logistic regression
visualize("McDonald's", lasso_coefficients)


# Burger King

# Coefficients from logistic regression
visualize("Burger King", coefficients)

# Coefficients from regularized logistic regression
visualize("Burger King", lasso_coefficients)


# Five Guys

# Coefficients from logistic regression
visualize("Five Guys", coefficients)

# Coefficients from regularized logistic regression
visualize("Five Guys", lasso_coefficients)


# Wendy's

# Coefficients from logistic regression
visualize("Wendy's", coefficients)

# Coefficients from regularized logistic regression
visualize("Wendy's", lasso_coefficients)


# Shake Shack

# Coefficients from logistic regression
visualize("Shake Shack", coefficients)

# Coefficients from regularized logistic regression
visualize("Shake Shack", lasso_coefficients)


# Bigram TF-IDF
(reviews_bigram_tf_idf <- fast_food_reviews %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% mystopwords$word,
           !word2 %in% mystopwords$word) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(name, bigram) %>%
    bind_tf_idf(bigram, name, n) %>%
    arrange(desc(tf_idf))
)

reviews_bigram_tf_idf %>%
  group_by(name) %>%
  slice_max(tf_idf, n=15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)














