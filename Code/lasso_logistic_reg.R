source("Code/data_preprocessing.R")

# Regularized logistic regression

# Fit model
Lasso_logistic_regression <- function(data_train) {
  
  start.time <- Sys.time()
  # Create the matrix of the predictors
  x_train <- model.matrix(label~., data_train)[,-1]
  y_train <- data_train$label
  
  # Find the best parameter lambds using cross-validation
  set.seed(628)
  cv.lasso <- cv.glmnet(x_train, y_train, alpha=1, 
                        family = "binomial")
  print(paste0("The best parameter lambda is:", cv.lasso$lambda.min))
  
  # Fit the final model on the training data
  lasso.glm <- glmnet(x_train, y_train,
                      family = "binomial",
                      lambda = cv.lasso$lambda.min)
  end.time <- Sys.time()
  print(time.take <- end.time - start.time)
  return(lasso.glm)
  
}

# Test
Lasso_test <- function(model, data_test) {
  # Create the matrix of the predictors
  x_test <- model.matrix(label~., data_test)[,-1]
  y_test <- data_test$label
  
  # Make predictions on the test data
  probabilities <- model %>%
    predict(newx = x_test)
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  
  # Model accuracy
  print(paste0("Test Accuracy:", 
               round(100*mean(predicted.classes == y_test), 2), "%"))
}

# Fit on training data
reviews.lasso.glm <- Lasso_logistic_regression(data_train)


# Predict on test data
Lasso_test(reviews.lasso.glm, data_test)

# Obtain the coefficients
tmp_coeffs <- coef(reviews.lasso.glm, s = "lambda.min")
lasso_coefficients <- data.frame(word = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
                                 coef = tmp_coeffs@x)


# Check significance
significance <- fixedLassoInf(x_train, as.numeric(y_train) - 1, 
                              beta = coef(reviews.lasso.glm, s = "lambda.min"), 
                              lambda = reviews.lasso.glm$lambda, family = "binomial")

names(significance$vars)[significance$pv < 0.1] 
