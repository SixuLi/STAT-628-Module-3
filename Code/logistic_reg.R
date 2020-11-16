source("Code/data_preprocessing.R")

# Logistic Regression

# Fit Model
Fit_logistic_regression <- function(data_train) {
  start.time <- Sys.time()
  model <- glm(label~., family = "binomial",
               data = data_train, maxit = 100)
  end.time <- Sys.time()
  print(time.taken <- end.time - start.time)
  return(model)
}

# Test
Test <- function(model, data_test) {
  prediction <- as.numeric(predict(model,data_test, 
                                   type="response") > 0.5)
  print(table(data_test$label, prediction, dnn = c("Obs", "Pred")))
  print(paste0("Test Accuracy: ", round(100 * mean(ifelse(data_test$label == prediction, 1, 0)), 2), "%"))
}

# Fit on training data
reviews.glm <- Fit_logistic_regression(data_train)

# Predict on test data
Test(reviews.glm, data_test)

# Obtain coefficients
coefficients <- data.frame("word"=names(coefficients(reviews.glm)),
                           "coef" = coefficients(reviews.glm))
