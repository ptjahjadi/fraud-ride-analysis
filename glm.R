# read and analyse csv --------------------------------------------------------
ride = read.csv(file = 'preprocessed.csv')


# remove white spaces from price_review_reason and categorise it
ride$price_review_reason = trimws(ride$price_review_reason)
ride$price_review_reason = factor(ride$price_review_reason)

# remove rows with price = 0
for (i in 1:nrow(ride)){
  if (ride[i, 16] == 0) {
    ride = ride[-c(i),]
  }
}

summary(ride)

# use 80% of the dataset to train the regression model 
train_ride = ride[1:(nrow(ride)*4/5), ]
test_ride = ride[(nrow(ride)*4/5+1):nrow(ride), ]

# create a logistic regression model --------------------------------------
model = glm(is_successful_payment ~ vincenty_distance*ride_distance + distance +
            + price * ride_price + price_review_reason + failed_attempts, 
            family = "binomial", data = train_ride)

summary(model)


# check if removing variables would have a better model fit ---------------
step_model = step(model)

summary(step_model)


# predict using test data -------------------------------------------------
testdata = data.frame(vincenty_distance = test_ride[, 11], 
                      distance = test_ride[, 15], 
                      ride_distance = test_ride[, 16], 
                      price = test_ride[, 17],
                      ride_price = test_ride[, 18], 
                      price_review_reason = test_ride[, 20], 
                      failed_attempts = test_ride[, 24])

results = predict(step_model, testdata, type = "response")

# evaluate accuracy of the model ------------------------------------------
true_positive = 0 # correctly predicted failed payment
false_positive = 0 # incorrectly predicted failed payment
true_negative = 0 # correctly predicted successful payment
false_negative = 0 # incorrectly predicted successful payment

for (i in 1:nrow(testdata)){
  #  Let probabilities of less than 0.5 be categorised as failed transaction
  if (results[i] < 0.8) {
    results[i] = 0
  }
  else {
    results[i] = 1
  }
  
  # check whether the prediction matches the actual payment outcome
  if (results[i] == test_ride[i, "is_successful_payment"]) {
    # model predicts failed payment correctly
    if (results[i] == 0){
      true_positive = true_positive + 1
    }
    # model predicts successful payment correctly
    else{
      true_negative = true_negative + 1
    }
  }
  else {
    # model predicts failed payment for successful payments
    if (results[i] == 0){
      false_positive = false_positive + 1
    }
    # model predicts successful payment for failed payments
    else {
      false_negative = false_negative + 1
    }
  }
}


