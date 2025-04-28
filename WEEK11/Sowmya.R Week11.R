library(mlbench)
library(purrr)

data("PimaIndiansDiabetes2")
ds <- as.data.frame(na.omit(PimaIndiansDiabetes2))
## fit a logistic regression model to obtain a parametric equation
logmodel <- glm(diabetes ~ .,
                data = ds,
                family = "binomial")
summary(logmodel)

cfs <- coefficients(logmodel) ## extract the coefficients
prednames <- variable.names(ds)[-9] ## fetch the names of predictors in a vector
prednames

sz <- 100000000 ## to be used in sampling
##sample(ds$pregnant, size = sz, replace = T)

dfdata <- map_dfc(prednames,
                  function(nm){ ## function to create a sample-with-replacement for each pred.
                    eval(parse(text = paste0("sample(ds$",nm,
                                             ", size = sz, replace = T)")))
                  }) ## map the sample-generator on to the vector of predictors
## and combine them into a dataframe

names(dfdata) <- prednames
dfdata

class(cfs[2:length(cfs)])

length(cfs)
length(prednames)
## Next, compute the logit values
pvec <- map((1:8),
            function(pnum){
              cfs[pnum+1] * eval(parse(text = paste0("dfdata$",
                                                     prednames[pnum])))
            }) %>% ## create beta[i] * x[i]
  reduce(`+`) + ## sum(beta[i] * x[i])
  cfs[1] ## add the intercept

## exponentiate the logit to obtain probability values of thee outcome variable
dfdata$outcome <- ifelse(1/(1 + exp(-(pvec))) > 0.5,
                         1, 0)

##XGBoost Direct:
library(xgboost)
library(dplyr)
library(caret)

# Set up sample sizes
sample_sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

# Prepare a dataframe to collect results
results <- data.frame(
  SampleSize = integer(),
  Accuracy = numeric(),
  TimeTaken = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each sample size
for (sz in sample_sizes) {
  
  cat("\nRunning sample size:", sz, "\n")
  
  # Sample the data
  set.seed(123) # for reproducibility
  idx <- sample(1:nrow(dfdata), size = sz, replace = FALSE)
  data_sample <- dfdata[idx, ]
  
  # Split into training and testing (80-20 split)
  trainIndex <- createDataPartition(data_sample$outcome, p = 0.8, list = FALSE)
  trainData <- data_sample[trainIndex, ]
  testData <- data_sample[-trainIndex, ]
  
  # Prepare data for xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(select(trainData, -outcome)),
                        label = trainData$outcome)
  dtest <- xgb.DMatrix(data = as.matrix(select(testData, -outcome)),
                       label = testData$outcome)
  
  # Set parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = "error",
    max_depth = 3,
    eta = 0.1
  )
  
  # Train model and measure time
  start_time <- Sys.time()
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50,
    verbose = 0
  )
  
  end_time <- Sys.time()
  
  # Predict
  preds <- predict(model, dtest)
  pred_labels <- ifelse(preds > 0.5, 1, 0)
  
  # Calculate Accuracy
  acc <- mean(pred_labels == testData$outcome)
  
  # Time taken
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Store results
  results <- rbind(results, data.frame(SampleSize = sz,
                                       Accuracy = acc,
                                       TimeTaken = time_taken))
}

# Show results
results


##XGBoost _ caret:
library(caret)
library(xgboost)
library(dplyr)

# Set up sample sizes
sample_sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

# Prepare a dataframe to collect results
results_caret <- data.frame(
  SampleSize = integer(),
  Accuracy = numeric(),
  TimeTaken = numeric(),
  stringsAsFactors = FALSE
)

# Set up trainControl for 5-fold Cross-Validation
train_control <- trainControl(method = "cv", number = 5)

# Loop over sample sizes
for (sz in sample_sizes) {
  
  cat("\nRunning sample size:", sz, "\n")
  
  # Sample the data
  set.seed(123)
  idx <- sample(1:nrow(dfdata), size = sz, replace = FALSE)
  data_sample <- dfdata[idx, ]
  
  # Split features and outcome
  x <- select(data_sample, -outcome)
  y <- as.factor(data_sample$outcome)
  
  # Measure time
  start_time <- Sys.time()
  
  # Train XGBoost via caret
  model_caret <- train(
    x = x,
    y = y,
    method = "xgbTree",
    trControl = train_control,
    tuneLength = 3,
    verbosity = 0
  )
  
  end_time <- Sys.time()
  
  # Best accuracy
  acc <- max(model_caret$results$Accuracy)
  
  # Time taken
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Store results
  results_caret <- rbind(results_caret, data.frame(SampleSize = sz,
                                                   Accuracy = acc,
                                                   TimeTaken = time_taken))
}

# Show results
results_caret

