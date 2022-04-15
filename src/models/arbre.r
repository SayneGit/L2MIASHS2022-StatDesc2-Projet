library(rpart)
library(rpart.plot)

train_validation_split <- function(data, fraction = 0.8, train = TRUE) {
  total_rows <- nrow(data)
  train_rows <- fraction * total_rows
  sample <- 1:train_rows
  if (train == TRUE) {
    return (data[sample, ])
  } else {
    return (data[-sample, ])
  }
}

set.seed(2022)
dtree <- rpart(isFraud ~ ., data = train, method = 'class')
rpart.plot(dtree, extra = 106)