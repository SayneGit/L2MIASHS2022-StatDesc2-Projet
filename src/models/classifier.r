library(e1071)

set.seed(2022)
nbClassifier <- naiveBayes(isFraud ~., data = train)
y_pred <- predict(nbClassifier, test)
y_true <- test$isFraud

nbClassifier_precision <- Precision(y_true, y_pred, positive = 1)
nbClassifier_recall <- Recall(y_true, y_pred, positive = 1)
nbClassifier_f1 <- F1_Score(y_true, y_pred, positive = 1)
nbClassifier_auc <- AUC(y_true, y_pred)
paste0("Precision: ", nbClassifier_precision)

