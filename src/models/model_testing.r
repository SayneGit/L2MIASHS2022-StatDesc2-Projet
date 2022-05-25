y_pred <- predict(dtree_tuned_fit, test, type = 'class')
y_true <- test$Survived

dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = 1)
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = 1)
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = 1)
dtree_tuned_fit_auc <- AUC(y_true, y_pred)

paste0("Precision: ", dtree_tuned_fit_precision)