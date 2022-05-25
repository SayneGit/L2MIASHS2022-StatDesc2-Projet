# ---- Arbre de Décisions #

library(rpart)
library(rpart.plot)
library(MLmetrics)
library(ROSE)

set.seed(2022)
dtree <- rpart(isFraud ~ ., data = train, method = 'class')
rpart.plot(dtree, extra = 106)

# Pas satisfait mais pas trouvé de variables plus significatives

y_pred <- predict(dtree, test, type = 'class')
y_true <- test$isFraud

dtree_precision <- Precision(y_true, y_pred, positive = 1)
dtree_recall <- Recall(y_true, y_pred, positive = 1)
dtree_f1 <- F1_Score(y_true, y_pred, positive = 1)
dtree_auc <- AUC(y_true, y_pred)

paste0("Precision: ", dtree_precision)
paste0("Recall: ", dtree_recall)
paste0("F1 Score: ", dtree_f1)
paste0("AUC: ", dtree_auc)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

set.seed(2022)

control <- rpart.control(minsplit = 8,
                         minbucket = 2,
                         maxdepth = 6,
                         cp = 0)
dtree_tuned_fit <- rpart(isFraud ~ ., data = train, method = 'class', control = control)
y_pred <- predict(dtree_tuned_fit, test, type = 'class')

dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = 1)
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = 1)
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = 1)
dtree_tuned_fit_auc <- AUC(y_true, y_pred)

paste0("Precision: ", dtree_tuned_fit_precision)
paste0("Recall: ", dtree_tuned_fit_recall)
paste0("F1 Score: ", dtree_tuned_fit_f1)
paste0("AUC: ", dtree_tuned_fit_auc)

rpart.plot(dtree_tuned_fit, extra = 106)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)