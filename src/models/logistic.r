set.seed(2022)

# On standardise les variables numériques
data_rescale <- mutate_if(train,
                          is.numeric,
                          list(~as.numeric(scale(.))))
logreg <- glm(isFraud ~ ., data = train, family = "binomial")
summary(logreg)

LR <- logreg$null.deviance - logreg$deviance
p <- logreg$df.null - logreg$df.residual
pchisq(LR, p, lower.tail = F)

aic_logreg <- AIC(logreg)
aic_logreg

bic_logreg <- BIC(logreg)
bic_logreg

y_true <- test$isFraud
y_pred <- predict(logreg, test, type = 'response')
y_pred <- as.factor(ifelse(y_pred > 0.5, 1, 0))

logreg_precision <- Precision(y_true, y_pred, positive = 1)
logreg_recall <- Recall(y_true, y_pred, positive = 1)
logreg_f1 <- F1_Score(y_true, y_pred, positive = 1)
logreg_auc <- AUC(y_true, y_pred)
paste0("Precision: ", logreg_precision)

paste0("F1 Score: ", logreg_f1)


# Marche po