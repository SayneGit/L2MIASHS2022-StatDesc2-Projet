library(dplyr)
library(GGally)
library(ggplot2)

cor.test(x = train$isFraud, y = train$amount, method = "pearson")
aov(train$type ~ train$isFraud, train)


train_chi_fraud_type <- chisq.test(train$isFraud, train$type)
train_chi_amount_fraud <- chisq.test(train$isFraud, train$amount)
train_chi_amount_type <- chisq.test(train$amount, train$type)

# Correlations
ggcorr(train,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

# How many frauds

ggplot(train, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

ggplot(train, aes(x = amount)) +
  geom_density(fill = 'coral') 

max(train$amount)

# Intervalles de montant en fonction de la fraude

train$Discretized.amount = cut(train$amount, c(0,5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 100000000))

ggplot(train, aes(x = Discretized.amount, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()