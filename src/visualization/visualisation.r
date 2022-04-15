library(dplyr)
library(GGally)
library(ggplot2)

# Correlations
ggcorr(df2,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

# How many frauds

ggplot(df2, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

# Intervalles de montant en fonction de la fraude

df2$Discretized.amount = cut(df2$amount, c(0,5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 100000000))

ggplot(df2, aes(x = Discretized.amount, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()