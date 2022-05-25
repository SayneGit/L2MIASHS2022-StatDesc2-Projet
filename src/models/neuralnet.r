# 1. Initialisation
## a) Librairie
library(matrixStats)
library(boot)
library(plyr)
library(neuralnet)

## b) Modèle
set.seed(2)
NN = neuralnet(isFraud ~ amount + nameOrig + nameDest, train, hidden = 4, linear.output = T)
plot(NN)


# c) Erreur Quadratique Moyenne
#Root Mean Square Error
# ------------------

# 2. Cross Validation du modèle de réseau neuronal ####

# a) Bibliothèques

# b) Intialisation des variables
set.seed(50)
k = 100
RMSE.NN = NULL

list = list()

# 3. Ajustement du modèle de réseau neuronal ####

## a) Boucle FOR imbriquée
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(train),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = train[-index,]
    
    NN = neuralnet(train$isFlaggedFraud ~ train$amount + train$type + train$nameOrig + train$oldbalanceDest  + train$oldbalanceOrg, hidden = 3, linear.output = T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(dataset$isFraud)-min(dataset$isFraud)))+min(dataset$isFraud)
    
    RMSE.NN [i]<- (sum((dataset$isFraud - predict_testNN)^2)/nrow(dataset))^0.5
  }
  list[[j]] = RMSE.NN
}


# b) Matrice
matrix.RMSE = do.call(cbind, list)

# c) Graphique
boxplot(matrix.RMSE[,56], ylab = "EQM", main = "Graphique EQM", col="lightblue")

# 4. Variation de la médiane EQM ####

## a) Calcul de la médiane
med = colMedians(matrix.RMSE)

# à vcoir si ça run

## b) Jeu de données
X = seq(10,65)

## c) Graphique
plot(med ~ X, type="l", xlab = "longueur du jeu de données", ylab = "médiane EQM", main =" Variation de l'EQM en fonction de la longueur du jeu de données", col="lightblue")