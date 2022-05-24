#### III - Sélection du meilleur modèle ####

library(leaps)
#### 1. Modèle de départ (constante uniquement) ###
modele.min = lm(isFraud ~ 1, data = train) 
modele.min$coefficients

#### 2. Modèle complet (maximal) ####

model.test = lm(train2$amount ~ train2$nameOrig + train2$nameDest, data = train2)

modele.max = lm(isFraud ~ ., data = train) 
modele.max$coefficients

#### 3. Méthode descendante avec AIC ####
# step(modele.max, data = train, direction = "backward")

# Méthode ascendante avec AIC
step(modele.min, scope=list(lower = modele.min, upper = modele.max), data = train, direction = "forward")

#### III - Régression logistique multiple ####

#### 1. Modèle complet ####
modele.log.max = glm(isFraud ~ ., data = train, family = binomial)

#### Informations sur le modèle ####
summary(modele.log.max)

#### 2. Autres modèles ####

#### Modèle : Cholesterol ####
modele.log.1 = glm(df$Diabete ~ df$Cholesterol, data = df, family = binomial)
summary(modele.log.1)

#### 3. Méthode descendante avec AIC ####
step(modele.log.max, data = train, direction = "backward")

#### PARTIE II ####

#### I - Chargement des données ####

# Modèle de type lasso
library(glmnet)

# Données
library(MASS)
data <- train
summary(data)

#### II - Sélection des données ####

#### 1. Échantillon ####
samplesize = 0.70 * nrow(data)
set.seed(1212)
index = sample(seq_len(nrow(data)), size = samplesize)

#### 2. Données centrées réduites ####
data.X.std <- scale(data[index,names(data)!="medv"])

#### 3. Jeu de données pour entraînement (Training) et validation (Test) ####
X.train <- as.matrix(data.X.std)
X.test <- as.matrix(scale(data[-index,names(data)!="medv"],
                          attr(data.X.std, "scaled:center"),
                          attr(data.X.std, "scaled:scale"))) 
Y.train <- data[index, "medv"]
Y.test <- data[-index, "medv"]

#### III - Apprentissage ####

#### 1. Influence du paramètre λ ####
lasso.fit<- glmnet(x = X.train, y = Y.train, family = "gaussian", alpha = 1) 
plot(lasso.fit, xvar = "lambda", label = TRUE, main = "Évolution des coefficients de régression en fonction de λ\n")

#### 2. Recherche du meilleur λ ####
# Sans lambda prédéfinis
#lasso.cv <- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, nfolds = 5)

# Sélection parmi des lambdas prédéfinis
lambda = c(10^(-4), 2*10^(-4), 2*10^(-4), 5*10^(-4), 7*10^(-4), 10^(-3), 2*10^(-3), 5*10^(-3), 10^(-2), 5*10^(-2), 10^(-1), 0.5, 1, 2, 5, 10, 20, 50, 100)
lasso.cv <- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, lambda = lambda, nfolds = 5)
plot(lasso.cv)

coef(lasso.cv, lasso.cv$lambda.1se) #parcimonieux
coef(lasso.cv, lasso.cv$lambda.min) #pas parcimonieux

# Lambda qui minimise l'erreur du cv
lasso.cv$lambda.min

# Plus grand λ dont erreur cv < erreur optimale
lasso.cv$lambda.1se

lasso.fit01<- glmnet(x = X.train, y = Y.train, family = "gaussian", alpha = 1, lambda = 0.1)
lasso.fit01$beta

#### 3. Évaluation des performances ####
pred.lasso.train <- predict(lasso.fit, newx = X.train, s=lasso.cv$lambda.min)
pred.lasso.min <- predict(lasso.fit, newx = X.test, s=lasso.cv$lambda.min)
pred.lasso.1se <- predict(lasso.fit, newx = X.test, s=lasso.cv$lambda.1se)

# Erreur (risque) quadratique moyenne
MSE.lasso <- mean((Y.train-pred.lasso.train)^2)
MSE.lasso.test <- mean((Y.test-pred.lasso.1se)^2)

#### 4. Modèle de régression multiple complet ####
modele.multi.max <- lm(data$medv ~ ., data = data) 

# Méthode descendante
step(modele.multi.max, data = data, direction = "backward")

# Meilleur Modèle
modele.multi <- lm(formula = data$medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, data = data)
summary(modele.multi)

# Erreur Quadratique Moyenne
MSE.multi <- mean((modele.multi$residuals)^2)
pred.multi <- predict(modele.multi, newx = X.test)
MSE.multi.test <- mean((Y.test-pred.multi)^2)