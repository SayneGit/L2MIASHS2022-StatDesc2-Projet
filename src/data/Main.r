library(disk.frame)
library(parallel)

nCores <- detectCores()    # Dépend de la machine
setup_disk.frame(workers = nCores)
options(future.globals.maxSize = Inf)

# Chargement du CSV + Génération des fichiers
df1 <- csv_to_disk.frame(
  file.path("data/", "train.csv"), 
  outdir = file.path("data/", "train.df"),
  inmapfn = base::I,
  recommend_nchunks(sum(file.size(file.path("data/" , "train.csv")))),
  backend = "data.table")

df2 <- collect(sample_frac(df1, 1)) # df en entier
train <- collect(sample_frac(df1, 0.60)) # Split avec un sample de 60% de df
validation <- setdiff(df2, train) # Différence entre le df en entier (df2) et train pour récupérer les 40% restants

df2 <- select(df2, step, type, amount, oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest, isFraud, isFlaggedFraud) # Uniquement les variables intéressantes (WIP)
train <- select(train, step, type, amount, oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest, isFraud, isFlaggedFraud)# ""

df2$isFraud <- factor(df2$isFraud) # Changement du type de variable pour isFraud
train$isFraud <- factor(train$isFraud)

df2$type <- factor(df2$type, order=TRUE, c(3,2,1)) # PAssage de la donné qualitative en quantitative (1, 2, 3)
train$type <- factor(train$type, order=TRUE, c(3,2,1))



