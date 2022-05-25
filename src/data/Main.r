# --------------- A UTILISER -------------------- #

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


train <- collect(sample_frac(df1, 0.1)) # Split avec un sample de 10% de df
df_full <- collect(sample_frac(df1, 1)) # df en entier
diff <- setdiff(df_full, train) # Différence entre le df en entier (df2) et train pour récupérer les 90% restants
test <- collect(sample_frac(diff, 0.05)) #

rm(df_full)
rm(diff)

train <- dplyr::select(train, type, amount, nameOrig, nameDest, isFraud, isFlaggedFraud)

train$type = as.numeric(as.factor(train$type))
train$nameOrig = as.numeric(as.factor(train$nameOrig))
train$nameDest = as.numeric(as.factor(train$nameDest))

test <- dplyr::select(test , type, amount, nameOrig, nameDest, isFraud, isFlaggedFraud)

test$type = as.numeric(as.factor(test$type))
test$nameOrig = as.numeric(as.factor(test$nameOrig))
test$nameDest = as.numeric(as.factor(test$nameDest))