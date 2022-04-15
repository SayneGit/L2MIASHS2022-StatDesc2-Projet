library(disk.frame)
library(parallel)

nCores <- detectCores()    # Dépend de la machine
setup_disk.frame(workers = nCores)
options(future.globals.maxSize = Inf)
df1 <- csv_to_disk.frame(
  file.path("data/", "train.csv"), 
  outdir = file.path("data/", "train.df"),
  inmapfn = base::I,
  recommend_nchunks(sum(file.size(file.path("data/" , "train.csv")))),
  backend = "data.table")

train <- collect(sample_frac(df1, 0.60))
df2 <- collect(sample_frac(df1, 1))
validation <- df2[-c(1:nrow(train)), ]

df2 <- select(df2, step, type, amount, oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest, isFraud, isFlaggedFraud)

df2$isFraud <- factor(df2$isFraud)
df2$type <- factor(df2$type, order=TRUE, c(3,2,1))
