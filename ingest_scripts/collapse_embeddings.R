##COLLAPSE EMBEDDINGS

## 10 JANUARY 2024

##This script collapses the embeddings for 23-24 and should be run every time the data is updated

##READ IN
embeddings_full <- read.csv('./data/shooter_embed_full24.csv')[,-1]

##COLLAPSE
embeddings_collapsed <- data.matrix(embeddings_full[,1:50])

##WRITE OUT
saveRDS(embeddings_collapsed, './data/embeddings_collapsed24.rds')
