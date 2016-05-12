cSum <- apply(Unsorted500reads,2,sum)
Index <- which(cSum > 5000)
Unsorted500reads <- Unsorted500reads[,Index]
