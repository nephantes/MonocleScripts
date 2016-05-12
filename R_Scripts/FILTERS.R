# Sort for UMI or FPKM counts
cSum <- apply(Allunsortedmyeloid,2,sum) 
hist(cSum)
mean <- mean(cSum)
sd <- sd(cSum)
Index <- which(cSum > 2500) # Min Column sum
Allunsortedmyeloid <- Allunsortedmyeloid[,Index]
rm(Index)
cSum <- apply(Allunsortedmyeloid,2,sum) 
hist(cSum)
mean <- mean(cSum)
sd <- sd(cSum)

#Normalize
cSum <- apply(Allunsortedmyeloid,2,sum) 
x.norm <- as.matrix(sweep(Allunsortedmyeloid,2,cSum,FUN='/'))*1e4 
rm(cSum)

# Sort for minimum gene number expressed
compThresh <- 1 #Min matrix value, aka miniumum single gene expression to be included in total gene expression number
gCount <- apply(x.norm,2,function(x) length(which(x>compThresh)))
hist(gCount)
mean <- mean(gCount)
sd <- sd(gCount)

cutoffdown <- (mean - 1*sd) #1000 UMI, 1.3 = 1000 Genes expressed 
x.norm <- x.norm[,which(gCount > cutoffdown)]
gCount <- apply(x.norm,2,function(x) length(which(x>compThresh)))
hist(gCount)
mean <- mean(gCount)
sd <- sd(gCount)

#cutoffup <- mean + 2*sd
#x.norm <- x.norm[,which(gCount < cutoffup)]
#gCount <- apply(x.norm,2,function(x) length(which(x>compThresh)))
#hist(gCount)
#mean <- mean(gCount)
#sd <- sd(gCount)

HSMM_expr_matrix <- x.norm

rm(Allunsortedmyeloid)
rm(x.norm)
rm(mean)
rm(sd)
rm(compThresh)
rm(gCount)
rm(cutoffdown)
#rm(cutoffup)