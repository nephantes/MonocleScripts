## normalize counts:
cSum <- apply(x,2,sum)    # recompute for remaining cells
x.norm <- as.matrix(sweep(x,2,cSum,FUN='/'))*1e6    # normalize to UMIs per million