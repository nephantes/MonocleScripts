library(kknn)
library(tsne)
library(fastICA)

index <- match(expressed_genes, row.names(HSMM_expr_matrix))
pc <- prcomp(t(log10(HSMM_expr_matrix[index,]+1)), scale=TRUE)
var <- pc$sdev^2/sum(pc$sdev^2)
#plot(var[1:20])   #check to determine which Principal components contribute
#plot(diff(var[1:20]))
#based on previous plots determinn nComp
nComp <- 10
d <- mahalanobis(pc$rotation[,1:nComp], center=rep(0, nComp), cov = cov(pc$rotation[,1:nComp]))
plot(density(d))
dThresh.pctile <- .8
dThresh <- quantile(d,dThresh.pctile)
abline(v = dThresh)
gList.keep <- names(which(d>dThresh))
gListComplement <- ordering_genes[-which(ordering_genes %in% gList.keep)]

ic <- fastICA(t(log10(HSMM_expr_matrix[gList.keep,]+1)), n.comp=nComp, alg.typ = 'parallel', fun='logcosh', alpha = 1.0, method = 'C') #try 2 for nComp
colnames(ic$A) <- gList.keep
x.tsne <- tsne(ic$S, initial_dims = nComp, max_iter=2000, k=2) #k can equal 3!!!
row.names(x.tsne) <- colnames(HSMM_expr_matrix)

spec <- specClust(ic$S, centers = nClust, method = 'random-walk')

cList.tsne <- c()
cList.tsne[1:9] <- c(crayons["Cerulean"],           #1dacd6
                     crayons["Screamin' Green"],    #76ff7a
                     crayons["Midnight Blue"],      #1a4876
                     crayons["Shocking Pink"],#fb7efd
                     crayons["Blue Violet"],#7366bd
                     crayons["Goldenrod"],#fcd975
                     crayons["Neon Carrot"],#ffa343
                     crayons["Caribbean Green"],#00CC99
                     crayons["Scarlet"])#fc2847
## or cList.tsne <- rainbow(10)

clusterCells = list()
xrng <- range(x.tsne[,1])
yrng <- range(x.tsne[,2])

#pdf(file=sprintf('Clusters_Comp%d_Clust%d_ext%s.pdf',nComp,nClust,ext))
for (i in 1:nClust) {
  idx.tmp <- which(spec$cluster==i)
  clusterCells[[i]] <- colnames(x.norm)[idx.tmp]
  if (i==1) {
    plot(x.tsne[idx.tmp,1],x.tsne[idx.tmp,2],pch=20,xlab='t-SNE[1]',ylab='t-SNE[2]',
         main=sprintf('%d-component ICA + t-SNE (specClust N=%d)',nComp,nClust),
         xlim=xrng, ylim=yrng,col=cList.tsne[i])#,family="A")
  } else {
    points(x.tsne[idx.tmp,1],x.tsne[idx.tmp,2],pch=20,col=cList.tsne[i])
  }
  text(mean(x.tsne[idx.tmp,1]),mean(x.tsne[idx.tmp,2]),labels=i,font=4)
}




