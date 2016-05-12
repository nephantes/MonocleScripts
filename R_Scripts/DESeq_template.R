require(DESeq2)  # for DESeq analysis
library(S4Vectors)
library(IRanges)
library(GenomicRanges)
library(SummarizedExperiment)
library(Rcpp)
library(RcppArmadillo)

# MUST RUN "Finding Genes that Change as a Function of Pseudotime" for timetestordered

load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /CellExpressionMatrices/Allunsortedmyeloid.Rdata") #RAW DATA FILE
load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/HSMM_2500_UMI_MinEx1_1SDGenecount__1381Cells_Top2_5Paths.Rdata")
load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /GeneInformation/Top2foldchange.RData") #Top2foldchange #Top10ofeach19Cluster #Allclusterinformative
HSMM <- HSMM_2500_UMI_MinEx1_1SDGenecount_Top2_5Paths
expressed_genes <- row.names(subset(fData(HSMM), num_cells_expressed >= 50))
ordering_genes <- Top2foldchange[,] 
ordering_genes <- intersect(ordering_genes, expressed_genes)

(pData(HSMM)[which(pData(HSMM)$State == 5),]) -> state5
order((pData(HSMM)[which(pData(HSMM)$State == 5),])$Pseudotime) -> indx
indx[1:10] -> indxf10
indx[(length(indx) - 9):length(indx)] -> indxl10
state5[indxf10,] -> earlyState5
row.names(earlyState5) -> earlyState5
state5[indxl10,] -> lateState5
row.names(lateState5) -> lateState5
rm(indx)
rm(indxl10)
rm(indxf10)
rm(state5)

match(earlyState5, colnames(Allunsortedmyeloid)) -> earlyindx
match(lateState5, colnames(Allunsortedmyeloid)) -> lateindx
append(lateindx, earlyindx) -> fullsubset
Allunsortedmyeloid[,fullsubset] -> deseqexprmatrix

match(rownames(timetestordered), rownames(deseqexprmatrix)) -> geneindx
geneindx[!is.na(geneindx)] -> geneindx
deseqexprmatrix[geneindx,] -> deseqexprmatrix

expDesign <- matrix(, nrow = ncol(deseqexprmatrix), ncol = 2)
colnames(expDesign)<- c( "Sample", "pTime")
rownames(expDesign)<- colnames(deseqexprmatrix)
expDesign[,1:2]<- c(colnames(deseqexprmatrix), colnames(deseqexprmatrix))
expDesign[1:10,2]<-1
expDesign[11:20,2]<-0
as.data.frame(expDesign) -> expDesign

rm(earlyindx)
rm(earlyState5)
rm(fullsubset)
rm(geneindx)
rm(lateindx)
rm(lateState5)

## create the DESeq object:
dds <- DESeqDataSetFromMatrix(countData=as.matrix(deseqexprmatrix), colData=expDesign, design = ~ pTime)
dds <- DESeq(dds,fitType='local')
res <- results(dds, addMLE=TRUE)
which((res$log2FoldChange < -1 | res$log2FoldChange > 1)) -> foldChangeIndx
which(res$padj < .01) -> signifIndx
intersect(foldChangeIndx, signifIndx) -> bothIndx
res[bothIndx,] -> trimmedRes
trimmedRes[order(-trimmedRes[,2]),] -> trimmedRes
View(trimmedRes)
