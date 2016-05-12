library(stringdist)
library(readxl)

#Input 1 excel spreadsheet with 4 columns. 
#Column 1 is the names of barcodes in Column 2 (query barcodes)
#Column 3 is the names of barcodes in Column 4 (used barcodes)

read_excel("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /HammingExample/BarcodesTemplate.xlsx") -> Barcodes

Barcodes[,2] -> queryBarcodes
queryBarcodes <- queryBarcodes[!is.na(queryBarcodes)]
Barcodes[,4] -> usedBarcodes
usedBarcodes <- usedBarcodes[!is.na(usedBarcodes)]

stringdistmatrix(queryBarcodes, usedBarcodes, method = "hamming") -> hammingDistMatrix
rownames(hammingDistMatrix)=queryBarcodes
colnames(hammingDistMatrix)=usedBarcodes

minimumHamming = c()

for (i in 1:nrow(hammingDistMatrix)){
  dmin = 8 #Max Barcode Size
  for (j in 1:ncol(hammingDistMatrix)){
    if (hammingDistMatrix[i,j]<dmin){
      dmin = hammingDistMatrix[i,j]
    } 
  } 
  minimumHamming <- c(minimumHamming, dmin)
} 

usableBarcodesMatrix <- matrix(minimumHamming, nrow = nrow(hammingDistMatrix), ncol = 1)
rownames(usableBarcodesMatrix)<-Barcodes[,1]
colnames(usableBarcodesMatrix)="minimumHamming"
View(usableBarcodesMatrix)

rm(i)
rm(j)
rm(dmin)

#usableBarcodes = c()

#for (i in 1:nrow(hammingDistMatrix)){
#  dmin = 8 #Max Barcode Size
#  for (j in 1:ncol(hammingDistMatrix)){
#    if (hammingDistMatrix[i,j]<dmin){
#      dmin = hammingDistMatrix[i,j]
#    } 
#  } 
#  if (dmin >= 3){
#    usableBarcodes = c(usableBarcodes, row.names(hammingDistMatrix)[i])
#  }
#} 


#usableBarcodesMatrix <- matrix(nrow = nrow(hammingDistMatrix), ncol = 1)
#rownames(usableBarcodesMatrix)<-queryBarcodes
#colnames(usableBarcodesMatrix)="USE?"
#indx <- match(usableBarcodes, queryBarcodes)
#usableBarcodesMatrix[indx,]<-"YES"
#usableBarcodesMatrix[-indx,]<-"NO"

#rm(indx)