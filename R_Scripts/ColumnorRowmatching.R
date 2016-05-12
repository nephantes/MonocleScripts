name1 <- colnames(x)
name2 <- colnames(y)
pos <- match(name2,name1)
Myeloidonly <- FullCellData[,pos]