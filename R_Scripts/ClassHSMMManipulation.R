#get phenoData data frrame
HSMM@phenoData@data

#access column name State
HSMM@phenoData@data["State"] #reutrn dataframe (contains row name, etc)
HSMM@phenoData@data[["State"]] #just vector
HSMM@phenoData@data$State #just vector

#get row name, in this case cells
rownames(HSMM@phenoData@data) 

#return vector of bools whether state equals 1 
HSMM@phenoData@data$State == 1

#get rows that have State equal to one
x <- row(HSMM@phenoData@data[HSMM@phenoData@data$State == 1,])

#get rows that have State equal to one and psuedotimes gerater than 5
x <- rownames(HSMM@phenoData@data[HSMM@phenoData@data$State == 1 & HSMM@phenoData@data$Pseudotime > 2,])
HSMM@phenoData@data[HSMM@phenoData@data$State == 1 & HSMM@phenoData@data$Pseudotime < 5,c("Pseudotime")]

#get rownames(in this case cells), of all the rows that have state of 1
rownames(HSMM@phenoData@data[HSMM@phenoData@data$State == 1,])

#Sort / Order by Pseudotime
x <- arrange(x, Pseudotime)

#Remove Cells by State
Index <- match(x,colnames(HSMM_expr_matrix))
HSMM_expr_matrix <- HSMM_expr_matrix[,-Index]

#Other notation??
rownames(pData(HSMM))
pData(HSMM)$State