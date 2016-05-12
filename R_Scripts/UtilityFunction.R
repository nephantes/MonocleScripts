load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top2_4Paths.Rdata")

HSMM2 <- HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top2_4Paths
rm(HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top2_4Paths)
names <- rownames(HSMM2@phenoData@data)  
numcells <- length(names)
example = data.frame(matrix(data = 0, ncol = numcells, nrow = numcells))
rownames(example) = names
colnames(example) = names
colPositions = 1:length(colnames(example))
rowPositions = colPositions


createStateList<-function(data){
  outputData = list()
  numberOfStates <- length(unique(pData(data)$State))
  for(state in 1:numberOfStates){
    outputData[state] = list(rownames(data@phenoData@data[data@phenoData@data$State == state,]))
  }
  return (outputData)
}

groupNames1 = createStateList(HSMM2)

load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top10_4Paths.RData")
HSMM10 <- HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top10_4Paths
rm(HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_Top10_4Paths)
groupNames2 = createStateList(HSMM10);

load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths.RData")
HSMMAllCluster <- HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths
rm(HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths)
groupNames3 = createStateList(HSMMAllCluster);

allGroups = list(groupNames1, groupNames2, groupNames3)
for(set in allGroups){
  for(state in set){
    for (cellName1 in state){
      for(cellName2 in state){
        if(cellName1 != cellName2){
          example[rowPositions[rownames(example) == cellName1], colPositions[colnames(example) == cellName2]] = example[rowPositions[rownames(example) == cellName1], colPositions[colnames(example) == cellName2]] + 1
        }
      }
    }
  }
}

coreGroups = list()
for(r in 1:length(rownames(example))){
  currentGroup = c()
  for(c in 1:length(colnames(example))){
    if(example[r,c] >= 3){
      currentGroup = c(currentGroup, colnames(example)[c])
    }
  }
  currentGroup = c(currentGroup, rownames(example)[r])
  coreGroups = append(coreGroups,list(sort(currentGroup)))
}

uniCoreGroup = unique(coreGroups)
largeUniCoreGroup = list()
for(group in uniCoreGroup){
  if(length(group) > 24){
    #print(group)
    #print(length(group))
    #print(class(group))
    largeUniCoreGroup = append(largeUniCoreGroup, list(group))
  }
}

save(largeUniCoreGroup, file = "CoreGroup_3match_24size_n7_HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_4Paths.Rdata")

#Subset Total Data set to only core cells 
WorkableCoreGroup <- unlist(largeUniCoreGroup)
  indx <- match(WorkableCoreGroup, rownames(pData(HSMM)))
    CoreSubset <- (pData(HSMM))[indx,]
      CoreSubsetOrdered <- (pData(HSMM))[indx,][with((pData(HSMM))[indx,], order(-State)),]
      
      Core1indx <- match(CoreGroups_n5_30_7group[[1]], rownames(pData(HSMM)))
      CoreSubset1 <- (pData(HSMM))[Core1indx,][with((pData(HSMM))[Core1indx,], order(-Pseudotime)),]
      print(nrow(CoreSubset1))

# Subset Total Data set to only core cells 
TotalCoreGroup <- unlist(largeUniCoreGroup)
indx <- match(TotalCoreGroup, rownames(pData(HSMM)))
CoreSubset <- (pData(HSMM))[indx,]


