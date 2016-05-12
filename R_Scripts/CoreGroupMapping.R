load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths.Rdata")
#load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_3Paths.Rdata")
  HSMM <- XXX
load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /IDOTrees/2500UMI/StateFiltered/CoreGroups/CoreGroup_3match_24size_n7_HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_4Paths.Rdata")

CoreGroup_3match_24size_n7 <- largeUniCoreGroup

Core1indx <- match(CoreGroup_3match_24size_n7[[1]], rownames(pData(HSMM)))
CoreSubset1 <- (pData(HSMM))[Core1indx,][with((pData(HSMM))[Core1indx,], order(-Pseudotime)),]
print(nrow(CoreSubset1))
print(unique(CoreSubset1$State))
print(range(CoreSubset1$Pseudotime))
# CoreGroup1 is # Cells, of State X, Pseudotime Range of Y to Z

Core2indx <- match(CoreGroup_3match_24size_n7[[2]], rownames(pData(HSMM)))
CoreSubset2 <- (pData(HSMM))[Core2indx,][with((pData(HSMM))[Core2indx,], order(-Pseudotime)),]
print(nrow(CoreSubset2))
print(unique(CoreSubset2$State))
print(range(CoreSubset2$Pseudotime))
# CoreGroup2 is # Cells, of State X, Pseudotime Range of Y to Z

Core3indx <- match(CoreGroup_3match_24size_n7[[3]], rownames(pData(HSMM)))
CoreSubset3 <- (pData(HSMM))[Core3indx,][with((pData(HSMM))[Core3indx,], order(-Pseudotime)),]
print(nrow(CoreSubset3))
print(unique(CoreSubset3$State))
print(range(CoreSubset3$Pseudotime))
# CoreGroup3 is # Cells, of State X, Pseudotime Range of Y to Z

Core4indx <- match(CoreGroup_3match_24size_n7[[4]], rownames(pData(HSMM)))
CoreSubset4 <- (pData(HSMM))[Core4indx,][with((pData(HSMM))[Core4indx,], order(-Pseudotime)),]
print(nrow(CoreSubset4))
print(unique(CoreSubset4$State))
print(range(CoreSubset4$Pseudotime))
# CoreGroup4 is # Cells, of State X, Pseudotime Range of Y to Z

Core5indx <- match(CoreGroup_3match_24size_n7[[5]], rownames(pData(HSMM)))
CoreSubset5 <- (pData(HSMM))[Core5indx,][with((pData(HSMM))[Core5indx,], order(-Pseudotime)),]
print(nrow(CoreSubset5))
print(unique(CoreSubset5$State))
print(range(CoreSubset5$Pseudotime))
# CoreGroup5 is # Cells, of State X, Pseudotime Range of Y to Z

Core6indx <- match(CoreGroup_3match_24size_n7[[6]], rownames(pData(HSMM)))
CoreSubset6 <- (pData(HSMM))[Core6indx,][with((pData(HSMM))[Core6indx,], order(-Pseudotime)),]
print(nrow(CoreSubset6))
print(unique(CoreSubset6$State))
print(range(CoreSubset6$Pseudotime))
# CoreGroup6 is # Cells, of State X, Pseudotime Range of Y to Z

Core7indx <- match(CoreGroup_3match_24size_n7[[7]], rownames(pData(HSMM)))
CoreSubset7 <- (pData(HSMM))[Core7indx,][with((pData(HSMM))[Core7indx,], order(-Pseudotime)),]
print(nrow(CoreSubset7))
print(unique(CoreSubset7$State))
print(range(CoreSubset7$Pseudotime))
# CoreGroup7 is # Cells, of State X, Pseudotime Range of Y to Z

