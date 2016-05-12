example = data.frame(matrix(data = 0, ncol = 26, nrow = 26))
rownames(example) = LETTERS
colnames(example) = LETTERS

colPositions = 1:length(colnames(example))
rowPositions = colPositions

groupNames1 = list(c("A", "B", "C"), c("F", "G", "H"))
groupNames2 =  list(c("A", "B", "C", "D"), c("F", "G", "H", "I"))
groupNames3 =  list(c("A", "B", "C", "E"), c("G", "H", "I", "J"))

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
for(group in uniCoreGroup){
  if(length(group) > 1){
    print(group)
  }
}

