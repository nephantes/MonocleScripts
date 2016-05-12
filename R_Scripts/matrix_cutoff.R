overlapmatrix

cutOff = 50
outputList = data.frame()
for(rNum in 1:nrow(overlapmatrix)){
  for(cNum in 1:rNum){
    if(rNum != cNum){
      if(overlapmatrix[rNum,cNum] > cutOff){
        outputList = rbind(outputList,data.frame(rName = rownames(overlapmatrix)[rNum], cName =colnames(overlapmatrix)[cNum], val = overlapmatrix[rNum,cNum] ))
      }
    }
  }
}