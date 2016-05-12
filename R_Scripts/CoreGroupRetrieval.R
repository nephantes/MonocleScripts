load("~/Documents/School/Grad School Home/UMASS Medical/Garber/Data /Unsorted5000reads.RData") 
  #OPT# NORMALIZE!
  cSum <- apply(Unsorted5000reads,2,sum)   
    x.norm <- as.matrix(sweep(Unsorted5000reads,2,cSum,FUN='/'))*1e4 
      #OPT# Run GENECOUNTFILTER!
      #CONSTRUCT HSMMCLASS
      HSMM_expr_matrix <- x.norm

Index <- match(WorkableCoreGroup, colnames(HSMM_expr_matrix))
HSMM_expr_matrix <- HSMM_expr_matrix[,Index]
save(HSMM_expr_matrix, file = "HSMM_State1Removed_coregroup_5000UMI_2fold_4Paths.Rdata")