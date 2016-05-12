#1
# Initial Loading of Files/Packages and Variable Assignment
library(monocle)
library(grid)
library(Hmisc)
library(knitr)
library(reshape2)
library(irlba)
  load("/Users/KGellatly/Documents/Work/Garber/Data/IDOTrees/2500UMI/HSMM_2500_UMI_MinEx1_1SDGenecount__1381Cells_Top2_5Paths.Rdata")
  load("/Users/KGellatly/Documents/Work/Garber/Data /CellExpressionMatrices/Allunsortedmyeloid.Rdata")
#####OPT Run FILTERS, Count Filter, Normalize, Genecountfilter, ("/Users/KGellatly/Documents/Work/Comp_Sci/R_Scripts/FILTERS.R")
###OPT CONSTRUCT HSMMCLASS ("/Users/KGellatly/Documents/Work/Comp_Sci/R_Scripts/ConstructHSMMClass.R")
  XXX -> HSMM
    
#2
# Detect Genes that have Minimum Expression Values and Minimum Number of Cells Expressing
HSMM <- detectGenes(HSMM, min_expr = 1)
  ### Prints Results ### print(head(fData(HSMM)))
    expressed_genes <- row.names(subset(fData(HSMM), num_cells_expressed >= 50)) # BEFORE STATE ASSIGNMENT
    
#3  
# Quality Control of Cells and Plot for Expression Values Being Roughly Log Normal
L <- log(exprs(HSMM[expressed_genes,]))
  melted_dens_df <- melt(t(scale(t(L))))
    ### Plots results ### qplot(value, geom="density", data=melted_dens_df) +  stat_function(fun = dnorm, size=0.5, color='red') + xlab("Standardized log(FPKM)") + ylab("Density")
      rm(L)
      rm(melted_dens_df)
      
#4            
# Ordering Cells by Progress
#Next line should be a file of a single column of genes of interest!!!
load("/Users/KGellatly/Documents/Work/Garber/Data/GeneInformation/Top2foldchange.RData") #Top2foldchange #Top10ofeach19Cluster #Allclusterinformative
ordering_genes <- Top2foldchange[,] 
ordering_genes <- intersect(ordering_genes, expressed_genes)
  HSMM <- setOrderingFilter(HSMM, ordering_genes)
  HSMM <- reduceDimension(HSMM, use_irlba= FALSE)
  options(expressions=100000)
  HSMM <- orderCells(HSMM, num_paths=5, reverse= FALSE) # 4/7 comes from, Thrombocytes Erythrocytes Lymphocytes Granulocytes Dendritic Monocytes Macrophages
    ### which((HSMM@reducedDimS < -5)[1,])        
    ### Plots results ### plot <- plot_spanning_tree(HSMM, color_by = "State")
      # range((pData(HSMM)[which(pData(HSMM)$State == 6),])$Pseudotime)  #Tells you range of pseudotime values of a state
      HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths <- HSMM
      save(HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths, file = "HSMM_2500_UMI_MinEx1_1SDGenecount_RmState6_7_4PseudotimeG20_811Cells_AllCluster_4Paths.RData")
      
#5
# Finding Genes that Distinguish Cell Type or State!
to_be_tested_state <- ordering_genes
  cds_subset_state <- HSMM[to_be_tested_state, pData(HSMM)$State == 5 | pData(HSMM)$State == 4] #Select States of Interest
    cds_subset_state <- detectGenes(cds_subset_state, min_expr = 1) #Recalculate num_cells_expressed
      indx <- which(fData(cds_subset_state)[,2]>=max(fData(cds_subset_state)[,2])*0.3) #Extract genes with minimum .2*num_cells_expressed
        fData(cds_subset_state)[indx,] -> fData(cds_subset_state) #overwrite thaese genes onto the cds_subset_state
          cds_subset_state <- HSMM[row.names(fData(cds_subset_state)), pData(HSMM)$State == 5 | pData(HSMM)$State == 4] #now, redefine cds_subset_state to only look at those genes
            rm(indx)
diff_test_res_state <- differentialGeneTest(cds_subset_state, fullModelFormulaStr="expression~State")
  State5toAll <- diff_test_res_state[with(diff_test_res_state, order(pval)),] ###Order###
    Top15Plot_state <- rownames(State5toAll[1:15,])
      ### Plots results ### plot_genes_jitter(cds_subset_state[Top15Plot_state,], color_by="State", nrow=3, ncol=5, plot_trend=TRUE)
      ### Plots results ### plot_genes_jitter(cds_subset_state, color_by="State", nrow=10, ncol=6, plot_trend=TRUE)

# Below tool will create a new cds, where all states other than one specified have been overwritten!!
HSMMState5toAll <- HSMM
to_be_tested_state <- ordering_genes
# Vector of States # pData(HSMMState5toAll)$State
  Index <- which(pData(HSMMState5toAll)$State != 5)
  Index2 <- which(pData(HSMMState5toAll)$State == 6)
    pData(HSMMState5toAll)$State[Index]<-9  
    pData(HSMMState5toAll)$State[Index2]<-6
      cds_subset_state <- HSMMState5toAll[to_be_tested_state, pData(HSMMState5toAll)$State == 5] #Select States of Interest
        cds_subset_state <- detectGenes(cds_subset_state, min_expr = 1) #Recalculate num_cells_expressed
          indx <- which(fData(cds_subset_state)[,2]>=max(fData(cds_subset_state)[,2])*0.3) #Extract genes with minimum .2*num_cells_expressed
            fData(cds_subset_state)[indx,] -> fData(cds_subset_state) #overwrite these genes onto the cds_subset_state
              cds_subset_state <- HSMMState5toAll[row.names(fData(cds_subset_state)), pData(HSMMState5toAll)$State == 5 | pData(HSMMState5toAll)$State == 9] #now, redefine cds_subset_state to only look at those genes
              rm(Index)
              rm(indx)
diff_test_res_state <- differentialGeneTest(cds_subset_state, fullModelFormulaStr="expression~State")
  State5toAll <- diff_test_res_state[with(diff_test_res_state, order(pval)),]  ###Order###
    Top15Plot_state <- rownames(State5toAll[1:15,])
      ### Plots results ### plot_genes_jitter(cds_subset_state[Top15Plot_state,], color_by="State", nrow=3, ncol=5, plot_trend=TRUE)

#6
# Finding Genes that Change as a Function of Pseudotime
to_be_tested_time <- ordering_genes
  cds_subset_time <- HSMM[to_be_tested_time, pData(HSMM)$State == 5] #Select States of Interest
    cds_subset_time <- detectGenes(cds_subset_time, min_expr = 1) #Recalculate num_cells_expressed
        indx <- which(fData(cds_subset_time)[,2]>=max(fData(cds_subset_time)[,2])*0.3) #Extract genes with minimum .2*num_cells_expressed
          fData(cds_subset_time)[indx,] -> fData(cds_subset_time) #overwrite thaese genes onto the cds_subset_time
            cds_subset_time <- HSMM[row.names(fData(cds_subset_time)), pData(HSMM)$State == 5] #now, redefine cds_subset_state to only look at those genes
            #range(fData(cds_subset_time)$num_cells_expressed)
            #nrow(fData(cds_subset_time))
            rm(indx)
diff_test_res_time <- differentialGeneTest(cds_subset_time, fullModelFormulaStr="expression~sm.ns(Pseudotime)")
    timetestordered <- diff_test_res_time[with(diff_test_res_time, order(pval)),] ###Order###
    #length(which(timetestordered$pval <.01))
      Top15Plot_time <- rownames(timetestordered[1:15,])
      Top30Plot_time <- rownames(timetestordered[16:30,])
      Top45Plot_time <- rownames(timetestordered[31:45,])
        ### Plots results ### plot_genes_in_pseudotime(cds_subset_time[Top15Plot_time,], color_by="Pseudotime", nrow=3, ncol=5)
      
