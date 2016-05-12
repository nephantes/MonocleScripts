#1
# Initial Loading of Files/Packages and Variable Assignment
library(monocle)
library(grid)
library(Hmisc)
library(knitr)
library(reshape2)
library(HSMMSingleCell)
  data(HSMM_expr_matrix)
  data(HSMM_gene_annotation)
  data(HSMM_sample_sheet)
  HSMM_sample_sheet <- HSMM_sample_sheet[,1:5]
  HSMM_gene_annotation <- HSMM_gene_annotation[,1:2]
    pd <- new("AnnotatedDataFrame", data = HSMM_sample_sheet)
    fd <- new("AnnotatedDataFrame", data = HSMM_gene_annotation)
      HSMM <- newCellDataSet(as.matrix(HSMM_expr_matrix), phenoData = pd, featureData = fd)
#2      
# Detect Genes that have Minimum Expression Values and Minimum Number of Cells Expressing
HSMM <- detectGenes(HSMM, min_expr = 0.1)
  ### Prints Results ### print(head(fData(HSMM)))
    expressed_genes <- row.names(subset(fData(HSMM), num_cells_expressed >= 50))
#3  
# Quality Control of Cells and Plot for Expression Values Being Roughly Log Normal
# Vignette Data pre-filtered # valid_cells <- row.names(subset(pData(HSMM), Cells.in.Well == 1 & Control == FALSE & Clump == FALSE & Debris == FALSE & Mapped.Fragments > 1000000))
# Vignette Data pre-filtered # HSMM <- HSMM[,valid_cells]
L <- log(exprs(HSMM[expressed_genes,]))
  melted_dens_df <- melt(t(scale(t(L))))
    ### Plots results ### qplot(value, geom="density", data=melted_dens_df) +  stat_function(fun = dnorm, size=0.5, color='red') + xlab("Standardized log(FPKM)") + ylab("Density")
#4    
# Differential Expression analysis!
marker_genes <- row.names(subset(fData(HSMM), gene_short_name %in% c("MEF2C", "MEF2D", "MYF5", "ANPEP", "PDGFRA", "MYOG", "TPM1", "TPM2", "MYH2", "MYH3", "NCAM1", "TNNT1", "TNNT2", "TNNC1", "CDK1", "CDK2", "CCNB1", "CCNB2", "CCND1", "CCNA1", "ID1")))
  diff_test_res <- differentialGeneTest(HSMM[marker_genes,], fullModelFormulaStr="expression~Media")
    sig_genes <- subset(diff_test_res, qval < 0.1)
    sig_genes <- merge(fData(HSMM), sig_genes, by="row.names")
      ### Prints Results ### sig_genes[,c("gene_short_name", "pval", "qval")]
        # *R has encountered a fatal error*, keeps crashing?!?! # MYOG_ID1 <- HSMM[row.names(subset(fData(HSMM), gene_short_name %in% c("MYOG", "ID1"))),]
        # *R has encountered a fatal error*, keeps crashing?!?! # plot_genes_jitter(MYOG_ID1, grouping="Media", ncol=2) 
#5    
# Ordering Cells by Progress
# Below not run, will identify genes diff exp between media switches on FULL Data set
# differentialGeneTest(HSMM[expressed_genes,], fullModelFormulaStr="expression~Media")
# ordering_genes <- row.names (subset(diff_test_res, qval < 0.01))
ordering_genes <- row.names (subset(diff_test_res, qval < 0.1))
ordering_genes <- intersect(ordering_genes, expressed_genes)
  HSMM <- setOrderingFilter(HSMM, ordering_genes)
  HSMM <- reduceDimension(HSMM, use_irlba= FALSE)
  HSMM <- orderCells(HSMM, num_paths=2, reverse= TRUE)
    ### Plots results ### plot_spanning_tree(HSMM)
      HSMM_filtered <- HSMM[expressed_genes, pData(HSMM)$State != 3]
        my_genes <- row.names(subset(fData(HSMM_filtered), gene_short_name %in% c("CDK1", "MEF2C", "MYH3")))
          cds_subset <- HSMM_filtered[my_genes,]
            ### Plots results ### plot_genes_in_pseudotime(cds_subset, color_by="Hours")
#6
# Finding Genes that Distinguish Cell Type or State!
to_be_tested <- row.names(subset(fData(HSMM),gene_short_name %in% c("TBP", "MYH3", "NCAM1", "PDGFRA", "ANPEP")))
  cds_subset <- HSMM[to_be_tested, pData(HSMM)$State != 1]
    diff_test_res <- differentialGeneTest(cds_subset, fullModelFormulaStr="expression~State")
    diff_test_res <- merge(fData(HSMM), diff_test_res, by="row.names")
      ### Prints Results ### diff_test_res[,c("gene_short_name", "pval", "qval")]
        ### Plots results ### plot_genes_jitter(cds_subset, color_by="Media", nrow=1, ncol=NULL, plot_trend=TRUE)
          # Another way of calling the full vs reduced models
          # full_model_fits <- fitModel(cds_subset,  modelFormulaStr="expression~State")
            # reduced_model_fits <- fitModel(cds_subset, modelFormulaStr="expression~1")
              # diff_test_res
#7
# Finding Genes that Change as a Function of Pseudotime
to_be_tested <- row.names(subset(fData(HSMM), gene_short_name %in% c("MYH3", "MEF2C", "CCNB2", "TNNT1")))
  cds_subset <- HSMM[to_be_tested, pData(HSMM)$State != 3]
    diff_test_res <- differentialGeneTest(cds_subset, fullModelFormulaStr="expression~sm.ns(Pseudotime)")
    diff_test_res <- merge(fData(HSMM), diff_test_res, by="row.names")
      ### Prints Results ### diff_test_res[,c("gene_short_name", "pval", "qval")]
        ### Plots results ### plot_genes_in_pseudotime(cds_subset, color_by="Hours")
#8
# Multifactorial Differential Expression Analysis
to_be_tested <- row.names(subset(fData(HSMM), gene_short_name %in% c("MT2A", "REXO2", "HDAC4")))
  cds_subset <- HSMM[to_be_tested, pData(HSMM)$Media == "DM" & pData(HSMM)$State != 1]
    diff_test_res <- differentialGeneTest(cds_subset, fullModelFormulaStr="expression~State * Hours", reducedModelFormulaStr="expression~Hours")
    diff_test_res <- merge(fData(cds_subset), diff_test_res, by="row.names")
      ### Prints Results ### diff_test_res[,c("gene_short_name", "pval", "qval")]
        ### Plots results ### plot_genes_jitter(cds_subset, grouping="Hours", color_by="State", plot_trend=TRUE) + facet_wrap( ~ feature_label, scales="free_y")
#9
# Clustering Genes by Pseudotemporal Expression Pattern
sampled_gene_cds <- HSMM_filtered[sample(nrow(fData(HSMM_filtered)), 100),]
  full_model_fits <- fitModel(sampled_gene_cds,  modelFormulaStr="expression~sm.ns(Pseudotime, df=3)")
    expression_curve_matrix <- responseMatrix(full_model_fits)
      dim(expression_curve_matrix)
        clusters <- clusterGenes(expression_curve_matrix, k=4)
          ### Plots results ### *extra lines on graph??* plot_clusters(HSMM_filtered[ordering_genes,], clusters) #drawSummary = FALSE)#