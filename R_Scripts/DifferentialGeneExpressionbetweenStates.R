#6
# Finding Genes that Distinguish Cell Type or State!
HSMMpDataNot5 <- HSMM_2500_UMI_MinEx1_1SDGenecount_Top2_5Paths
# Vector of States # pData(HSMMpDataNot5)$State
Index <- which(pData(HSMMpDataNot5)$State != 5)
pData(HSMMpDataNot5)$State[Index]<-9

to_be_tested <- ordering_genes
cds_subset <- HSMMpDataNot5[to_be_tested, pData(HSMMpDataNot5)$State == 5 | pData(HSMMpDataNot5)$State == 9]
diff_test_res <- differentialGeneTest(cds_subset, fullModelFormulaStr="expression~State")
diff_test_res <- merge(fData(HSMMpDataNot5), diff_test_res, by="row.names")
### Prints Results ### diff_test_res[,c("gene_short_name", "pval", "qval")]
### Plots results ### plot_genes_jitter(cds_subset, color_by="Media", nrow=1, ncol=NULL, plot_trend=TRUE)