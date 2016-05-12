plot_clusters_ownTest <-function (cds, clustering, drawSummary = TRUE, sumFun = mean_cl_boot, 
          ncol = NULL, nrow = NULL, row_samples = NULL, callout_ids = NULL) 
{
  m <- as.data.frame(clustering$exprs)
  m$ids <- rownames(clustering$exprs)
  if (is.null(clustering$labels) == FALSE) {
    m$cluster = factor(clustering$labels[clustering$clustering], 
                       levels = levels(clustering$labels))
  }
  else {
    m$cluster <- factor(clustering$clustering)
  }
  m.melt <- melt(m, id.vars = c("ids", "cluster"))
  m.melt <- merge(m.melt, pData(cds), by.x = "variable", by.y = "row.names")
  if (is.null(row_samples) == FALSE) {
    m.melt <- m.melt[sample(nrow(m.melt), row_samples), ]
  }

  c <- ggplot(m.melt)
  c <- c + stat_density2d(aes(x = Pseudotime, y = value), geom = "polygon", 
                          fill = "white", color = "black", size = I(0.1)) + facet_wrap("cluster", 
                                                                                       ncol = ncol, nrow = nrow)
  if (drawSummary) {
    c <- c + stat_summary(aes(x = Pseudotime, y = value, 
                              group = 1), fun.data = sumFun, color = "red", fill = "black", 
                          alpha = 0.2, size = 0.5, geom = "smooth")
  }
  c <- c + scale_color_hue(l = 50, h.start = 200) + theme(axis.text.x = element_text(angle = 0, 
                                                                                     hjust = 0)) + xlab("Pseudo-time") + ylab("Expression")
  c <- c + theme(strip.background = element_rect(colour = "white", 
                                                 fill = "white")) + theme(panel.border = element_blank(), 
                                                                          axis.line = element_line(size = 0.2)) + theme(axis.ticks = element_line(size = 0.2)) + 
    theme(legend.position = "none") + theme(panel.grid.minor.x = element_blank(), 
                                            panel.grid.minor.y = element_blank()) + theme(panel.grid.major.x = element_blank(), 
                                                                                          panel.grid.major.y = element_blank())
  if (is.null(callout_ids) == FALSE) {
    callout_melt <- subset(m.melt, ids %in% callout_ids)
    c <- c + geom_line(aes(x = Pseudotime, y = value), data = callout_melt, 
                       color = I("steelblue"))
  }
  c <- c + monocle:::monocle_theme_opts()
  c
}