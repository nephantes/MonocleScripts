`2h_TophatHisat` <- read.delim("~/Downloads/2h_TophatHisat.tsv", header=FALSE) #file you sent me

`2h_TophatHisat`[,1] -> genes
as.vector(genes) -> genes
`2h_TophatHisat`[,2:3] -> graph
genes -> row.names(graph)
c("X", "Y") -> colnames(graph)

plot(graph)

###### EVERYTHING BELOW IS EXTRA, adds best fit line, r2, pvalue to graph ######

lm(graph) -> statSummary #calculate r2 and other stats
statSummary$coefficients #gives intercept and slope for best fit line, plotted below
abline(lm(graph)) #plots linear regression line on graph

summary(statSummary) -> modelSummary #just type modelSummary to see this output
r2 = modelSummary$adj.r.squared #r2 value
mylabelr2 = bquote(italic(R)^2 == .(format(r2, digits = 3))) #r2 label creation
text(x = 2000, y = 22000, labels = mylabelr2) #plots r2 label, x and y are the position

my.p = modelSummary$coefficients[2,4] # p value
mylabelpval = bquote(italic(pval) == .(format(my.p, digits = 3)))
text(x = 4700, y = 20000, labels = mylabelpval)

### Residuals based on line of best fit ###
abs(modelSummary$residuals) -> absResidual #orders residuals
plot(absResidual) 
mean(abs(modelSummary$residuals)) -> meanResidual #mean residual 
sd(abs(modelSummary$residuals)) -> sdResidual #sd residual
which((abs(modelSummary$residuals)) > (meanResidual + 2*sdResidual)) -> outlierResidual #2sd residuals
(meanResidual + 2*sdResidual) -> residSDline #adds residual line
abline(residSDline, 0)

#One point is much much higher than the rest... so this will filter points based on some value you can assign
#Not necessary but will help with scaling, feel free to use or not

(which(graph[,2] > 5000)) -> Index #this prints the position in the vector that satisfies the condition ie X > 5000
row.names(graph[Index,]) -> identity #this is the gene that is over your cutoff value
graph[-Index,] -> graphtrim #the fourth row is over 5000, so we are trimming to include all rows BUT A2M
abline(lm(graphtrim))
#plot(graphtrim)