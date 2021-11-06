library(xtable)

table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/homogeneity_post_analysis/homogeneity_post_analysis_1_cancer.tsv.gz', header=T, sep='\t')
table
keep.cols = c('disease','G.pval','total.number.of.mutations')
out = table[,keep.cols]
out$G.pval = p.adjust(out$G.pval, 'bonferroni')
out
xtable(out)
