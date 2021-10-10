library(ggplot2)
# gle 
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/gle/f1_summary.tsv', header=T)
ggplot(data=f1, mapping = aes(metric,f1_scores)) +
  geom_boxplot()

# sce
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/f1_summary.tsv',header=T)
ggplot(data=f1, mapping = aes(kmer,f1_scores)) +
  geom_point(aes(colour=mut_type,shape=mut_type))

# sce 5 mer submotif
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/sce/mut_all/5mer/f1_summary.tsv',header=T)
ggplot(data=f1, mapping = aes(mut_type,f1_scores)) +
  geom_boxplot()

