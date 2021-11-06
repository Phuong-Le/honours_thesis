library(ggplot2)

path.them = '/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/combined/knn/train_coefs_bins_euclid_semi_symmetric'
f1=data.frame(row.names = c('matrix1','matrix2','f1_score','accuracy_scores'))
for (i in 0:9){
  path.i = paste(path.them,'/loop',i,'/accuracy_summary.tsv',sep="")
  table=read.table(path.i,header=T)
  f1=rbind(f1,table)
}
m1 = unique(f1$matrix1)
meanf1s = data.frame(row.names = c('matrix1','mean_f1'))
for (m in m1) {
  filtered = f1[f1$matrix1==m,]
  meanf1=data.frame(matrix1=m,mean_f1=mean(filtered$f1_score),stdev=sd(filtered$f1_score))
  meanf1s=rbind(meanf1s,meanf1)
}
meanf1s$type = rep('convention',11)


path.us = '/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/combined/knn/train_coefs'
f1=data.frame(row.names = c('matrix1','matrix2','f1_score','accuracy_scores'))
for (i in 0:9){
  path.i = paste(path.us,'/loop',i,'/accuracy_summary.tsv',sep="")
  table=read.table(path.i,header=T)
  f1=rbind(f1,table)
}
m1 = unique(f1$matrix1)
for (m in m1) {
  filtered = f1[f1$matrix1==m,]
  meanf1=data.frame(matrix1=m,mean_f1=mean(filtered$f1_score),stdev=sd(filtered$f1_score),type='proposed')
  meanf1s=rbind(meanf1s,meanf1)
}

cols = c('proposed'='#B5739D','convention'='#577794')
labels = c('proposed'='smooth/asymmetric 3-mer','convention'='bin/semi-symmetric 3-mer')
p=ggplot(data=meanf1s, mapping = aes(x=as.character(matrix1))) +
  geom_errorbar(mapping = aes(ymin= (mean_f1-stdev), ymax = (mean_f1+stdev), colour = type), width = 0.1) +
  geom_point(mapping = aes(y=mean_f1,colour = type)) +
  ylim(0,1) +
  ylab('F1') +
  xlab('Weight given to GLE') +
  scale_colour_manual(name = NULL, values = cols, labels = labels) +
  theme(
    axis.text.x = element_text(hjust=0.5, size=9),
    axis.text.y = element_text(size=9),
    axis.title.y = element_text(size=9)
  )
p
pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/f1_combined.pdf',width=8,height=5)
p
dev.off()

