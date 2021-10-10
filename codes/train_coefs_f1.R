library(ggplot2)

path = '/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/combined/knn/train_coefs_bins_euclid_mut_symmetric'
f1=data.frame(row.names = c('matrix1','matrix2','f1_score','accuracy_scores'))
for (i in 0:9){
  path.i = paste(path,'/loop',i,'/accuracy_summary.tsv',sep="")
  table=read.table(path.i,header=T)
  f1=rbind(f1,table)
}
f1

m1 = unique(f1$matrix1)
meanf1s = data.frame(row.names = c('matrix1','mean_f1'))
for (m in m1) {
  filtered = f1[f1$matrix1==m,]
  meanf1=data.frame(matrix1=m,mean_f1=median(filtered$f1_score))
  meanf1s=rbind(meanf1s,meanf1)
}

ggplot() +
  geom_boxplot(data=f1,mapping = aes(as.character(matrix1),f1_score)) +
  xlab('GLE contributed') +
  scale_x_discrete() 
  

