library(ggplot2)
# gle
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/knn_loop/f1_summary_errorbars.tsv', header=T)
cols = c('smooth'='#B5739D','bins'='#577794')
p=ggplot(data=f1, mapping = aes(x=distance)) +
  geom_errorbar(mapping = aes(ymin= (mean-stdev), ymax = (mean+stdev), colour = representation), width = 0.1) +
  geom_point(mapping = aes(y=mean, colour = representation)) +
  scale_colour_manual(name = NULL, values = cols) +
  ylim(0,1) +
  ylab('F1') +
  xlab('') +
  scale_x_discrete(labels = c('wasserstein'='Wasserstein',
                            'euclidean'='Euclidean')) +
  theme(
    axis.text.x = element_text(hjust=0.5, size=15),
    axis.text.y = element_text(size=15),
    axis.title.y = element_text(size=15)
  )
p
pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/f1_gle.pdf',width=7,height=5)
p
dev.off()


# sce
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/f1_summary_errorbars.tsv',header=T)
cols = c('asymmetric'='#B5739D','semi_symmetric'='#577794','full_symmetric'='#000000')
p=ggplot(data=f1, mapping = aes(x=kmer)) +
  geom_errorbar(mapping = aes(ymin= (mean-stdev), ymax = (mean+stdev), colour = mut_type), width = 0.1) +
  geom_point(mapping = aes(y=mean, colour = mut_type)) +
  scale_colour_manual(name = NULL, values = cols,labels = c('asymmetric'='asymmetry',
                                                            'semi_symmetric'='semi-symmetry',
                                                            'full_symmetric'='full-symmetry')) +
  ylim(0,1) +
  ylab('F1') +
  xlab('') +
  theme(
    axis.text.x = element_text(hjust=0.5, size=15),
    axis.text.y = element_text(size=15),
    axis.title.y = element_text(size=15)
  )
p
pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/f1_sce.pdf',width=7.5,height=5)
p
dev.off()



# sce 5 mer submotif
f1=read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/mut_all/5mer/f1_summary_errorbars.tsv',header=T)
p=ggplot(data=f1, mapping = aes(x=mut_type)) +
  geom_errorbar(mapping = aes(ymin= (mean-stdev), ymax = (mean+stdev)), width = 0.1, colour = '#B5739D') +
  geom_point(mapping = aes(y=mean),colour = '#B5739D') +
  ylim(0,1) +
  ylab('F1') +
  xlab('') +
  # scale_x_discrete(labels = c('wasserstein'='Wasserstein',
  #                             'euclidean'='Euclidean')) +
  theme(
    axis.text.x = element_text(hjust=0.5, size=15),
    axis.text.y = element_text(size=15),
    axis.title.y = element_text(size=15)
  )
p
pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/f1_sce_submotif.pdf',width=6,height=5)
p
dev.off()
