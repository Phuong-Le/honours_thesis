library(honours)
library(viridis)
library(reshape2)

# good
good_matrix = diag(5)
diag(good_matrix) = c(6,8,5,4,5)
colnames(good_matrix) = c('Skin-Melanoma','Prost-AdenoCA','Liver-HCC','Breast-AdenoCA','Panc-AdenoCA')
draw_confusion_matrix_static(good_matrix,color = rocket(100),main = 'Very accurate')

#bad
bad_matrix = matrix(0,nrow=5,ncol=5)
bad_matrix[1,3]=4; bad_matrix[1,5]=2
bad_matrix[2,1]=2;bad_matrix[2,4]=6
bad_matrix[3,1]=1;bad_matrix[3,2]=1;bad_matrix[3,4]=1;bad_matrix[3,5]=2
bad_matrix[4,2]=3;bad_matrix[4,1]=1
bad_matrix[5,4]=5
colnames(bad_matrix) = c('Skin-Melanoma','Prost-AdenoCA','Liver-HCC','Breast-AdenoCA','Panc-AdenoCA')
draw_confusion_matrix_static(bad_matrix,color = rocket(100),main = 'Very inaccurate')

# gle
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/knn/smooth_euclidean/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='smooth',cellnote_size = 10)

# sce
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn/5mer/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='5-mer',cellnote_size = 10)

# combined 
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/combined/knn/train_coefs/confustion_matrix_0.30000000000000004_0.7.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='3-mer & smooth',cellnote_size = 10)


# sce submotif
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn/5mer/size2/5mer_3_subkmer_2/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='base change & pos +1, +2',cellnote_size = 10)

# symmetric
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn/mut_symmetric/5mer/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='1 mer sym',cellnote_size = 10)



# ods
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/odds_ratio_mix_diseases/mixed_or.tsv', header=T)
or = dcast(table, disease_test ~ disease_template)
or = or[,-1]
or=round(or, digits = 4)
or
draw_confusion_matrix_static(or, color=rocket(100), cellnote_size = 8)

table_template=table[,c('disease_template','odds_ratio')]
table_template
p = ggplot(data=table_template,mapping = aes(disease_template,odds_ratio)) +
  geom_violin(fill = violin.col, size=0.3) +
  geom_boxplot(width=0.03,fill=boxplot.col,outlier.colour = boxplot.col) + 
  xlab('cell type') +
  ylab('odds ratio') +
  theme(
    panel.background = element_rect(fill = "#FFFFFF",
                                    size = 0.05, linetype = "solid"),
    panel.grid.major = element_line(size = 0.05, linetype = 'solid',
                                    colour = "black"),
    panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                    colour = "lightblue"),
    axis.text.x = element_text(angle = 60, hjust=1, size=15),
    axis.text.y = element_text(size=15)
  )
p

