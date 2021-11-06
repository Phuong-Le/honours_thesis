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
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/knn/bins_wasserstein/confusion_matrix.tsv', header = T)
p=draw_confusion_matrix_static(table, color=rocket(100),cellnote_size = 12)

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/confustion_matrix_smooth_wasserstein.pdf',width=5,height=5)
p
dev.off()

# sce
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn/1mer/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), cellnote_size = 10)

# combined
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/combined/knn/train_coefs/confustion_matrix_0.30000000000000004_0.7.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), main='3-mer & smooth',cellnote_size = 10)


# sce submotif
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/mut_all/5mer/combined_size2/loop0/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100),cellnote_size = 10)

# symmetric
table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/sce/knn_loop/semi_symmetric/5mer/loop0/confusion_matrix.tsv', header = T)
draw_confusion_matrix_static(table, color=rocket(100), cellnote_size = 10)

