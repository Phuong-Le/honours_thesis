library(ggplot2)
library(tibble)
library(ggrepel)
library(honours)

distance_matrix=read.table('/Users/phuongle/Documents/repos/PhuongData/loop2_processed/encode/chromatin_summary/distances.tsv', header=T)
headers = distance_matrix[,1]
distance_matrix = distance_matrix[,-1]
colnames(distance_matrix) = headers

# check that the distance matrix is symmetric
test_matrix = data.matrix(distance_matrix)
stopifnot(test_matrix == t(test_matrix))
#pca
dimensions = 8
pca = distance_table_to_pca(distance_matrix,dimensions)
# PC 2 vs PC1
pc.x = 1
pc.y = 2
pca = rownames_to_column(pca, var = 'cancer_type')
p1 = ggplot(data = pca, mapping = aes(x=pca[,paste('PC',pc.x,sep='')],y=pca[,paste('PC',pc.y,sep='')], label=cancer_type)) +
  geom_text_repel() +
  geom_point(colour = '#B5739D') +
  labs(x=paste('principal coordinate ',pc.x, ' (',round((variance_proportion(pca,dimensions)[pc.x]),2),'% explained)',sep=''),
       y=paste('principal coordinate ', pc.y, ' (',round((variance_proportion(pca,dimensions)[pc.y]),2),'% explained)',sep=''))

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/encode_pca_1_2.pdf',width=4.5,height=5)
p1
dev.off()

# PC3 vs PC1
pc.x = 1
pc.y = 3
p2 = ggplot(data = pca, mapping = aes(x=pca[,paste('PC',pc.x,sep='')],y=pca[,paste('PC',pc.y,sep='')], label=cancer_type)) +
  geom_text_repel() +
  geom_point(colour = '#B5739D') +
  labs(x=paste('principal coordinate ',pc.x, ' (',round((variance_proportion(pca,dimensions)[pc.x]),2),'% explained)',sep=''),
       y=paste('principal coordinate ', pc.y, ' (',round((variance_proportion(pca,dimensions)[pc.y]),2),'% explained)',sep=''))
p2

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/encode_pca_1_3.pdf',width=4.5,height=5)
p2
dev.off()
