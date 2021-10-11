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
pc.x = 1
pc.y = 2
pca = rownames_to_column(pca, var = 'cancer_type')
ggplot(data = pca, mapping = aes(x=pca[,paste('PC',pc.x,sep='')],y=pca[,paste('PC',pc.y,sep='')], label=cancer_type)) +
  geom_text_repel() +
  geom_point(colour = '#B5739D') +
  labs(x=paste('principal coordinate ',pc.x, ' (',round((variance_proportion(pca,dimensions)[pc.x]),2),'% explained)',sep=''),
       y=paste('principal coordinate ', pc.y, ' (',round((variance_proportion(pca,2)[dimensions]),2),'% explained)',sep=''))

