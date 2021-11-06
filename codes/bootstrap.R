library(tibble)
library(pheatmap)

table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/pairwise_aggregated/bootstrap/bootstrap_gle_loop_smooth_euclidean.tsv', header = T)
table=column_to_rownames(table, var = "names")

coloured.matrix=table
diag(table)=1000
coloured.matrix[coloured.matrix>0]=1
diag(coloured.matrix)=2

color = c('black','#C5FFF5','white')

# wasserstein
p=pheatmap(coloured.matrix, cluster_cols = F, #show_rownames = F,
         cluster_rows = F, display_numbers = table/1000,
         fontsize_number = 8, color = color,
         number_color = 'black', legend = F)

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/bootstrap_smooth_wasserstein.pdf',width=5.4,height=5)
p
dev.off()

# euclidean
p=pheatmap(coloured.matrix, cluster_cols = F, show_rownames = F,
           cluster_rows = F, display_numbers = table/1000,
           fontsize_number = 8, color = color,
           number_color = 'black', legend = F)

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/bootstrap_smooth_euclidean.pdf',width=4,height=5)
p
dev.off()
