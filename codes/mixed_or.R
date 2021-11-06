library(reshape2)
library(ggplot2)
library(viridis)
library(pheatmap)

table = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/odds_ratio_mix_diseases/mixed_or.tsv', header=T)
table.wrong = table[table$disease_test != table$disease_template,]
table.right = table[table$disease_test == table$disease_template,]
table.liver = table[table$disease_test=='Liver-HCC',]
# violin by column of heatmap
p = ggplot() +
  geom_violin(data=table.wrong, mapping = aes(x=disease_template,y=odds_ratio), fill ='#67AB9F') +
  # geom_boxplot(data=table.wrong, mapping = aes(x=disease_template,y=odds_ratio), width=0.03) +
  geom_point(data=table.right,colour = '#330066', mapping = aes(x=disease_template,y=odds_ratio)) +
  xlab('') +
  ylab('Odds ratio') +
  theme(
    panel.background = element_rect(fill = "white",
                                    size = 0.01, linetype = "dotted"),
    panel.grid.major = element_line(size = 0.01, linetype = 'dotted',
                                    colour = "black"),
    panel.grid.minor = element_line(size = 0.01, linetype = 'dotted',
                                    colour = "black"),
    axis.text.x = element_text(angle = 60, hjust=1, size=15),
    axis.text.y = element_text(size=15),
  )



pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/mixed_or_violin.pdf',width=10,height=5)
p
dev.off()

# by row
p = ggplot() +
  geom_violin(data=table.wrong, mapping = aes(x=disease_test,y=odds_ratio), fill ='#67AB9F') +
  # geom_boxplot(data=table.wrong, mapping = aes(x=disease_template,y=odds_ratio), width=0.03) +
  geom_point(data=table.right,colour = '#330066', mapping = aes(x=disease_test,y=odds_ratio)) +
  xlab('') +
  ylab('Odds ratio') +
  coord_flip() +
  theme(
    panel.background = element_rect(fill = "white",
                                    size = 0.01, linetype = "dotted"),
    panel.grid.major = element_line(size = 0.01, linetype = 'dotted',
                                    colour = "black"),
    panel.grid.minor = element_line(size = 0.01, linetype = 'dotted',
                                    colour = "black"),
    axis.text.x = element_text(angle = 60, hjust=1, size=15),
    axis.text.y = element_text(size=15),
  )

p

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/mixed_or_violin_by_row.pdf',width=8,height=10)
p
dev.off()


# heatmap of raw or
or = dcast(table, disease_test ~ disease_template)
or = or[,-1]
or=round(or, digits = 2)
or
color = viridis(12)

# rank wrt cols
transposed = as.matrix(or) %>% t() %>%  as.data.frame()
transposed.ranked=sapply(transposed, rank)
or.ranked = as.matrix(transposed.ranked) %>% t()
colnames(or.ranked) = colnames(or)
rownames(or.ranked) = colnames(or)
or.ranked
setHook("grid.newpage", function() grid::pushViewport(grid::viewport(x = 1,
                                                                     y = 1, width = 0.9, height = 0.9, name = "vp", just = c("right",
                                                                                                                             "top"))), action = "prepend")
p=pheatmap::pheatmap(or.ranked, cluster_cols = F,
                   cluster_rows = F, display_numbers = or,
                   fontsize_number = 12, color = color,
                   number_color = 'black', legend = F)
setHook("grid.newpage", NULL, "replace")
grid::grid.text("DHS data for chromatin status", y = -0.03, gp = grid::gpar(fontsize = 12))
grid::grid.text("Mutation data", x = -0.03, rot = 90, gp = grid::gpar(fontsize = 12))

# rank wrt rows
or.ranked = sapply(or,rank)
colnames(or.ranked) = colnames(or)
rownames(or.ranked) = colnames(or)
or.ranked

setHook("grid.newpage", function() grid::pushViewport(grid::viewport(x = 1,
                                                                     y = 1, width = 0.9, height = 0.9, name = "vp", just = c("right",
                                                                                                                             "top"))), action = "prepend")
p=pheatmap::pheatmap(or.ranked, cluster_cols = F,
                   cluster_rows = F, display_numbers = or,
                   fontsize_number = 10, color = color,
                   fontsize_row = 12, fontsize_col = 12,
                   number_color = 'black', legend = F)
setHook("grid.newpage", NULL, "replace")
grid::grid.text("DHS data for chromatin status", y = -0.03, gp = grid::gpar(fontsize = 12))
grid::grid.text("Mutation data", x = -0.03, rot = 90, gp = grid::gpar(fontsize = 12))

p

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/mixed_or_heatmap.pdf',width=10,height=10)
p
dev.off()
