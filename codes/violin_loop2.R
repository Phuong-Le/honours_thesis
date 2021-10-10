library(ggplot2)

violin_file='/Users/phuongle/Documents/repos/PhuongData/loop2_processed/pcawg/violin_plot_data.tsv.gz'
violin_table=read.table(violin_file,header = T)
violin_table=violin_table[!violin_table$Project_Code %in% c('Breast-DCIS','Breast-LobularCa'),]

boxplot.col = '#B5739D'    # '#666633' 
violin.col ='#A9C4EB'   #'#FFCC66'
p = ggplot(data=violin_table,mapping = aes(Project_Code,log10(count))) +
  geom_violin(fill = violin.col, size=0.3) +
  geom_boxplot(width=0.03,fill=boxplot.col,outlier.colour = boxplot.col) + 
  xlab('') +
  ylab('number of mutations (log scale)') +
  theme(
    panel.background = element_rect(fill = "#FFFFFF",
                                    size = 0.05, linetype = "solid"),
    panel.grid.major = element_line(size = 0.05, linetype = 'solid',
                                    colour = "black"),
    panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                    colour = "lightblue"),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 12)
  ) #+
#ggtitle('Mutation density - loop 1')
p


pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/mutation_summary.pdf',width=10,height=5)
p
dev.off()
