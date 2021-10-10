library(ggplot2)

jackknife = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/homogeneity_post_analysis/jackknifed_or.tsv.gz', sep = '\t', header = T)
jackknife = jackknife[!jackknife$disease %in% c('Breast-DCIS','Breast-LobularCa'),]


cols = c('observed'='#006600','jackknifed'='#996600')
shapes = c('observed'='circle','jackknifed'='diamond')
p = ggplot(data=jackknife, mapping = aes(x=disease)) +
  geom_errorbar(mapping = aes(ymin= (jackknifed_odds_ratio-standard_error), ymax = (jackknifed_odds_ratio+standard_error)), width = 0.1, color = '#996600') +
  geom_point(mapping = aes(y=jackknifed_odds_ratio, colour = 'jackknifed'), size = 1.5) +
  # geom_point(mapping = aes(y=odds_ratio, colour='observed'), alpha = 0.9, size=1.5) +
  scale_colour_manual(name = NULL, values = cols) +
  xlab('cancer type') +
  ylab('Odds ratio') +
  ylim(0.5,2.5) +
  geom_hline(yintercept=1, size = 0.9) +
  annotate('text',x=11,y=0.97,label='unbiased threshold', size=4) +
  theme(
    panel.background = element_rect(fill = "white",
                                    size = 0.05, linetype = "dotted"),
    panel.grid.major = element_line(size = 0.05, linetype = 'dotted',
                                    colour = "black"),
    panel.grid.minor = element_line(size = 0.05, linetype = 'dotted',
                                    colour = "black"),
    axis.text.x = element_text(angle = 50, hjust=1, size=15),
    axis.text.y = element_text(size=15),
    legend.position = "none"
  ) 

# pdf('~/Documents/honours/final_seminar/graphics/jackknife_OR.pdf',width=7,height=5)
p
# dev.off()
