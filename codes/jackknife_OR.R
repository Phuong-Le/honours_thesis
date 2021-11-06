library(ggplot2)

jackknife = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/homogeneity_post_analysis/jackknifed_or.tsv.gz', sep = '\t', header = T)
jackknife = jackknife[!jackknife$disease %in% c('Breast-DCIS','Breast-LobularCa'),]


cols = c('observed'='#330066','jackknifed'='#67AB9F')
shapes = c('observed'=11,'jackknifed'=19)
p = ggplot(data=jackknife, mapping = aes(x=disease)) +
  geom_errorbar(mapping = aes(ymin= (jackknifed_odds_ratio-standard_error), ymax = (jackknifed_odds_ratio+standard_error)), width = 0.1, color = '#67AB9F') +
  geom_point(mapping = aes(y=jackknifed_odds_ratio, colour = 'jackknifed', shape = 'jackknifed'), size = 1.5) +
  geom_point(mapping = aes(y=odds_ratio, colour='observed', shape = 'observed'), alpha = 0.9, size=1.5) +
  scale_colour_manual(name = NULL, values = cols) +
  scale_shape_manual(name = NULL, values = shapes) +
  xlab('') +
  ylab('Odds ratio') +
  ylim(0.5,2.3) +
  geom_hline(yintercept=1, size = 0.9) +
  annotate('text',x=11,y=0.92,label='unbiased threshold', size=4) +
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
pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/jackknife_OR.pdf',width=8,height=5)
p
dev.off()




or = read.table('/Users/phuongle/Documents/repos/PhuongAnalysis/results/loop2/gle/homogeneity_post_analysis/homogeneity_post_analysis_1_cancer.tsv.gz', sep='\t', header = T)
or = or[!or$disease %in% c('Breast-DCIS','Breast-LobularCa'),]
plot(log(or$total.number.of.mutations), or$odds.ratio)
mod=lm(or$odds.ratio ~ or$total.number.of.mutations)
summary(mod)
