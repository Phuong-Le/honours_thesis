library(ggplot2)
library(ggpubr)

muts = c(34:38,47,48,74:86)
mut_density = density(muts, bw='nrd',n=10, from=5, to=95)
mut_bins = hist(muts, breaks = seq(0,100,10))
mut_bins = mut_bins$density

table = data.frame(mutpos=muts, mut_density_x = mut_density$x, mut_density_y=mut_density$y, bins = mut_bins)

cols = c('observed'='black', 'intensity'='#330066', 'bins' = '#31524C')
p1 = ggplot(data=table, mapping=aes(x=mutpos)) +
  geom_histogram(mapping = aes(y = ..density.., colour='bins'),binwidth = 10, fill='#67AB9F', boundary=0) +
  geom_point(aes(x=mut_density_x,y=bins, colour='bins')) +
  geom_density(bw = 'nrd', aes(colour = 'intensity')) +
  geom_point(aes(y=rep(-0.001,length(mutpos)), colour = 'observed'), alpha = 0.5) +
  geom_point(aes(x=mut_density_x,y=mut_density_y, colour = 'intensity')) +
  scale_x_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  scale_colour_manual(name=NULL,values = cols) +
  guides(color=F) +
  ggtitle('(a) bin boundary starts at 0') +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  xlab('chromosomal positions') +
  ylim(-0.001,0.051)

p1 + ylab('density')
# shift 5 units
mut_density = density(muts, bw='nrd',n=10, from=0, to=90)
mut_bins = hist(muts, breaks = seq(-5,95,10))
mut_bins = mut_bins$density

table = data.frame(mutpos=muts,mut_density_x = mut_density$x, mut_density_y=mut_density$y, bins = mut_bins)

#fill = #FFCC66
cols = c('observed'='black', 'intensity'='#330066', 'bins' = '#31524C')
p2 = ggplot(data=table, mapping=aes(x=mutpos)) +
  geom_histogram(mapping = aes(y = ..density.., colour='bins'),binwidth = 10, fill='#67AB9F', boundary = 15) +
  geom_point(aes(x=mut_density_x,y=bins, colour='bins')) +
  geom_density(bw = 'nrd', aes(colour = 'intensity')) +
  geom_point(aes(y=rep(-0.001,length(mutpos)), colour = 'observed'), alpha = 0.5) +
  geom_point(aes(x=mut_density_x,y=mut_density_y, colour = 'intensity')) +
  scale_x_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  scale_colour_manual(name=NULL,values = cols)  +
  guides(color=F) +
  ggtitle('(b) bin boundary starts at 5') +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  xlab('chromosomal positions') +
  ylim(-0.001,0.051)

p2
# combined
p = ggarrange(p1,p2,ncol=1,nrow=2)

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/mutdistribution_demo.pdf', width = 4, height = 6)
p
dev.off()



