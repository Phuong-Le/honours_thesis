library(ggplot2)

set.seed(1)

a = rnorm(100,2)
b = rnorm(100,5)

gle = data.frame(cancer=c(rep('Cancer A',100), rep('Cancer B',100)), dens=c(a,b))
cols = c('Cancer A'='#B5739D','Cancer B'='#577794')
linetypes = c('Cancer A'='solid','Cancer B'='twodash')
p = ggplot(data=gle, mapping = aes(x=dens,color=cancer, linetype=cancer)) +
  geom_density() +
  scale_color_manual(name = NULL, values = cols) +
  scale_linetype_manual(name = NULL, values = linetypes) +
  theme_void()
p

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/gle_discussion.pdf',width=8,height=4)
p
dev.off()
