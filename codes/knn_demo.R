library(ggplot2)

type = c(rep('A',5), rep('B', 5))
cols = c('#7FAED9','#B5739D')
names(cols)=c('A','B')
shapes = c('circle','triangle')
data = data.frame(x=c(2,3,4,5,6,6.5,7,9,10,10), y=c(5,3,6,4,6.5,3,4,2,1,3), type=type)

p =ggplot() +
  geom_point(data=data, mapping = aes(x=x,y=y, colour=type, shape=type), size = 3)

knn = p +
  annotate('point', x=6, y=3.5, size = 3, shape = 'square') +
  annotate('text', x=6.2, y=3.5, label='?' ) +
  annotate('text', x=6.3, y=3.9, label='K=3') +
  annotate('point',x=6, y=3.5, size = 70, alpha = 0.1) +
  theme_void() +
  scale_colour_manual(name=NULL,values = cols) +
  scale_shape_manual(name=NULL, values = shapes) +
  theme(legend.position='top',
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(6, 6, 6, 6))
knn


pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/knn_demo.pdf', width = 5, height = 4)
knn
dev.off()

# imbalanced design

type = c(rep('A',3), rep('B', 9))
cols = c('#7FAED9','#B5739D')
names(cols)=c('A','B')
shapes = c('circle','triangle')
data = data.frame(x=c(6,5.5,5,6.5,8,8,7,7.5,8.5,8.5,6,7.5), y=c(6.5,4,5,3,4,2,2.5,2,1.5,2,2.2,3), type=type)

p =ggplot() +
  geom_point(data=data, mapping = aes(x=x,y=y, colour=type, shape=type), size = 3)

knn = p +
  annotate('point', x=6, y=3.5, size = 3, shape = 'circle') +
  # annotate('text', x=6.2, y=3.5, label='?' ) +
  # annotate('text', x=6.3, y=3.9, label='K=3') +
  annotate('point',x=6, y=3.5, size = 70, alpha = 0.1) +
  theme_void() +
  scale_colour_manual(name=NULL,values = cols) +
  scale_shape_manual(name=NULL, values = shapes) +
  theme(legend.position='top',
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(6, 6, 6, 6))
knn

pdf('/Users/phuongle/Documents/repos/honours_thesis/graphics/imbalanced.pdf', width = 5, height = 4)
knn
dev.off()




