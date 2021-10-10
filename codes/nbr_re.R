library(ggplot2)

col_vector = c("#e6194b", "#3cb44b", "#000000", "#911eb4", "#f58231", 
               "#46f0f0", "#aaffc3", "#bcf60c", "#fabebe", "#008080", "#e6beff", 
               "#9a6324", "#f032e6", "#800000", "#4363d8", "#808000", "#ffd8b1", 
               "#000075", "#808080", "#ffe119", "#ffffff")

shape_vector = c("circle", "plus", "triangle", "square", "star", "diamond", 
                 "cross")
table = read.table('~/Downloads/nbr_re_demo.tsv',header=T)
subtable = table[table$disease=='Skin-Melanoma',]

transitions = c('AtoG','GtoA','CtoT','TtoC')
shapes_transitions=shape_vector[1:4]; names(shapes_transitions)=transitions
colour_transitions=col_vector[1:4]; names(colour_transitions)=transitions
subtable_transition = subtable[subtable$direction %in% transitions,]
ggplot(subtable_transition,aes(x=pos,y=RE)) +
  geom_point(aes(colour=direction, shape=direction), size=3) +
  scale_shape_manual(values=shapes_transitions) +
  scale_colour_manual(values=colour_transitions) +
  xlim("pos-2","pos-1","pos1","pos2") +
  ggtitle('transition mutations') + 
  theme(
    axis.text.x = element_text(angle = 60, hjust=1, size=15),
    axis.text.y = element_text(size=15)
  )

transversion = c('AtoC','AtoT','CtoA','TtoA','GtoC','GtoT','CtoG','CtoA')
shapes_transversion=c(shape_vector, shape_vector[1]); names(shapes_transversion)=transversion
colour_transversion=col_vector[14:21]; names(colour_transversion)=transversion
colour_transversion['CtoG']='black'
subtable_transversion = subtable[subtable$direction %in% transversion,]
ggplot(subtable_transversion,aes(x=pos,y=RE)) +
  geom_point(aes(colour=direction, shape=direction), size=3) +
  scale_shape_manual(values=shapes_transversion) +
  scale_colour_manual(values=colour_transversion) +
  xlim("pos-2","pos-1","pos1","pos2") +
  theme(
    axis.text.x = element_text(angle = 60, hjust=1, size=15),
    axis.text.y = element_text(size=15)
  )
  ggtitle('transversion mutations')

