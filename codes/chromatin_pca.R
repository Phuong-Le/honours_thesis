library(tibble)
col_vector_fixed = c("#e6194b", "#3cb44b", "#ffe119", "#000000", "#f58231", "#911eb4", 
               "#46f0f0", "#aaffc3", "#bcf60c", "#fabebe", "#008080", "#e6beff", 
               "#9a6324", "#f032e6", "#800000", "#4363d8", "#808000", "#ffd8b1", 
               "#000075", "#808080", "#ffffff", "#aaffc3")

shape_vector = c("circle", "plus", "triangle", "square", "star", "diamond", 
                 "cross")
shape_vector_fixed = c(shape_vector,shape_vector[1:4])

pc_names = function(pc_number) {
  #' Generates a vector of principal component names
  #' @description takes a number of desired dimensions,
  #' returns a string that will be pc_dataframe names
  #' @param pc_number: numeric, the desired number of principal components
  #' @author Phuong Le
  pc_names = c()
  for (i in 1:pc_number) {
    pc_name = paste('PC',i,sep = '',collapse='')
    pc_names = append(pc_names,pc_name)
  }
  return(pc_names)}

distance_table_to_pca = function(pc_dist_table,pc_number) {
  #' @description takes a distance table (matrix), 
  #' returns the so-called pca
  #' @param pc_dist_table: dataframe, the input distance matrix 
  #' @param pc_number: numeric, the desired number of principal components 
  #' @author Phuong Le
  pc_dist_table=as.dist(pc_dist_table)
  pca=cmdscale(pc_dist_table,k=pc_number) %>% as.data.frame()
  colnames(pca) = pc_names(pc_number) 
  # pc_names returns a vector from PC1, PC2 to PC(input)
  return(pca)
}

pca_plot = function(pc_dataframe, pc_number, x_pc, y_pc, col_vector, shape_vector, by_donors = T) {
  #' Plot of pca dataframe
  #' @param pc_dataframe: a dataframe of principle components, 
  #' the principle component content starts at column 2
  #' @param pc_number: numeric, the desired number of principal components 
  #' @param x_pc: numeric, the principle component to be put on the x axis
  #' @param y_pc: numeric, the principle component to be put on the y axis
  #' @param col_vector: the colour vector, length must equal the number of cancer types
  #' @param shape_vector: the shape vector, length must equal the number of cancer types
  #' @author Phuong Le
  if (by_donors == T) {
    stopifnot(names(pc_dataframe) == c('donor_id',pc_names(pc_number),'cancer_type')) 
  }
  else {
    stopifnot(names(pc_dataframe) == c('cancer_type',pc_names(pc_number))) 
  }
  pc_dataframe$cancer_type = as.character(pc_dataframe$cancer_type)
  ggplot(pc_dataframe) +
    aes(x=pc_dataframe[,1+x_pc],y=pc_dataframe[,1+y_pc],colour=cancer_type,shape=cancer_type) +
    geom_point(size=3) +
    labs(x=paste('principal coordinate ',x_pc, ' (',round((variance_proportion(pc_dataframe,pc_number)[x_pc]),2),'% explained)',sep=''),
         y=paste('principal coordinate ', y_pc, ' (',round((variance_proportion(pc_dataframe,pc_number)[y_pc]),2),'% explained)',sep='')) +
    scale_color_manual(values=col_vector) +
    scale_shape_manual(values=shape_vector) +
    theme_dark()}

variance_proportion = function(pc_dataframe,pc_number) {
  #' @description returns the variance for each principal components in pc_dataframe
  #' @param pc_dataframe: the dataframe of principal components
  #' @param pc_number: numeric, the desired number of principal components 
  #' @author Phuong Le
  pc_variance = c() 
  for (i in 1:pc_number) {
    pc_variance = append(pc_variance,pc_dataframe[names(pc_dataframe) == pc_names(pc_number)[i]] %>% var())
  }
  return(pc_variance*100/sum(pc_variance))}

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
pca
pca = rownames_to_column(pca, var = 'cancer_type')
pca_plot(pca,dimensions,1,3,col_vector_fixed,shape_vector_fixed,by_donors = F)

