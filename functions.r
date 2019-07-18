
# paste strings operator
"%+%" = function(a,b) paste(a,b,sep="")

# max min transformation
maxmin = function(x) (x - min(x))/(max(x)-min(x))

# matrix to graph
mat_to_graph <-  function(mat_g){
  mat_to_use = mat_g
  # adjacency list and vertices list
  vertices = dimnames(mat_g)  %>% unlist() %>% unique()
  adj_list = data.table::melt(mat_g) %>% 
    setNames(c("from","to","weight")) %>% 
    dplyr::filter(weight>0) 
  # (drop weight if using UW?)
  
  # create graph
  library(igraph)
  g = graph_from_data_frame(adj_list, directed=T, vertices=vertices)
  return(g)
}

