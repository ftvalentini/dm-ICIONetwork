
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

# extract communities from graph
get_coms = function(grafo, metodo) {
  
  if (metodo=="infomap") com = cluster_infomap(grafo, modularity=F)
  if (metodo=="lp") com = cluster_label_prop(grafo)
  out = igraph::groups(com) %>%
    # keep communities with more than 1 node
    "["(map_lgl(., function(x) length(x)>1)) %>% 
    # keep communities with more than 2 countries
    "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 2))
  
  return(out)
}


# identifica heavy paths
heavy_paths = function(matriz, community, minsize){
  
  # sectores de la comunidad
  nodos = community %>% unlist(use.names=F) %>% sort()
  # primarios = nodos
  # alternativa: 
  primarios = nodos[str_detect(nodos, '_01T03|_05T06|_07T08|_09')]
  
  # NOTA: NO SE USA GRAFO CON SOLO LOS SECTORES DE LA COMUNIDAD
  # SE USAN TODOS LOS EDGES DONDE ESTAN POR LO MENOS UNO DE ESOS SECTORES
  # (si no crashea el pathranker)
  
  # crea grafo
  adj_list = data.table::melt(matriz) %>%
    setNames(c("from","to","weight")) %>%
    dplyr::filter(from %in% nodos | to %in% nodos) %>%
    dplyr::filter(weight > 0)
  g = graph_from_data_frame(adj_list, directed=T)
  
  # paths mas probables del grafo -- se conservan solo si tiene mas de 2 paises
  paths = pathRanker(g, method="prob.shortest.path",
                     start=primarios, K=50, minPathSize=minsize)$paths %>% 
    map("genes") %>% 
    "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 2))
  
  return(paths)
}
