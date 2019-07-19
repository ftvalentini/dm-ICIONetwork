source("libraries.r")
source("functions.r")


# read matrices ------------------------------------------------------------

mats = readRDS("data/working/matrices.rds")




# looping graphs ----------------------------------------------------------

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


# generating graphs 
gs <- map(mats, mat_to_graph )

# looping communities -----------------------------------------------------


detCom <- function(g){ 
  
  c_im = cluster_infomap(g, modularity=F)
  c_lp = cluster_label_prop(g)
  
  # keep communities with more than 1 node
  grupos_im = igraph::groups(c_im) %>% "["(map_lgl(., function(x) length(x)>1))
  grupos_lp = igraph::groups(c_lp) %>% "["(map_lgl(., function(x) length(x)>1))
  
  # keep communities with more than 1 country
  gruposf_im = grupos_im %>%
    "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 1))
  gruposf_lp = grupos_lp %>%
    "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 1))
  
  out <- list(gruposf_im = gruposf_im,
              gruposf_lp = gruposf_lp)
}

# generating communites
gs_com <- map(gs, detCom)

# commnuity diagnostics --------------------------------------------------

diagnostics1 <- function(gc){
  q_com <- map_depth(gc,1,length)
  num_country <- map_depth(gc, 2, .f = ~length(unique(str_sub(.,1,3))))
  avg_country <- map_depth(num_country,1,~ mean(unlist(.)))
  #return(q_com)
  return(unlist(avg_country))
  
  
}

diagnostics2 <- function(gc){
  q_com <- map_depth(gc,1,length)
  return(unlist(q_com))
  
  
}

d1 <- map(gs_com, diagnostics1)
d2 <- map(gs_com, diagnostics2)

full_diagnostics <- function(diag1, diag2){
  d1 <- bind_rows(diag1)
  d2 <- bind_rows(diag2)
  df <- rbind.data.frame(d1,d2)
  df2 <- df %>% mutate(var = c("Avg_Country_im",
                               "Avg_Country_lp",
                               "N_communities_im",
                               "N_communities_lp"))
  
}

algorithm_diagnostics <- full_diagnostics(d1,d2)

# rank paths ----------------------------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("NetPathMiner")
library(NetPathMiner)

# rank paths starting from primary sectors (but not from north america)
path_network <- function(icoi_mat, community, matrices){
  
  nodos = community %>% unlist(use.names=F) %>% sort()
  #del_nodos = vertices[!vertices %in% nodos]
  
  primarios = nodos[str_detect(nodos, pattern = paste(c('_01T03', 
                                                        '_05T06',
                                                        '_07T08'), collapse="|"))]
  
  primarios_sin_eeuu = primarios[str_detect(primarios, pattern = paste(c('USA', 
                                                                         'CAN'), collapse="|"), negate = TRUE)]
  matt = icoi_mat
  matt[c(matrices[["mat_w1"]])<0.01] = 0
  
  adj_list2 = data.table::melt(matt) %>%
    setNames(c("from","to","weight")) %>%
    # dplyr::filter(str_detect(from,"ARG") | str_detect(to,"ARG")) %>%
    dplyr::filter(from %in% nodos | to %in% nodos) %>%
    dplyr::filter(weight > 0)
  
  g2 = graph_from_data_frame(adj_list2, directed=T)
  #g2 = graph_from_data_frame(adj_list, directed=T)
  
  aa = pathRanker(g2, method="prob.shortest.path",start = primarios_sin_eeuu, K=1, minPathSize=5)
  paths = aa$paths %>% map("genes")
  keep = map_lgl(paths,
                 function(x) length(unique(str_extract(x,".+_")))>=3 & length(x)<9)
  paths[keep]
  
  bb = extractPathNetwork(aa, g2)
  return(bb)
}


# genrate rank for each community of each matrix
path_in_community = vector(mode = "list")
temp_res <- vector(mode = "list")
path_in_mat <- vector(mode = "list")
for (i in 1:length(mats)){
  print(i)
  for (j in 1:length(gs_com[[i]])){
    print(j)
    for (k in 1:length(gs_com[[i]][[j]])){
    temp_res[[k]] = path_network(icoi_mat = mats[[i]],community = gs_com[[i]][[j]],matrices = mats )
    }
  path_in_community[[j]] <-temp_res
  }
path_in_mat[[i]] <- path_in_community
}



# plot paths ----------------------------------------------------------------

somePDFPath = "output/plots/ranks.pdf"
pdf(file=somePDFPath)  

for (i in 1:length(path_in_mat)){   
  # par(mfrow = c(2,1))
  print(i)
  for (j in 1:length(path_in_mat[[i]])){
    print(j)
    for (k in 1:length(path_in_mat[[i]][[j]])){
      print(k)
      
    if (ecount(path_in_mat[[i]][[j]][[k]]) != 0){  
    lay = layout_with_fr(path_in_mat[[i]][[j]][[k]])
    plot(path_in_mat[[i]][[j]][[k]], layout=lay,
         edge.color="gray",
         edge.arrow.size=.1,
         edge.curved=0.1,
         # vertex.label="",
         vertex.label.dist=-0.8,
         vertex.label.color="black",
         vertex.label.cex=0.75,
         vertex.size=4,
         vertex.color="black")
    }
    # lay = layout_with_fr(path_in_community[[i]][[2]])
    # plot(path_in_community[[i]][[2]], layout=lay,
    #      edge.color="gray",
    #      edge.arrow.size=.1,
    #      edge.curved=0.1,
    #      # vertex.label="",
    #      vertex.label.dist=-0.8,
    #      vertex.label.color="black",
    #      vertex.label.cex=0.75,
    #      vertex.size=4,
    #      vertex.color="black")
    }
  }
} 
dev.off() 


# plot full ------------------------------------------------------------------
g <- gs[[2]]
E(g)$edge.weights = E(g)$weight
E(g)$edge.weights

png("output/plots/prueba3.png", width=1600, height=1200)
lay = layout_with_kk(g)
plot(g, layout=lay, edge.color="white",
     vertex.size=1,
     vertex.label="",
     vertex.color="black")
dev.off()
