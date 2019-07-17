source("libraries.r")
source("functions.r")


# read --------------------------------------------------------------------

# load matrix
mat_raw = readRDS("data/working/matrix_raw.rds")


# clean -------------------------------------------------------------------

# 1. remove totals and taxes-subsidies and change in inventories and VA
i_remove = str_detect(rownames(mat_raw), "OUTPUT") | 
  str_detect(rownames(mat_raw), "_TAXSUB") 
j_remove = str_detect(colnames(mat_raw), "TOTAL") | 
  str_detect(colnames(mat_raw), "INVNT")
mat = mat_raw[!i_remove, !j_remove]

# 2. replace negatives with 0
mat[mat<0] = 0

# 3. aggregate (MEX,MX1,MX2) and (CHN,CN1,CN2)
rownames(mat) %<>% str_replace("MEX|MX1|MX2","MEX") 
rownames(mat) %<>% str_replace("CHN|CN1|CN2","CHN") 
mat = rowsum(mat, rownames(mat))
colnames(mat) %<>% str_replace("MEX|MX1|MX2","MEX") 
colnames(mat) %<>% str_replace("CHN|CN1|CN2","CHN") 
mat = rowsum(t(mat), colnames(mat)) %>% t()

# 4. sum final demand by country
dfnames = c("HFCE","NPISH","GGFC","GFCF","INVNT","P33")
colnames(mat) %<>% str_replace(paste(dfnames, collapse="|"),"DF") 
mat = rowsum(t(mat), colnames(mat)) %>% t()

# 5. append submatrix of VA (might be needed or not)
# remove and keep VA
va = mat[str_detect(rownames(mat), "VALU"), , drop=F]
mat = mat[!str_detect(rownames(mat), "VALU"),]
# number of countries
n_countries = sum(str_count(colnames(mat), "_DF"))
# number of vertices by country (industries + DF)
n_inds = sum(str_count(colnames(mat), "USA"))
# auxiliary matrix to create submatrix of VA
ones = rep(1, n_inds)
aux_mat = c(ones, rep(0,ncol(mat))) %>% rep(n_countries-1) %>% c(ones) %>% 
  matrix(nrow=n_countries, byrow=T) %>% 
  set_rownames(colnames(mat) %>% "["(str_detect(.,"_DF")))
# create and append submatrix of VA
va_mat = aux_mat %*% diag(c(va))
mat = rbind(mat, va_mat)

# 6. set loops = 0 (compraventas del mismo sector)
mat = mat[sort(rownames(mat)),sort(colnames(mat))]
diag(mat) = 0

# 7. 0 if byrow and bycol < 0.1 (o sea 0.1%)
lim_perc = 0.1
# % of each cell by row (no entiendo por qué traspone)
mat_byrow = mat %>% apply(1, function(x) x/sum(x)*100) %>% t()
# % of each cell by col
mat_bycol = mat %>% apply(2, function(x) x/sum(x)*100)
# quedan en NA los sectores con ventas totales = 0 o compras totales = 0
# se remplazan por 0
mat_byrow[is.na(mat_byrow)] = 0
mat_bycol[is.na(mat_bycol)] = 0
mat[c(mat_bycol)<lim_perc & c(mat_byrow)<lim_perc] = 0

# 8. 0 if value<1 (o sea 1 mill USD)
lim_value = 1
mat[c(mat)<lim_value] = 0

# 9. weighted version 1 (weights equal to XM index)
# (XM = (%row + %col)/2)
mat_w1 = ((mat_bycol + mat_byrow)/2) / 100

# 10. weighted version 2 (weights equal to log(dollars))
mat_w2 = mat
mat_w2[mat_w2>0] = log(mat_w2[mat_w2>0])

# 11. unweighted version 
mat_uw = mat
mat_uw[mat_uw>0] = 1

# 12. remove VA rows and servicio domestico (son solo VA por definicion)
mats = list(mat_w1=mat_w1, mat_w2=mat_w2, mat_uw=mat_uw)
for (i in seq_along(mats)) {
  mats[[i]] = mats[[i]][!str_detect(rownames(mats[[i]]), "_DF"), ]
  mats[[i]] = mats[[i]][!str_detect(rownames(mats[[i]]), "_97T98"), ]
  mats[[i]] = mats[[i]][,!str_detect(colnames(mats[[i]]), "_97T98")]
}



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
                                                        '_07T08',
                                                        '_16',
                                                        '_24'), collapse="|"))]
  
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
  
  aa = pathRanker(g2, method="prob.shortest.path",start = primarios_sin_eeuu, K=1000, minPathSize=5)
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
for (i in 1:length(mats)){
  print(i)
  for (j in 1:length(gs_com[[i]])){
    print(j)
    temp_res[[j]] = path_network(icoi_mat = mats[[i]],community = gs_com[[i]][[j]],matrices = mats )
  }
  path_in_community[[i]] <-temp_res
}



# plot paths ----------------------------------------------------------------

somePDFPath = "output/plots/ranks.pdf"
pdf(file=somePDFPath)  

for (i in 1:length(path_in_community)){   
 # par(mfrow = c(2,1))
  
  lay = layout_with_fr(path_in_community[[i]][[1]])
  plot(path_in_community[[i]][[1]], layout=lay,
       edge.color="gray",
       edge.arrow.size=.1,
       edge.curved=0.1,
       # vertex.label="",
       vertex.label.dist=-0.8,
       vertex.label.color="black",
       vertex.label.cex=0.75,
       vertex.size=4,
       vertex.color="black")
  
  lay = layout_with_fr(path_in_community[[i]][[2]])
  plot(path_in_community[[i]][[2]], layout=lay,
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


# # graph -------------------------------------------------------------------
# 
# mat_to_use = "mat_w2"
# # adjacency list and vertices list
# vertices = dimnames(mats[[mat_to_use]]) %>% unlist() %>% unique()
# adj_list = data.table::melt(mats[[mat_to_use]]) %>% 
#   setNames(c("from","to","weight")) %>% 
#   dplyr::filter(weight>0) 
# # (drop weight if using UW?)
# 
# # create graph
# library(igraph)
# g = graph_from_data_frame(adj_list, directed=T, vertices=vertices)
# # g = graph_from_data_frame(adj_list, directed=T)
# 
# 
# # communities -------------------------------------------------------------
# 
# c_im = cluster_infomap(g, modularity=F)
# c_lp = cluster_label_prop(g)
# 
# # keep communities with more than 1 node
# grupos_im = igraph::groups(c_im) %>% "["(map_lgl(., function(x) length(x)>1))
# grupos_lp = igraph::groups(c_lp) %>% "["(map_lgl(., function(x) length(x)>1))
# 
# # keep communities with more than 1 country
# gruposf_im = grupos_im %>%
#   "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 1))
# gruposf_lp = grupos_lp %>%
#   "["(map_lgl(., function(x) length(unique(str_extract(x,"(.+)_"))) > 1))
# 
# 
# 
# # plot --------------------------------------------------------------------
# 
# png("output/plots/prueba3.png", width=1600, height=1200)
# lay = layout_with_kk(g)
# plot(g, layout=lay, edge.color="white",
#      vertex.size=1,
#      vertex.label="",
#      vertex.color="black")
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 




# OLD ---------------------------------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("NetPathMiner")
# library(NetPathMiner)


# adjacency list and vertices list (W)
# vertices = colnames(mat_w) %>% c(rownames(mat_w)) %>% unique()
# adj_list = data.table::melt(mat_w) %>% 
  # setNames(c("from","to","weight")) %>% 
  # dplyr::filter(weight>0)
         


# adj_list2 = data.table::melt(mat_w) %>% 
#   setNames(c("from","to","weight")) %>% 
#   mutate(cfrom = str_match(from, "(.+)_")[,2]
#          ,cto = str_match(to, "(.+)_")[,2]
#          , weight = ifelse(cfrom==cto, 0, weight)) %>% 
#   dplyr::filter(weight>0)