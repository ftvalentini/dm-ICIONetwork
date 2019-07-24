source("libraries.r")
source("functions.r")

semilla = 1001

# read data ------------------------------------------------------------

mats = readRDS("data/working/matrices.rds")
# create graphs and save
library(igraph)
gs <- map(mats, mat_to_graph)
write_rds(gs, "data/working/grafos.rds")

# communities -----------------------------------------------------

set.seed(semilla)
# tibble con matrices y grafo y algoritmo_community
dat_coms = tibble(matriz_name = names(gs)
                  , matriz = mats
                  , grafo = gs) %>% 
  tidyr::crossing(metodo=c("infomap","lp")) %>% 
  # identifica comms
  mutate(coms = map2(.x=grafo, .y=metodo,
                     function(x,y) get_coms(grafo=x,metodo=y)))

# number of nodes of each community
dat_coms %>% mutate(l = map_depth(coms, 2, length)) %>% 
  unnest(l) %>% mutate( l = unlist(l))
# DECISION -> TRABAJAMOS SOLO CON W1/W2 e INFOMAP

# tibble con una fila por community
dat = dat_coms %>%
  dplyr::filter(matriz_name %in% c("mat_w1","mat_w2") & metodo=="infomap") %>% 
  unnest(coms, .preserve=matriz) %>% 
  mutate(com_id = row_number())
# save communities
saveRDS(dat %>% select(-matriz), "data/working/communities.rds")

# paths ----------------------------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("NetPathMiner")
library(NetPathMiner)

# paths con mas de 2 paises para cada com_id cada minsize
dat_out = dat %>%
  tidyr::crossing(minsize = 5:10) %>% 
  mutate(
    paths = pmap(list(x=matriz, y=coms, z=minsize),
                 function(x,y,z) heavy_paths(matriz=x, community=y, minsize=z))
    )

com_paths = dat_out %>% 
  unnest(paths, .preserve=com_id) %>% 
  left_join(dat %>% select(com_id, coms), by="com_id")

# save paths with their communities
saveRDS(com_paths, "data/working/com_paths.rds")


