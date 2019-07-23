source("libraries.r")
source("functions.r")

semilla = 1001

# read data ------------------------------------------------------------

mats = readRDS("data/working/matrices.rds")
# create graphs 
library(igraph)
gs <- map(mats, mat_to_graph)


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
  unnest(paths, .preserve=com_id)




# save com - paths tibble ------------------------------------------------

saveRDS(com_paths, "data/working/com_paths.rds")


# table with paths W1 and Infomap ----------------------------------------

w1_info <- com_paths %>%
  dplyr::filter(matriz_name == "mat_w1") 

clean_path <- vector(mode ="list")

for (i in 1:length(w1_info$paths)){
  clean_path[[i]] = unlist(w1_info$paths[[i]])
  q_country = map(clean_path,.f = ~length(unique(str_sub(.,1,3))))
  
}

path_string <- tibble(path_string = character())

for (i in 1:length(clean_path)){
  path_string[i,1] =   as.character(paste(clean_path[[i]], collapse = " "))
}


w1_info2 <- cbind.data.frame(w1_info, q_country = unlist(q_country)) %>%
  mutate(id = row_number()) %>%
  cbind.data.frame(., path_string) %>% 
  arrange(com_id, minsize, desc(q_country)) %>%
  group_by(com_id, minsize) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, path_string) %>%
  distinct(path_string, .keep_all = TRUE)

# save table to output -------------------------------------------------

saveRDS(w1_info2, "output/w1_info2.rds")
