source("libraries.r")
source("functions.r")

semilla = 1001

# read data ---------------------------------------------------------------

gs = readRDS("data/working/grafos.rds")
com_paths = read_rds("data/working/com_paths.rds")


# plot and table per community --------------------------------------------

paths_to_show = list()
library(igraph)
#dev.off()
# para cada matriz:
for (m_name in c("mat_w1","mat_w2")) {
  # graph
  g = gs[[m_name]]
  # subset table of results
  tab = com_paths %>% 
    dplyr::filter(matriz_name==m_name)
  # ids of communities
  com_ids = unique(tab$com_id)
  # para cada comunidad:
  for (i in seq_along(com_ids)) {
    # get subgraph with all nodes from paths
    all_paths = tab %>% dplyr::filter(com_id==com_ids[i]) %>% pull(paths)
    nodes = all_paths %>% unlist() %>% unique()
    subg = induced_subgraph(g, nodes)
    # plot and save subgraph
    set.seed(semilla)
    lay = layout_with_fr(subg)
    png("output/plots/"%+%m_name%+%"_"%+%com_ids[i]%+%".png", width=600, height=600)
    par(mai=c(0.2,0.2,0.2,0.2))
    plot(subg,
         layout=lay,
         edge.color="gray",
         edge.arrow.size=.1,
         edge.curved=0.1,
         vertex.label.dist=-0.8,
         vertex.label.color="black",
         vertex.label.cex=0.9,
         vertex.size=2,
         vertex.color="black"
    )
    dev.off()
    # table with one path per size and save (maxsize = 10)
    paths_to_show[[m_name]][[as.character(com_ids[i])]] = 
      tab %>%
      dplyr::filter(com_id==com_ids[i]) %>% 
      mutate(size = map_dbl(paths, length)) %>% 
      group_by(size) %>%
      slice(1) %>% ungroup() %>% dplyr::filter(size <= 10) %>% 
      mutate(cgv = map_chr(paths, function(x) paste0(x, collapse=" -> "))) %>% 
      select(size, cgv)
    # saveRDS(paths_to_show, "output/paths"%+%m_name%+%"_"%+%com_ids[i]%+%".rds")
  }
}
saveRDS(paths_to_show, "output/paths_to_show.rds")





# # FBETTEO ----------------------------------------
# 
# w1_info <- com_paths %>%
#   dplyr::filter(matriz_name == "mat_w1") 
# 
# clean_path <- vector(mode ="list")
# 
# for (i in 1:length(w1_info$paths)){
#   clean_path[[i]] = unlist(w1_info$paths[[i]])
#   q_country = map(clean_path,.f = ~length(unique(str_sub(.,1,3))))
#   
# }
# 
# path_string <- tibble(path_string = character())
# 
# for (i in 1:length(clean_path)){
#   path_string[i,1] =   as.character(paste(clean_path[[i]], collapse = " "))
# }
# 
# 
# w1_info2 <- cbind.data.frame(w1_info, q_country = unlist(q_country)) %>%
#   mutate(id = row_number()) %>%
#   cbind.data.frame(., path_string) %>% 
#   arrange(com_id, minsize, desc(q_country)) %>%
#   group_by(com_id, minsize) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(id, path_string) %>%
#   distinct(path_string, .keep_all = TRUE)
# 
# # save table to output -------------------------------------------------
# 
# saveRDS(w1_info2, "output/w1_info2.rds")
