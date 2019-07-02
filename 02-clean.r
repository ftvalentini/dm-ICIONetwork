source("libraries.r")
source("functions.r")


# read --------------------------------------------------------------------

# load matrix
mat_raw = readRDS("data/working/matrix_raw.rds")


# clean -------------------------------------------------------------------

# 1. remove totals and taxes-subsidies and change in inventories
i_remove = str_detect(rownames(mat_raw), "OUTPUT") | 
  str_detect(rownames(mat_raw), "_TAXSUB")
j_remove = str_detect(colnames(mat_raw), "TOTAL") | 
  str_detect(colnames(mat_raw), "INVNT")
mat = mat_raw[!i_remove, !j_remove]

# 2. replace negatives with 0
mat[mat<0] = 0

# stats iniciales
# density inicial (pero ojo que tiene value added y despues lo vamos a sacar)
sum(mat>0) / (nrow(mat)*ncol(mat)) * 100
# distribucion de los montos positivos (en mill de USD)
summary(c(mat[mat>0]))

# 3. cell=0 if byrow and bycol < 0.1 (o sea 0.1%)
lim_perc = 0.1
# % by row
mat_byrow = mat %>% apply(1, function(x) x/sum(x)*100)
# % by col
mat_bycol = mat %>% apply(2, function(x) x/sum(x)*100)
# quedan en NA los sectores con ventas totales = 0 o compras totales = 0
  # se remplazan por 0
mat_byrow[is.na(mat_byrow)] = 0
mat_bycol[is.na(mat_bycol)] = 0
mat_new = mat
mat_new[c(mat_bycol)<lim_perc & c(mat_byrow)<lim_perc] = 0

# 4. 0 if value<1 (o sea 1 mill USD)
lim_value = 1
mat_new[c(mat_new)<lim_value] = 0
# new stats
sum(mat_new>0) / (nrow(mat_new)*ncol(mat_new)) * 100
summary(c(mat_new[mat_new>0]))

# 5. remove value added
mat_new = mat_new[!str_detect(rownames(mat_new), "VALU"), ]

# 6. set loops = 0 (compraventas del mismo sector)
diag(mat_new) = 0

# 7. matrix unweighted
mat_uw = mat_new
mat_uw[mat_uw>0] = 1


# save -------------------------------------------------------------------

# weighted matrix
saveRDS(mat_new, "data/working/matrix_weighted.rds")
# unweighted matrix
saveRDS(mat_uw, "data/working/matrix_unweighted.rds")





# OLD

# log of values
# mat_new_log = mat_new
# mat_new_log[mat_new_log>0] = log(mat_new_log[mat_new_log>0])
# density(mat_new_log[mat_new_log>0]) %>% plot()
# density(mat_new[mat_new>0]) %>% plot()

# aa = mat_new %>% reshape2::melt()

# aa %>% 
  # dplyr::filter(str_detect(Var1,"ARG")) %>%
  # dplyr::filter(!str_detect(Var2,"ARG")) %>% 
  # dplyr::filter(value>0) %>% 
  # arrange(-value)

# aa %>% 
#   dplyr::filter(str_detect(Var2,"ARG")) %>% 
#   dplyr::filter(!str_detect(Var1,"ARG")) %>% 
#   dplyr::filter(value>0) %>% 
#   arrange(-value)



# (str_detect(colnames(mat_raw), "TOTAL")) %>% which()
# (str_detect(rownames(mat_raw), "OUTPUT")) %>% which()
# 
# 
# View(rownames(mat_raw) %>% sort())
# 
# dim(mat_raw)

# grafos no pesados
# grafo = map(red_np, ~graph.adjacency(.x,mode="undirected",diag=F)),