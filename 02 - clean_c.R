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

# 5. append submatrix of VA
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

str(mat)

#6. cell=0 if byrow and bycol < 0.1 (o sea 0.1%)
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

# 7. set loops = 0 (compraventas del mismo sector)
diag(mat_new) = 0

# 8. matrix unweighted
mat_uw = mat_new
mat_uw[mat_uw>0] = 1
str(mat_uw)
attr(mat_uw , "dimnames")[[2]]

#9. Graph
g <- igraph::graph_from_adjacency_matrix(mat_uw ,mode = "undirected")

