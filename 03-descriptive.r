source("libraries.r")
source("functions.r")

semilla = 101

# read and transform matrices -----------------------------------------------

mats = readRDS("data/working/matrices.rds")
# use matrix with log dollars
mat = mats$mat_w2
# create matrix in million USD
mat_usd = mat
mat_usd[mat_usd>0] = exp(mat_usd[mat_usd>0])
# create matrix only with international links
mat_int = mat_usd
cols = colnames(mat_int); rows = rownames(mat_int) 
for (c in seq_along(cols)) {
  for (r in seq_along(rows)) {
    if (str_sub(cols[c],1,3) == str_sub(rows[r],1,3)) mat_int[r,c] = 0
  }
}


# create graphs ------------------------------------------------------------

gs = list(log=mat, usd=mat_usd, int=mat_int) %>% 
  map(mat_to_graph)


# degree ------------------------------------------------------------------

# ALL unweighted
degree = tibble(
  sector = names(degree(gs$usd))
  ,d_tot = degree(gs$usd, mode="total")
  ,d_in = degree(gs$usd, mode="in")
  ,d_out = degree(gs$usd, mode="out")
)

# ALL weighted
degree_w = tibble(
  sector = names(strength(gs$usd))
  ,d_tot = strength(gs$usd, mode="total")
  ,d_in = strength(gs$usd, mode="in")
  ,d_out = strength(gs$usd, mode="out")
)

# INTNL unweighted
degree_int = tibble(
  sector = names(degree(gs$int))
  ,d_tot = degree(gs$int, mode="total")
  ,d_in = degree(gs$int, mode="in")
  ,d_out = degree(gs$int, mode="out")
)

# INTNL weighted
degree_w_int = tibble(
  sector = names(strength(gs$int))
  ,d_tot = strength(gs$int, mode="total")
  ,d_in = strength(gs$int, mode="in")
  ,d_out = strength(gs$int, mode="out")
)



# plot --------------------------------------------------------------------

g_use = "log"
paint = "continent"

# append region-continent attributes to each vertex
atr = read_csv("resources/countries.csv") %>% 
  select(id, region, continent) %>% 
  right_join(
    tibble(name = V(gs[[g_use]])$name %>% str_sub(1,3))
    ,by = c("id"="name"))
gs[[g_use]] = gs[[g_use]] %>% set_vertex_attr(paint, value=atr[[paint]])

# vertex color based on region/continent
pal = RColorBrewer::brewer.pal(length(unique(atr[[paint]])),"Set3")
# pal = rainbow(length(unique(atr[[paint]])))
v_color = pal[as.factor(atr[[paint]])]

# edge opacity based on weight
edge_scale = colorRampPalette(c("gray95","gray10"))
edge_order = floor(maxmin(E(gs[[g_use]])$weight)*100) %>% replace(.==0, 1)
e_color = edge_scale(100)[edge_order]

# layout
set.seed(semilla)
lay = layout_with_kk(gs[[g_use]])
# lay = layout_in_circle(gs[[g_use]])
# set.seed(semilla)
# lay = layout_with_fr(g_usd)

# make plot
png("output/plots/prueba12.png", width=1600, height=1200)
# par(mai=c(0,0,0,0)) # no borders
plot(gs[[g_use]],
     layout=lay
     ,edge.color=e_color
     ,vertex.color=v_color
     ,edge.arrow.size=.1
     ,vertex.frame.color=NA
     ,vertex.size=1
     ,vertex.label=""
)
legend(x=-1.5, y=-0.7, levels(as.factor(atr[[paint]]))
       , pch=21
       , pt.bg=pal, pt.cex=6, cex=2, bty="n", ncol=1)
dev.off()
