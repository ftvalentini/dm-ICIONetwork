

plot(path_in_mat[[i]][[j]][[k]], layout=lay,
     edge.color="gray",
     edge.arrow.size=.1,
     edge.curved=0.1,
     # vertex.label="",
     vertex.label.dist=-0.8,
     vertex.label.color="black",
     vertex.label.cex=0.75,
     vertex.size=4,
     vertex.color="black",
     main = paste(mapping[i,1], mapping[i,j+1], sep = " "))



# plot paths ----------------------------------------------------------------

somePDFPath = "output/plots/ranks.pdf"
pdf(file=somePDFPath)  


mapping = data.frame(matrix = c("w1","w2","UW"), 
                     algo1 = c("Infomap"),
                     algo2 = c("LP"))

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
             vertex.color="black",
             main = paste(mapping[i,1], mapping[i,j+1], sep = " "))
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
