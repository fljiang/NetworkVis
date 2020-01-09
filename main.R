# devtools::install_github("mkuhn/dict")
# install.packages("igraph")
# install.packages("network")
# install.packages("threejs")
# install.packages("htmlwidgets")

library("igraph") 
library("network")
library("threejs")
library("htmlwidgets")
library("dict")

nodes <- read.csv("network_nodes.csv")
edges <- read.csv("network_edges.csv")
d <- dict()

# Populate dict for first order connections
for (i in 1:nrow(edges)) {
  n1 <- edges$from[i]
  n2 <- edges$to[i]
  if (n1 %in% d$keys()) {
    d[[n1]] <- c(d[[n1]], n2) 
  }
  else {
    d[[n1]] <- n2
  }
  if (n2 %in% d$keys()) {
    d[[n2]] <- c(d[[n2]], n1) 
  }
  else {
    d[[n2]] <- n1
  }
}

# First order connections function, returns a list of nodes
find_connections <- function(node) {
  foc_list <- list()
  val <- d[[node]]
  for (n in val) {
    foc_list <- c(foc_list, n)
  }
  unique(foc_list)
}

# Maps first order connections to a list of colors
generate_cols <- function(node) {
  connections_list <- find_connections(node)
  cols <- list()
  for (i in nrow(nodes)) {
    if (node == i) {
      cols[i] <- "tomato" 
    }
    else if (i %in% connections_list) {
      cols[i] <- "slateblue"
    }
    else {
      cols[i] <- "gray50"
    }
  }
  cols_list
}

generate_layouts <- function() {
  layouts <- list()
  for (i in 1:nrow(nodes)) {
    lay <- list(layout_randomly(net_js, dim = 3))
    layouts <- c(layouts, lay)
  }
  layouts
}

# Returns a vector of animation layouts or colors
generate_all_cols <- function() {
  all_cols <- list()
  for (i in 1:nrow(nodes)) {
    cols <- list(generate_cols(i))
    all_cols <- c(all_cols, cols)
  }
  all_cols
}

# Base visualization
net <- graph_from_data_frame(d = edges,
                             vertices = nodes,
                             directed = T)

# Partition color by portfolio
colors <- c("gray50", "tomato", "seashell", "thistle", "slateblue")
V(net)$color <- colors[V(net)$portfolio]

# Set node size
degs <- list()
for (i in 1:29) {
  val <- d[[i]]
  degs <- c(degs, length(val))
}
degs <- as.double(degs)
V(net)$size <- degs

# Arrow size 
E(net)$arrow.size <- 0.5

# Plot
graph_attr(net, "layout") <- layout_with_lgl
plot(net)
net_js <- net

# Animation 1: Layouts
layouts <- graphjs(net_js, 
                   layout = list(
                     layout_randomly(net_js, dim = 3),
                     layout_on_sphere(net_js),
                     layout_with_fr(net_js, dim = 3)),
                   main = list("Random", "Sphere", "Fr"),
                   vertex.label = V(net)$label,
                   brush = TRUE,
                   fpl = 3000)

# Opens html file in browser
saveWidget(layouts, file = "Order_Network_Animation1.html")
browseURL("Order_Network_Animation1.html")

# Animation 2: Shows first order connections for each node
connections <- graphjs(net_js, 
                       layout = generate_layouts(),
                       main = V(net)$label,
                       vertex.color = generate_all_cols(),
                       brush = TRUE,
                       fpl = 1000)

saveWidget(connections, file = "index.html")
browseURL("index.html")

# Click animation 1: Non-cumulative expandable

# Find largest vertex from each portfolio
i <- order(degs, decreasing = TRUE)
idx <- aggregate(seq(1:nrow(nodes))[i],
                 by = list(V(net)$portfolio[i]),
                 FUN = head, 1)$x

# Create default layout 
l1 <- norm_coords(layout_with_fr(net, dim = 3))

# Collapse the layout to idx nodes
l0 <- Reduce(rbind,
             Map(function(i) l1[idx[i],],
                 V(net)$portfolio))

# Grouped vertex colors, setting all but idx vertices transparent
col <- rainbow(length(idx), alpha = 0)[V(net)$portfolio]
col[idx] <- rainbow(length(idx), alpha = 1)

click1 <- Map(function(i) {
  # Expand layout for new nodes
  layout <- l0
  layout[V(net)$portfolio == i, ] <- l1[V(net)$portfolio == i, ]
  
  # Show nodes of same portfolio
  c <- col
  c[V(net)$portfolio == i] <- rainbow(length(idx), alpha = 1)[i]
  
  # Update graph
  list(layout = layout, vertex.color = c)
  
}, seq(idx))
names(click1) <- paste(idx)

net.js <- net
click_ani <- (graphjs(net_js,
                      layout = l0,
                      click = click1,
                      vertex.color = col,
                      vertex.label = V(net)$label,
                      fps = 20))

saveWidget(click_ani1, file = "Order_Network_Click1.html")
browseURL("Order_Network_Click1.html")





