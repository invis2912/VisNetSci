# ================ Introduction ================ 

# CONTENTS:
#
#   1. Generating networks
#   2. Reading in data to create networks
#   3. Visualising networks
#   4. Visualising more online datasets

# Much of Sections 2 and 3 were taken from the work of Katherine Ognyanova which is well made:
# https://github.com/kateto/R-Network-Visualization-Workshop

# Clear the environment
rm(list=ls())

# KEY PACKAGES:
# The packages below (and their dependencies) are the main packages necessary
# for the code in this document.

required_packages = c("networkD3", "igraph", "ndtv", "network", "gganimate",
                      "ggnetwork", "intergraph", "randomNames", "networkDynamic")


# This will install the packages above if they have not already been installed.
for (package in required_packages) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package)
  }
}

library(igraph)
library(network)

# DATA: Make sure to download the 'Datasets' folder and place it in the same directory
# as this source file: https://github.com/invis2912/VisNetSci/Datasets.

# Set working directory to be the current dictory of the source file.
current_path = rstudioapi::getActiveDocumentContext()$path 
current_directory = dirname(current_path)
setwd(current_directory)

# Create folder for outputs
if (!dir.exists("Output")) {
  dir.create("Output")
}



# ================ 1. Generating Networks ================ 

# The network models that we will consider are:
#   1. Random network (Erdos-Renyi)
#   2. Scale-free network (Barabasi-Albert)
#
# We wil generate these networks using the 'igraph' package. There are other networks
# like the Watts-Strogatz 'small-world' model, forestfire model, etc. which can be
# generated using 'igraph': see 'igraph' documentation: https://igraph.org/r/doc/

# 'igraph' has built-in functions for visualising networks which we mention at the start of
# Section 3. Details on how to use these functions are well-documented in Katherine Ognyanova's
# article referred to in the section.
#
# There is also a function in the 'networkD3'package that allows us to convert
# from an 'igraph' object and use 'networkD3' visualisations.

detach(package:ndtv)
detach(package:sna)

## Random Network (Erdos-Renyi):

# Parameters
n <- 30            # Number of nodes/vertices
p <- 0.05          # Probability of drawing an edge between two arbitrary nodes
m <- 75000         # Total number of edges in the graph (use EITHER this parameter OR the 'n' parameter,
#    specified by the 'type' parameter)
type <- "gnp"      # Can be "gnm" or "gnp" based on model notation. See: https://igraph.org/r/doc/erdos.renyi.game.html
directed <- TRUE   # Whether to create a directed network
loops <- FALSE     # Whether self loops are allowed

# Generate random network
er <- erdos.renyi.game(n, p, type, directed, loops)

class(er) # returns: "igraph"



## Scale-Free Network (Barabasi-Albert)

# Simple scale-free network: https://igraph.org/r/doc/sample_pa.html
# Parameters
n <- 100          # Total number of vertices
power <- 1        # The power of the preferential attachment, default is one, i.e. linear
m <- 2            # The number of edges to add in each time step
directed <- FALSE # Whether to generate a directed network

# Generate scale-free network
ba <- sample_pa(n, power=power, m=m, directed=directed)

class(ba) # returns: "igraph"



# Evolving (scale-free) network with preferential attachment and aging: https://igraph.org/r/doc/sample_pa_age.html
# Parameters
n <- 120          # Total number of vertices
pa.exp <- 1       # Preferential attachment exponent
aging.exp <- -1   # Aging exponent, which is usually a non-positive number
m <- 2            # The number of edges each new vertex creates in each time step
directed <- TRUE  # Whether to generate a directed network

# Generate (evolving) scale-free network
ba.age <- sample_pa_age(n, pa.exp, aging.exp, m, directed=directed)

class(ba.age) # returns: "igraph"


## How to interpret 'igraph' objects:

summary(er)
print(er)

# summary(ba)
# print(ba)

# summary(ba.age)
# print(ba.age)

# 'IGRAPH' denotes that this is an igraph graph. Then come four bits that denote the kind of
# the graph: the first is 'U' for undirected and 'D' for directed graphs. The second is
# 'N' for named graph (i.e. if the graph has the 'name' vertex attribute set). The third is
# 'W' for weighted graphs (i.e. if the 'weight' edge attribute is set). The fourth is
# 'B' for bipartite graphs (i.e. if the 'type' vertex attribute is set).
#
# Then come two numbers, the number of vertices and the number of edges in the graph,
# and after a double dash, the name of the graph (the 'name' graph attribute) is printed
# if present. The second line is optional and it contains all the attributes of the graph.
# This graph has a 'name' graph attribute, of type character,
# and two other graph attributes called 'mutual' and 'circular', of a complex type.
# A complex type is simply anything that is not numeric or character.


## Accessing igraph attributes/charateristics (more details in Section 2)

V(er) # Vertices
E(er) # Edges
degree(er) # Degree of each vertex

delete_vertices(er, v=c(1,3)) # Returns the graph with vertices 1 and 3 removed
delete_edges(er,edges=c("1|50", "44|1")) # Returns the graph with edges 1->50 and 44->1 removed.
# NOTE: passing a non-existent edge will cause an error.


# Note the effect on the max degree when we vary the magnitude of the preferential attachment and aging exponents:

# Increasing the magnitude of the aging exponent makes it less likely an old vertex will be connected to,
# thus reducing the max degree of the network.
g1 <- sample_pa_age(10000, pa.exp=1, aging.exp=0, aging.bin=1000)
g2 <- sample_pa_age(10000, pa.exp=1, aging.exp=-1, aging.bin=1000)
g3 <- sample_pa_age(10000, pa.exp=1, aging.exp=-3, aging.bin=1000)

max(degree(g1))
max(degree(g2))
max(degree(g3))


# Increasing the preferential attachment exponent makes it likely that an old vertex will be connected to,
# thus increasing the max degree of the network.
g4 <- sample_pa_age(10000, pa.exp=0, aging.exp=0, aging.bin=1000)
g5 <- sample_pa_age(10000, pa.exp=1, aging.exp=0, aging.bin=1000)
g6 <- sample_pa_age(10000, pa.exp=1.1, aging.exp=0, aging.bin=1000)

max(degree(g4))
max(degree(g5))
max(degree(g6))




# ================ 2. Reading in Data to Create Networks ================ 

# NOTE: This section is taken exactly from Katherine Ognyanova's R code in "Sunbelt 2019 R Network
#       Visualization Workshop.R". The directories of the datasets have been modified for this
#       script so that the "Datasets" folder is in the same directory as this source file:

# Download an archive with the data files from http://bit.ly/sunbelt2019  

# Make sure to set the working directory to the appropriate directory containing the "Data Files"
# folder, using the setwd() function.

# If you don't know the path to the folder and you're in RStudio, go to the
# "Session" menu -> "Set Working Directory" -> "To Source File Location"


# -------~~ DATASET 1: edgelist  --------


# Read in the data:
nodes <- read.csv("./Datasets/Kateto Datasets/Data Files/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("./Datasets/Kateto Datasets/Data Files/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# Examine the first few rows of the data:
head(nodes)
head(links)

# Converting the data to an igraph object:
# The graph_from_data_frame() function takes two data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

# Examine the resulting object:
class(net) # returns: "igraph"
net 

# We can access the nodes, edges, and their attributes:
E(net)
V(net)
E(net)$type
V(net)$name

# Or find specific nodes and edges by attribute:
# (that returns objects of type vertex sequence / edge sequence)
V(net)[media=="BBC"]
E(net)[type=="mention"]


# If you need them, you can extract an edge list 
# or a matrix back from the igraph networks.
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

# You can also look at the network matrix directly:
net[1,]
net[5,7]
E(net)$weight
# First attempt to plot the graph:
plot(net) # not pretty!

# Removing loops from the graph:
net <- simplify(net) 

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)

# -------~~ DATASET 2: matrix  --------


# Read in the data:
nodes2 <- read.csv("./Datasets/Kateto Datasets/Data Files/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("./Datasets/Kateto Datasets/Data Files/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1, as.is=T)

# Examine the data:
head(nodes2)
head(links2)

# links2 is a matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

# Create an igraph network object from the two-mode matrix: 
net2 <- graph_from_incidence_matrix(links2)
names(nodes2)

# A built-in vertex attribute 'type' shows which mode vertices belong to.
table(V(net2)$type)

plot(net2)

# Examine the resulting object:
class(net2)
net2 

# To transform a one-mode network matrix into an igraph object,
# we would use graph_from_adjacency_matrix()


# ================ 3. Visualising Networks ================

## Static Network Visualisation:
#
# Once again, we refer the reader to Katherine Ognyanova's R code and pdf file entitled, "Sunbelt 2019 R Network Visualization Workshop.pdf".
# Chapters 4 (igraph) and 5 (Statnet, ggraph and igraph) give a good documentation of the possibilities of network visualisation.
#
# Chapter 4 gives a large number of examples on using igraph to plot networks with varying sizes, colours,
# types of edges (e.g. multilinks and self-loops), network layouts (e.g. bipartite networks and circular networks), etc.
#
# Chapter 5 explores in some detail the use of the 'network' package (Chapter 5.1) and the 'ggraph' package (Chapter 5.2) for
# static network visualisation. Chapter 5.3 gives two examples (using 'igraph') of how to use network measures (e.g. degree distribution and
# adjacency matrix) to visualise a network.
#
# When the network is very big and/or dense, these plots and charts are  more useful than 'giant hairball' plots.


## Interactive Network Visualisation:
#
# This section shows the use of the 'networkD3' and 'ndtv' packages
# to create interactive network visualisations using the D3.js library.


# --- 'networkD3' ---

# The following snippet is from Katherine Ognyanova's Sunbelt Workshop R code which uses Dataset 1 from Section 2

# -------------------------------------------------------------------------------------------

# d3ForceNetwork expects node IDs that are numeric and start from 0
# so we have to transform our character node IDs:

links.d3 <- data.frame(from=as.numeric(factor(links$from))-1, 
                       to=as.numeric(factor(links$to))-1 )

# The nodes need to be in the same order as the "source" column in links:
nodes.d3 <- cbind(idn=factor(nodes$media, levels=nodes$media), nodes) 

# The `Group` parameter is used to color the nodes.
# Nodesize is not (as you might think) the size of the node, but the
# number of the column in the node data that should be used for sizing.
# The `charge` parameter guides node repulsion (if negative) or 
# attraction (if positive).

forceNetwork(Links = links.d3, Nodes = nodes.d3, Source="from", Target="to",
             NodeID = "idn", Group = "type.label",linkWidth = 1,
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 600, height = 600)

# Zoom in:  Double click
# Zoom out: Shift + double-click

# -------------------------------------------------------------------------------------------

# We can also visualise networks that were generated with 'igraph' (e.g. Erdos-Renyi,
# Watts-Strogatz, etc.) using 'networkD3'. In the following examples,
# we create dynamic visualisations of the 'igraph' objects created in Section 1.

# The 'networkD3' package also has a function, igraph_to_networkD3(), to convert an igraph object to list suitable for networkD3.
# The following example is used on the random network and scale-free network that we generated previously.
# Note that because the networks were generated randomly, we do not have explicit groups, and since forceNetwork() requires a 'groups'
# parameter, we use the cluster_walktrap() and membership() functions to automatically assign groups by finding densely connected subgraphs.

class( er )                               # returns: "igraph"
class( cluster_walktrap(er) )             # returns: "communities"
class( membership(cluster_walktrap(er)) ) # returns: "membership"


# Random network
er.groups <- membership(cluster_walktrap(er))
er.d3 <- igraph_to_networkD3(er, group=er.groups)

forceNetwork(Links=er.d3$links, Nodes=er.d3$nodes,
             Source="source", Target="target",
             NodeID="name", Group="group", opacity=1.8,
             zoom=T, charge=-100)


# Scale-free network
ba.groups <- membership(cluster_walktrap(ba))
ba.d3 <- igraph_to_networkD3(ba, group=ba.groups)

forceNetwork(Links=ba.d3$links, Nodes=ba.d3$nodes,
             Source="source", Target="target",
             NodeID="name", Group="group", opacity=1.8,
             zoom=T, charge=-80)


# Evolving scale-free network
ba.age.groups <- membership(cluster_walktrap(ba.age))
ba.age.d3 <- igraph_to_networkD3(ba.age, group=ba.age.groups)

forceNetwork(Links=ba.age.d3$links, Nodes=ba.age.d3$nodes,
             Source="source", Target="target",
             NodeID="name", Group="group", opacity=1.8,
             zoom=T, charge=-80)


# --- 'ndtv' ---

library(ndtv)

# Below is a snippet from the Sunbelt Workshop R code on using 'ndtv' for visualisation.

# Make sure to run this line first as it is used as an example below.
net3 <- network(links, vertex.attr=nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)

# -------------------------------------------------------------------------------------------

# You should not need additional software to produce web animations with 'ndtv' (below).
# If you want to save the animations as  video  files ( see ?saveVideo), you have to
# install a video converter called FFmpeg (ffmpg.org). To find out how to get the right 
# installation for your OS, check out ?install.ffmpeg  To use all available layouts, 
# you need to have Java installed on your machine.


# Remember net3, our original media network turned into a 'network' object:
net3 

# Let's create an interactive (but not yet dynamic!) visualization of net3.
# You will recognize a lot of the plotting parameters from 'network':
# Two new parameters set the tooltips (the popup labels you see when you 
# click on network elements); note that those can take html format.
# 'launchBrowser=T' will open file 'filename' in your default browser.

render.d3movie(net3, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net3 %v% "col",
               vertex.cex = (net3 %v% "audience.size")/8, 
               edge.lwd = (net3 %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3 %v% 'media') , "<br>",
                                      "<b>Type:</b>", (net3 %v% 'type.label')),
               edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net3 %e% "weight" ) ),
               launchBrowser=T, filename="Output/Media-Network.html" )  

# -------------------------------------------------------------------------------------------

# Some further examples of using 'ndtv' package by James Curley called "ndtv_interactivity.R"
# with small changes and explanatory comments added: https://github.com/jalapic/rmeetup_examples

# -------------------------------------------------------------------------------------------

# ndtv & networkDynamic

detach(package:sna)
detach(package:network)
detach(package:igraph)

library(igraph)
library(intergraph)
library(network)
library(randomNames)

# Code outline:
# Build forestfire model using 'igraph', convert to 'network' object.

set.seed(5)

# Create random dataset of 30 people, each corresponding to a node
N=30
genders <- sample(c("Female", "Male"), N,T)

df <- data.frame(id = randomNames(N,gender=genders, name.order = "first.last", name.sep = " "),
                 sex=genders)

# Build a forest fire network model for 30 nodes
g <- sample_forestfire(N, fw.prob=0.3,bw.factor=.9,directed=F)

# Convert from 'igraph' to 'network'
net<-intergraph::asNetwork(g)

# Create/set vertex attributes
net %v% "col" <- c("green", "gold", "blue", "red", "pink")[edge.betweenness.community(g)$membership]
net %v% "sex" <- genders
net %v% 'id'<- as.character(df$id)
net %v% "sizevar" <- sample(5:15,vcount(g),T)

# Create/set edge attributes
net %e% "type" <- sample(LETTERS[1:4],ecount(g),T) 
net %e% "weight"  <- igraph::degree(g)

# Create network animation
render.d3movie(net, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net %v% "col",
               vertex.cex = (net %v% "sizevar")/8, 
               edge.lwd = (net %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net %v% 'id') , "<br>",
                                      "<b>Gender:</b>", (net %v% 'sex')),
               edge.tooltip = paste("<b>Edge type:</b>", (net %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net %e% "weight" ) ),
               launchBrowser=T, filename="Output/Network.html" )  



library(networkDynamic)
library(ndtv)

## Animation 6.

# use 'net' made in ndtv_interactivity
detach(package:igraph)

net #30 nodes, #44 edges

# Convert net from 'network' to data.frame
vs <- data.frame(onset=0, terminus=45, vertex.id=1:30)
es <- data.frame(onset=1:45, terminus=45, 
                 head=as.matrix(net, matrix.type="edgelist")[,1],
                 tail=as.matrix(net, matrix.type="edgelist")[,2])

# Convert to 'networkDynamic' object
net.dyn <- networkDynamic(base.net=net, edge.spells=es, vertex.spells=vs)


# Prepare the 'networkDynamic' object for animation.
# Note: This step is not always necessary.
compute.animation(net.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=45, interval=2, 
                                 aggregate.dur=1, rule='any'))

# Create animation
render.d3movie(net.dyn, usearrows = F, displaylabels = F, label=net %v% "id",
               bg="#111111", 
               #vertex.border="#ffffff", 
               vertex.col =  net %v% "col",
               vertex.cex = function(slice){ degree(slice)/2.5 },  
               edge.lwd = (net %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net %v% 'id') , "<br>",
                                      "<b>Gender:</b>", (net %v% 'sex')),
               edge.tooltip = paste("<b>Edge type:</b>", (net %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net %e% "weight" ) ),
               launchBrowser=T, filename="./Output/NetworkDynamic2.html",
               render.par=list(tween.frames = 15, show.time = F), 
               script.type='remoteSrc')

# -------------------------------------------------------------------------------------------



# When the network becomes too big/dense for direct visualisation of the network, other
# visualisations should be considered that present the main characteristics of the network,
# e.g. clustering, degree, density, weights, etc.
#
# The following is also taken from the Sunbelt Workshop R Code

par(mfrow=c(1,1))
library(igraph)

# -------------------------------------------------------------------------------------------

# -------~~ Other ways to represent a network -------- 

# This section is a reminder that there are other ways to represent a network.
# For example, we can create a heatmap of a network matrix.

# First, we'll extract a matrix from our igraph network object. 
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- simplify(net)
netm <-  as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

# Generate a color palette to use in the heatmap:
palf <- colorRampPalette(c("gold", "dark orange")) 

# The Rowv & Colv parameters turn dendrograms on and off
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(20), 
        scale="none", margins=c(10,10) )

# Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(degree(net)), y=1-deg.dist, pch=19, cex=1.4, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

# -------------------------------------------------------------------------------------------
head(links)
head(nodes)
# ================ 4. Visualising More Online Datsets ================ 

# Clear the environment
rm(list=ls())

# Set working directory to be the current dictory of the source file.
current_path = rstudioapi::getActiveDocumentContext()$path 
current_directory = dirname(current_path)
setwd(current_directory)

# In this section, we make use of some of the network visualisation tools referred to in this
# document and from the other sources.

# The datasets are available from Network Science Book by Albert Barabasi:
# http://networksciencebook.com/translations/en/resources/data.html

# -------~~ DATASET 1: Actor  --------
# Based on 2004 imdb data. Nodes represent actors, two nodes are connected if
# the corresponding actors made at least one movie together. Made for TV,
# direct-to-video and video game entries were removed.
# Preprocessed data courtesy of Hawoong Jeong.

library(igraph)

# Read in the data (NOTE: this will take a while)
actor <- read.table("./Datasets/Network Science Book/actor.edgelist.txt", header=F)
colnames(actor) <- c("movies", "actors")
dim(actor) # 29,397,908 rows


# Transform data for conversion to 'igraph' network
movies = actor[1]
actors = actor[2]

movies.unique <- unique(movies)
actors.edgelist <- data.frame(from=integer(), to=integer())

# Construct edge list from data by matching actors that appear in the same movie
# (This will take much longer with the whole dataset as we will be building millions of networks)
for (movie in movies.unique[1:100,]) {
  linked_actors <- actors[which(movies==movie),]
  
  pairs = expand.grid(from=linked_actors, to=linked_actors)
  
  actors.edgelist = rbind(actors.edgelist,pairs)
}


# Convert edge list data.frame to 'igraph' network
actors.network <- graph_from_data_frame(d=actors.edgelist,
                                        vertices=unique(c(actors.edgelist$from,actors.edgelist$to)), directed=F)

actors.network <- simplify(actors.network)


# Compute and plot the degree distribution.
actors.deg.dist <- degree_distribution(actors.network, cumulative=T, mode="all")
plot(x=0:max(degree(actors.network)), y=1-actors.deg.dist, pch=19, cex=1.4, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency", main="Number of Common Actor Appearances")

# The degree of each node (actor) corresponds to the number of actors that co-starred
# with that particular actor in at least one movie.


# -------~~ DATASET 2: Protein  --------
# Network representing the protein-protein interactions in yeast. Each node represents
# a protein, and they are connected if they physically interact within the cell.
#
# Original data: http://interactome.dfci.harvard.edu/S_cerevisiae/index.php?page=download
# Ref: Yu, H., Braun, P., Yıldırım, M. A., Lemmens, I., Venkatesan, K., Sahalie, J.
# ... & Vidal, M. (2008).High-quality binary protein interaction map of the yeast interactome network. Science, 322(5898), 104-110.