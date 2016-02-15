# ProjectMosaicNetwork
Collaboration Network of Social Science Research at UNCC


# Working Directory & Libraries
```{r}
# Working Directory & Libraries
setwd("~/Documents/Work/MosaicWorkshop/ProjectMosaicCollaboration/")

##if first time, download these libraries:
##install.packages('dplyr')
##install.packages('igraph')
library(dplyr)
library(igraph)
```

# Filter & Aggregate

```{r}
## Load "Cleaned" Dataset
rawfile <- read.csv(file = "temp2.csv", header = TRUE, stringsAsFactors = FALSE)

#filter only UNCC social science researchers
socsci <- subset(rawfile, University == "UNCC" & (Department == "Geography & Earth Sciences" | Department == "Psychology"
                                              | Department == "Sociology" | Department == "Political Science"
                                              | Department == "Economics" | Department == "Criminal Justice"
                                              | Department == "Public Health" | Department == "Business" 
                                              | Department == "Education" | Department == "Organizational Science" ))

aggclean <- socsci %>% group_by(StandardAuthor, Department) %>% summarise(number = n()) 

# Save Article ID, Standardized Author and Department
socsci <- socsci[,c(2,4,7)]
```
# Create Edge List
```{r}
ss <- split.data.frame(socsci[,c(2,3)],socsci$ID)

## Make edgelist by repeating 1st elements each length(vector)-1L
num = length(ss)
edge.final <- data.frame(Source=character(),
                         Target=character(),
                         ID=character())

for (x in 1:num){
  templist <- ss[[x]][1] 
  idlist <- names(ss[x])
  len <- length(unlist(templist))
  while (len > 1){
    temp <- cbind(
      unlist(lapply(templist, tail, -1L)),                        # col1
      rep(sapply(templist, `[`, 1L), times=lengths(templist)-1L),   # col2
      idlist)
        edge.final <- rbind(edge.final, temp)
    templist <- data.frame(templist[[1]][-1],stringsAsFactors = FALSE)
    len <- len - 1
  }
}

## create final edge list (as matrix)
edge.final[3] <- lapply(edge.final[3], as.character)
head(edge.final)
names(edge.final) <- c("Source","Target","EID")
edge.final$Weight <- 1
edge.final$Type <- "Undirected"
row.names(edge.final) <- NULL

mat <- as.matrix(edge.final[,1:2])
```

# Make Graph & Calculate Centrality Measures

```{r}
g <- graph_from_edgelist(mat, directed=FALSE)

simplify(g, remove.loops = TRUE)

diameter(g)
#15
average.path.length(g)
#5.725341

plot(degree.distribution(g))

bet <- betweenness(g)
eig <- eigen_centrality(g)
deg <- degree(g, mode="all")
cls <- closeness(g)
cmp <- components(g, mode = c("weak","strong"))
V(g)$size <- log(deg)
```
# Combine and find Authors with highest centrality measures
```{r}
rankedAuthors <- data.frame(Author = V(g)$name,
                            Department = as.character(aggclean$Department[match(V(g)$name,aggclean$StandardAuthor)]),
                            Degree = deg,
                            Betweenness = bet,
                            Eigenvector = eig$vector,
                            Closeness = cls,
                            Component = cmp$membership)
#Degree
tmp <- rankedAuthors[order(-deg),2:7]
head(tmp, 10)

#Betweenness
tmp <- rankedAuthors[order(-bet),2:7]
head(tmp, 10)

#Eigenvector
tmp <- rankedAuthors[order(-eig$vector),2:7]
head(tmp, 10)

#Closeness
tmp <- rankedAuthors[order(-cls),2:7]
head(tmp, 10)
```
# Network Plots

```{r}
## Offset labels a bit: nodes printed from +x-axis counter-clockwise
ord <- V(g)                                               # node order
theta <- seq(0, 2*pi-2*pi/length(ord), 2*pi/length(ord))  # angle
theta[theta>pi] <- -(2*pi - theta[theta>pi])              # convert to [0, pi]
dists <- rep(c(.05, .15), length.out=length(ord))           # alternate distance

## Layout
co <- layout.fruchterman.reingold(g, dim = 2)

## Remove labels with less than 10 degrees
vlabels = V(g)$name
vlabels[deg < 10] = NA

## Color by Department
V(g)$color=as.character(aggclean$Department[match(V(g)$name,aggclean$StandardAuthor)])
V(g)$color=gsub("Psychology","red",V(g)$color)
V(g)$color=gsub("Organizational Science","red",V(g)$color)
V(g)$color=gsub("Geography & Earth Sciences","blue",V(g)$color)
V(g)$color=gsub("Economics","green",V(g)$color)
V(g)$color=gsub("Business","green",V(g)$color)
V(g)$color=gsub("Education","yellow",V(g)$color)
V(g)$color=gsub("Sociology","red",V(g)$color)
V(g)$color=gsub("Political Science","violet",V(g)$color)
V(g)$color=gsub("Public Health","orange",V(g)$color)
V(g)$color=gsub("Criminal Justice","violet",V(g)$color)
```

# Plots

```{r}
## Network Plot
plot(g, layout=co, 
     vertex.label.dist=dists, vertex.label.cex=0.4, vertex.label = vlabels , vertex.label.cex = "black",
     edge.curved = .2) #, vertex.color='#FFFFCC', edge.color='#E25822')

legend(x=-1.2, y=-0.6, c("Public Health", "Education", "Geography & Earth Sciences",
                         "Psychology, Org Sci & Sociology","Political Science & Crim Justice",
                         "Business"), pch=21,
       col="#777777", pt.bg=c("orange","yellow","blue","red","violet","green"), pt.cex=2, cex=.6, bty="n", ncol=1)

## Degree distribution
dd <- degree.distribution(g, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")
```

