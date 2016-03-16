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

depts <- c("Economics",
"Finance",
"BISOM",
"Management",
"Accounting",
"Missing",
"Counseling",
"Educational Leadership",
"Middle, Secondary, and K-12 Education",
"Reading and Elementary Education",
"Special Education and Child Development",
"Africana Studies",
"Anthropology",
"Communication Studies",
"Criminal Justice and Criminology",
"Geography & Earth Sciences",
"Global, International & Area Studies",
"Organizational Behavior",
"Political Science & Public Administration",
"Psychology",
"Sociology",
"Social Work",
"Nursing",
"Public Health")

colleges <- c("Liberal Arts & Sciences",
           "CHHS",
           "Education",
           "Business")

socsci_articles <- subset(rawfile, University == "UNCC" & (Department %in% depts) & (College %in% colleges))

socsci_key <- unique(socsci_articles$ID)

uncc_articles <- subset(rawfile, University == "UNCC")

subsets <- subset(uncc_articles, ID %in% socsci_key)

aggtemp <- subsets %>% group_by(College, Department) %>% summarise(number = n()) 

# Save Article ID, Standardized Author and Department
socsci <- subsets[,c(2,4,7,8)]

socsci$Department[socsci$Department %in% c('UNCC (No Department Listed)','Other')] <- "Missing"
socsci$College[socsci$College == "Not UNCC"] <- "Missing"
socsci$College[socsci$College == "Other"] <- "Missing"

aggclean <- socsci %>% group_by(StandardAuthor, Department, College) %>% summarise(number = n()) 

```
# Create Edge List
```{r}
ss <- split.data.frame(socsci[,c(2,3,4)],socsci$ID)

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

##
edge.final[3] <- lapply(edge.final[3], as.character)
head(edge.final)
names(edge.final) <- c("Source","Target","EID")
edge.final$Weight <- 1
edge.final$Type <- "Undirected"
row.names(edge.final) <- NULL
```
# Export Node & Edges for Gephi

```{r}
write.csv(edge.final, file = "SocialScienceEdgeList.csv")

aggauthor <- socsci %>% group_by (StandardAuthor, College, Department) %>% summarise(number = n())

tmp <- data.frame(aggauthor[,c(1,2,3,4)])
names(aggauthor) <- c("ID","College","Department","Deg")

write.csv(aggauthor, file = "SocialScienceNodeList.csv")
```

# Make Graph & Calculate Centrality Measures

```{r}
mat <- as.matrix(edge.final[,1:2])

g <- graph_from_edgelist(mat, directed=FALSE)
```

# Network Graph Function to Calculate Network Metrics and Plot Graph

```{r}
networkGraph <- function(g,NodeSize,labelsize,minDegree){

simplify(g, remove.loops = TRUE)

print("Diameter:")
print(diameter(g))

print("Average Path Length:")
print(average.path.length(g))

bet <- betweenness(g)
eig <- eigen_centrality(g)
deg <- degree(g, mode="all")
cls <- closeness(g)
cmp <- components(g, mode = c("weak","strong"))
V(g)$size <- NodeSize

rankedAuthors <- data.frame(Author = V(g)$name,
                            Department = as.character(aggclean$Department[match(V(g)$name,aggclean$StandardAuthor)]),
                            Degree = deg,
                            Betweenness = bet,
                            Eigenvector = eig$vector,
                            Closeness = cls,
                            Component = cmp$membership)
#Degree
tmp <- rankedAuthors[order(-deg),2:7]
print(head(tmp, 10))

#Betweenness
tmp <- rankedAuthors[order(-bet),2:7]
print(head(tmp, 10))

#Eigenvector
tmp <- rankedAuthors[order(-eig$vector),2:7]
print(head(tmp, 10))

#Closeness
tmp <- rankedAuthors[order(-cls),2:7]
print(head(tmp, 10))

## Offset labels a bit: nodes printed from +x-axis counter-clockwise
ord <- V(g)                                               # node order
theta <- seq(0, 2*pi-2*pi/length(ord), 2*pi/length(ord))  # angle
theta[theta>pi] <- -(2*pi - theta[theta>pi])              # convert to [0, pi]
dists <- rep(c(.05, .25), length.out=length(ord))           # alternate distance

## Layout
co <- layout.fruchterman.reingold(g, dim = 2)

## Remove labels with less than 10 degrees
vlabels = V(g)$name
vlabels[deg < minDegree] = NA

## Color by Department
V(g)$color=as.character(aggclean$College[match(V(g)$name,aggclean$StandardAuthor)])
V(g)$color=gsub("Liberal Arts & Sciences","red",V(g)$color)
V(g)$color=gsub("CHHS","blue",V(g)$color)
V(g)$color=gsub("Business","green",V(g)$color)
V(g)$color=gsub("Education","yellow",V(g)$color)
V(g)$color=gsub("Missing","gray",V(g)$color)
V(g)$color=gsub("Engineering","orange",V(g)$color)
V(g)$color=gsub("Architecture","violet",V(g)$color)
V(g)$color=gsub("Computing & Informatics","brown",V(g)$color)

## Plot
plot(g, layout=co, 
     vertex.label.dist=dists, vertex.label.cex=labelsize, vertex.label = vlabels , vertex.label.cex = "black",
     edge.curved = .2, vertex.label.degree=theta) #, vertex.color='#FFFFCC', edge.color='#E25822')

legend(x=-1.2, y=-0.6, c("Liberal Arts & Sciences",
                         "CHHS",
                         "Business",
                         "Education",
                         "Missing",
                         "Engineering",
                         "Architecture",
                         "CIS"), pch=21,
       col="#777777", pt.bg=c("red","blue","green","yellow","gray","orange","violet","brown"), pt.cex=1, cex=.6, bty="n", ncol=1)


dd <- degree.distribution(g, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")

return(rankedAuthors)
}
```

# Run the Full Network
```{r}
rankedAuthors <- networkGraph(g,2,.4,10)
```
```

