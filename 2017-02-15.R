setwd("E:\\workspace\\R")
library(xlsx)
data=read.xlsx("data.xlsx","Sheet1",encoding = "UTF-8")
library(igraph)
library(dplyr)
library(maps)

init.graph<-function(data,dir=F,rem.multi=T){
  labels<-union(unique(data[,1]),unique(data[,2]))
  ids<-1:length(labels);names(ids)<-labels
  from<-as.character(data[,1]);to<-as.character(data[,2])
  edges<-na.omit(matrix(c(ids[from],ids[to]),ncol=2))
  g<-graph.empty(directed=dir)
  g<-add.vertices(g,length(labels))
  V(g)$label<-labels
  g<-add.edges(g,t(edges))
  V(g)$bte <- betweenness(g, directed = dir)
  if(rem.multi){
    E(g)$weight<-count.multiple(g)
    g<-simplify(g,remove.multiple=TRUE,remove.loops=TRUE,edge.attr.comb="mean")
  }
  g
}

g<-init.graph(data,dir=F)

jpeg(filename="tmp.jpg",width=2048,height=2048,units='px')
#member<-clusters(g,mode = "weak")
#V(g)$member<-member$membership
#mem.col<-rainbow(length(unique(member$membership)),alpha=0.3)
#V(g)$color<-mem.col[member$membership]

V(g)$color<-ifelse(V(g)$label %in% unique(data[,1]),ifelse(V(g)$label %in% unique(data[,2]),'green','red'),'blue')
V(g)$labelext<-ifelse(V(g)$label %in% unique(data[,1]),ifelse(V(g)$label %in% unique(data[,2]),paste(V(g)$label,"source\ntarget",sep = "\n"),paste(V(g)$label,"source",sep = "\n")),paste(V(g)$label,"target",sep = "\n"))

plot(g,
     layout=layout.fruchterman.reingold,
     vertex.size=log(V(g)$bte+10),
     vertex.shape='circle',
     vertex.label=V(g)$labelext,
     vertex.color=V(g)$color,
     vertex.label.cex=1.5,
     vertex.label.color='black',
     edge.width=log(E(g)$weight+5),
     edge.label=E(g)$weight
)
dev.off()

gn<-graph.neighborhood(g,order=1)
gs<-gn[[which(V(g)$label=="北京起源财富网络科技")]]
jpeg(filename="tmpsub.jpg",width=1024,height=1024,units='px')
plot(gs,
     layout=layout.fruchterman.reingold,
     vertex.size=log(V(gs)$bte+10),
     vertex.shape='circle',
     vertex.label=V(gs)$labelext,
     vertex.color=V(gs)$color,
     vertex.label.cex=1.2,
     vertex.label.color='black',
     edge.width=log(E(gs)$weight+5),
     edge.label=E(gs)$weight
)
dev.off()
