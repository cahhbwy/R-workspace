setwd("E:\\workspace\\R")
data=read.csv("result_data1.csv",header=TRUE)
library(igraph)
library(dplyr)
library(maps)
relation=c(
  c("ENT_NAME","CORP_RPT"),
  c("ENT_NAME","NAME"),
  c("ENT_NAME","ENTNAME"),
  c("ENT_NAME","MORE"),
  c("CORP_RPT","CZ_PERSON.x"),
  c("CORP_RPT","CZ_PERSON.y"),
  c("CZ_PERSON.x","PAWNEE.x"),
  c("CZ_PERSON.y","PAWNEE.y"),
  c("MORE","MORTGAGOR")
  )
dd<-data.frame("from"=NA,"to"=NA)
dd<-dd[-1,]
for(i in seq(1,length(relation),2)){
  ddtmp<-data[relation[c(i,i+1)]]
  colnames(ddtmp)<-c('from','to')
  ddtmp<-na.omit(ddtmp)
  dd<-rbind(dd,ddtmp)
}

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

g<-init.graph(dd)

jpeg(filename="tmp.jpg",width=2048,height=2048,units='px')
member<-clusters(g,mode = "weak")
V(g)$member<-member$membership
mem.col<-rainbow(length(unique(member$membership)),alpha=0.3)
V(g)$color<-mem.col[member$membership]
plot(g,
     layout=layout.fruchterman.reingold,
     vertex.label=V(g)$name,
     vertex.size=log(V(g)$bte+2),
     vertex.shape='circle',
     vertex.color=V(g)$color,
     vertex.label.cex=1.0,
     edge.width=log(E(g)$weight+2),
     edge.label=E(g)$weight
)
dev.off()
