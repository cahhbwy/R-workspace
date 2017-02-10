setwd("E:\\workspace\\R")
data=read.csv("result_data.csv",header=TRUE)
library(igraph)
library(dplyr)
library(maps)
#data=data[1:33000,]
ddtmp1<-data[,3:4]
colnames(ddtmp1)<-c('from','to')
ddtmp1<-na.omit(ddtmp1)
ddtmp2<-data[,4:5]
colnames(ddtmp2)<-c('from','to')
ddtmp2<-na.omit(ddtmp2)
ddtmp3<-data[,6:7]
colnames(ddtmp3)<-c('from','to')
ddtmp3<-na.omit(ddtmp3)
ddtmp4<-data[,c(3,8)]
colnames(ddtmp4)<-c('from','to')
ddtmp4<-na.omit(ddtmp4)
dd<-rbind(ddtmp1,ddtmp2)
dd<-rbind(dd,ddtmp3)
dd<-rbind(dd,ddtmp4)

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

g0<-init.graph(dd)
c<-clusters(g0,mode = "weak")

jpeg(filename="tmp0.jpg",width=8192,height=8192,units='px')
g<-induced.subgraph(g0, which(membership(c)==1))
plot(g,
    layout=layout.fruchterman.reingold,
    vertex.label=V(g)$name,
    vertex.size=log(V(g)$bte+2,sqrt(c[[2]][1]))/10.0,
    vertex.shape='circle',
    vertex.label.cex=1.0,
    edge.width=log(E(g)$weight+2),
    edge.label=E(g)$weight
)
dev.off()

i<-2
count<-0
num<-1
while(i<=c[[3]]){
  g<-induced.subgraph(g0, which(membership(c)==i))
  count<-count+c[[2]][i]
  i<-i+1
  while(count<500&&i<=c[[3]]){
    count<-count+c[[2]][i]
    g<-g+induced.subgraph(g0, which(membership(c)==i))
    i<-i+1
  }
  jpeg(filename=paste("tmp",num,".jpg",sep = ""),width=2048,height=2048,units='px')
  plot(g,
       layout=layout.fruchterman.reingold,
       vertex.label=V(g)$name,
       vertex.size=log(V(g)$bte+2,sqrt(count))*5,
       vertex.shape='circle',
       vertex.label.cex=1,
       edge.width=log(E(g)$weight+2),
       edge.label=E(g)$weight
  )
  dev.off()
  num<-num+1
  count<-0
}


