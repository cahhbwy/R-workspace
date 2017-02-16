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

g<-init.graph(dd,dir=F)

V(g)$color<-ifelse(V(g)$label %in% data$CORP_RPT,"red",
            ifelse(V(g)$label %in% data$CZ_PERSON.x | V(g)$label %in% data$CZ_PERSON.y,"blue",
            ifelse(V(g)$label %in% data$PAWNEE.x | V(g)$label %in% data$PAWNEE.y,"yellow",
            ifelse(V(g)$label %in% data$NAME,"pink",
            ifelse(V(g)$label %in% data$ENTNAME,"purple",
            ifelse(V(g)$label %in% data$ENT_NAME,"magenta",
            ifelse(V(g)$label %in% data$MORE,"green",
            ifelse(V(g)$label %in% data$MORTGAGOR,"cyan","white"))))))))
V(g)$labelext<-ifelse(V(g)$label %in% data$CORP_RPT,paste(V(g)$label,"\nCORP_RPT"),
               ifelse(V(g)$label %in% data$CZ_PERSON.x | V(g)$label %in% data$CZ_PERSON.y,paste(V(g)$label,"\nCZ_PERSON"),
               ifelse(V(g)$label %in% data$PAWNEE.x | V(g)$label %in% data$PAWNEE.y,paste(V(g)$label,"\nPAWNEE"),
               ifelse(V(g)$label %in% data$NAME,paste(V(g)$label,"\nNAME"),
               ifelse(V(g)$label %in% data$ENTNAME,paste(V(g)$label,"\nENTNAME"),
               ifelse(V(g)$label %in% data$ENT_NAME,paste(V(g)$label,"\nENT_NAME"),
               ifelse(V(g)$label %in% data$MORE,paste(V(g)$label,"\nMORE"),
               ifelse(V(g)$label %in% data$MORTGAGOR,paste(V(g)$label,"\nMORTGAGOR"),V(g)$label))))))))

jpeg(filename="tmp.jpg",width=2048,height=2048,units='px')
plot(g,
     layout=layout.fruchterman.reingold,
     vertex.size=log(V(g)$bte+10),
     vertex.shape='circle',
     vertex.label=V(g)$labelext,
     vertex.color=V(g)$color,
     vertex.label.cex=1.0,
     vertex.label.color='black',
     edge.width=log(E(g)$weight+5),
     edge.label=E(g)$weight
)
dev.off()

gn<-graph.neighborhood(g,order=1)
gs<-gn[[which(V(g)$label=="嘉寓新新投资（集团）有限公司")]]
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