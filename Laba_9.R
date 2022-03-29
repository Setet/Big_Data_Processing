library(igraph)
#Вариант №13
#Задание №1
#1
N=13
G_size <- sample((N+10):((N/10+5)^2+(5*N)),1);G_size # кол-во вершин+узлов
g<-graph.ring(n=G_size);g
plot(g)
g[]
vcount(g)
ecount(g)
#2
g1<-graph.empty()+vertices(1:G_size)

g1<-g1+edges(sample(V(g1),(8*N),replace=TRUE),color="red")
plot(g1, edge.arrow.size=.2,vertex.size=20)
g1[]

g1<-g1+edges(sample(V(g1),(10*N),replace=TRUE),color="blue") 
plot(g1, edge.arrow.size=.2,vertex.size=20)
g1[]
#3
#c(2*N+23,2*N+20,2*N+12,N+15,2*N-1,N+8,2*N,2*N+1,N+7,N+13)
g<-graph(c(2*N+23,2*N+20,2*N+12,N+15,2*N-1,N+8,2*N,2*N+1,N+7,N+13),n=10)

#u %in% V(g1) 
#v %in% V(g1)

#подумать как сделать in !!!
g1<-g1+edges(V(g1),(g%in%g1),replace=TRUE,color="black")
plot(g1, edge.arrow.size=.2,vertex.size=20)

#вроде правильно...
q<-neighbors(g1, V(g1)[N], mode = 1);q

q<-incident(g1,V(g1)[N], mode=c("all", "out", "in", "total"));q
q<-are.connected(g1, V(g1)[N+10], V(g1)[N+12]);q

g1[]
#4
g1<-g1+vertices(G_size+1)

for(i in 1:size(g1)){
  q<-incident(g1,V(g1)[i], mode=c("all", "out", "in", "total"))
}

plot(g1, edge.arrow.size=.2,vertex.size=20)

