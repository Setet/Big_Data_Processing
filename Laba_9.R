library(igraph)

#Вариант №13
#Задание №1
#1

N=13
G_size <- sample((N+10):((N/10+5)^2+(5*N)),1);G_size
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

u<-c(2*N+23,2*N+12,2*N-1,2*N,N+7)
v<-c(2*N+20,N+15,N+8,2*N+1,N+13)

u_1<-u%in%V(g1);u_1
v_1<-v%in%V(g1);v_1


for(i in 1:length(u)){
  if(u_1[i]&v_1[i] == TRUE ){
    g1<-g1+edge(u[i],v[i],replace=TRUE,color="black")
  }
}

plot(g1, edge.arrow.size=.2,vertex.size=20)

q<-neighbors(g1, V(g1)[N], mode = 1);q

q<-incident(g1,V(g1)[N], mode=c("all", "out", "in", "total"));q

q<-are.connected(g1, V(g1)[N+10], V(g1)[N+12]);q

g1[]

#4

w<-G_size+1;w
g1<-g1+vertices(w)

vec_graf<-c()
for(i in 1:G_size){
  q<-incident(g1,V(g1)[i], mode=c("all", "out", "in", "total"))
  vec_graf<-c(vec_graf,length(q))
}
q<-which.max(vec_graf);q

g1<-g1+edge(w,q,replace=TRUE,color="blue")

plot(g1, edge.arrow.size=.2,vertex.size=20)

g1[]

vec_vershin<-c()
for(i in 1:length(vec_graf)){
  if(vec_graf[i]<5&vec_graf[i]>2){
    vec_vershin<-c(vec_vershin,i)
  }
}
vec_vershin

#5

coords = layout_(g1, in_circle())
plot(g1,layout = coords)

coords = layout_(g1, as_tree())
plot(g1,layout = coords)

g1<-graph.lattice(length=100,dim=1,nei=5, circular = TRUE)
plot(g1,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai)

#6

diameter(g1)

all_shortest_paths(g1, 1, to = V(g1), mode = c("out", "all", "in"),weights = NULL)

#Задание №2

n=13

vec_size<-c(1:n)

vec_vershin_1<-c()
for(i in 1:length(vec_size)){
  for(j in 1:length(vec_size)){
    if(vec_size[i]!=vec_size[j]){
      vec_vershin_1<-c(vec_vershin_1,vec_size[i])
      vec_vershin_1<-c(vec_vershin_1,vec_size[j])
    }
  }
}
vec_vershin_1

g2<-graph(vec_vershin_1,n)

plot(g2, edge.arrow.size=.2,vertex.size=20)

k<-length(E(g2));k

vec_ves_redr_1<-c(sample(1:100,k,replace = TRUE));vec_ves_redr_1

vec_color<-rainbow(k)

E(g2)$weight<-vec_ves_redr_1

E(g2)$color<-vec_color

plot(g2, edge.arrow.size=.2,vertex.size=20)

vec_putei<-c()
vec_summ_vessa<-c()
counter_vessa<-c()
for(i in 2:n){
  vec_putei<-as.integer(shortest_paths(g2,V(g2)[1],V(g2)[i])$vpath[[1]])
  vec_putei<-c(vec_putei,1)
  proiz_vessa=1
  for(i in 1:(length(vec_putei)-1)){
    counter_vessa<-g2[vec_putei[i],vec_putei[i+1]]
    if(is.double(counter_vessa)){
      proiz_vessa=proiz_vessa*counter_vessa
    }
  }
  vec_summ_vessa<-c(vec_summ_vessa,proiz_vessa)
}

max(vec_summ_vessa)