library(rvest)

c<-c()
for(i in 2014:2021)
{
  x<-'https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=';
  y<-i;
  c<-c(c,paste0(x,y))
}

url= read_html(c[1])
nodes = html_nodes(url, 'table#t2');nodes

df_2014 = html_table(nodes[[1]])%>%as.data.frame()
df_2014[,1]<-c(1:length(df_2014[,1]))

url= read_html(c[2])
nodes = html_nodes(url, 'table#t2');nodes

df_2015 = html_table(nodes[[1]])%>%as.data.frame()
df_2015[,1]<-c(1:length(df_2015[,1]))

url= read_html(c[3])
nodes = html_nodes(url, 'table#t2');nodes

df_2016 = html_table(nodes[[1]])%>%as.data.frame()
df_2016[,1]<-c(1:length(df_2016[,1]))

url= read_html(c[4])
nodes = html_nodes(url, 'table#t2');nodes

df_2017 = html_table(nodes[[1]])%>%as.data.frame()
df_2017[,1]<-c(1:length(df_2017[,1]))

url= read_html(c[5])
nodes = html_nodes(url, 'table#t2');nodes

df_2018 = html_table(nodes[[1]])%>%as.data.frame()
df_2018[,1]<-c(1:length(df_2018[,1]))

url= read_html(c[6])
nodes = html_nodes(url, 'table#t2');nodes

df_2019 = html_table(nodes[[1]])%>%as.data.frame()
df_2019[,1]<-c(1:length(df_2019[,1]))

url= read_html(c[7])
nodes = html_nodes(url, 'table#t2');nodes

df_2020 = html_table(nodes[[1]])%>%as.data.frame()
df_2020[,1]<-c(1:length(df_2020[,1]))

url= read_html(c[8])
nodes = html_nodes(url, 'table#t2');nodes

df_2021 = html_table(nodes[[1]])%>%as.data.frame()
df_2021[,1]<-c(1:length(df_2021[,1]))

#вариант 13 (Бразилия, Индия, Ливан, Турция, Дания)
#пока оставляю костыли
df_Brazil<-df_2014[1,]
df_India<-df_2014[1,]
df_Lebanon<-df_2014[1,]
df_Turkey<-df_2014[1,]
df_Denmark<-df_2014[1,]

a<-c(df_2014[,2],df_2015[,2]);a

q=0
for (i in 1:length(df_2014[,2])){
  q=q+1
  p=df_2014[i,2]
  if(p=="Brazil")
  {
    df_Brazil[1,] <- df_2014[q,1:11]
  }
  if(p=="India")
  {
    df_India[1,] <- df_2014[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[1,] <- df_2014[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[1,] <- df_2014[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[1,] <- df_2014[q,1:11]
  }
}

q=0
for (i in 1:length(df_2015[,2])){
  q=q+1
  p=df_2015[i,2]
  if(p=="Brazil")
  {
    df_Brazil[2,] <- df_2015[q,1:11]
  }
  if(p=="India")
  {
    df_India[2,] <- df_2015[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[2,] <- df_2015[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[2,] <- df_2015[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[2,] <- df_2015[q,1:11]
  }
}

q=0
for (i in 1:length(df_2016[,2])){
  q=q+1
  p=df_2016[i,2]
  if(p=="Brazil")
  {
    df_Brazil[3,] <- df_2016[q,1:11]
  }
  if(p=="India")
  {
    df_India[3,] <- df_2016[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[3,] <- df_2016[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[3,] <- df_2016[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[3,] <- df_2016[q,1:11]
  }
}

q=0
for (i in 1:length(df_2017[,2])){
  q=q+1
  p=df_2017[i,2]
  if(p=="Brazil")
  {
    df_Brazil[4,] <- df_2017[q,1:11]
  }
  if(p=="India")
  {
    df_India[4,] <- df_2017[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[4,] <- df_2017[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[4,] <- df_2017[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[4,] <- df_2017[q,1:11]
  }
}

q=0
for (i in 1:length(df_2018[,2])){
  q=q+1
  p=df_2018[i,2]
  if(p=="Brazil")
  {
    df_Brazil[5,] <- df_2018[q,1:11]
  }
  if(p=="India")
  {
    df_India[5,] <- df_2018[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[5,] <- df_2018[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[5,] <- df_2018[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[5,] <- df_2018[q,1:11]
  }
}

q=0
for (i in 1:length(df_2019[,2])){
  q=q+1
  p=df_2019[i,2]
  if(p=="Brazil")
  {
    df_Brazil[6,] <- df_2019[q,1:11]
  }
  if(p=="India")
  {
    df_India[6,] <- df_2019[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[6,] <- df_2019[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[6,] <- df_2019[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[6,] <- df_2019[q,1:11]
  }
}

q=0
for (i in 1:length(df_2020[,2])){
  q=q+1
  p=df_2020[i,2]
  if(p=="Brazil")
  {
    df_Brazil[7,] <- df_2020[q,1:11]
  }
  if(p=="India")
  {
    df_India[7,] <- df_2020[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[7,] <- df_2020[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[7,] <- df_2020[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[7,] <- df_2020[q,1:11]
  }
}

q=0
for (i in 1:length(df_2021[,2])){
  q=q+1
  p=df_2021[i,2]
  if(p=="Brazil")
  {
    df_Brazil[8,] <- df_2021[q,1:11]
  }
  if(p=="India")
  {
    df_India[8,] <- df_2021[q,1:11]
  }
  if(p=="Lebanon")
  {
    df_Lebanon[8,] <- df_2021[q,1:11]
  }
  if(p=="Turkey")
  {
    df_Turkey[8,] <- df_2021[q,1:11]
  }
  if(p=="Denmark")
  {
    df_Denmark[8,] <- df_2021[q,1:11]
  }
}

df_Brazil<-df_Brazil[,-c(1,2)]
df_India<-df_India[,-c(1,2)]
df_Lebanon<-df_Lebanon[,-c(1,2)]
df_Turkey<-df_Turkey[,-c(1,2)]
df_Denmark<-df_Denmark[,-c(1,2)]

vec_year<-c(2014,2015,2016,2017,2018,
            2019,2020,2021)

country_award<-c("Бразилия","Индия","Ливан","Турция","Дания")

vec_col<-rainbow(5)

vec_grafick<-c("График по индексу качества жизни",
               "График по индексу покупательной способности",
               "График по индексу безопасности",
               "График по индексу здравохранения",
               "График по индексу стоимости жизни",
               "График по индексу соотношения цены недвижимости к доходу",
               "График по индексу времени в пути на работу",
               "График по индексу загрязнения",
               "График по климатическому индексу")

vec_grafick_years<-c("в странах 2014-2021")

for (i in 1:9)
{
  plot(vec_year,df_Brazil[,i] , type="b",  ann=FALSE,
       col=vec_col[1], lty=2, pch=2, lwd=2,ylim=c(0, 400))
  
  lines(vec_year,df_India[,i] , type="b",  ann=FALSE,
        col=vec_col[2], lty=2, pch=2, lwd=2,ylim=c(0, 400))
  
  lines(vec_year,df_Lebanon[,i] , type="b",  ann=FALSE,
        col=vec_col[3], lty=2, pch=2, lwd=2,ylim=c(0, 400))
  
  lines(vec_year,df_Turkey[,i] , type="b",  ann=FALSE,
        col=vec_col[4], lty=2, pch=2, lwd=2,ylim=c(0, 400))
  
  lines(vec_year,df_Denmark[,i] , type="b",  ann=FALSE,
        col=vec_col[5], lty=2, pch=2, lwd=2,ylim=c(0, 400))
  
  title(main=paste(vec_grafick[i],vec_grafick_years), col.main="red",
        xlab="Года",
        ylab="Индексы",
        cex.lab=1)
  
  legend("topright", country_award,lty=2,pch=2, col=vec_col)
}
