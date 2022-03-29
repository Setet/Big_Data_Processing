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
vec_country<-c("Brazil","India","Lebanon","Turkey","Denmark")

list_year <- list(df_2014,df_2015,df_2016,df_2017,
                df_2018,df_2019,df_2020,df_2021)

df_Brazil<-df_2014[1,]
df_India<-df_2014[1,]
df_Lebanon<-df_2014[1,]
df_Turkey<-df_2014[1,]
df_Denmark<-df_2014[1,]

list_country<-list(df_Brazil,df_India,df_Lebanon,
                   df_Turkey,df_Denmark)

for(i in 1:8){
  qwerty=which(list_year[[i]][,2]==vec_country[1])
  if(is.integer(qwerty)&&length(qwerty)!=0L)
  {
    list_country[[i]] <- list_year[[i]][qwerty,1:11]
  }
  else
  {
    list_country[[i]]<-NA
  }
  df_Brazil[i,]<-list_country[[i]]
}

for(i in 1:8){
  qwerty=which(list_year[[i]][,2]==vec_country[2])
  if(is.integer(qwerty)&&length(qwerty)!=0L)
  {
    list_country[[i]] <- list_year[[i]][qwerty,1:11]
  }
  else
  {
    list_country[[i]]<-NA
  }
  df_India[i,]<-list_country[[i]]
}

for(i in 1:8){
  qwerty=which(list_year[[i]][,2]==vec_country[3])
  if(is.integer(qwerty)&&length(qwerty)!=0L)
  {
    list_country[[i]] <- list_year[[i]][qwerty,1:11]
  }
  else
  {
    list_country[[i]]<-NA
  }
  df_Lebanon[i,]<-list_country[[i]]
}

for(i in 1:8){
  qwerty=which(list_year[[i]][,2]==vec_country[4])
  if(is.integer(qwerty)&&length(qwerty)!=0L)
  {
    list_country[[i]] <- list_year[[i]][qwerty,1:11]
  }
  else
  {
    list_country[[i]]<-NA
  }
  df_Turkey[i,]<-list_country[[i]]
}

for(i in 1:8){
  qwerty=which(list_year[[i]][,2]==vec_country[5])
  if(is.integer(qwerty)&&length(qwerty)!=0L)
  {
    list_country[[i]] <- list_year[[i]][qwerty,1:11]
  }
  else
  {
    list_country[[i]]<-NA
  }
  df_Denmark[i,]<-list_country[[i]]
}

vec_year<-c(2014:2021)

country_award<-c("Бразилия","Индия","Ливан","Турция","Дания")

vec_col<-rainbow(5)

vec_grafick<-c("","График по индексу качества жизни",
               "График по индексу покупательной способности",
               "График по индексу безопасности",
               "График по индексу здравохранения",
               "График по индексу стоимости жизни",
               "График по индексу соотношения цены недвижимости к доходу",
               "График по индексу времени в пути на работу",
               "График по индексу загрязнения",
               "График по климатическому индексу")

vec_grafick_years<-c("в странах 2014-2021")

df_Brazil<-df_Brazil[,-c(2)]
df_Brazil[,1]<-vec_year

df_India<-df_India[,-c(2)]
df_India[,1]<-vec_year

df_Lebanon<-df_Lebanon[,-c(2)]
df_Lebanon[,1]<-vec_year

df_Turkey<-df_Turkey[,-c(2)]
df_Turkey[,1]<-vec_year

df_Denmark<-df_Denmark[,-c(2)]
df_Denmark[,1]<-vec_year

max_axis_y=300

for (i in 2:10)
{
  plot(vec_year,df_Brazil[,i] , type="b",  ann=FALSE,
       col=vec_col[1], lty=2, pch=2, lwd=2,ylim=c(0, max_axis_y))
  
  lines(vec_year,df_India[,i] , type="b",  ann=FALSE,
        col=vec_col[2], lty=2, pch=2, lwd=2,ylim=c(0, max_axis_y))
  
  lines(vec_year,df_Lebanon[,i] , type="b",  ann=FALSE,
        col=vec_col[3], lty=2, pch=2, lwd=2,ylim=c(0, max_axis_y))
  
  lines(vec_year,df_Turkey[,i] , type="b",  ann=FALSE,
        col=vec_col[4], lty=2, pch=2, lwd=2,ylim=c(0, max_axis_y))
  
  lines(vec_year,df_Denmark[,i] , type="b",  ann=FALSE,
        col=vec_col[5], lty=2, pch=2, lwd=2,ylim=c(0, max_axis_y))
  
  title(main=paste(vec_grafick[i],vec_grafick_years), col.main="red",
        xlab="Года",
        ylab="Индексы",
        cex.lab=1)
  
  legend("topright", country_award,lty=2,pch=2, col=vec_col)
}

url = read_html("https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/")
selector_name <- ".post-list-item-title-link span"
name <- html_text(html_nodes(url, selector_name), trim = TRUE) %>% as.array()
selector_name <- "address"
address <- html_text(html_nodes(url, selector_name), trim = TRUE) %>% as.array()
address <- gsub("\u{e01e}", "", address)
address <- substr(address, 2, nchar(address))
address <- gsub("\n", "", address)
address <- gsub("  ", "", address)
selector_name <- ".post-list-item-preview"
link <- html_nodes(url, selector_name) %>% html_attr("href") %>% as.array()
link <- paste0("https://kudago.com", link)
remove <- c(41)
link <- link[-remove]
name <- name[-remove]
df_Museums <- data.frame(Название = name, Адрес = address, Ссылка = link)
