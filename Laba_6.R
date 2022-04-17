#df <- read.csv("Econom_Cities_data.csv" , sep = "," , header = TRUE)

#  Пример классификации с помошью кластерного анализа

#  Шаг 1.  Чтение данных. Обратите внимание на опцию: dec=','
city.01 <- read.table("Help_fo_Laba_6.csv", header = TRUE,";", dec=',')

#   Шаг 2.  Удаление пропущенных значений
#   В данной задаче пропущенных значений нет.
#   Удалять нечего.

#   Шаг 3.  Стандартизация переменных.
#   В данной задаче переменные существенно различны.
#   Стандартизировать надо.
city.02 <- scale(city.01[,2:4], center = TRUE, scale = TRUE)
#   Вариант 2 - к минимуму 0 и максимуму 1

#Исключим колонку "Страна"
city.02<-protein.01[,-1]
maxs <- apply(city.02, 2, max)
mins <- apply(city.02, 2, min)

city.02 <- scale(city.02, center = mins, scale = maxs - mins)

#Вернем колонку "Страна"
Countries<-city.01$City
city.02<-data.frame(Countries,city.02)

#  Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)
dist.city <- dist(city.02 [,2:4])

#  Проводим кластерный анализ, 
#  результаты записываем в список clust.protein
# hclust ожидает матрицу расстояния, а не исходные данные.

clust.city <- hclust(dist.city, "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.city

#  Шаг 4.  Построение дендрограммы
k = 4

plot(clust.city, labels = city.01$City)
rect.hclust(clust.city, k = 4, border="red")
abline(h = 1.5, col = "blue", lwd='3') # h - horizontal line, col - color

#plot(1:24, clust.city$height, type='b') # каменная осыпь(не работает!!!)

#  Разделим Страны на 4 кластера
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.city, k) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups

dend <- as.dendrogram(clust.city)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)
#dev.off() 


#  Выведем страны соответсвенно сформированным кластерам 
city.01[groups==1, 1]
city.01[groups==2, 1]
city.01[groups==3, 1]
city.01[groups==4, 1]

#  Для каждого вида еды определяем, 
#  какая доля потребителей в среднем кластере приобретала этот вид еды

#  в 1-ом кластере
g1<-colMeans(city.01[groups==1, 2:4])
#  во 2-ом кластере
g2<-colMeans(city.01[groups==2, 2:4])
#  в 3-ем кластере
g3<-colMeans(city.01[groups==3, 2:4])
#  во 4-ом кластере
g4<-colMeans(city.01[groups==4, 2:4])

df<-data.frame(g1,g2,g3,g4); df
df1<-t(df); df1
barplot(df1, ylim=range(pretty(c(0, ))), 
        main="Распредление пищевых предпочтенй в странах Европы", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

#Построим боксплот, который поможет нам убедиться, что мы 
#действительно имеем 5 класса существенно отличных друг от друга:

boxplot(df)
# Для датасета assess
#barplot(df1, ylim=range(pretty(c(0, 60))), main="Распредление претендентов", col=c("magenta","red","yellow","blue"),legend=rownames(df1))