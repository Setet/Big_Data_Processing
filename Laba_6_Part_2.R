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

#   Исключим колонку "Страна"
city.02<-city.01[,-1]
maxs <- apply(city.02, 2, max)
mins <- apply(city.02, 2, min)

city.02 <- scale(city.02, center = mins, scale = maxs - mins)

#   Вернем колонку "Страна"
Countries<-city.01$City
city.02<-data.frame(Countries,city.02)

#   Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)
dist.city <- dist(city.02 [,2:4])

#   Проводим кластерный анализ, 
#   результаты записываем в список clust.protein
#   hclust ожидает матрицу расстояния, а не исходные данные.

clust.city <- hclust(dist.city, "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.city

#  Шаг 4.  Построение дендрограммы
k = 4   #явно 4 кластера

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

#  Для каждого столбц определяем, 
#  какая доля стран в среднем кластере приобретала этот столбец

#   в 1-ом кластере
g1<-colMeans(city.01[groups==1, 2:4])
#   во 2-ом кластере
g2<-colMeans(city.01[groups==2, 2:4])
#   в 3-ем кластере
g3<-colMeans(city.01[groups==3, 2:4])
#   во 4-ом кластере
g4<-colMeans(city.01[groups==4, 2:4])

#   делаем дата фрейм из векторов групп кластеров
df<-data.frame(g1,g2,g3,g4); df
df1<-t(df); df1

#------------------------------------------------------------
library (lattice)
library("scatterplot3d")

df <- read.table("Help_fo_Laba_6.csv", header = TRUE,";", dec=',')

my_data <- df

#выведем график рассеяния с минимальным количеством параметров без выделения имени
xyplot(Work ~ Price, data = my_data)

#выведем график рассеяния с минимальным количеством параметров с выделением имени
xyplot(Work ~ Price,group = City, data = my_data,auto.key = TRUE)

#Боксплот, отражающий характеристики классов типов 
boxplot(Salary ~ City, data = df, ylab = "Work", frame = FALSE, col = "red")

# График, классифицирующий типы согласно их полей
xyplot(Work~Salary+Price|City,data=df, grid = T, auto.key=TRUE)

#Добавим линию сглаживания:(как-то что-то странно)
xyplot( Work~Salary+Price|City,layout = c(3, 1),group = City, data = df, type = c("p", "smooth"), scales = "free")

#Построим трехмерный график наших классов
cloud(Work~Salary*Price, group = City, data = df, auto.key = TRUE) 


vec_color<-rainbow(48)
#Трехмерная классификация типов:
scatterplot3d(df[,2:4], pch = 16, color=vec_color,main = "Трехмерный график классов") 
#-------------------------------------------------------

library(klaR)
naive_df<-df
naive_df$groups<- c(as.factor(groups))
naive_df_ <- NaiveBayes(naive_df$groups ~ ., data = naive_df) 
naive_df$tables 
naive_df_$tables$Work.Width 
naive_df_



#делаем графики по байсу
opar=par() 
opar
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) 
plot(naive_df_,lwd = 2, legendplot=TRUE)
#восстановление
par=opar


# Классификация Decision Tree

set.seed(1234)
ind <- sample(2, nrow(naive_df), replace=TRUE, prob=c(0.7, 0.3))
trainData <- naive_df[ind==1,]
testData <- naive_df[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(naive_df)



library(party)
myFormula <- groups ~ Work + Price
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
table(predict(df_ctree), trainData$Work) 
predict(df_ctree)
plot(predict(df_ctree))

#Алгоритм Random Forest 

library(randomForest) 
rf <- randomForest(groups ~ .,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$groups)
print(rf)