# Часть 1
library(lattice)
library(tibble)

df<-read.csv("F:/GitHub/Big_Data_Processing/Help_fo_Laba_6.csv", header=TRUE,sep = ";")

vec_country<-df[,1]
new_df<-df[,2:4]

num <- lapply(new_df, as.numeric)
res<-as.data.frame(num)
maxs <- apply(res, 2 , max)
maxs[is.na(maxs)] <- 0
maxs
mins <- apply(res, 2, min)
mins[is.na(mins)] <- -9999
View(res)

new_df <- scale(res, center = mins, scale = maxs - mins)
new_df[is.na(new_df)] <- 0
view(new_df)

dist.df <- dist(new_df)
clust.df <- hclust(dist.df, "ward.D")
plot(clust.df, vec_country, cex=0.5)
rect.hclust(clust.df, k=6, border="red")

groups <- cutree(clust.df, k=6);groups
g1<-colMeans(new_df[groups == 1, 1:3])
g2<-colMeans(new_df[groups==2, 1:3])
g3<-colMeans(new_df[groups==3, 1:3])
g4<-colMeans(new_df[groups==4, 1:3])
g5<-colMeans(new_df[groups==5, 1:3])
g6<-colMeans(new_df[groups==6, 1:3])


df[groups == 1, 1]
df[groups == 2, 1]
df[groups == 3, 1]
df[groups == 4, 1]
df[groups == 5, 1]
df[groups == 6, 1]

new_df_v2<-data.frame(g1,g2,g3,g4,g5,g6)
new_df_v2[is.na(new_df_v2)] <- 0

vec_color<-rainbow(3)

df1<-t(new_df_v2)
new_df_v2<-t(df1);new_df_v2
barplot(new_df_v2,col=vec_color) 
barplot(new_df_v2, ylim=c(0,5),  
        main = "Groups of countries", axes = FALSE, col=vec_color, 
        beside=TRUE)
axis(2, at = 0:5, labels = 0:5)
legend("topright", legend = rownames(new_df_v2), col=vec_color, lwd=10, bty = "n")
plot(1:47, clust.df$height, type='b')



class <- as.factor(groups)
my_data <- add_column(res, class, .after = "Salary")
my_data[is.na(my_data)] <- 0

xyplot(Price ~ Salary, data = my_data, group = my_data$class,auto.key = TRUE , cex = 3, pch = 20)
boxplot(Work ~ my_data$class, data = my_data, ylab = "Work",frame = FALSE, col = "blue")
xyplot(Work ~ Price + Salary|my_data$class,data=my_data, grid = T, auto.key=TRUE, cex = 3, pch = 20)

cloud(Work ~ Price  * Salary, group = my_data$class, data = my_data, auto.key = TRUE,cex = 3, pch = 20)




# Часть 2
library("scatterplot3d")


scatterplot3d(my_data, pch = 16, color=rainbow(7))
legend("bottom", legend = levels(my_data$class),
       col =  rainbow(7), 
       pch = c(16, 17, 18), 
       inset = -0.1, xpd = TRUE, horiz = TRUE)

library(klaR)

naive_data <- NaiveBayes(my_data$class ~ Work, data = my_data)
naive_data$tables
naive_data

par(mfrow=c(2,3))
plot(naive_data)

pred <- predict(naive_data, my_data)$class
pred

table(my_data$class, pred) 
Acc <- mean(pred == my_data$class)
paste("Точность = ", round(100*Acc, 2), "%", sep = "")

set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,]
nrow(trainData)             
nrow(testData)              
nrow(my_data) 


library(party)

myFormula <- class ~ Work + Price + Salary
data_ctree <- ctree(myFormula, data=trainData)
table(predict(data_ctree), trainData$class)

plot(data_ctree)

test_predicted <- predict(data_ctree, newdata=testData)
table(test_predicted, testData$class)


library(randomForest)
rf <- randomForest(class ~ .,data=trainData, ntree=30, proximity=TRUE)
table(predict(rf), trainData$class)
print(rf)
