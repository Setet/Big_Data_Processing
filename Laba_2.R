#основной дата фрейм
df <- read.csv("Help_fo_Laba_2.csv", sep = ";" , header = TRUE);df

fill_name<-colnames(df)
fill_name<-fill_name[-1]

#1
#фрейм макс
fill_max <-c()
for (i in 2:11){
  p<-df[,i]
  fill_max<-c(fill_max,max(p))
}

df_max = data.frame(
  Цифра = fill_name,
  Максимум = fill_max);df_max

#фрейм мин
fill_min <-c()
for (i in 2:11){
  p<-df[,i]
  fill_min<-c(fill_min,min(p))
}

df_min = data.frame(
  Цифра = fill_name,
  Минимум = fill_min);df_min

#фрейм средн
fill_mean <-c()
for (i in 2:11){
  p<-df[,i]
  fill_mean<-c(fill_mean,mean(p))
}

df_mean = data.frame(
  Цифра = fill_name,
  Среднее = fill_mean);df_mean
#2
#фрейм людей которые выбрали >7 и <3
fill_all <-c()
for (i in 1:10){
  p<-df[,i]
  fill_all<-c(fill_all,length(p[which(p<3 | p>7)]))
}
sum(fill_all)

#3
fill_average<-c()
a<-order(df_mean[,"Среднее"],decreasing = TRUE)
for (i in 1:length(df_mean[,c("Среднее")])){
  fill_average[i] <- df_mean[,"Среднее"][a[i]]
}
fill_average 

#4
fill_colors <- c()
for ( i in 1:length(df_mean[,c("Среднее")]) ) {
  if (fill_average [i] > 6){
    fill_colors <- c(fill_colors, "#00FF00")
  } else {
    fill_colors <- c(fill_colors, "#FF0000")
  }
}

p<-df_mean[a,"Цифра"];p
barplot(fill_average ,names.arg = p, col = fill_colors,
        xlab = "Рейтинг",
        ylab = "Цифры",
        main="Топ лучших цифр",
        width = sqrt(fill_average))
