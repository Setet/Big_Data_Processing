#основной дата фрейм
df <- read.csv("Help_fo_Laba_3.csv", sep = ";" , header = TRUE);df

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
#фрейм людей которые выбрали >5
fill_all <-c()
for (i in 2:10){
  p<-df[,i]
  fill_all<-c(fill_all,p[which(p>5)])
}
fill_all

#3
fill_average<-c()
a<-order(df_mean[,"Среднее"],decreasing = TRUE);a
for (i in 1:length(df_mean[,c("Среднее")])){
  fill_average[i] <- df_mean[,"Среднее"][a[i]]
}
fill_average 

#4
df_clone<-df;df_clone
df_clone[df_clone<7] <- 0;df_clone #<-NA

#5
wer <-c()
for (i in 2:10){
  p<-df_clone[,i]
  wer<-c(wer,p)
}
wer

boxplot(df[,-c(1)],col = rainbow(6),
        xlab = "Боксплоты",
        ylab = "Значения",
        main="Рейтинг цифр 39гр.")

hist(wer,breaks=20,col = rainbow(6),
     xlab = "Значения",
     ylab = "Частота встречаимости",
     main ="Гистограмма встречаемости оценки 7 и больше")

boxplot(df_clone[,-c(1)],col = rainbow(6),
        xlab = "Боксплоты",
        ylab = "Значения",
        main="Боксплот встречаемости оценки 7 и больше.")

