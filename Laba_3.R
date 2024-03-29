#�������� ���� �����
df <- read.csv("Help_fo_Laba_3.csv", sep = ";" , header = TRUE);df

fill_name<-colnames(df)
fill_name<-fill_name[-1]

#1
#����� ����
fill_max <-c()
for (i in 2:11){
  p<-df[,i]
  fill_max<-c(fill_max,max(p))
}

df_max = data.frame(
  ����� = fill_name,
  �������� = fill_max);df_max

#����� ���
fill_min <-c()
for (i in 2:11){
  p<-df[,i]
  fill_min<-c(fill_min,min(p))
}

df_min = data.frame(
  ����� = fill_name,
  ������� = fill_min);df_min

#����� �����
fill_mean <-c()
for (i in 2:11){
  p<-df[,i]
  fill_mean<-c(fill_mean,mean(p))
}

df_mean = data.frame(
  ����� = fill_name,
  ������� = fill_mean);df_mean

#2
#����� ����� ������� ������� >5
fill_all <-c()
for (i in 2:10){
  p<-df[,i]
  fill_all<-c(fill_all,p[which(p>5)])
}
fill_all

#3
fill_average<-c()
a<-order(df_mean[,"�������"],decreasing = TRUE);a
for (i in 1:length(df_mean[,c("�������")])){
  fill_average[i] <- df_mean[,"�������"][a[i]]
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
        xlab = "���������",
        ylab = "��������",
        main="������� ���� 39��.")

hist(wer,breaks=20,col = rainbow(6),
     xlab = "��������",
     ylab = "������� �������������",
     main ="����������� ������������� ������ 7 � ������")

boxplot(df_clone[,-c(1)],col = rainbow(6),
        xlab = "���������",
        ylab = "��������",
        main="�������� ������������� ������ 7 � ������.")

