#1+2
df <- read.csv("Help_fo_Laba_4.csv", sep = ";" , header = TRUE);df

#делаем матрицу(мб можно проще,пока думаю)
#Узнать/спросить как именовать столбцы в матрице!!!
A=matrix(0,9,6);A

A[1,]<-df[,2];A
A[2,]<-df[,3];A
A[3,]<-df[,4];A
A[4,]<-df[,5];A
A[5,]<-df[,6];A
A[6,]<-df[,7];A
A[7,]<-df[,8];A
A[8,]<-df[,9];A

barplot(A, beside = TRUE,main = "Олимпийских достижений по лёгкой атлетике",
        col = c("#FFD700","#C0C0C0","#A0522D","#A9A9A9","#CD5C5C","#F08080","#FA8072","#FF0000"),
        #мешает,подумать как двигать или убрать вовсе
        legend.text = c("Золото","Серебро","Бронза","4","5","6","7","8"),
        xlab = "Года", 
        ylab = "Кол-во наград",
        ylim = c(0, 20))

x <- c(A[1,]);x
labels <- c("2020 Токио", "2012 Лондон", "2008 Пекин", "2004 Афины", "2000 Сидней", "1996 Атланта")
pie(x, labels, main = "Кол-во первых мест в каждой из олимпиад", col = rainbow(length(x)))

x_man<-c(df[,10])
x_women<-c(df[,11])


plot(1:6,x_man , type="b",  ann=FALSE,
             col="green", lty=2, pch=2, lwd=2,
             xlim=c(0, 6), ylim=c(0, 100))

lines(1:6,x_women , type="b",  ann=FALSE,
     col="red", lty=2, pch=2, lwd=2,
     xlim=c(0, 6), ylim=c(0, 100))

title(main="Тенденции изменения
      количества призовых мест", col.main="red",
      xlab="Олимпиады",
      ylab="Кол-во призовых мест",
      cex.lab=1)

legend("topright", c("Женщина","Мужчина"),lty=2,pch=2, col=c("red", "green")) 

#3(1)
plot(1:6,x , type="b",  ann=FALSE,
     col="green", lty=2, pch=2, lwd=2,
     xlim=c(0, 6), ylim=c(0, 50))

title(main="Изменения спортивных достижений
      по золотым медалям", col.main="red",
      xlab="Олимпиады",
      ylab="Кол-во золотых медалей",
      cex.lab=1)

#3(2)
#НИХЕРА НЕ ПОНЯТНО,Я НЕ КРИПТОГРАФ РАСШИФРОВЫВАТЬ ЗАДАНИЯ ПАМАГИТЕ!!!!!!!!!!!!! 