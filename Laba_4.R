#1+2
df <- read.csv("Help_fo_Laba_4.csv", sep = ";" , header = TRUE);df

olimp_years <-c()
for (i in 6:1){
  olimp_years<-c(olimp_years,df[i,1])
}
olimp_years

olimp_col<-c("#FFD700","#C0C0C0","#A0522D","#A9A9A9","#CD5C5C","#F08080","#FA8072","#FF0000")
olimp_awards<-c("Золото","Серебро","Бронза","4-ое","5-ое","6-ое","7-ое","8-ое")

vec_year<-c(2020,2012,2008,2004,2000,1996)

A=matrix(0,8,6);

A[1,]<-rev(df[,2])
A[2,]<-rev(df[,3])
A[3,]<-rev(df[,4])
A[4,]<-rev(df[,5])
A[5,]<-rev(df[,6])
A[6,]<-rev(df[,7])
A[7,]<-rev(df[,8])
A[8,]<-rev(df[,9]);A

barplot(A,names.arg = olimp_years , beside = TRUE,main = "Олимпийских достижений России по лёгкой атлетике",
        col = olimp_col,
        legend.text = olimp_awards,
        xlab = "Олимпиады", 
        ylab = "Кол-во наград",
        ylim = c(0, 20))

x <- c(A[1,]);x
pie(x, olimp_years, main = "Кол-во первых мест в каждой из олимпиад", col = rainbow(length(x)))

x_man<-c(df[,10])
x_women<-c(df[,11])

plot(vec_year,x_man , type="b",  ann=FALSE,
      col="green", lty=2, pch=2, lwd=2,
      ylim=c(0, 80))

lines(vec_year,x_women , type="b",  ann=FALSE,
      col="red", lty=2, pch=2, lwd=2,
      ylim=c(0, 80))

title(main="Тенденции изменения
      количества призовых мест", col.main="red",
      xlab="Олимпиады",
      ylab="Кол-во призовых мест",
      cex.lab=1)

legend("topright", c("Женщины","Мужчины"),lty=2,pch=2, col=c("red", "green")) 

#3
plot(vec_year,x , type="b",  ann=FALSE,
      col="#FFD700", lty=2, pch=2, lwd=2,
      ylim=c(0, 20))

title(main="Изменения спортивных достижений
      по золотым медалям", col.main="red",
      xlab="Олимпиады",
      ylab="Кол-во золотых медалей",
      cex.lab=1)


#4
plot(vec_year,df[,2] , type="b",  ann=FALSE,
     col=olimp_col[1], lty=2, pch=2, lwd=2,
     ylim=c(0, 20))

lines(vec_year,df[,3] , type="b",  ann=FALSE,
      col=olimp_col[2], lty=2, pch=2, lwd=2,
       ylim=c(0, 15))

lines(vec_year,df[,4] , type="b",  ann=FALSE,
      col=olimp_col[3], lty=2, pch=2, lwd=2,
      ylim=c(0, 15))

lines(vec_year,df[,5] , type="b",  ann=FALSE,
      col=olimp_col[4], lty=2, pch=2, lwd=2,
      ylim=c(0, 15))

lines(vec_year,df[,6] , type="b",  ann=FALSE,
      col=olimp_col[5], lty=2, pch=2, lwd=2,
       ylim=c(0, 15))

lines(vec_year,df[,7] , type="b",  ann=FALSE,
      col=olimp_col[6], lty=2, pch=2, lwd=2,
       ylim=c(0, 15))

lines(vec_year,df[,8] , type="b",  ann=FALSE,
      col=olimp_col[7], lty=2, pch=2, lwd=2,
       ylim=c(0, 15))

lines(vec_year,df[,9] , type="b",  ann=FALSE,
      col=olimp_col[8], lty=2, pch=2, lwd=2,
      ylim=c(0, 15))

title(main="Тенденции изменения
      количества всех мест на олимпиада", col.main="red",
      xlab="Олимпиады",
      ylab="Кол-во медалей",
      cex.lab=1)

legend("topright", olimp_awards,
       lty=2,pch=2, col=olimp_col) 