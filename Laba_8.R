library(ggplot2)

#1
countries<-read.csv("C:/Users/Setet/Desktop/Lab8_Data.csv", header=TRUE, encoding="UTF-8")
greece<-subset(countries, countries$X.U.FEFF.Country.Name=='Greece')

#2
#a
GDP<-subset(greece, greece$Series.Name=="GDP growth (annual %)"
            |greece$Series.Name=="Population growth (annual %)", select=c(5:34))
cor(as.numeric(GDP[1,1:29]),as.numeric(GDP[2,1:29]),method="spearman")
windows()
plot(1989:2017, ylim = c(-15,15), as.numeric(GDP[1,1:29]), type = "b", col = "black", 
     lty = 1, pch = 1, xlab = "Года", ylab = "Значения", main = 'ВВП')

#b
unemp<-subset(greece, greece$Series.Name=="Unemployment with advanced education (% of total labor force with advanced education)"
              |greece$Series.Name=="Population growth (annual %)", select=c(5:34))
cor(as.numeric(unemp[1,10:29]),as.numeric(unemp[2,10:29]),method="spearman")

#c
health<-subset(greece, greece$Series.Name=="Domestic general government health expenditure per capita (current US$)"
               |greece$Series.Name=="Life expectancy at birth, total (years)"
               |greece$Series.Name=="Death rate, crude (per 1,000 people)", select=c(5:34))
cor(as.numeric(health[1,12:28]),as.numeric(health[2,12:28]),method="pearson")
cor(as.numeric(health[1,12:28]),as.numeric(health[3,12:28]),method="pearson")

#d
ed<-subset(greece, greece$Series.Name=="Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)"
           |greece$Series.Name=="Exports of goods and services (annual % growth)"
           |greece$Series.Name=="Medium and high-tech Industry (including construction) (% manufacturing value added)", select=c(4:34))
cor(as.numeric(ed[1,27:29]),as.numeric(ed[2,27:29]),method="pearson")
cor(as.numeric(ed[1,3:28]),as.numeric(ed[3,3:28]),method="pearson")

#e
fem<-subset(greece, greece$Series.Name=="Government expenditure on education, total (% of GDP)"
            |greece$Series.Name=="Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)", select=c(5:34))
cor(as.numeric(fem[1,26:27]),as.numeric(fem[2,26:27]),method="spearman")

#f
sci<-subset(greece, greece$Series.Name=="Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)"
            |greece$Series.Name=="Scientific and technical journal articles", select=c(4:34))
cor(as.numeric(sci[1,27:29]),as.numeric(sci[2,27:29]),method="spearman")



data<-subset(countries, countries$X.U.FEFF.Country.Name=='Greece', select=c(4:34))
cor(data.matrix(data[,2:30]))
states<-data.matrix(data[,2:30])


#регрессионный анализ на выявление независимых и зависимых переменных
col<-c(greece$Series.Name);col
res<-as.data.frame(t(data[,2:30]))
colnames(res)<-col
View(cor(data.matrix(res)))
res$`Domestic general government health expenditure per capita (current US$)`<-as.numeric(res$`Domestic general government health expenditure per capita (current US$)`)
res$`Life expectancy at birth, total (years)`<-as.numeric(res$`Life expectancy at birth, total (years)`)
sub_new_df <- res[which(res$`Domestic general government health expenditure per capita (current US$)` > 1),]


fit <- lm(`Life expectancy at birth, total (years)`~`Domestic general government health expenditure per capita (current US$)` , data=sub_new_df)
summary(fit);fit


pred<-predict(fit, sub_new_df);pred
plot(sub_new_df$`Life expectancy at birth, total (years)`,xaxt="n", ylab="Возраст", xlab="1989 - 2018г",  pch = 19, cex = 2,  main="Ожидаемая продолжительность жизни")
points(pred,xaxt="n", col="red", pch = 19, cex = 2)

labels <- c("Реальное значение", "Прогнозируемое значение")
colors <- c("red", "black")


legend("bottom" ,cex = 1,
       labels,
       fill = colors,
       bg ="white",
       text.width = 1,
       box.lty=0)


plot(sub_new_df$`Domestic general government health expenditure per capita (current US$)`,sub_new_df$`Life expectancy at birth, total (years)`, xlab="Гос.траты на здравоохранение", ylab="Ожидаемая продолжительность жизни",col="blue"); abline(fit, col="red")
par(mfrow=c(1,2))


library(car)
states1 <-data.matrix(data[,1:30])
test<-as.data.frame(states[,c(12, 15, + 7 , 22, 18)])
pairs(as.data.frame(states[, c(12, 15, + 7 , 22, 18)]))
windows()
scatterplotMatrix(test, spread=FALSE, lty.smooth=2,  main="Матрица диаграмм рассеивания")
