df<-read.csv("C:/Users/Setet/Desktop/Athlet_Events/athlete_events.csv", sep = "," , header = TRUE)

for(i in 1:length(df[,6])){
  if(is.na(df[i,6]))
  {
    df[i,6]<-68
  }
}

df_sport<-df[1,]

q=1
for(i in 1:length(df[,13])){
  if(df[i,13]=="Basketball" || df[i,13]=="Athletics")
  {
    df_sport[q,]<-df[i,]
    q=q+1
  }
}

df_sport_shapiro<-df_sport[1,]
maxs<-5000

for(i in 2:maxs){
  df_sport_shapiro[i,]<-df_sport[i,]
}

t.test(df_sport$Weight,mu=70.92)
boxplot(df_sport$Weight)
hist(df_sport$Weight, freq=FALSE)
 
wilcox.test(df_sport$Weight, mu=69.25,conf.int=TRUE)

shapiro.test(df_sport_shapiro$Weight)

qqnorm(df_sport$Weight)
qqline(df_sport$Weight, col=2)

fligner.test(df_sport$Weight~df_sport$Sport, data=df_sport)

t.test(df_sport$Weight~df_sport$Sport)