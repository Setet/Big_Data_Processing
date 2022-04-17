#df <- read.csv("Econom_Cities_data.csv" , sep = "," , header = TRUE)

#  ������ ������������� � ������� ����������� �������

#  ��� 1.  ������ ������. �������� �������� �� �����: dec=','
city.01 <- read.table("Help_fo_Laba_6.csv", header = TRUE,";", dec=',')

#   ��� 2.  �������� ����������� ��������
#   � ������ ������ ����������� �������� ���.
#   ������� ������.

#   ��� 3.  �������������� ����������.
#   � ������ ������ ���������� ����������� ��������.
#   ����������������� ����.
city.02 <- scale(city.01[,2:4], center = TRUE, scale = TRUE)
#   ������� 2 - � �������� 0 � ��������� 1

#�������� ������� "������"
city.02<-protein.01[,-1]
maxs <- apply(city.02, 2, max)
mins <- apply(city.02, 2, min)

city.02 <- scale(city.02, center = mins, scale = maxs - mins)

#������ ������� "������"
Countries<-city.01$City
city.02<-data.frame(Countries,city.02)

#  ������� ������� �������� ���������� (�� ��������� - ��������� ����������)
dist.city <- dist(city.02 [,2:4])

#  �������� ���������� ������, 
#  ���������� ���������� � ������ clust.protein
# hclust ������� ������� ����������, � �� �������� ������.

clust.city <- hclust(dist.city, "ward.D")

#  ������� ������� ������ ����������� �������
clust.city

#  ��� 4.  ���������� ������������
k = 4

plot(clust.city, labels = city.01$City)
rect.hclust(clust.city, k = 4, border="red")
abline(h = 1.5, col = "blue", lwd='3') # h - horizontal line, col - color

#plot(1:24, clust.city$height, type='b') # �������� �����(�� ��������!!!)

#  �������� ������ �� 4 ��������
#  ������ groups �������� ����� ��������, � ������� ����� ���������������� ������ 
groups <- cutree(clust.city, k) 
# ��������� ������, ��������, ���������� � ���������� hclust, �� ��������� ����� 
# ����� �������� ��������� ���������� ����� ��� ������ �����.
groups

dend <- as.dendrogram(clust.city)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)
#dev.off() 


#  ������� ������ ������������� �������������� ��������� 
city.01[groups==1, 1]
city.01[groups==2, 1]
city.01[groups==3, 1]
city.01[groups==4, 1]

#  ��� ������� ���� ��� ����������, 
#  ����� ���� ������������ � ������� �������� ����������� ���� ��� ���

#  � 1-�� ��������
g1<-colMeans(city.01[groups==1, 2:4])
#  �� 2-�� ��������
g2<-colMeans(city.01[groups==2, 2:4])
#  � 3-�� ��������
g3<-colMeans(city.01[groups==3, 2:4])
#  �� 4-�� ��������
g4<-colMeans(city.01[groups==4, 2:4])

df<-data.frame(g1,g2,g3,g4); df
df1<-t(df); df1
barplot(df1, ylim=range(pretty(c(0, ))), 
        main="������������ ������� ����������� � ������� ������", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

#�������� ��������, ������� ������� ��� ���������, ��� �� 
#������������� ����� 5 ������ ����������� �������� ���� �� �����:

boxplot(df)
# ��� �������� assess
#barplot(df1, ylim=range(pretty(c(0, 60))), main="������������ ������������", col=c("magenta","red","yellow","blue"),legend=rownames(df1))