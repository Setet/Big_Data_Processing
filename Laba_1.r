#5
q<- c (1, 0, 2, 3, 6, 8, 12, 15, 0, 78, 99, 9, 4, 16, 2 ,0)
x <- sample(c(T,F), length(q), replace=T, prob=c(.4,.6));x
q[x]<-NA;q

#19
s1 = " ����� ����������� ����� - ����� �������."
s4 = " ����� ����������� ����� - ����� ��������"
s2 = "Ÿ �����������������"
s3 = "��"


df = data.frame(�������� = c("�����", "�������", "�����", "Ƹ����", "����������", "���������", "������"), ������������� = c(4,2,3,9,6,3,1));df
p=max(df[,c("�������������")])
p1=which.max(df[,c("�������������")])
p2 =df[p1,c("��������")]

e=min(df[,c("�������������")])
e1=which.min(df[,c("�������������")])
e2 =df[e1,c("��������")]

s = paste(p2, s1 , s2, p, s3);s
q = paste(e2,s4 , s2, e, s3);q