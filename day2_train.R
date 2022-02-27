#1
exam<-c(80,60,70,50,90)
exam

#2
mean(exam)

#3
mean_score<-mean(exam)

#4
df<-data.frame(product=c("사과","딸기","수박"), price=c(1800,1500,3000),
               sales=c(24, 38, 13))
df

#5
mean(df[,2])
mean(df[,3])

#6
library(ggplot2)
new_mpg<-mpg

#7
library(dplyr)
new_mpg<-rename(new_mpg, city=cty, highway=hwy)

#8
head(new_mpg)