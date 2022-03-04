source("trans.R")

translength<-function(x){
  tlength<-round(x*0.914,1)
  res<-paste(tlength, "m", sep="")
  return(res)
}

translength(c(10,20,30))
translength(50)

trans2<-translength
trans2(c(10,20,30))

#switch함수: 첫번째 인수로 주어진 값에 따라 두번째 이후의 인수에 대해 코드가 실행

center<-function(x, type){
  switch(type,
         mean=mean(x),
         median=median(x),
         trimmed=mean(x,trim=0.1),
         "choose one of mean, median, and trimmed"
  )
}
x<-c(2,3,5,7,11,13,17)
center(x, "mean") #평균
center(x, "median") #중위수수
center(x, "trimmed") #절사평균
center(x, "sum") 

mode(3.14)
class(3.14)

mode(as.Date("2022-03-04")+1)#날짜형식도 숫자형식의 일부

as.Date("2022-03-04")+1
#"2022-03-04"+1 에러

d<-as.Date("2022-03-04")
d-1
as.integer(d) #19055
length(d)

as.integer(1.618) #정수
as.character(1.618) #문자
as.numeric(1.618) #수치
as.numeric("test") #NA

as.character(10:20)
as.numeric(as.character(10:20))

as.numeric(TRUE)
sum(c(TRUE,TRUE,FALSE))

#c(1,3,5,7,9) 벡터에서 4보다 큰 수는 몇개?
# sum(as.numeric(c(1,3,5,7,9)>4))
# sum(c(1,3,5,7,9)>4)
# x<-c(1,3,5,7,9) 
# length(x[x>4])

x<-c(2,3,5,7,11,13,17)
x
as.data.frame(x)
as.list(x)
as.factor(x)

#as.타입명(변수) 변수에 저장된 값의 타입을 변경

mat<-matrix(1:6, 3, 2)
mat
as.vector(mat)
as.list(mat)
as.data.frame(mat)

month.name
paste("The month of", month.name)

english<-c(90,80,60,70)
math<-c(50,60,100,20)
class<-c(1,1,2,2)
df1<-data.frame(english,math,class)
df1

#df1에서 class열을 제외한 나머지 열에 대해 평균을 구하시오
colMeans(df1[-3])
mean(df1[,1])
mean(df1[,2])
#apply(데이터,행/열,함수)
apply(df1[,-3], 2, mean)


# prod(5:1)
# prod(1:5)

for(i in 2:9){
  v<-c(1:9)
  res<-paste(i,"*",v,"=",i*v)
  print(res)
}

#str(read.csv("train.csv"))


df<-read.csv("train.csv", na.strings = "")
#na.strings = "" : ""는 데이터가 입력 안된 상태를 의미하며, NA로 읽어들임
str(df)

install.packages("Amelia")
library(Amelia)
missmap(df, col=c("red","grey"))

df<-na.omit(df)
missmap(df, col=c("red","grey"))

#범주형 : 명목형, 순서형

df$Survived<-factor(df$Survived) #수치 -> 범주형(명목형)
df$Pclass #수치 -> 범주형(순서형)
df$Pclass<-factor(df$Pclass, order=TRUE, levels=c(3,2,1))
df$Pclass

library(ggplot2)
install.packages("GGally")
library(GGally)

df<-read.csv("train.csv", na.strings = "")
ggcorr(df, label=TRUE, label_size = 2) #상관관계 시각화

df

mtcars

#양/음의 상관관계가 큰 변수?
ggcorr(mtcars, label=TRUE, label_size = 2) #상관관계 시각화

a<-c(1,2,3,4,5)
b<-c(2,5,7,8,9)
match(a,b) #첫번째 인수 a가 두번째 인수 b의 몇번째 위치에 있는지 알려줌. 없으면 NA

car<-mtcars
str(car)
summary(car)
car

#행 이름 추출 -> 데이터 열에 추가
car$name<-row.names(car)
car
#행 이름 제거
row.names(car)<-NULL
car

#car에서 145마력(hp)이 넘는 힘 쎈 자동차 모델
car[car$hp>145,]
car$name[car$hp>145]

highhp.car<-car[car$hp>145,]

#car에서 무게(wt)가 3.2파운드 미만 자동차 모델
lightwt.car<-car[car$wt<3.2,]

#무게가 가벼우면서 힘이 쎈 자동차 모델 추출(match함수 활용)
index<-match(highhp.car$name, lightwt.car$name)
lightwt.car[na.omit(index),]

# a<-c(1,2,3,4,5)
# b<-c(2,5,7,8,9)
# match(a,b) #첫번째 인수 a가 두번째 인수 b의 몇번째 위치에 있는지 알려줌. 없으면 NA

vec<-11:20
vec
17 %in% vec
c(20,8,5,15,19,0) %in% vec #TRUE FALSE FALSE  TRUE  TRUE FALSE
match(c(20,8,5,15,19,0), vec)#10 NA NA  5  9 NA

!is.na(match(c(20,8,5,15,19,0), vec))# TRUE FALSE FALSE  TRUE  TRUE FALSE

#무게가 가벼우면서 힘이 쎈 자동차 모델 추출(%in%연산자 활용)
lightwt.car[highhp.car$name %in% lightwt.car$name,]

ans<- highhp.car$name %in% lightwt.car$name
lightwt.car[ans,]

mtcars$mpg #벡터 mtcars[[1]], mtcars[['mpg']]와 같음
mtcars[1] #데이터프레임 mtcars['mpg']와 같음

mtcars[c(1,4)]
mtcars[c('mpg','hp')]
mtcars[c(1,2,3)]
mtcars[-c(1,2,3)] #특정 열 제외
#mtcars[,-c('mpg', 'hp')] 에러 발생

mtcars[1:5, ]
mtcars[1:5, 3:4]

mtcars[c('mpg','hp')]


subset(mtcars, select=-mpg)
subset(mtcars, select=-c(mpg, hp, disp))

#iris Sepal.Length가 7을 초과한 데이터 추출
iris[iris$Sepal.Length>7,] #iris[iris['Sepal.Length'] >7,]
#iris[iris$Sepal.Length>7,c(열번호벡터)]

subset(iris, select=c(Sepal.Length))
subset(iris, select=c('Sepal.Length'))

subset(iris, select=c('Sepal.Length'), subset = Sepal.Length>7) 
#subset(Sepal.Length)의 subset(Sepal.Length>7)을 추출


#트레이닝, 테스트, 밸리데이션
#모델

#범위 : 1장~10장, 각 장당 10문항 = 10*10=100문항
모의고사(train data, 100문항) -> 공부(training, 100문항중 70문항 랜덤추출) -> 30문항 검증(validation) -> accuracy:25/30
==========================================================================>모델(예측,판별 가능한)

->  실전고사(테스트)


set.seed(20220304)

data<-sample(1:100,70)
#data -> a,b,c...알고리즘 -> 모델 -> 평가(어떤 알고리즘이 좋은가?) -> 좋은 알고리즘 모델 -> 예측/분류
data

duplicated(c(1,2,3,1,1,4))


id<-c("a001","a002","a003")
name<-c("mouse","keyboard","usb")
price<-c(3000,9000,5000)

product<-data.frame(id=id, name=name, price=price)
product

product<-rbind(product, c("a001","mouse",3000))
product

duplicated(product)
#중복행을 제외한 데이터프레임 추출
product.unique<-product[!duplicated(product),]

#중복된 자료의 위치 -> 중복 제외
which(duplicated(product))

idx<-which(duplicated(product))
product.unique<-product[-idx,]

#중복 제외
unique(product)

airquality

#결측값이 없는 행 추출
complete.cases(airquality) #결측값이 있으면 false
airquality[complete.cases(airquality), ]
#결측값이 있는 행 추출
airquality[!complete.cases(airquality), ]
#결측값이 있는 행 제거
na.omit(airquality)


min(iris$Sepal.Width) #2
max(iris$Sepal.Width) #4.4
cut(iris$Sepal.Width, breaks=c(0,2,4,6)) #0~2, 2~4, 4~6 구간 생성
#( ) : 개구간, [ ] : 폐구간
#Levels: (0,2] (2,4] (4,6]

class(cut(iris$Sepal.Width, breaks=c(0,1,2,3,4,5)))

iris.cut1<-cut(iris$Sepal.Width, breaks=c(0,1,2,3,4,5))
iris.cut1 #범주형
table(iris.cut1)
summary(iris.cut1)

iris.cut2<-cut(iris$Sepal.Width, breaks=c(0,1,2,3,4,5), 
               labels=c("smaller","small","medium","big","bigger"))
iris.cut2

table(cut(iris$Sepal.Width, breaks=5))

x<-c(11,33,22,44,55)
order(x) #1 3 2 4 5 -> 11 22 33 44 55 
x[order(x)]

y<-c(11,22,33,55,44)
z<-c("ss","bb","ii","aa","pp")
df<-data.frame(y,z)
df

#y열을 기준으로 오름차순정렬
df[order(df$y),]
#내림차순정렬
df[order(df$y, decreasing = ),]

w<-c("c","c","n","n","n")
df<-data.frame(y,z,w)
df

#1차정렬 w, 2차정렬 y열
df[order(df$w, df$y),]

df$w
xtfrm(df$w)

#w열을 기준으로 내림차순, y열 기준 오름차순
df[order(-xtfrm(df$w),df$y),]






