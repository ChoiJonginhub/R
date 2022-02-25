#타입변환
#as.타입()함수

#matrix -> dataframe
df<-data.frame(matrix(c(1,2,3,4),ncol=2))
colnames(df)<-c("c1","c2")
df

#list -> dataframe
data.frame(list(x=c(1,2), y=c(3,4)))

#문자열 -> factor
x<-c("male", "female")
x

x<-as.factor(x)
x

#factor -> 숫자 벡터
as.numeric(x)

factor(c("male", "female"), levels=c("male", "female"))


#r에서는 데이터를 벡터 단위로 연산, 결측치(NA) 부분이 다름

x<-1
y<-2
if(x>y){
  print("hello")
}else{
  print("world")
}

for(i in 1:10){
  print(i)
}

i<-0
while(i<10){
  i<-i+1
  print(i)
}

#NA(결측값, 누락값, ex. 국어, 수학, 영어) 처리
#값이 없음, 입력자의 실수(누락), 삭제, ..

#NA와 연산을 수행하면 NA가 나옴
NA & TRUE
NA+2
sum(c(1,2,3))
sum(c(1,2,3,NA))
sum(c(90, 95, NA), na.rm=T)


x <- data.frame (a = c (1, 2 , 3) , b = c ( " a " , NA , " c " ) , c = c ( " a " , " b " , NA ) )
x

na.omit(x)

#함수명<-function(매개변수){
#구문...
#}
f1<-function(x){
  if(x>0){
    return("양수")
  }
  return("0 또는 음수")
}

f1(-5)


x<-c(1,2,3) #3
y<-c(1,2,3,4,5,6) #4
#x*2
x+y #(벡터 요소간 덧셈)
#벡터간 연산은 벡터 길이가 동일하거나 배수인 경우만 가능하다

x<-c(1,2,3)
x==c(1,2,3)

TRUE & TRUE
TRUE & FALSE
c(TRUE, TRUE) & c(TRUE, FALSE)

sum(x)
mean(x)
median(x) #중위수

ifelse(x%%2==0, "even", "odd") #%% : 나머지 연산자

x<-c(1,2,3)
ifelse(x%%2==0, "even", "odd")

d <- data.frame ( x = c (1 , 2 , 3 , 4 , 5) , y = c ( " a " , " b " , " c " , " d " , " e " ) )
d

d$x %% 2 #1:TRUE, 0:FALSE

#불린 참조
d[c(TRUE, FALSE, TRUE, FALSE, TRUE) , ]

c(TRUE, FALSE, TRUE, FALSE, TRUE)
d$x %% 2 

d[d$x %% 2==1, ] #d[c(TRUE, FALSE, TRUE, FALSE, TRUE) , ]
#d$x %% 2==1


ls() #메모리 상에 적재된 변수 목록
rm(list=ls()) #변수 제거

ls()
d

str(iris)
class(iris3) #3차원 배열
iris3

mtcars

bike<-read.csv("Bikesharing.csv")
bike
#cannot open file 'Bikesharing.csv': No such file or directory
str(bike)

#헤더가 있는 경우
df<-read.csv("a.csv")
df
names(df)<-c("id","name","score")
df
str(df)

#헤더가 없는 경우
df<-read.csv("a.csv", header = FALSE)
df
names(df)<-c("id","name","score")
df

str(df)

#name 열 값을 문자형이 아닌 범주형으로 읽고 싶은 경우
df<-read.csv("a.csv", header = FALSE, na.strings = "NIL")
#na.strings = "NIL" : NIL 문자열을 na로 인식함
#stringsAsFactors:범주형으로 문자열을 읽겠습니까?
#stringsAsFactors = FALSE(TRUE) 문자열(범주형)로 읽겠습니다
str(df)
df

#시험 미응시자 점수는 NIL로 표기함 ->  R시스템이 NIL을 NA로 인식하도록 설정
#결측값 : 모름, 몰라, 없음, 없어, NOT, NO,... -> NA로 인식하도록 설정

#벡터들을 합침(rbind, cbind) -> 행렬(데이터프레임)

rbind(c(1,2,3), c(4,5,6))

#데이터프레임에 벡터를 합침
df<-data.frame(id=c(1,2), name=c("a","b"))
v<-c(3,"c")
df<-rbind(df,v)
df
str(df)

cbind(c(1,2,3), c(4,5,6))



# apply함수 : 벡터 or 행렬 등에 특정 함수를 적용한 결과를 얻는 기능
# apply, lapply, sapply, tapply
# apply : 행렬의 열 또는 행 방향으로 함수를 적용

sum(1:10)
sum(c(1,2,3))

df<-matrix(1:9,ncol=3)
df
sum(df) #전체합

apply(df,1,sum) #df에서 행 방향으로 sum함수를 적용하라라
apply(df,2,sum) #df에서 열 방향으로 sum함수를 적용하라라

iris
#iris의 첫번째 열~네번째 열까지 값들의 합을 출력하시오.

iris[1:4]
iris[,1:4] #데이터프레임변수[행,열]
iris[,-5]

#열합계
apply(iris[1:4],2,sum)

#행 합계:rowSums(), 열 합계:colSums()
rowSums(iris[1:4])
colSums(iris[1:4])

#lapply(벡터/행렬/리스트/데이터프레임,함수), 출력 결과의 타입이 list

res<-lapply(1:3, function(x){x*2})
res
res[[2]]
#입력:벡터 -> lapply -> 출력:리스트

#리스트 -> 벡터
unlist(res)

x<-list(data1=10:12, data2=20:22)
x
#리스트에 저장된 각 변수마다 평균
lapply(x, mean)


iris
apply(iris[1:4],2,mean)
lapply(iris[1:4], mean)
colMeans(iris[1:4])

rowMeans(iris[1:4])


class(apply(iris[1:4],2,mean)) #[1] "numeric"
class(lapply(iris[1:4], mean))#[1] "list"

sapply(iris[1:4], mean)

sapply(iris[1], function(x){x>5})

class(sapply(iris[1:4], function(x){x>5}))


#tapply:데이터에 대해 그룹별로 함수를 적용
#tapply(데이터(주로 벡터), 그룹, 함수)

iris #species별로 특정 컬럼 값의 평균
tapply(iris$Sepal.Length , iris$Species,mean)

#1~10까지 데이터에 대해 짝수합, 홀수합을 출력
#tapply함수를 활용

# 1%%2==0
# 2%%2==0
#1:10%%2==0

tapply(1:10 ,1:10%%2==0,sum)


install.packages("ggplot2")
library(ggplot2)

data<-c('a','a','b','c')
#빈도그래프
qplot(data)

qplot(data=mpg, cty)
#cty:시내주행시 연비
#hwy :고속도로 주행시 연비


qplot(data=mpg, cyl)


mpg

qplot(data=mpg, x=drv, y=hwy)

qplot(data=mpg, x=drv, y=hwy, geom='line')

qplot(data=mpg, x=drv, y=hwy, geom='boxplot')

qplot(data=mpg, x=drv, y=hwy, geom='boxplot', color=drv)

eng<-c(90,80,65,75)
mat<-c(100,50,70,80)
df_midterm<-data.frame(eng,mat)

df_midterm$class<-c(1,1,2,2)
df_midterm

mean(df_midterm$eng)


df_midterm<-data.frame(eng=c(90,80,65,75),
           mat=c(100,50,70,80),
           class=c(1,1,2,2))

install.packages("readxl")

library(readxl)

df_exam<-read_excel("excel_exam.xlsx")
data.frame(df_exam)


mean(df_exam$english)

df<-read_excel("excel_exam_novar.xlsx", col_names = F)
names(df)<-c("id","class","mat","eng","sci")
df

df<-read_excel("excel_exam_sheet.xlsx", sheet = 3)
df

df
write.csv(df, file="write_test.csv")


exam<-read.csv("csv_exam.csv")
head(exam)
tail(exam)

View(exam)

dim(exam)
str(exam)
#수치 : int float numeric

#요약통계
summary(exam)

#1st Qu. Quantile : 1사분위수(25%)
#가장 작은값 <----------------------------->가장 큰값
#           0      25      50      75     100
#          Min  1st Qu  Median   3rd Qu.  Max
#                    중위수(중앙값) 

#Mean은 극단적인 값에 영향을 크게 받음
#Median(중위수)


mpg # ggplot2::mpg
#패키지명::데이터셋 or 함수명

mpg
head(mpg)
View(mpg)
str(mpg)
summary(mpg)


#rename함수가 포함되어있는 패키지 설치
install.packages("dplyr")
library(dplyr)

df<-data.frame(v1=c(1,2,1), v2=c(2,3,2))
df_new<-df #복사본:원본 변경없이 데이터프레임 다양한 작업

#df_new에서 변수명 v2를 v3로 변경
df_new<-rename(df_new,v3=v2)
df_new
df








