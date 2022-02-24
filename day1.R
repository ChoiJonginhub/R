# print("hi") #ctrl+enter
# ?print
# 
# print {base}
# 함수 {패키지}

# 변수?데이터분석 대상
# 변수 예 : 성별, 직업, 나이
# 상수 예 : 국적(모두 대한민국) 

#a=1
a<-2
print(a)
b<-3
print(b)

a+b
4/a
3*b

ap<-c(5,4,2)
ap
sb<-c(3,2,5)
sb

var1<-c(1:5)
var1

#벡터 : 숫자 리스트, 크기&방향
# 
# ex)매출 장부
# 사과 딸기
# 5     3
# 4     2
# 2     5
# ...
# 
# 사과벡터

seq(1,5)


v1<-seq(1,5, by=2) #ctrl + enter 를 반드시 입력
print(v1)
#벡터화 연산은 벡터를 구성하는 각 요소에 대해 1씩 증가
#리스트에 저장된 [1,3,5]에 대해 1씩 증가

v1+1 #벡터화 연산(속도가 빠름), 파이썬에서는 numpy에서 벡터화 연산을 지원

v1*5

a1<-c(1,2,3)
a2<-c(4:6)
a1+a2 #벡터의 덧셈

s1<-"a"
s2<-"bcd"

s2

s3<-c("x","y","z")
s3
#s3+3

x<-c(10,20,30)
mean(x)
max(x)
min(x)

s3 #문자 벡터
paste(s3, collapse = ",") #쉼표를 구분자로 하여 s3에 저장된 문자들을 합치기


#패키지:함수가 여러개 저장된 꾸러미, 함수를 사용하기 위해 패키지를 먼저 설치해야 함



var1<-c(1:5)
var1

install.packages("ggplot2") #패키지 설치
library(ggplot2) #패키지 로드(패키지에 담긴 함수들을 메모리 상에 적재)

#.libPaths("C:/Program Files/R/R-4.1.2/library")


r변수
<-

x<-3 #r에서 자료  벡터, 길이가 1인 벡터를 스칼라라고 함
기본형은
NA : 데이터 값이 없음(누락), 바구니(변수)에 아무것도 담기지 않은 상황
NULL : 변수가 초기화 되지 않음, 바구니가 없음

addr<-NA
is.na(addr) #addr 변수에 na가 저장되어 있나요?
"hello"

TRUE | FALSE #참 OR 거짓 => 참참
!TRUE

x<-c(TRUE, TRUE)
y<-c(TRUE, FALSE)
x & y #벡터의 요소끼리 and 연산 수행

범주형(factor, ex. 성별, 혈액형, 거주지역)

gender<-"male" #범주형이 아니라 단순 문자열을 저장

gender<-factor("male",c("male","female")) #범주형 자료 저장

#bloodtype에 여러분의 혈액형을 저장하세요(범주형으로)
bloodtype<-factor('O',c('A','B','O','AB')) #기본값, 명목형
bloodtype

nlevels(bloodtype)

#범주형(카테고리형) - 명목형, 순서형으로 나누어짐
#명목형:데이터 순서가 없음(혈액형, 성별 등), 기본값
#순서형:데이터 순서가 있음(학점, 나쁨, 조금나쁨...좋음)

hj<-factor('A',c('A','B','C','D','F'), ordered = TRUE) #순서형
hj

#벡터는 동일한 타입 데이터가 저장
x<-c(1,'2','3')
x

#중첩 구조로 저장 -> 리스트 형태로

x<-c(1,3,2)
x

names(x)<-c("a","b","c")  #x벡터를 구성하는 각 요소에 대해 이름을 부여해라
x

x[3] #벡터는 []안에 인덱스 번호 작성하여 접근, 인덱스가 1번부터 부여
x[-2] #- 인덱스 부여하면 제외됨

x[c(1,3)] #1,3번 요소 추출
x[1:2] #인덱스 범위로 추출
x
x['b']
x[c('c','a')]

length(x) # NROW(x)

3 %in% x #x벡터 내에 3값이 있습니까?
"k" %in% c("j","i","k")

rep(1:3, 2) #2 times
rep(1:3, each=2) #각 요소에 대해 2번 반복

#r 리스트는 파이썬 리스트와는 다름, 딕셔너리와 비슷
#r 리스트도 (키,값) 쌍으로 데이터 구성

x<-list(height=70, name="abc")
x

x2<-list(height=c(70,80,90), name="abc")
x2

#리스트내에 리스트 저장
list(a=c(1,2,3), b=list(v=c(10,20,30)))

v=c(10,20,30)
v
list(v) #벡터 -> 리스트

x<-list(addr="seoul", age=c(25,35,27))
x$addr
x$age

x
x[2]

x[[1]]
x[[2]]

x[2]
x[[2]]

 #변수명[[인덱스]] : 리스트 내부의 데이터를 접근

x[[2]]


#행렬 : 벡터와 마찬가지로 행렬도 1가지 데이터 타입만 저장
c(1,2,'3')

#데이터프레임(행렬과 비슷, 열벡터 단위로 다양한 타입들이 존재), 벡터, 행렬

#1~6 벡터 생성
c(1:6) #1차원 벡터

matrix(c(1:6)) #2차원 행렬(6행 1열)

matrix(c(1:6), nrow=3) #2차원 3행 2열(열 우선)

matrix(c(1:6), nrow=3, byrow = TRUE) #2차원 3행 2열(행 우선)

m<-matrix(c(1:6), nrow=3, byrow = TRUE, dimnames = list(c('r1','r2','r3'),c('c1','c2')))

m['r1',] #첫번째 행
m[1,]#첫번째 행

m['r1','c2']
m[,2]

#m['r1':'r2',] #행 인덱스 접근 불가
m[1:2,] #인덱스 접근 가능

m[-3,] #3번 행 제외

#스칼라 : 0차원벡터, ... ,행렬:2차원벡터

m*2
m/2

# element-wise 연산
# m+m
# m-m
# m*m
# m/m

x<-matrix(c(10,20,30,40), nrow=2)
y<-matrix(c(0,0,1,1), nrow=2)
x
y
x%*%y  #%*%는 행렬의 곱셈 연산자

#   사과(영어) 딸기(국어)
# 어제    10    5      사과   500  0.6 
# 오늘    3     2      딸기   1000 0.4
# +1          2*2      2*1    => 2*1
# +2
# 
# 10*500 +  5*1000 =   10000
# 3*500 + 2*1000  =    3500

m%*%m

t(m) #전치행렬(transpose matrix, 행/열이 바뀜)
m%*%t(m) 

solve(x) #x행렬의 역행렬
x%*%solve(x) #단위행렬 (행렬 * 역행렬)
solve(x)%*%x

x
ncol(x)
nrow(x)

배열:n차원 행렬


matrix(1:12, ncol=4)
array(1:12, dim=c(3,4))  #2차원 -> 2차원 행렬(3행4열)
x<-array(1:12, dim=c(2,2,3))  #3차원 배열(2행2열3면)

x[2,2,2] #[행,열,면]
x[1,,]
x[,2,]
x
x[,,3]

x[1,2,]
x[1,,3]

dim(x) #배열의 차원 정보



#데이터프레임:행렬과 비슷, 다양한 변수가 존재, 열 단위로 다양한 타입이 존재
df<-data.frame(x=c(10,20,30,40), y=c(2,4,6,8), z=c('m','f','f','m'))
df

df$k<-c(1,2,3,4)
df

df$y #열 단위 데이터 추출
df[1,] #행 단위 데이터 추출
df[2,]
df[3,1]

df[, 'y']
df[, 2]
df[, c('x','k')]
df[,c(2,4)]

df[c(1,2),2]

df[-c(1,2),2]
df[-c(1,2),-2]


#df에서 x열 추출
df[,'x']
df$x
df[,c('x')]

class(df) #데이터프레임

class(df[,'x']) #numeric 숫자 벡터

class(df[,c('x','y')]) #데이터프레임
#추출하고자 하는 열이 2개 이상인 경우에는 데이터프레임, 그렇지 않은 경우에는 1차원 벡터

class(df[,'x', drop=FALSE])

df
str(df)

관측치

head(df)
tail(df)


a<-data.frame(10:12)
a
colnames(a)<-c('value')
a
rownames(a)<-c('b','c','d')
a

df2<-data.frame(value=10:12, value2=15:17,value3=20:22)
df2


names(df2) #열이름 벡터


names(df2) %in% "value" #TRUE FALSE FALSE


df2[ , names(df2) %in% "value"] #df2[,c(TRUE, FALSE, FALSE)]

df2[,c(TRUE, FALSE, FALSE)] #불린 참조


df[ , !names(df) %in% "z"]

#x열과 y열을 추출
df
df[,c(1,2)]
df[,c('x','y')]
df[,-c(3,4)]
df[,names(df) %in% c("x", "y")]












