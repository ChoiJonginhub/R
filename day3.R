df<-data.frame(제품=c("사과","딸기","수박"), 가격=c(1800,1500,3000), 수량=c(24,38,13))
df

#mean(df[['가격']])

#mean(df$price)
#mean(df$가격)

df[1,]
df[1,2]
df[c(1,3), ]
df[c(1,3),2]

# 참조열이름<-'제품'
# df[,참조열이름] #데이터프레임에서 제품 변수에 있는 값을 열 이름으로 하여 참조

df[,'제품'] #데이터프레임에 있는 '제품' 열에 대해 참조
mean(df[,'가격']) # df$가격 과 같은 의미의 문장

df[,c('가격','수량')]


mean(as.matrix(df[,c('가격','수량')]))
#데이터프레임에 대해서는 mean함수가 적용 불가, 다만,각각의 수치형 열벡터에 대해 mean함수 적용
#matrix는 전체 요소값에 대해 mean함수 적용 가능 mean(df[,c('가격','수량')])는 에러발생

df
colnames(df)<-c('prod','price','volume')
df
rownames(df)<-c('a','b','c')
df

#colnames(df$prod)<-'product' 에러 발생
#help->name


df
names(df) #전체 컬럼 이름 추출
#colnames(df)<-c('prod','price','volume') 전체 컬럼 이름 변경
names(df)<-c('product','price') #마지막 열 이름 누락됨
df
names(df)<-c('product','price','volume')
df

#df에서 product만 prod로 변경, 나머지 컬럼 이름은 그대로
names(df)[names(df)=='product']
#[TRUE에 해당되는 데이터가 참조됨]
names(df)
names(df)=='product'

#df에서 product만 prod로 변경, 나머지 컬럼 이름은 그대로
names(df)[names(df)=='product']<-c("prod")
df

library(dplyr)
#df에서 prod만 product로 변경, 나머지 컬럼 이름은 그대로
df<-rename(df, 'product'='prod')
df

#결론적으로
#열 이름을 모두 변경하고자 하는 경우에는,
#colnames(df)<-c('prod','price','volume') 와 같이 작성

#특정 열 이름만 변경하고, 나머지 열 이름은 그대로 사용하고자 하는 경우에는,
#rename(df, 'product'='prod')

df<-df%>% rename('prod'='product')
df

#df에서 열이름 volume을 vol로 변경하시오
df<-rename(df, 'volume'='vol')
df<-df %>% 
  rename('vol'='volume')
df

install.packages("doBy")
library(doBy)
#doBy패키지: 특정 값에 따라 데이터를 처리하는 함수들의 묶음
#ex. 붓꽃의 종별 Sepal.Length와 Width에 대해 요약
#summaryBy함수(데이터 ~ 특정값,요약대상 데이터)
summaryBy(Sepal.Length~species ,iris, FUN=sum)

fun <- function(x){
  c(m=mean(x), v=var(x), n=length(x))
}
summaryBy(Sepal.Length~species ,iris, FUN=fun)
summaryBy(Sepal.Length+Sepal.Width~species ,iris, FUN=fun)
summaryBy(.~species ,iris, FUN=fun) #all columns

#orderBy :정렬함수
orderBy(~Sepal.Width+Petal.Length, iris)

#sampleBy : 샘플링 함수(머신러닝에서 트레이닝/테스트 데이터 분할)
sampleBy(~Species, frac=0.1, data=iris) #종별로 10%(5건) 샘플링 수행

#split : 데이터 분리 함수
split(iris, iris$Species) #종별로 데이터 분리, 결과가 리스트

#종별로 데이터를 분리하고, Sepal.Length의 평균을 구함
lapply(split(iris$Sepal.Length, iris$Species), mean)

#subset : 부분집합 추출 함수(split과 유사함)
subset(iris, Species=='setosa')
subset(iris, Species=='setosa' & Sepal.Length>5.0)
subset(iris, select = c(Sepal.Length, Species))
subset(iris, select = -c(Sepal.Length, Species)) #특정 열 제외


#iris에서 'virginica' 종류 이거나 Sepal.Width가 3미만인 자료만 추출

subset(iris, Species=='virginica' | Sepal.Width<3)


#merge함수 : 데이터프레임 합치기

x<-data.frame(name=c("a","b","c"), math=c(10,20,30))
y<-data.frame(name=c("c","b","a"), eng=c(40,50,30))
merge(x,y)

x<-data.frame(name=c("a","b","c","d"), math=c(10,20,30,40))
y<-data.frame(name=c("c","b","a","e"), eng=c(40,50,30,70))
merge(x,y)
merge(x,y, all=TRUE) #small data에서는 데이터를 최대한 살려야 함





#base패키지:r설치시 기본적으로 함께 설치되는 패키지.
#가장 많이 사요된는 함수들의 묶음.  
#특정 값에 따라 데이터를 처리하는 함수들이 있음
#summary함수:데이터 통계 요약
summary(iris)
#quantile:수치형 자료 분포
quantile(iris$Sepal.Length) 
quantile(iris$Sepal.Length, seq(0,1,by=0.1))  #seq(1,2, by=0.1)

#order함수:정렬
order(iris$Sepal.Width)
iris[100,]
iris[order(iris$Sepal.Width),]

orderBy(~Sepal.Width, iris) #iris[order(iris$Sepal.Width),] 방식이 더 많이 쓰임

sample(1:5, 3)
sample(1:5, 5)
sample(1:45, 6)
#sample(1:5, 10) 에러발생. 비복원추출
sample(1:5, 10, replace = TRUE) #복원추출

#sample함수를 이용한 무작위 데이터 섞기
length(iris)
NROW(iris)
nrow(iris)
ncol(iris)
length(iris$Sepal.Length)

#데이터를 섞는 작업은 언제 필요한가? 머신러닝/딥러닝에서
#모델링 하기 전에 데이터를 섞어야 함. 편향을 없애기 위해서
#예를 들어, 연습문제가 10개 챕터에 대해 100문항이 있고,
#1개 챕터당 10문항씩 존재

#잘못된 공부 및 테스트 방식->정상적인 방식은 문제를 임의로 섞어서
#연습문제로 공부를 함(1~70번 문항) -> 테스트 수행(71번~100번)

# 공부하는것(모델링)-> 지식(모델)
# 새로운 문제 -> 모델 -> 답 제시

# sample함수와 nrow함수를 적절하게 사용하여
# iris데이터를 임의로 뒤죽박죽 섞어보세요

sample(nrow(iris))
nrow(iris)
iris[sample(nrow(iris), nrow(iris)),]

x<-c(20,10,5,30,50)
sort(x)
sort(x, decreasing = TRUE)
x

#c(20,10,5,30,50)  
order(x) #오름차순 정렬시 index
order(-x)#내림차순 정렬시 index

order(iris$Sepal.Length)
head(iris[order(iris$Sepal.Length), ])
tail(iris[order(iris$Sepal.Length), ])

head(iris[order(iris$Sepal.Length, iris$Petal.Length), ])


#with:데이터프레임에서 변수에 접근하는 함수
with(iris, {
  print(mean(Sepal.Length))
  print(mean(Petal.Length))
})

x<-data.frame(val=c(1,2,NA,4,NA))
x
median(x$val)
median(x$val, na.rm=TRUE) # 1 2 4

ifelse()
is.na(x$val)

#NA를 중위수로 처리
x$val<-ifelse(is.na(x$val),median(x$val, na.rm=TRUE), x$val) #참:NA
#ifelse(조건식, 참, 거짓)
x

x<-data.frame(val=c(1,2,NA,4,NA))
x
m<-median(x$val, na.rm=TRUE) #중위수

# x$val의 NA를 x$val값으로 대체하시오
# 1 2 2 4 2
# ifelse 안쓰고 is.na함수를 적절하게 사용하시오
# 불린참조를 활용

x$val[is.na(x$val)] <-m #중위수

is.na(x$val)
x$val[is.na(x$val)]<-m
x

#iris 데이터셋의 Sepal.Length열에서 1번째 값을 na로 변경
iris[1,1]<-NA
iris


#iris 데이터 중에 NA값이 있다면, 결측값을 해당 종의(Species)의 중앙값으로 변경



sapply(iris[,1:4],median, na.rm=TRUE) #sapply(행렬or벡터, 함수)

split(iris$Sepal.Length, iris$Species)

median_per_species<-sapply(split(iris$Sepal.Length, iris$Species),median, na.rm=TRUE) #sapply(행렬or벡터, 함수)
median_per_species #na를 제외한 종별 중위수 벡터

iris$Sepal.Length<-ifelse(is.na(iris$Sepal.Length), median_per_species[iris$Species], iris$Sepal.Length)
iris

#which:조건에 만족하는 데이터의 색인 리턴
x<-c(2,4,6,7,10)
x%%2

x%%2==0
which(x%%2==0) #1235 => x를 2로 나눈 나머지가 0과 같은 데이터의 색인
#which(조건) 조건이 참인 데이터의 색인

x[which(x%%2==0)]

which.max(x)
which.min(x)
x[which.max(x)]
x[which.min(x)]


####################데이터 가공 처리 실습#######################
#dplyr패키지에 있는 데이터 처리 관련 함수
library(dplyr)
exam<-read.csv("csv_exam.csv")
exam

#class가 1인 학생들 데이터만 추출하시오
#1)
exam$class==1
exam[exam$class==1,]
#dplyr - filter함수 : 조건에 따라 만족하는 데이터 추출
exam %>% filter(class==1) #%>% 기호 : ctrl + shift + M
exam %>% 
  filter(class==1)

#class가 2인 학생들 데이터만 추출하시오
exam %>% filter(class==2) 

#class가 2가 아닌 학생들 데이터만 추출하시오
exam %>% filter(class!=2) 

#math가 50점 초과한 학생들 데이터만 추출하시오
exam %>% filter(math>50) 

#class가 1이면서 math가 50점 이상인 데이터만 추출하시오
exam %>% filter(class ==1 & math>=50) 

#math가 90이상 이거나(이면서) english가 90 이상인 데이터만 추출하시오
exam %>% filter(math>=90 & english>=90) 
exam %>% filter(math>=90 | english>=90) 

#class가 1반 3반 5반 데이터를 추출
exam %>% filter(class==1 | class==3 | class==5) 
exam %>% filter(class %in% c(1,3,5))

# 제곱
# 5^2
# 5**2
# 나머지와 몫
# 5%%3
# 5%/%3


#dplyr - select함수 : 데이터프레임의 특정 컬럼만 추출
exam %>% select(math)
exam %>% select(math, english,class)
exam %>% select(-math) #math를 제외한 나머지 컬럼 추출
exam %>% select(-math, -english) #math, english를 제외한 나머지 컬럼 추출


#dplyr - 함수 조합
#class가 1인 자료만 추출
exam %>%
  filter(class==1) %>% 
  select(english)#여기에 오는 데이터는 오직 class가 1인 자료

#class가 1인 자료만 추출한 다음(filter), english열만 출력(select)

#exam %>% filter(class==1) %>% select(english)
  
exam %>% 
  select(id,math) %>% 
  head(3)

exam %>% 
  arrange(math)

exam %>% 
  arrange(desc(math))

#1차정렬:class, 2차정렬:math 오름차순정렬
exam %>% 
  arrange(class, math)

#1차정렬:science 오름차순, 2차정렬:math 내림차순정렬
exam %>% 
  arrange(science, desc(math))

#파생변수 : 기존 변수로 부터 연산을 수행한 결과로 새로운 변수를 생성
#ex) 국어,영어,수학 점수 => 총합, 평균 점수
#          일반변수           파생변수

#dplyr - mutate함수 : 파생변수 생성함수
exam %>% 
  mutate(total=math+english+science)

exam %>% 
  mutate(total=math+english+science, 
         avg=(math+english+science)/3)


#exam에서 math와 science의 합으로 english를 뺀 결과를 res라는 새로운 열에 저장
#res 열을 내림차순 정렬하여 출력하시오

exam %>% 
  mutate(res=math+science-english) %>% 
  arrange(desc(res))

exam %>%
  mutate(res=math+science-english)%>%
  select(res)%>%
  arrange(desc(res))


# exam에서 class가 짝수인 데이터를 추출하고,
# math열과 science열을 출력
exam %>% 
  filter(class%%2==0) %>%
  select(math,science)



























