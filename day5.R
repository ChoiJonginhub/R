#그래프 패키지 : ggplot2
#1 layer:배경, 2 layer :그래프, 3 layer : 설정

#산점도 그래프 : x,y축에 점으로 표현 그래프, 두 변수간의 관계 표현

#displ, hwy 관계 그래프

#1 layer - 배경 설정
ggplot(data=mpg, aes(x=displ, y=hwy))
#2 layer :그래프
ggplot(data=mpg, aes(x=displ, y=hwy))+geom_point()
#3 layer : 설정
ggplot(data=mpg, aes(x=displ, y=hwy))+
  geom_point()+
  xlim(0,10)+
  ylim(0,50)

#ggplot함수 구조
#ggplot(데이터, 축)+그래프종류+그래프세부설정

#시각화 : 분석 결과를 최종적으로 보고, 데이터 전처리 단계에서 데이터 확인
# ggplot() : 최종적으로 보고용
# qplot() : 전처리 단계 사용

#str(mpg)


#mpg$cty 도시 연비와 mpg$hwy 고속도로 연비의 관계 그래프
ggplot(data=mpg, aes(x=cty, y=hwy))+geom_point()

midwest

ggplot(data=midwest, aes(x=poptotal, y=popasian))+geom_point()+
  xlim(0,500000)+
  ylim(0,10000)

#2e+06 => 2* 10^6

#막대 그래프 : 집단 간 비교

mpg<-ggplot2::mpg
df_mpg<-mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

ggplot(data=df_mpg, aes(x=drv, y=mean_hwy))+geom_col()
#geom_col(): 평균값을 막대 그래프로 출력
#geom_bar(): 빈도를 막대 길이로 표현
ggplot(data=mpg, aes(x=drv))+geom_bar()


cars
#speed에 따른 평균 dist가 막대 그래프로 출력
ggplot(data=cars, aes(x=speed, y=dist))+geom_col()

#speed에 따른 빈도수를 막대 그래프로 출력
ggplot(data=cars, aes(x=speed))+geom_bar()


mpg<-ggplot2::mpg
df_mpg<-mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))
#오름차순
ggplot(data=df_mpg, aes(x=reorder(drv,mean_hwy), y=mean_hwy))+geom_col()

#내림차순
ggplot(data=df_mpg, aes(x=reorder(drv,-mean_hwy), y=mean_hwy))+geom_col()

#선그래프 : 시간에 흐름에 따라 달라지는 데이터 표현
economics

ggplot(data=economics, aes(x=date, y=unemploy))+geom_line()


#상자그림(boxplot) : 집단간 분포 비교
ggplot(data=mpg, aes(x=drv, y=hwy))+geom_boxplot()


install.packages("ggiraphExtra")
library(ggiraphExtra)

USArrests

head(USArrests)

install.packages("tidyverse")
library(tibble)

#행 인덱스를 컬럼 값으로 변환하여 데이터프레임 생성
crime<-rownames_to_column(USArrests, var='state')
str(USArrests)
USArrests

crime

#지도 그릴때 state 명을 소문자로 변환
crime$state<-tolower(crime$state)
crime

library(ggplot2)
#미국 주 지도 그릭
states_map<-map_data('state')
states_map

ggChoropleth(data=crime, #지도 표시 데이터
             aes(fill=Assault, #색상으로 표시할 변수
                 map_id=state), #지역 기준 변수
                map=states_map #지도 데이터
             )
states_map


ggChoropleth(data=crime, #지도 표시 데이터
             aes(fill=Assault, #색상으로 표시할 변수
                 map_id=state), #지역 기준 변수
             map=states_map, #지도 데이터
             interactive = T
)



#인구 구분도 
install.packages("stringi") # 문자열 처리 패키지
install.packages("devtools") #패키지 개발 도구
devtools::install_github("cardiomoon/kormaps2014", force = TRUE) #깃헙에서 패키지를 다운로드 설치
library(kormaps2014)
korpop1
# str(changeCode(korpop1)) #시도
# str(changeCode(korpop2)) #구군
# str(changeCode(korpop3)) #읍면동


library(dplyr)
korpop1<-rename(korpop1, pop=총인구_명, name=행정구역별_읍면동)

korpop1<-changeCode(korpop1)

library(ggiraphExtra)
ggChoropleth(data=korpop1,
             aes(fill=pop,  #인구 표시
                 map_id=code, #지역 기준 변수
                 tooltip=name), #지도 위 표시 지역명
               map=kormap1,
             interactive = T)

install.packages("plotly")
library(plotly)
#plotly는 상호작용형 시각화 패키지
library(ggplot2)
ggplot(data=mpg, aes(x=displ, y=hwy, col=drv))+geom_point()

p<-ggplot(data=mpg, aes(x=displ, y=hwy, col=drv))+geom_point()
ggplotly(p)

diamonds

ggplot(data=diamonds, aes(x=cut))+geom_bar() #cut등급별 빈도수

#cut등급별(5개) clarity(8가지)의 빈도수
ggplot(data=diamonds, aes(x=cut, fill=clarity))+geom_bar()
ggplot(data=diamonds, aes(x=cut, fill=clarity))+geom_bar(position='dodge')
p<-ggplot(data=diamonds, aes(x=cut, fill=clarity))+geom_bar(position='dodge')
ggplotly(p)

#시계열 데이터를 interactive하게 시각화
install.packages("dygraphs")
library(dygraphs)
eco<-ggplot2::economics
head(eco)
library(xts) #시계열 데이터를 다루기 위한 툴
eco<-xts(eco$unemploy, order.by=eco$date) #xts타입으로 변경
eco
dygraph(eco)


class(eco)
str(eco)


# dygraph그래프로 출력
# #저축률
# economics$psavert
# #실업자수
# economics$unemploy
eco2<-xts(economics$psavert,order.by=economics$date)
dygraph(eco2)

# economics$unemploy
eco3<-xts(economics$unemploy,order.by=economics$date)
dygraph(eco3)

ecob<-cbind(eco2, eco3)

head(ecob)


psavert<-xts(economics$psavert,order.by=economics$date)
unemploy<-xts(economics$unemploy/1000,order.by=economics$date)
ecob<-cbind(psavert, unemploy)

dygraph(ecob)

#install.packages("mlbench")

mpg
plot(mpg$cty, mpg$hwy)
plot(mpg$displ, mpg$hwy)

opar<-par(mfrow=c(1,2)) #1줄 2칸 설정
plot(mpg$cty, mpg$hwy)
plot(mpg$displ, mpg$hwy)
par(opar) #이전 설정으로 돌아옴

opar<-par(mfrow=c(1,1))
plot(mpg$cty, mpg$hwy)


plot(iris$Sepal.Width, iris$Sepal.Length, cex=.5, pch=20)
#plot(iris$Petal.Width, iris$Petal.Length)
points(iris$Petal.Width, iris$Petal.Length, col="#ff0000")

x<-seq(0, 2*pi, 0.1)
x
y<-sin(x)
y
plot(x,y)
lines(x,y)


mpg
# mpg$cty
# mpg$hwy
#new_mpg<- mpg에서 cty와 hwy열을 추출

new_mpg<-mpg %>% 
  select(cty,hwy)
new_mpg
plot(new_mpg)
lines(lowess(new_mpg)) #적합 모델
# y=ax+b
# y=ax^2+bx+c

cars
#speed에 따라 dist 변화량이 궁금? => 함수 정의 dist= a*speed+b
#dist=3.5*speed-5

plot(cars)
abline(a=-5, b=3.5, col='blue') #직선 그래프
abline(h=mean(cars$dist), col='blue')
abline(v=mean(cars$speed), col='red', lty=2)


plot(cars)
text(10,50, "text")
text(cars$speed, cars$dist, pos=4)


#히스토그램 : 자료의 분포 그래프
hist(iris$Sepal.Width) #빈도

hist(iris$Sepal.Width, freq = FALSE)#확률 밀도
hist(iris$Sepal.Width, freq = FALSE, breaks=20)
#breaks의 기본값은 log(n)+1 -> n은 데이터개수
#log(150)+1



#밀도 그래프
plot(density(iris$Sepal.Length))

#파이그래프 : 데이터 비율

#cut함수:구간 나눔
cut(1:10, breaks=5)

cut(iris$Sepal.Width, breaks=10)
table(cut(iris$Sepal.Width, breaks=10))

pie(table(cut(iris$Sepal.Width, breaks=10)))

df<-read.csv("train.csv")
df

library(dplyr)

str(df)
#pclass 종류별로 인원수 출력
df %>% 
  group_by(Pclass) %>% 
  summarise(n=n())

table(df$Pclass)
summary(df)

str(df)
#df의 Age열에 na에 몇개 있는지 출력

length(df$Age[is.na(df$Age)])
table(is.na(df$Age))

#df의 Age열에 있는 NA를 모두 30으로 치환

#df$Age<-ifelse(is.na(df$Age),30,df$Age)
df$Age[is.na(df$Age)]<-30

length(df$Age[is.na(df$Age)])

#Fare열에 대해서 NA는 15로 모두 치환

df$Fare<-ifelse(is.na(df$Fare),15, df$Fare)

#승선 항구 : S, Q, C 승선항구중에서 가장 많은 승객이 승선한 항구명은?

df %>% 
  group_by(Embarked) %>% 
  summarise(c=n()) %>% 
  arrange(desc(c)) %>% 
  head(1)

#"" <- 모두 "C"로 치환하시오
df$Embarked<-ifelse(df$Embarked=="","C",df$Embarked)
df$Embarked[df$Embarked==""] <- "C"

















