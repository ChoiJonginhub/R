# 1. mpg 데이터를 이용해 분석 문제를 해결해 보세요.
# • Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. displ(배기량)이 4 이하인 자동차와
# 5 이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)가 평균적으로 더 높은지 알아보세요.
library(ggplot2)
m<-mpg %>% 
  filter(displ<=4) %>% 
  select(hwy)
n<-mpg %>% 
  filter(displ>=5) %>% 
  select(hwy)    
m
n
ifelse(mean(m$hwy)>mean(n$hwy),"배기량이 4이하인 자동차의 고속도로 연비가 더 큽니다.","배기량이 5이상인 자동차의 고속도로 연비가 더 큽니다.")


mean(mpg[mpg$displ<=4,]$hwy)
mean(mpg[mpg$displ>=5,]$hwy)


# • Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. "audi"와 "toyota" 중 어느
# manufacturer(자동차 제조 회사)의 cty(도시 연비)가 평균적으로 더 높은지 알아보세요.

audi<-mpg %>% 
  filter(manufacturer=="audi") %>% 
  select(cty)

toyota<-mpg %>% 
  filter(manufacturer=="toyota") %>% 
  select(cty)

ifelse(mean(audi$cty)>mean(toyota$cty),"audi의 평균 도시연비가 더 높습니다.","toyota의 평균 도시연비가 더 높습니다.")


audi<-mpg %>% filter(manufacturer=="audi")
mean(audi$cty)

toyota<-mpg %>% filter(manufacturer=="toyota")
mean(toyota$cty)

tapply(mpg$cty ,mpg$manufacturer, mean)


# • Q3. "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의
# 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요
total<-mpg %>% 
  filter(manufacturer=="chevrolet" | manufacturer=="ford" |     manufacturer=="honda" ) %>% 
  select(manufacturer, hwy)
mean(total$hwy)

total<-mpg %>% 
  filter(manufacturer=="chevrolet" | manufacturer=="ford" |     manufacturer=="honda" )
mean(total$hwy)

total<-mpg %>% 
  filter(manufacturer %in% c("chevrolet","ford","honda" ))
mean(total$hwy)

total
tapply(total$hwy,total$manufacturer , mean)




# 2. mpg 데이터를 이용해서 분석 문제를 해결해보세요.
# • Q1. mpg 데이터는 11 개 변수로 구성되어 있습니다. 이 중 일부만 추출해서 분석에 활용하려고 합니다. mpg
# 데이터에서 class(자동차 종류), cty(도시 연비) 변수를 추출해 새로운 데이터를 만드세요. 새로 만든
# 데이터의 일부를 출력해서 두 변수로만 구성되어 있는지 확인하세요.

mpg_new<-mpg %>% 
  select(class,cty)

head(mpg_new)


# • Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한 데이터를 이용해서
# class(자동차 종류)가 "suv"인 자동차와 "compact"인 자동차 중 어떤 자동차의 cty(도시 연비)가 더
# 높은지 알아보세요

suv<-mpg_new %>% 
  filter(class=="suv")

compact<-mpg_new %>% 
  filter(class=="compact")

ifelse(mean(suv$cty)>mean(compact$cty),"suv인 자동차의 평균 도시연비가 더 높습니다.","compact인 자동차의 평균 도시연비가 더 높습니다.")
mean(suv$cty)
mean(mpg_new[mpg_new$class=="suv",]$cty)
mean(mpg_new[mpg_new$class=="compact",]$cty)



# 3. mpg 데이터를 이용해서 분석 문제를 해결해보세요.
# • "audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지 알아보려고 합니다. 
# "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요

mpg %>%
  filter(manufacturer=="audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

mpg %>%
  filter(manufacturer=="audi") %>% 
  select(class,hwy) %>%  
  arrange(desc(hwy)) %>% 
  head(5)





# 4. mpg 데이터를 이용해서 분석 문제를 해결해보세요.
# mpg 데이터는 연비를 나타내는 변수가 hwy(고속도로 연비), cty(도시 연비) 두 종류로 분리되어 있습니다. 두
# 변수를 각각 활용하는 대신 하나의 통합 연비 변수를 만들어 분석하려고 합니다.
# • Q1. mpg 데이터 복사본을 만들고, cty 와 hwy 를 더한 '합산 연비 변수'를 추가하세요.


mpg_copy<-mpg
mpg_copy<-mpg_copy %>% 
  mutate(total_cty_hwy=cty+hwy)

mpg_copy$total_cty_hwy


# • Q2. 앞에서 만든 '합산 연비 변수'를 2 로 나눠 '평균 연비 변수'를 추가세요.

mpg_copy<-mpg_copy %>% 
  mutate(avg=total_cty_hwy/2)

mpg_copy$avg


mpg %>% mutate(total=cty+hwy, average=total/2)



# • Q3. '평균 연비 변수'가 가장 높은 자동차 3 종의 데이터를 출력하세요.

mpg_copy %>% 
  arrange(desc(avg)) %>% 
  head(3)




# • Q4. 1~3 번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요. 데이터는 복사본 대신
# mpg 원본을 이용하세요

mpg %>% 
  mutate(total_cty_hwy=cty+hwy,
         avg=total_cty_hwy/2) %>% 
  arrange(desc(avg)) %>% 
  head(3)





# 5. 다음 문제를 해결하세요.
# • Q1. funtion()을 사용하여 다음의 함수를 만드시오.
# • Q2. myfunc(x,y)=x^2+y+10
myfunc<-function(x,y){
  x**2+y+10    
}
myfunc(3,2)


# • Q3. sample()을 활용해서 로또번호(1 ~ 45, 6개)를 추첨하여라.
sample(1:45,6)




############################################################################

library(dplyr)
exam<-read.csv("csv_exam.csv")
exam

exam %>% 
  summarise(mean_math=mean(math))

#반별 수학 평균
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))


#반별 수학 평균, 합계, 중앙값, 학생수
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n())

#요약 통계 함수 : mean, sum, median, min, max, n, sd, ...

#그룹을 세분화
#제조사(15개)별, 구동방식(drv, 3개)별로 그룹화 => 3*15 = 45개 그룹
str(mpg)

mpg %>% 
  group_by(drv) %>% 
  summarise(n=n())

mpg %>% 
  group_by(manufacturer, drv) %>%  #이론적으로는 15*3=45개 그룹 존재
  summarise(n=n()) #실제로는 15개 제조사, 22개 그룹 존재

mpg %>% 
  group_by(manufacturer, drv) %>%  #이론적으로는 15*3=45개 그룹 존재
  summarise(mean_cty=mean(cty)) %>% 
  head(10)
  

#mpg에서 제조사별로 suv자동차 추출
mpg %>% 
  group_by(manufacturer) %>% #제조사는 15개 그룹이 있음 
  filter(class=='suv') #class에 suv가 있는 제조사는 10개 그룹이 있음


#mpg에서 제조사별로 suv자동차의 도시+고속 연비 평균 구하여 avg열에 저장
mpg %>% 
  group_by(manufacturer) %>% #제조사는 15개 그룹이 있음 
  filter(class=='suv') %>%  #class에 suv가 있는 제조사는 10개 그룹이 있음
  mutate(avg=(cty+hwy)/2) %>%  #평균 연비 
  arrange(desc(avg)) %>% 
  head(5)


#mpg에서 제조사별로 suv자동차의 도시+고속 연비 평균 구하여 avg열에 저장

#10*2, 10개 그룹
mpg %>% 
  group_by(manufacturer) %>% #제조사는 15개 그룹이 있음 
  filter(class=='suv') %>%   #class에 suv가 있는 제조사는 10개 그룹이 있음
  mutate(avg=(cty+hwy)/2) %>%  #평균 연비 
  summarise(mean_avg=mean(avg)) #각 자동차의 평균 연비에 대한 제조사별 평균값 요약

  
#15*2, 15개 그룹
mpg %>% 
  group_by(manufacturer) %>% #제조사는 15개 그룹이 있음 
  mutate(avg=(cty+hwy)/2) %>%  #평균 연비 
  summarise(mean_avg=mean(avg)) #각 자동차의 평균 연비에 대한 제조사별 평균값 요약



#데이터 합치기
test1<-data.frame(id=c(1,2,3,4,5),
           midterm=c(60,70,80,90,50))
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(30,70,20,90,50))

#id를 기준으로 데이터프레임 합치기
total<-left_join(test1, test2, by="id")
total

name<-data.frame(class=c(1,2,3,4,5),
                  teacher=c("kim","lee","park","choi","cho"))

left_join(exam, name, by='class')


g1<-data.frame(id=c(1,2,3,4,5),
                  test=c(60,70,80,90,50))
g2<-data.frame(id=c(1,2,3,4,5),
                  test=c(30,70,20,90,50))
g1
g2

bind_rows(g1,g2)


# rbind(), cbind() : vector에 대해서도 연결 가능 => matrix로 출력
# bind_rows, bind_cols() :vector에 대해서는 연결안됨, 속도가 아주 빠름, 
#열이 다르더라도 행 기준으로 합칠수 있음

df1<-data.frame(x=1:3, y=1:3)
df2<-data.frame(x=7:9, z=7:9)
#rbind(df1,df2) #에러 발생
df1
df2

bind_rows(df1,df2)


df1<-data.frame(x=1:10000000, y=1:10000000)
df2<-data.frame(x=1:10000000, y=1:10000000)
system.time(rbind(df1,df2)) #1.61

system.time(bind_rows(df1,df2)) #0.1




#데이터수집 -> 데이터전처리(분석 목적에 부합되는 형식의 데이터로 변환) -> 데이터분석 -> 시각화 -> 알고리즘(예측/분류 모델) -> 유지보수 및 개선
#동의어처리:한국,남한,고려,대한민국,코리아...=>대한민국
#다의어처리:배(먹는?타는?신체?배), 트럼프(대통령? 게임?)


#결측값(누락값) 처리
#누락값 발생 이유? 원래부터 값이 없었음(ex.시험 미응시), 입력자의 실수, 합치기 작업에서 발생(bind_rows)

df<-data.frame(gender=c("m","f",NA,"m","f"),
           score=c(80,90,50,70,NA))
df

#결측값 확인
is.na(df)

is.na(df$gender)

#빈도수
table(is.na(df$gender))

#빈도수
table(is.na(df))

mean(df$score)
sum(df$score)
NA+1
NA*2

mean(df$score,na.rm=TRUE)


df %>% 
  filter(is.na(score)) #score가 na인 데이터 추출

df %>% 
  filter(!is.na(score)) #score가 na가 아닌 데이터 추출


df_nomiss<-df %>% 
  filter(!is.na(score))

df_nomiss
sum(df_nomiss$score)

#df에서 score와 gender열 값에 대해 결측값이 있는 경우는 모두 제외하고 출력
df %>% 
  filter(!is.na(score) & !is.na(gender))

df %>% 
  filter(!is.na(score),!is.na(gender))

df
na.omit(df)


exam[3,"math"]<-NA
exam

exam[c(8,15), "math"]<-NA
exam


exam %>% 
  summarise(mean_math=mean(math, na.rm=T),
            sum_math=sum(math, na.rm=T),
            median_math=median(math, na.rm=T)
            )

exam


#결측값 대체방법
#최빈수, 평균, 중앙값 대체
#통계분석 기법(회귀, 머신러닝, 딥러닝...) 활용 예측값으로 대체


#평균값으로 대체
mean(exam$math)
mean(exam$math, na.rm=T)

#math 열 값이 NA면 평균값으로 대체하시오
exam

#ifelse(조건, 참, 거짓)
exam$math<-ifelse(is.na(exam$math),as.integer(mean(exam$math, na.rm=T)), exam$math)
exam


exam<-read.csv("csv_exam.csv")
exam$math<-ifelse(is.na(exam$math),mean(exam$math, na.rm=T), exam$math)
exam

exam


df<-data.frame(gender=c(1,2,1,3,2,1),
           score=c(50,40,77,88,99,22))
df

#이상치 확인 방법
table(df$gender) #빈도수

#이상치 처리방법:이상치를 NA로 대체, 연산시에는 NA를 제외
df$gender<- ifelse(df$gender==3, NA, df$gender)
df

#SCORE가 50점 만점
#50점 초과시 NA로 저장
df$score<- ifelse(df$score>50, NA, df$score)
df

df<-data.frame(gender=c(1,2,1,3,2,1,2),
               score=c(50,40,77,88,99,22,33))
df


#성별은 1또는 2, 점수는 0~50점 사이 -> 범위를 벗어나는 경우에는 결측값으로 대체
#결측값을 제외하고 gender로 그룹화하여 score의 평균 출력
df$gender <- ifelse(df$gender!=1 & df$gender!=2, NA, df$gender)
df$score <- ifelse(df$score>50|df$score<0, NA, df$score)

df %>% 
  filter(!is.na(score) & !is.na(gender)) %>% 
  group_by(gender) %>% 
  summarise(score_mean=mean(score))


df$gender<-ifelse((!df$gender==1 & !df$gender==2), NA, df$gender)

df$score<-ifelse(df$score>50 | df$score<0, NA, df$score)


df %>%  
  filter(!is.na(score) & !is.na(gender)) %>% 
  group_by(gender) %>%
  summarise(mean=mean(score))


df$score<- ifelse(df$score>50, NA, df$score)
df$gender<- ifelse(df$gender==3, NA, df$gender)
df_nomiss<-df %>% 
  filter(!is.na(gender),!is.na(score))
df_nomiss %>% 
  group_by(gender) %>% 
  summarise(gender_score=mean(score))



df %>%  
  filter(!is.na(score) & !is.na(gender)) %>% 
  group_by(gender) %>%
  summarise(mean=mean(score))


mpg
mpg$hwy
boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats
#12보다 작거나 37보다 크면 극단치로 간주

#12보다 작거나 37보다 크면 극단치로 간주하고 NA로 대체한 다음
#NA를 제외한 나머지 데이터에 대해  통계조사

mpg$hwy<-ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)
mpg$hwy
table(is.na(mpg$hwy))
 
# mpg데이터에서 drv방식으로 그룹화한 다음,
# 각 그룹별로 hwy에 대한 평균값으로 요약(na 제외)


mpg %>% 
  filter(!is.na(mpg$hwy)) %>% 
  group_by(drv) %>% 
  summarise(drv_hwy=mean(hwy))


mpg %>%
  group_by(drv) %>%
  summarise(hwy_mean=mean(hwy, na.rm=T))


mpg_new %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(hwy_mean=mean(hwy))


mpg<-as.data.frame(ggplot2::mpg)
mpg

mpg

ggplot2::mpg %>% 
  print(n=100, width=Inf)

ggplot2::mpg %>% 
  View()


#산점도 : plot()
install.packages("mlbench")
library(mlbench)
data(Ozone)
Ozone

#데이터셋에 포함된 데이터 목록 확인
library(help="mlbench")

plot(mpg$cty, mpg$hwy, xlab="cty", ylab="hwy", main="mpg",pch=20, cex=1,
     col="#ff0000", xlim=c(0,50),ylim=c(0,50), type="p") #rrggbb
#plot(Ozone$V8, Ozone$V9)

#iris

cars
plot(cars) #speed 기준으로 정렬
#선 그래프는 위->아래 순서대로 그려나감
#plot(cars, type="l")
plot(cars, type="o", cex=0.5)




