library(dplyr)

# 1. for문으로 다음과 같이 월 이름을 출력
# The month of January
# ...
# The month of December

for (i in month.name) {
  res <- paste("the month of",i)
  print(res)
}

# 2. 짝수이면 TRUE, 홀수이면 FALSE를 출력하는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)

even <- function(x){x%%2==0}

even(c(-5:5))

even(c(-5:5))
c(-5:5)[even(c(-5:5))]
length(c(-5:5)[even(c(-5:5))])

myfunc<-function(x){
  cnt<-0
  for(data in x){
     if(data%%2==0){
       cnt<-cnt+1
     }
  }
  return (cnt)
}

myfunc(c(-5:5))


func1 <- function(x){x>pi}

func1(c(3,1:5))



english=c(90,80,60,70)
math=c(50,60,100,20)
class=c(1,1,2,2)

df_midterm <- data.frame(english=english,math=math,class=class)

df_midterm
# - 각 과목별 평균을 구하시오.
colMeans(df_midterm[,c(1,2)])
apply(df_midterm[-3], 2, mean)

rowMeans(df_midterm[,-3])
apply(df_midterm[-3], 1, mean)
df_midterm %>% 
  mutate(mean_eng_math=(english+math)/2) %>% 
  select(mean_eng_math)

sum(c(2:99)[c(2:99)%%3==0])
length(c(2:99)[c(2:99)%%3==0])

data<-c(2:99) #2:99
s<-0
cnt<-0
for(i in data){
  if(i%%3==0){
    s<-s+i
    cnt<-cnt+1
  }
}
print(s)
print(cnt)

f1<-function(x){
prod(1:x)
}
f1(5)

for (i in 2:9){
  v <- c(1:9)
  ggd <- paste(i,"*",v,"=",i*v)
  print(ggd)
}

for (i in seq(1,4)){ #1,2,3,4
  cat(rep(" ",4-i), rep("*",i*2-1))
  cat('\n')
  # k=(7-i) %/% 2
  # print(rep(' ', k))
  # print(rep("*",i))
  # print('\n')
}
   *      공백3개, 별1개 i*2-1
  ***     공백2개, 별3개 i*2-1
 *****    공백1개, 별5개 i*2-1
*******   공백0개, 별7개 i*2-1

#NA로 처리하고자 하는 문자열 : "", "no", "unknown"
df <- read.csv("train.csv",na.strings = c("","no","unknown") )
df


median(mtcars$wt)
mtcars<-mtcars %>% 
  mutate(weight=ifelse(mtcars$wt>median(mtcars$wt),"heavy","light"))


table(mtcars$weight)

str(mtcars)

prop.table(table(mtcars$weight))


iris_ran <- sample(1:nrow(iris),nrow(iris)*0.7)
iris[iris_ran,]

df <- read.csv("train.csv",na.strings = "")
table(df$Survived)


table(df$Pclass)/nrow(df)
round(table(df$Pclass)/nrow(df)*100,2)

table(cut(df$Age,breaks = c(0,10,20,30,40,50,Inf),right = F))

table(df$Age)

df_cabin <- df %>% 
  filter(!is.na(df$Cabin))

df_cabin$Cabin

substr(df_cabin$Cabin,1,1)


rm(list=ls())
ls()


#foreign 설치 - spss 파일읽기

#한국보건사회연구원 - 한국복지패널데이터 분석
#경제활동, 생활실태 데이터 -> 분석 주제 : 성별/나이/지역/직업... 급여 차이?

install.packages("foreign")
library(foreign) 

str(read.spss("Koweps_hpc10_2015_beta1.sav"))

raw_welfare<-read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
head(raw_welfare,1)

#복사본 생성
welfare<-raw_welfare

head(welfare)
tail(welfare)
dim(welfare) #16664 행, 957 열
View(welfare)
summary(welfare)

#데이터프레임 컬럼명 변경
#welfare<-rename(welfare, 새로운이름=기존이름,...)
library(dplyr) #rename함수가 포함
welfare<-rename(welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                religion=h10_g11, code_job=h10_eco9, income=p1002_8aq1,
                code_region=h10_reg7)
#성별, 연도, 혼인여부, 종교, 직종, 수입, 지역코드


#데이터 분석 순서
#변수 확인(성별이 1또는 2가 아닌 다른 값 존재?, 수입이 0?, 직업 존재하지 않는경우) & 전처리
#변수 관계 확인


#1. 분석주제 : 성별에 따라 급여 차이가 있을까?
#변수? 성별, 수입
#급여 차이? 성별에 따른 평균 급여표 비교(그래프)
welfare$sex
table(welfare$sex)
#만약 성별 값으로 이상치(모름=9)이 존재한다면?제거 or 추정
#ex) 성별=9, 직종=데이터분석가, 월급여=500 => 남자 5명, 여자 11명

#성별이 9인 경우 NA
welfare$sex<-ifelse(welfare$sex==9, NA,welfare$sex )
table(welfare$sex)
table(is.na(welfare$sex))
library(ggplot2)
qplot(welfare$sex)


welfare$sex<-ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

welfare$income
summary(welfare$income)
qplot(welfare$income, bins=30) #default bins=30

hist(welfare$income, breaks=30, xlab='수입', ylab='빈도')
#income의 평균
mean(welfare$income, na.rm=T)
abline(v=mean(welfare$income, na.rm=T), col='blue')

qplot(welfare$income, bins=30)+xlim(0,1000)

#급여가 0 또는 9999인 자료가 있는지 확인 -> 몇건 존재?

summary(welfare$income==0 | welfare$income==9999)
table(welfare$income %in% c(0,9999))
table(welfare$income==0 | welfare$income==9999)
#summary는 NA도 출력, table은 na가 나오지 않음

welfare$income<-ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

table(is.na(welfare$income))
# FALSE  TRUE(NA+0+9999포함) 
# 4620        12044 

#성별에 따른 평균급여(NA 제외)
#welfare$income에서 na제외 -> 성별에 따라 그룹화 -> 그룹에 대한 평균 급여 요약

welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
# sex    mean_income
# <chr>        <dbl>
# 1 female        163.
# 2 male          312.

sex_income<-welfare %>% 
  select(sex,income) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income,na.rm=T))

ggplot(data=sex_income, aes(x=sex, y=mean_income))+
  geom_col()


#분석 주제 : 언제 급여를 많이 받을까?
#나이, 급여
 
welfare$birth 
summary(welfare$birth )
qplot(welfare$birth)

#태어난 연도가 1900~2014 범위를 벗어나는 데이터 몇 건?
table(welfare$birth<1900|welfare$birth>2014)
table(welfare$birth==9999)

welfare$birth<-ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))

#파생변수 생성
welfare$age <- 2015-welfare$birth+1
summary(welfare$age) #2세~109세, 2015년 조사

qplot(welfare$age)
#나이에 따른 평균 월급 출력
# 월급은 na를 제외
# 나이를 기준 그룹화
# 그룹별 급여 평균
welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(birth) %>% 
  summarise(mean_income=mean(income))

welfare %>% 
  select(age,income) %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(income_mean=mean(income))



age_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))

ggplot(data=age_income, aes(x=age, y=mean_income))+
  geom_line()


#어떤 연령대의 급여가 가장 많을까?
#~30, 30~59, 60~ 구간별 평균 급여 조사 -> 시각화
#welfare에 ageg 변수 추가, young, middle, old
welfare<-welfare %>% 
  mutate(ageg= ifelse(age<30,"young", 
                      ifelse(age<=59,"middle","old"))) #"young", "middle", "old"
table(welfare$ageg)

qplot(welfare$ageg)

#연령대별 평균 월급 출력
#income에서 na제외 -> 연령대별 그룹화 -> 그룹별 평균 월급

ageg_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income= mean(income))

ageg_income
ggplot(data=ageg_income,
       aes(x=ageg, y=mean_income))+ #알파벳 순서로 정렬
  geom_col() 


ggplot(data=ageg_income,
       aes(x=ageg, y=mean_income))+ 
  geom_col() +
  scale_x_discrete(limits=c("young","middle","old")) # 정렬 순서를 지정

#분석 주제 : 성별 월급 차이가 연령대별로 다를까?
ggplot(data=ageg_income,
       aes(x=ageg, y=mean_income))+ 
  geom_col() +
  scale_x_discrete(limits=c("young","middle","old")) # 정렬 순서를 지정


#연령대별, 성별 기준으로 그룹화
#young/middle/old 그룹(남/여)
sex_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>%   #6개 그룹
  summarise(mean_income= mean(income))

sex_income
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col()


ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits=c("young","middle","old"))  


#분석 주제: 나이 및 성별에 따른 급여 차이 (그래프까지 출력)


sex_age<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

sex_age
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex))+
  geom_line()
#  geom_col(position = "dodge")

#분석 주제 : 가장 많은 월급을 받는 직업?
#직업과 급여 추출(na제외)->직업별 평균 급여->상위10개 직업군 추출

#welfare의 code_job에는 직업 코드만 존재, 직업명은 없음
# -> Koweps_codebook의 직종코드 시트에 있는 직업 코드와  매칭 => 직업명 추출
welfare$code_job
table(welfare$code_job)
#  welfare와 code book 엑셀문서를 연결

#코드북 - 직종 코드 시트 읽기
library(readxl)
list_job<-read_excel("Koweps_Codebook.xlsx", sheet = 2)
dim(list_job) #149개 직업군

#welfare와 lisst_job을 합침
welfare<-left_join(welfare, list_job, id='code_job')

welfare %>% 
filter(!is.na(welfare$code_job)) %>% 
  select(code_job,job)

#직업별 평균 월급
job_income<-welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))

#월급이 가장 많은 10개 직업 추출
job_income %>% 
  arrange(mean_income) %>% 
  tail(10)

top10<-job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

#가로 막대
ggplot(data=top10, aes(x=job , y=mean_income))+
  geom_col()+
  coord_flip()

#가로 막대
ggplot(data=top10, aes(x=reorder(job,mean_income) , y=mean_income))+
  geom_col()+
  coord_flip()


#가로 막대
ggplot(data=top10, aes(x=reorder(job,-mean_income) , y=mean_income))+
  geom_col()+
  coord_flip()


############################################################
# 1. 평균 급여의 하위 10개 직종 추출 -> 시각화
# 2. 성별에 따라 어떤 직업이 많은지 추출(남성/여성 별 가장 빈도가 높은 직업 10개) -> 시각화
# 3. 종교 유무에 따른 이혼율(종교 있으면 이혼을 덜할까?)
# 4. 연령대(young,middle,old) 및 종교 유무(yes/no)에 따른 이혼율 조사
# #어려운 문제
# 5. 노년층이 많이 사는 지역은?
# list_region과 welfare를 연결
# welfare$code_region #지역코드:1~7

# list_region <- data.frame(code_region = c(1:7),
#                           region = c("서울",
#                                      "수도권(인천/경기)",
#                                      "부산/경남/울산",
#                                      "대구/경북",
#                                      "대전/충남",
#                                      "강원/충북",
#                                      "광주/전남/전북/제주도"))















