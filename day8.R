# clustering(클러스터링, 군집화)
# -주어진 데이터를 여러 개의 집단으로 나누는 작업(유유상종)
# kmeans 클러스터링, 계층적 클러스터링, DBSCAN 클러스터링

teens<-read.csv("snsdata.csv")
str(teens)

teens$gender
table(teens$gender, useNA = "ifany")#전체 3만건 중에 NA가 2724건 존재
summary(teens$gender)      
summary(teens$age) #전체 3만건 중에 NA가 5086건 존재

# teens$gender=="F" : 22054건
# teens$gender=="M" : 5222건

teens$gender
#female : 1, male : 0
teens$female<-ifelse(teens$gender=="F"&!is.na(teens$gender),1,0) #여성인지 아닌지를 나타내는 열(여성:1, 남성 or NA:0)
teens$no_gender<-ifelse(is.na(teens$gender),1,0) #NA이면 1, 남성 OR 여성인 경우에는 0
table(teens$female) #0:7946, 1:22054

table(teens$gender, useNA = "ifany")
table(teens$female, useNA="ifany")
table(teens$no_gender, useNA="ifany")

#age열 값 평균
mean(teens$age[!is.na(teens$age)])
mean(teens$age, na.rm=T)

#졸업연도별 평균나이
library(dplyr)
teens %>%
  group_by(gradyear) %>%
  summarise(mean_age=mean(age, na.rm=T))

teens %>% 
  select(gradyear,age) %>% 
  group_by(gradyear) %>% 
  summarise(mean_age=mean(age,na.rm=T))

#help -> aggregate
#aggregate(data=teens, 대상~그룹화기준변수, 적용함수)
aggregate(data=teens, age~gradyear, mean)

#help -> ave : 부분집합(서브그룹)의 평균값으로, 각각의 원소 값을 변환
#ave(종속변수, 독립변수(범주형))

df<-data.frame(score=c(75,73,79,83,85,92,99,100,72,83),
               class=c('A','A','A','B','B','B','C','C','C','C'))
df
#각각의 class별로 평균을 구하고, 각 값을 평균으로 대체
ave(df$score, df$class)

#ave(나이, 졸업연도) #졸업연도별로 subset이 만들어지고, 각 subset별로 나이 평균
#ave(teens$age, teens$gradyear)
ave_age<-ave(teens$age, teens$gradyear,FUN=function(x) mean(x, na.rm=TRUE)) 
#기본적으로 평균이 적용, 지금은 na를 제외한 평균을 구해야하는 상황
#x에는 각각의 age 값이 전달됨

length(ave_age) #30000

#teens$age 값이 NA이면 ave_age값으로 대체하시오
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

summary(teens$age)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3.086  16.504  17.443  17.982  18.391 106.927 

#연습문제
#이상치를 파악 -> 적정한 값으로(해당 학생의 졸업연도 참고) 대체

str(teens)
interests<-teens[5:40]
interests
#고려할 사항:
# kmeans 알고리즘 : k개 랜덤한 점(중심점)을 생성-> 
# c1, c2, c3, c4, c5
# dist(c1,d1) ... dist(c1,d30000)
# dist(c2,d1) ... dist(c2,d30000)
# dist(c3,d1) ... dist(c3,d30000)
# dist(c4,d1) ... dist(c4,d30000)
# dist(c5,d1) ... dist(c5,d30000)
# 
# => 각각의 c1~c5점과 가까운 데이터 점을 찾아서 클러스터에 할당

#정규화와 표준화는 모든 열들에 대해 수행
# 정규화 : 모든 수치 데이터의 범위를 0~1사이로 스케일링
# =(각 단어의 빈도수 - 최소값) / (각 단어의 빈도수 최대값 - 최소값)
# ex)drugs 단어를 최대 100번 언급, 최소 0번 언급
# drugs 단어를 5번 언급한 경우에 정규화 = (5-0) /(100-0) = 0.05
# 
# 표준화 : 데이터를 0을 중심으로 양쪽으로 분포시키는 방법. 각 데이터가 평균을 기준으로
# 얼마만큼 떨어져 있는지를 나타내는 값으로 변환
# 표준화(z-score) =  (각 단어의 빈도수 - 각 단어의 빈도수평균) / 표준편차

#표준편차 : sqrt(sum((각 단어의 빈도수 - 각 단어의 빈도수평균)^2) / n)


# 0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
# 0	0	1	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	2	2	1	0	0	0	6	4	0	1	0	0	0	0	0	0	0	0
# 유클리디안 거리(d1,d2)=sqrt((0-0)^2+(0-0)^2+...+(0-0)^2)
# 0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	2	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0
# 유클리디안 거리(d1,d3)=sqrt((0-0)^2+(0-1)^2+...+(0-0)^2)
#             ...
# 유클리디안 거리(d1,d30000)
            

head(interests)

scale(데이터, center=TRUE, scale=TRUE)  #표준화
# center=true는 각 data-data평균
# scale=TRUE 데이터의 평균을 뺀 다음 표준편차로 나누는 옵션

#표준화
scale(USArrests, center=TRUE, scale=TRUE)

#정규화
usmin<-min(USArrests$Murder)
usmax<-max(USArrests$Murder)
scale(USArrests$Murder, center=usmin, scale=usmax-usmin)

normalization<-function(x){
  return( (x-min(x)) / (max(x)-min(x)) )
}
normalization(USArrests$Murder)


str(interests)
#interests의 모든 열 값들을 표준화
class(scale(interests, center = T, scale=T))
as.data.frame(apply(interests, 2, scale))
class(lapply(interests, scale))

interests_z<-as.data.frame(lapply(interests, scale))


summary(interests$basketball)
summary(interests_z$basketball)

set.seed(20220308)
teen_clusters<-kmeans(interests_z, 5)
teen_clusters

# K-means clustering with 5 clusters of sizes 1027, 4002, 2715, 810, 21446
# 1번그룹 : 문제아
# 2번그룹 : 꾸미는 것 좋아함(여학생 비율 88.7%)
# 3번그룹 : 운동(여학생 비율 69.5%)
# 4번그룹 : 밴드, 행진(악기)
# 5번그룹 : 별 감흥이 없는, 내성적인, 별 관심없는...

teen_clusters$size #각 클러스텅 속한 데이터 갯수
teen_clusters$centers #각 클러스터의 중심점(centroid of cluster)
teen_clusters$tot.withinss # withinss의 총합, 946151.9
teen_clusters$withinss #withinss: 클러스터의 응집도, 작을수록 좋음
teen_clusters$betweenss #클러스터간 얼마나 떨어져 있는지 나타내는 분리도, 클수록 좋음
teens$cluster<-teen_clusters$cluster #각 데이터가 소속된 클러스터 번호
teens[1:5,c('cluster','gender', 'age', 'friends')]

#1번~5번 그룹에 속한 학생들의 평균나이
teens %>% 
  group_by(cluster) %>% 
  summarise(mean_age=mean(age))
#data속 insight를 발견하기 위해 다양한 formula 작성 필요
aggregate(data=teens, age~cluster, mean)
aggregate(data=teens, female~cluster, mean) #female열 0 또는 1의 값, 1에 가까울수록 female

#어떤 그룹이 친구가 적고 많을까?2번그룹이 가장 많음, 5번 그룹이 가장 적음
aggregate(data=teens, friends~cluster, mean) 


#NbClust : 적당한 클러스터 개수 찾아주는 패키지
install.packages("NbClust")
library(NbClust)
str(teens)

teens_mat<-as.matrix(teens[5:40])
teens_mat
#NbClust(teens_mat[,1:3], min.nc=2, max.nc = 10, method='kmeans')
NbClust(iris[,1:4], min.nc=2, max.nc = 10, method='kmeans')

#============================================================================
member <- data.frame( spent = c(10,1,1,17,4,6,1,15,22,3,0,3,7,0,2),
                      time =  c(15,2,10,7,5,7,1,10,18,3,1,3,7,10,2))
member

plot(member)


res<-kmeans(member, 3)
res

#클러스터링 그래프 도구
install.packages("fpc")
library(fpc)
plotcluster(member, res$cluster, color=TRUE)




















