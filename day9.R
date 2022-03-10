head(iris)
str(iris)
plot(iris)
summary(iris[-5])
iris.data<-scale(iris[-5])
summary(iris.data)

#for i=2 to 10
#kmeans(iris.data, i)
#클러스터들 사이의 거리는 최대화
#높은 응집도(클러스터를 구성하는 각각의 데이터 사이의 거리가 최소인 k값을 조사)
#시각화 -> k값을 직접 설정
iris.kmeans<-kmeans(iris.data, 3)
iris.kmeans

plot(iris[-5], pch=iris.kmeans$cluster, col=iris.kmeans$cluster)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width))+
  geom_point(aes(col=Species))

set.seed(20220310)
#kmeans, h-clustering, dbscan
irisCluster<-kmeans(iris[,1:4], 3)
#kmeans에 의해 분류된 클래스와 실제 클래스를 비교
table(irisCluster$cluster, iris$Species)

#elbow 그래프
tot.within<-vector(mode='character', length=10)
for (i in 1:10){ #i:군집의 개수
  irisCluster<-kmeans(iris[,1:4], i)
  tot.within[i]<-irisCluster$tot.withinss
}
tot.within
plot(1:10, tot.within, type='b', pch=19)


cars.med<-median(mtcars$wt)
cars.med
mtcars$weight<-ifelse(mtcars$wt > cars.med, "heavy", "light")
table(mtcars$weight)
summary(mtcars)

table(mtcars$weight)
table(mtcars$weight)/nrow(mtcars)

names(mtcars)
order(names(mtcars))
mtcars[, order(names(mtcars))]

train<-read.csv("train.csv", na.strings="")
str(train)

#Survived열-1:생존, 0:사망, 생존자수와 사망자수를 각각 출력하시오.
table(train$Survived)

num.survivor<-sum(train$Survived==1)
num.death<-sum(train$Survived==0)

table(train$Pclass)
pclass.table<-as.data.frame(table(train$Pclass))
pclass.table
colnames(pclass.table)<-c('pclass', 'num')

pclass.table
#pclass.table에 ratio 열을 추가. ratio열에는 train전체 데이터 중에서 각 pclass값 비율
pclass.table$ratio<-round(pclass.table$num/nrow(train), 3)
pclass.table

sum(train$Survived)
sum(!train$Survived)

sum(proportions(train$Survived))
#C=1, Q=2, S=3 변경
train$Embarked<-ifelse(train$Embarked=="C",1,ifelse(train$Embarked=="Q",2,3))
train$Embarked
table(train$Embarked)
str(train$Embarked)

trainOmit<-na.omit(train$Embarked) #na 제거
proportions(trainOmit) #비율

#비율
#prob.table()

mydata<-matrix(sample(100, 15), ncol=3)
mydata
colnames(mydata)<-LETTERS[seq(1,3)]
mydata

rownames(mydata)<-sprintf('s-%d', seq(5))
mydata

prop.table(mydata) #mydata/sum(mydata)
sum(prop.table(mydata))

prop.table(mydata,1) # 행 기준 비율
prop.table(mydata,2) # 열 기준 비율

rowSums(prop.table(mydata,1) )
colSums(prop.table(mydata,2))



strsplit(train$Name, split=" ")

lst<-unlist(strsplit(train$Name, split=" "))
#unlist함수 list -> vector
lst
grep("Mr.", lst) #호칭이 Mr.인 사람들의 index
grep("Mr.", lst, ignore.case = TRUE) #호칭이 Mr.인(대소문자 구분없이) 사람들의 index
grep("George", lst)
grep("^G", lst) #G로 시작하는 모든 문자열의 index
grep("am$", lst) #am로 끝나는 모든 문자열의 index
lst[24]
lst

grep("..l", lst) #세번째 글자가 l인 문자열의 index, 점 1개가 문자 1개
grep("\\d", lst) #숫자가 들어가있는 텍스트를 검색

#grepl과 grep은 동일함. 차이점은 수행 결과가 grep은 index, grepl은 논리값으로 출력
lst[grepl("Mr.", lst)]

lst[grep(".{2,}", lst)] #2글자 이상인 자료들 추출
train$names2<-lst[grep(".{2,}\\.$", lst)] #호칭만 추출, 문자가 2글자 이상이며 점(.)으로 끝나는 문자열 추출

lev<-factor(train$names2)
lev
length(lev)


for(i in 1:length(lev)){
  mean.age<-mean(train$Age[train$names2==lev[i]], na.rm=T) #호칭이 동일한 사람들의 나이 평균
  #print(mean.age)
  train$Age[train$names2==lev[i] & is.na(train$Age)]<-mean.age #평균 나이(mean.age)로 NA를 대체
}

head(train,1) #Mr. 나이는 22살

train$Pclass
#각각의 Pclass 비율
sum(ifelse(train$Pclass==1, proportions(train$Survived),0))
sum(ifelse(train$Pclass==2, proportions(train$Survived),0))
sum(ifelse(train$Pclass==3, proportions(train$Survived),0))

#train에서 SibSp열에 대한 합계
apply(train['SibSp'],2,sum) #466
sum(train['SibSp'])

###########################################################################################

str(train)



# train<-read.csv("train.csv",stringsAsFactors = TRUE)
# train$Name<-as.character(train$Name) #factor -> char
# str(train)


# train<-read.csv("train.csv") #char
# str(train)

# train$Name<-as.character(train$Name) #factor -> char
# str(train)

# train$Embarked<-as.factor(train$Embarked)
# str(train)


train<-read.csv("train.csv")
test<-read.csv("test.csv")
str(test)
str(train)
table(train$Survived)

#training 데이터 -> 알고리즘 선택 -> 모델(생존여부 판단)
#test 데이터 ------------------------------------> 모델  => 생존 여부 예측 => gender_submission.csv에 작성 => 캐글 제출
test$Survived<-rep(0,418) #418명 모두 사망한 것으로 예측
head(test)

mysubmit<-data.frame(PassengerId=test$PassengerId, Survived=test$Survived)
write.csv(mysubmit, file="submit.csv", row.names = FALSE) #13956등


test$Survived<-rep(1,418) #418명 모두 생존한 것으로 예측
head(test)
mysubmit<-data.frame(PassengerId=test$PassengerId, Survived=test$Survived)
write.csv(mysubmit, file="submit2.csv", row.names = FALSE) #13956등


#사전지식 -> 남성 : 사망, 여성 : 생존
#test의 성별 값에 대해 남성 -> 사망, 여성 -> 생존

#규칙기반 알고리즘으로 예측
test$Survived<-ifelse(test$Sex=='male' & !is.na(test$Sex), 0, 1)
mysubmit<-data.frame(PassengerId=test$PassengerId, Survived=test$Survived)
write.csv(mysubmit, file="submit3.csv", row.names = FALSE) #10000등


#사전지식이 없다면?
train$Sex
summary(train$Sex)

class(train$Sex)

train$Sex<-as.factor(train$Sex)
summary(train$Sex)

#여성/남성 생존 비교 => 탑승한 대다수 여성이 생존, 남성은 대부분 사망
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived)) #전체 승객수 대비 성별/생존여부별 비율
prop.table(table(train$Sex, train$Survived), 1) #행 데이터 비율
prop.table(table(train$Sex, train$Survived), 2) #열 데이터 비율

test$Survived<-0
test$Survived[test$Sex=='female']<-1


#우리의 모델에 나이 추가

train$Age

summary(train$Age) #결측값 177건, 대부분 승객 평균 연령 20대 후반


#승객이 18세 미만인지 변수

train$Child<-0 #모두 성인으로 가정
train

#18세 미만은 모두 생존 -> Child를 1로 설정(18세 미만)
train$Child[train$Age<18]<-1


#Child 값과 Sex 값에 따라 Survived 열 값을 요약하시오.
table(train$Sex, train$Survived)
table(train$Child, train$Survived)

library(dplyr)

train %>% 
  group_by(Child,Sex,Survived) %>% 
  summarise(n=n())


#aggregate(formula, data, FUN)#data에서 formula(종속변수~독립변수)를 정의하고 FUN에 해당되는 함수로 요약


aggregate(Survived~Sex,data=train,FUN=sum) #성별에 따른 생존자 수, 여성/남성이 몇 명있는지 확인이 불가

#생존=1, 사망=1로 코딩 -> 단순 합계(sum,0+1+...)만으로는 생존자 수만 알 수 있음. 전체 인원수는 알 수 없음
aggregate(Survived~Sex+Child,data=train,FUN=sum) 

# Sex Child Survived
# 1 female     0      195
# 2   male     0       86
# 3 female     1       38
# 4   male     1       23


#그룹별 전체 인원수를 알 수 있음
aggregate(Survived~Sex+Child,data=train,FUN=length) 
# Sex Child Survived
# 1 female     0      259
# 2   male     0      519
# 3 female     1       55
# 4   male     1       58


#그룹별 인원수의 비율을 구하는 함수가 없음 => 직접 제작

# Sex Child Survived  생존자수/각그룹별인원수
# 1 female     0      195/259
# 2   male     0      86/519
# 3 female     1       38/55
# 4   male     1       23/58

aggregate(Survived~Sex+Child,data=train,FUN=function(x){ sum(x)/length(x) }) #x에는 각 그룹 데이터가 전달
# Sex Child  Survived
# 1 female     0 0.7528958
# 2   male     0 0.1657033
# 3 female     1 0.6909091
# 4   male     1 0.3965517





