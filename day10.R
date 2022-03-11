train<-read.csv('train.csv')

#1. Survived 열 요약
#- 생존자/사망자 전체 비율
prop.table(table(train$Survived)) #proportions(table(train$Survived))

#- 성별에 따른 생존자/사망자
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived),1)
with(train, tapply(Survived, Sex, mean))


#2.Pclass별 요약
#- 등실 비율
table(train$Pclass)/nrow(train)
#- 등실에 따른 생존자/사망자 비율
prop.table(table(train$Pclass, train$Survived),1)

#3. Age 열
#- 나이대를 구분한 후, 각 나이대별 생존율

train$Ageg<-cut(train$Age, breaks=c(0,10,20,30,40,50,60,70,Inf))
prop.table(table(train$Ageg, train$Survived),1)


#4. 호칭을 아래와 같이 변경하여 name2열에 추가하시오.

lst<-unlist(strsplit(train$Name, split=" "))
lst<-lst[grep('.{2,}\\.$', lst)] 
train$name2<-lst


#* "Mlle", "Ms", "Lady", "Dona" 는 "Miss"로 변경
train$name2<-ifelse(train$name2 %in% c("Mlle.", "Ms.", "Lady.", "Dona."), 'Miss.', train$name2)
#불린참조 형식으로 변경
#train$name2[train$name2 %in% c("Mlle.", "Ms.", "Lady.", "Dona.")]<-'Miss.'


#* "Mme"는  "Mrs"로 변경
train$name2<-ifelse(train$name2=='Mme.', 'Mrs.', train$name2)

#* "Capt", "Col", "Major", "Dr", "Rev", "Don",  "Sir", "the Countess", "Jonkheer"는 "Officer"로 변경
train$name2<-ifelse(train$name2 %in% c("Capt.", "Col.", "Major.", "Dr.", "Rev.", "Don.",  "Sir.", "the Countess.", "Jonkheer."), 
                    'officer.', train$name2)
table(train$name2)
#* "Mr", "Mrs", "Miss"는 그대로
#* 나머지 호칭은 "Others"
train$name2<-ifelse(train$name2 %in% c("Mr.", "Mrs.", "Miss."), train$name2, 'others')
table(train$name2)

#5. 아래와 같이 요금은 10달러 미만, 10달러에서 20달러 사이, 20달러에서 30달러 이상으로 나누어 Fare2에 저장
#하고, 요약합시다.
#Fare2 Pclass    Sex  Survived
table(train$Fare)
train$Fare2<- cut(train$Fare, breaks=c(0,10,20,30,Inf))

library(dplyr)
aggregate(Survived~Fare2+Pclass+Sex, data=train,FUN=function(x){sum(x)/length(x)}) 


train$Fare2<-'30+'
train$Fare2[train$Fare<30 & train$Fare>=20]<-'20-30'
train$Fare2[train$Fare<20 & train$Fare>=10]<-'10-20'
train$Fare2[train$Fare<10]<-'<10'
head(train)
aggregate(Survived~Fare2+Pclass+Sex, data=train,FUN=function(x){sum(x)/length(x)}) 

# 캐글 제출
# 1)모두 사망/생존 #13956등
# 2)여성-생존, 남성-사망 #10000등
# 3)여성-사망 조건 추가(여성&3등실&30불이상) 7800등(0.775)
# 4) decision tree ??등 0.655 점수가 많이 낮아짐. 수식 : train$Survived ~ Pclass+Fare+Age
# 4-1)수식 : train$Survived ~ Pclass+Fare+Age 3200등 (0.7799)
# 4-2)수식 : 


test<-read.csv("test.csv")
test$Survived<-0
test$Survived[test$Sex=='female']<-1 # 2)여성-생존, 남성-사망
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare>=30]<-0  #사망 조건 추가(여성&3등실&30불이상)

mysubmit<-data.frame(PassengerId=test$PassengerId, Survived=test$Survived)
write.csv(mysubmit, file="submit.csv", row.names = FALSE) 


#의사결정트리 : 다양한 변수들에 대한 최적의 생존/사망 규칙(조건)을 찾아주는 알고리즘
#의사결정트리(스무고개):나무 한 그루로는 성능 보장을 못함 
#=> 랜덤 포레스트(forest) : 나무가 여러그루 모여서 의사 결정(다수결)
#현재 가장 많이 쓰이는 머신러닝알고리즘 : random forest, svm, nn


# 
# 애기(머신)가 우리가 가르쳐준 것을 배우게 됨(러닝) => 머신러닝
# 
# 겨울가족 사진(의사결정트리 알고리즘)
# 1. 사람이 있어야 함
# 2. 겨울(눈)사진
# 3. 사람이 여러명(가족)
# feature : 카툰? 겨울? 2명이상?
# 
# 정보 획득량(Information Gain) : 분류 이전의 엔트로피 - 분류(3가지 속성) 이후의 엔트로피
# 정보획득량이 가장 큰 feature를 선택
# 
# 
# 겨울가족사진 분류 이전의 엔트로피(base entropy)=?
# 겨울가족사진 : p(+)= 1/8,  not 겨울가족사진 : p(-)=7/8
# base entropy= -(1/8)*log(1/8) - (7/8)*log(7/8) = 0.543
# 
# # E[+,-] = 
# # entropy= -p(+)*log(p(+)) - p(-)*log(p(-))
# 
# 
# 정보 획득량(Information Gain)= 
# 
# => E(겨울가족사진) - E(겨울가족사진, 카툰?)
# => 0.543 - 0.45 = 0.093
# 
# E(겨울가족사진, 카툰?)=
#   8개 사진중 카툰 = p(+) = 3/8, 8개 사진중 NOT 카툰 =  p(-) =5/8
# # entropy= -p(+)*log(p(+)) - p(-)*log(p(-))
# =-3/8*log(3/8) - 5/8*log(5/8) = 0.45
#   
# 
# 3개의 특성중에서 가장 정보 획득량이 큰 특성으로 나눔



install.packages("rpart")
library(rpart)

model<-rpart(train$Survived ~ Pclass+Fare+Age+Sex+SibSp+Parch+Embarked,
      data=train,
      method="class") #class(1또는0), anova(연속형 변수 예측)

plot(model)
text(model)
model

#install.packages("fancyRpartPlot")
#install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rpart.plot)
library(RColorBrewer)
#library(rattle)

prediction<-predict(model, test, type="class")
prediction

submit<-data.frame(PassengerId=test$PassengerId, Survived=prediction)
write.csv(submit, file="myfirstdtree.csv", row.names = FALSE)


#과적합(overfitting):너무 과하게 트레이닝 데이터에 대해서
# 훈련을 하게되면, 트레이닝 데이터에 대한 분류는 잘 하지만
# 테스트 데이터에 대한 분류는 잘 못하는 경우.
#=> 튜닝(과적합을 방지하기 위한 여러 변수 설정)이 필요함

#iris 데이터에 대해 decition tree
iris

#7:3의 비율
set.seed(20220311)
data<-sample(nrow(iris), size=nrow(iris)*0.7)
#150*0.7=105
iris_train<-iris[data,]
iris_test<-iris[-data,]

head(iris_train)
iris_model<-rpart(Species ~ . ,
             data=iris_train,
             method="class") #class(1또는0), anova(연속형 변수 예측)
iris_model

plot(iris_model)
text(iris_model)

dev.new()
par(mfrow=c(1,3))
plot(iris_model)
text(iris_model, cex=1)
plot(iris_model, compress = T)
prp(iris_model, type=4, extra=2, digits=3)

ylabel<-iris_test$Species #테스트 데이터의 정답(45건)
prediction<-predict(iris_model, newdata = iris_test, type='class')
sum(ylabel==prediction) / nrow(iris_test)*100 # 93.3%

#Sepal.Length=4.7, Sepal.Width=3.8, Petal.Length=1.9, Petal.Width=0.3
#이 붓꽃은 종이 뭘까?
new<-data.frame(Sepal.Length=4.7, 
           Sepal.Width=3.8, 
           Petal.Length=1.9, 
           Petal.Width=0.3)
predict(iris_model, newdata = new, type='class')


#모델 튜닝:모델링시 속성값을 최적화
#모델 과적합 -> 줄이기 위함 -> 테스트 데이터의 정확도 향상
iris_model
iris_model$control


iris_model<-rpart(Species ~ . ,
                  data=iris_train,
                  control = rpart.control(minbucket=2), #default=7
                  method="class") #class(1또는0), anova(연속형 변수 예측)

dev.new()
par(mfrow=c(1,3))
plot(iris_model)
text(iris_model, cex=1)
plot(iris_model, compress = T)
prp(iris_model, type=4, extra=2, digits=3)


# 연습문제
#1. minbucket값을 1~10까지 변경해 가면서 모델생성 -> 모델 평가 -> 가장 정확도가 
# 높은 모델에 대한 minbucket과 그때의 정확도를 출력

#2. maxdepth를 얼마로 했을때 테스트 데이터에 대해서도 정확도가 높은지 조사

#3. 타이타닉 모델 -> 튜닝 -> 등수를 최대한 올려보세요!(어려움)
#-> 최대 점수/등수를 카페에 자랑해주세요(화면 캡쳐, 코드)

#4. 호칭 정보 -> 모델링시 포함 -> 모델













