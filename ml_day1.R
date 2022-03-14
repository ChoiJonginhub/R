# library(rpart)
# set.seed(20220311)
# data<-sample(nrow(iris),size = nrow(iris)*0.7)
# iris_train<-iris[data,]
# iris_test<-iris[-data,]
# 
# new<-data.frame(Sepal.Length=4.7, 
#                 Sepal.Width=3.8, 
#                 Petal.Length=1.9, 
#                 Petal.Width=0.3)
# 
# ylabel<-iris_test$Species
# 
# 
# for(i in seq(1,10)){
#   iris_model<-rpart(Species~.,
#                     data=iris_train, 
#                     control = rpart.control(minbucket = i),
#                     method="class")
#   prediction<-predict(iris_model,newdata=iris_test,type="class")
#   print(sum(ylabel==prediction)/nrow(iris_test)*100)
#   
# }
# 
# for(i in c(1:20)){
#   iris_model<-rpart(Species~.,
#                     data=iris_train, 
#                     control = rpart.control(maxdepth = i),
#                     method="class")
#   prediction<-predict(iris_model,newdata=iris_test,type="class")
#   print(sum(ylabel==prediction)/nrow(iris_test)*100)
#   
# }

# feature engineering(특성공학:기존 데이터로부터 가공처리를 하여 의미있는 정보를 추출) 알고리즘
# decision tree(오버피팅) -> random forest 알고리즘



train<-read.csv("train.csv")
test<-read.csv("test.csv")

train


library(rpart)
library(rpart.plot)

train$Name[1]


# traing data와 test data가 동일 (Survived 열 유/무 차이)
# train->model->test->예측/평가
# Name -> 호칭 추출 -> train$Name2 열 추가 -> age, sex, fare,...,name2 -> 모델

str(train) #12개 열
str(test) #11개 열
test$Survived<-NA
combi<-rbind(train,test)
str(combi)

strsplit(combi$Name[1], split="[,.]") #리스트:다양한 자료구조들이 저장될 수 있는 자료구조
strsplit(combi$Name[1], split="[,.]")[[1]]
strsplit(combi$Name[1], split="[,.]")[[1]][2]

#strsplit(combi$Name, split="[,.]")[[1]][2] 1번째 데이터에 대해서만 적용
combi$Title<-sapply(combi$Name, FUN=function(x){strsplit(x, split="[,.]")[[1]][2]})
combi$Title
combi$Title<-sub(" ","",combi$Title)
combi$Title

# table(combi$Title)
# table(combi[1:891,]$Title)
# table(combi[892:1309,]$Title)
#모델은 트레이닝 데이터로 만들어 짐. 따라서, 테스트 데이터를 모델에 입력해서
#예측할 때, 테스트 범주형 데이터에 있는 종류는 모두 트레이닝 범주형 데이터에도 있어야 함

#Title 빈도수가 작은 것들을 그루핑

combi$Title[combi$Title %in% c('Mme', 'Mlle')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major','Sir')]<-'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess','Jonkheer')]<-'Lady'

table(combi$Title) #11개 종류
combi$Title<-factor(combi$Title)
table(combi$Title)
str(combi$Title)


str(combi)
#패밀리사이즈가 생존 여부를 판단하는데 유의미한 정보를 제공할것이라고 가정(혼자보다는 여럿이 있을때 생존율이 높지 않을까?)
combi$FamilySize<-combi$SibSp+combi$Parch+1 

#combi$Name 에서 'Surname'을 추출하시오
combi$Surname<-sapply(combi$Name, FUN=function(x){strsplit(x, split="[,.]")[[1]][1]})


combi$FamilyID<-paste(as.character(combi$FamilySize),combi$Surname,sep="")
# paste("hi","hello",sep="*")
# paste("hi","hello",sep="")


combi$FamilyID[combi$FamilySize<=2]<-'Small' #2명 이하는 모두 small, 나머지는 그대로

table(combi$FamilyID)

class(table(combi$FamilyID))
famIDs<-data.frame(table(combi$FamilyID))
famIDs

famIDs<-famIDs[famIDs$Freq<=2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1]<-'Small'#빈도수가 2 이하는 모두 small, 나머지는 그대로

table(combi$FamilyID)

combi$FamilyID<-factor(combi$FamilyID)

##############################################################################################
str(combi) #16개

train<-combi[1:891,]
test<-combi[892:1309,]
dim(combi)

model<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID,
      data=train, method="class")
Prediction<-predict(model, test, type='class')

Prediction
submit<-data.frame(PassengerId=test$PassengerId, Survived=Prediction)
write.csv(submit, file="fe_submission.csv", row.names = FALSE)


#########################랜덤 포레스트##########################
#RF, SVM, 딥러닝
# 1. 의사결정트리를 여러개 생성 -> 다수결로 판단(voting)
# 2. 의사결정트리를 어떻게 생성? 배깅(bagging)방식
# 배깅?
  #기존 방식 : data가 1000개 -> 의사결정트리(정보획득량) 1개 -> 예측
  #랜덤포레스트:data가 1000개 -> 의사결정트리 11개 구성 -> 랜덤하게 data 1000개 중에서
  #100개씩 샘플링(중복허용) -> 각각의 트리를 생성함
  #트리를 만들때 의사결정트리 알고리즘과는 달리, 모든 속성을 사용하지 않음.
  #각각의 트리마다 사용되는 속성이 다름
  
  #전체 속성에서 일부 속성만을 사용(랜덤하게)하여 트리를 작성
#   ex)원 데이터셋 : 1000건의 데이터, 20개 속성
# 의사결정트리 : 1000개 데이터 모두 사용, 20개 속성 모두 사용하여 정보획득량을 계산.
# 정보획득량이 가장 큰 속성부터 사용하여 트리를 만들어 감
# 
# 랜덤포레스트 : 트리를 11개(홀수) 생성.
# 각 트리는 
# 1000개 데이터에서 일부(100개)를 랜덤하게 추출, 20개 속성에서 일부(5개)를 랜덤하게 추출(11번 반복)
# 
# 속성의 개수는 몇개가 좋을까? sqrt(전체속성의개수), sqrt(20) -> 약 4.xxxx => 4~5개 속성

#0.77990

###################################################################################

rm(list=ls())

#랜덤포레스트 구현
#{party} cForest(범주형의 경우 32가지 이상 지원 가능)
#{randomForest} randomForest(범주형의 경우 최대 32가지 종류까지 지원)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

test$Survived<-NA
#train데이터+test데이터 => feature engineering => 분리 => train->모델->test->결과

combi<-rbind(train,test)
str(combi)

combi$Title<-sapply(combi$Name, FUN=function(x){strsplit(x, split="[,.]")[[1]][2]})
combi$Title<-sub(" ","",combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major','Sir')]<-'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess','Jonkheer')]<-'Lady'

combi$Title<-factor(combi$Title)

combi$FamilySize<-combi$SibSp+combi$Parch+1 
combi$Surname<-sapply(combi$Name, FUN=function(x){strsplit(x, split="[,.]")[[1]][1]})
combi$FamilyID<-paste(as.character(combi$FamilySize),combi$Surname,sep="")

combi$FamilyID[combi$FamilySize<=2]<-'Small' #2명 이하는 모두 small, 나머지는 그대로

famIDs<-data.frame(table(combi$FamilyID))
famIDs<-famIDs[famIDs$Freq<=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1]<-'Small'#빈도수가 2 이하는 모두 small, 나머지는 그대로
table(combi$FamilyID)
combi$FamilyID<-factor(combi$FamilyID)
combi$Sex<-factor(combi$Sex)


head(combi)
summary(combi)

#NA처리 - 나이 예측?
# -동일한 특성 그룹의 평균값으로 대체
# -의사결정트리를 만들어서 나이 예측(method='anova')
Agefit<-rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamilySize, 
      data=combi[!is.na(combi$Age),], 
      method="anova")

combi$Age[is.na(combi$Age)]<-predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

summary(combi$Embarked)
#NA 조사
table(is.na(combi$Embarked))
sum(is.na(combi$Embarked))
table(combi$Embarked)

which(combi$Embarked=="")  # 62 830
combi$Embarked[c(62,830)]<-"S"
table(combi$Embarked)
str(combi)
combi$Embarked<-factor(combi$Embarked)

summary(combi$Fare)
which(is.na(combi$Fare)) #1044
combi$Fare[1044]<-median(combi$Fare, na.rm=TRUE)

length(table(combi$FamilyID)) #종류가 너무 많음(61개)
#랜덤포레스트(randomforest)에서 범주형 데이터의 종류를 최대 32개 미만해주는게 좋음(cforest는 가능)
combi$FamilyID2<-combi$FamilyID

combi$FamilyID2<-as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<=3]<-'Small'
combi$FamilyID2<-factor(combi$FamilyID2)
table(combi$FamilyID2) #22가지 종류

train<-combi[1:891,]
test<-combi[892:1309,]

# 랜덤포레스트 알고리즘 구현/개선
# 예측 -> 정확도 70%

set.seed(20220314)

install.packages("randomForest")
library(randomForest)
install.packages("party")
library(party)

# model<-randomForest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID2, 
#              data=train, 
#              importance=TRUE,
#              mtry=3,
#              ntree=2001)
# #ntree:의사결정트리 개수
# 
# Prediction<-predict(model, test)
# submit<-data.frame(PassengerId=test$PassengerId, Survived=Prediction)
# write.csv(submit, file="rf_titanic.csv", row.names = FALSE) #0.76

set.seed(20220314)
model2<-cforest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID, 
                    data=train, 
                    controls = cforest_unbiased(mtry=5,ntree=2001)) #mtry=5 속성



Prediction<-predict(model2, newdata=test, OOB=TRUE, type="response") #out of bag
Prediction

submit<-data.frame(PassengerId=test$PassengerId, Survived=Prediction)
write.csv(submit, file="rf_titanic2.csv", row.names = FALSE) #0.76



#iris data decision tree / randomforest
#7:3의 비율(set.seed(12345))
#mtry=2 or 3 , ntree=2001

#knn 알고리즘























