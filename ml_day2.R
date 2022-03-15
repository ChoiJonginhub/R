#party패키지 cforest함수 이용하여 랜덤포레스트
data(iris) #iris데이터 불러오기
iris

library(party)

set.seed(12345)
model<-cforest(Species~.,
        data=iris,
        controls = cforest_unbiased(mtry=2, ntree=101)
        )
model
#variable importance : Species를 분류할 때 가장 영향을 크게 끼치는 변수를 확인
#vari + imp = varimp
model_varimp<-varimp(model)
model_varimp

# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 0.026102610  0.001620162  0.338253825  0.274167417 

#Species 가장 영향을 크게 끼치는 변수 :Petal.Length > Petal.Width > Sepal.Length > Sepal.Width


#randomforest패키지 randomforest함수 이용하여 랜덤포레스트
library(randomForest)
set.seed(12345)
rfmodel<-randomForest(Species~.,
             data=iris,
             mtry=2,
             ntree=101)

rfmodel

#variable importance : Species를 분류할 때 가장 영향을 크게 끼치는 변수를 확인
rf_varimp<-importance(rfmodel)
rf_varimp
#Petal.Width > Petal.Length > Sepal.Length > Sepal.Width


################################################################################

# wbcd<-read.csv("wisc_bc_data.csv", stringsAsFactors = T)
# str(wbcd)

wbcd<-read.csv("wisc_bc_data.csv")
str(wbcd)
factor(wbcd$diagnosis)

wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
wbcd$diagnosis

head(wbcd)
wbcd<-wbcd[-1]

table(wbcd$diagnosis)
#비율로 출력

round(prop.table(table(wbcd$diagnosis))*100,digits=1)

summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])

#정규화(0~1) 또는 표준화

#normalize함수 구현(전달받은 데이터를 정규화 해주는 함수)
normalize<-function(x){
  #데이터-최소/최대-최소
   return ((x-min(x)) / (max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

head(wbcd)

wbcd[2:31] #normalize함수를 활용하여 정규화하시오.
#apply계열함수 활용: 1)연산 대상 데이터의 타입 2)결과 출력 형태 3)연산단위
#apply, lapply, sapply, tapply ...
apply(wbcd[2:31],2,normalize) #1:행, 2:열, na.rm=T 옵션 적용 가능
apply(wbcd[2:31],2,mean)
#lapply = list + apply => 실행 결과가 리스트로 리턴됨

lapply(wbcd[2:31], normalize) #정규화 결과가 리스트 타입
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))

#569건(train data, test data 분할) #1번~469번, 470번~569번
head(wbcd_n)
head(wbcd)

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#wbcd_n 데이터로 knn 모델 생성
#wbcd의 1번째 열에 라벨(레이블)

install.packages("class")
library(class)

wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)
#예측값의 정확도를 출력하시오

install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred) #교차표


prop.table(table(wbcd_test_pred==wbcd_test_labels))
print(sum(wbcd_test_pred==wbcd_test_labels)/nrow(wbcd_test)*100)

#k를 얼마로 하는게 좋을까?
#k를 1~30까지 변경해가면서 모델 정확도 출력
res=NULL
for(i in 1:30){
  pred<-knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=i)
  res[i]<-sum(pred==wbcd_test_labels)/nrow(wbcd_test)*100
  cat(i,'=',res[i],"%","\n")
}

plot(res, type='b', xlab='k-value', ylab='accuracy')

#k=5 or k=21






# 1=96
# 2=95
# ...

# for(i in 1:10){
#   print(i)
# }
# 
# for(i in 1:10){
#   cat(i)
# }
# 
# for(i in 1:10){
#   cat("\n", i, "번째 출력문")
# }
# for(i in 1:10){
#   cat("\n", paste(i, "번째 출력문"))
# }
# 
# for(i in 1:10){
#   print("\n")
# }




#confusion matrix(혼동행렬) 사용법
wbcd_test_pred
wbcd_test_labels

# install.packages("caret")
# library(caret)
# 
# confusionMatrix(wbcd_test_pred, wbcd_test_labels)
# #암분류기 => 암환자/일반환자
#       실제값
#      Y     N
# Y    TP   FP(Y로 예측했는데, 실제는 N이므로 예측이 틀림)   
# N    FN   TN
# 
# 도둑/거주자
# 
# TP(TRUE POSITIVE) : 긍정 예측이 성공. (실제)암환자(도둑)를 (예측)암환자(도둑)라고 판단
# TN(TRUE NEGATIVE) : 부정 예측이 성공. (실제)일반환자(거주자)를 (예측)일반환자(거주자)라고 판단
# FP(FALSE POSITIVE) : 긍정 예측이 실패. (실제)암환자를 (예측)일반환자라고 판단
# FN(FALSE NEGATIVE) : 부정 예측이 실패. (실제)일반환자를 (예측)암환자라고 판단
# 
# 정확도(accuracy)=(TP+TN) / (TP+TN+FP+FN)
# 암환자 예측 = (TP+FP)
# 정밀도(precision)=암환자로 예측한 것 중에 실제로도 암환자인 경우의 비율 = TP / (TP+FP)
# 실제 암환자 수 = (TP+FN)
# 재현율(recall)= 실제로 암환자인 것들 중에서 예측도 암환자로 분류된 비율 = TP / (TP+FN)
# f1 점수(F1 score)=정밀도와 재현율의 조화평균(값이 클수록 좋은 모델, 0~1) = 1/((1/precision)+(1/recall))*2
# 
# FP RATE(FPR)= FP / (FP+TN) = 실제로는 일반환자였는데, 예측이 암환자로 분류된 비율


#########################################################################################
#표준화 -> KNN 모델링
#표준화=(각데이터-평균)/표준편차
# ex)국어점수, 수학점수,   토플,  토익
#        60       60        100    200


wbcd_z<-as.data.frame(scale(wbcd[-1])) #표준화

summary(wbcd_z$area_mean)

wbcd_train<-wbcd_z[1:469, ]
wbcd_test<-wbcd_z[470:569, ]

wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)
wbcd_test_pred

CrossTable(x=wbcd_test_labels, y=wbcd_test_pred) #95%

#k=1~30까지 변경해가면서 가장 정확도가 높은 k? 시각화 코드 작성 및 채팅
res=NULL
for(i in 1:30){
  wbcd_test_pred<-knn(train=wbcd_train, 
                      test=wbcd_test, 
                      cl=wbcd_train_labels,
                      k=i)
  res[i]<-sum(wbcd_test_labels==wbcd_test_pred)/nrow(wbcd_test)*100
  cat(i,"=",res[i],"%","\n")
}


plot(res,type='b',xlab='k-value',ylab='accuracy')


#####################################################################################
#kaggle titanic-knn 적용 -> 제출(점수 최대한 올려보세요)
#credit card fraud detection 28만건 * 20% = 5만6천건
set.seed(20220315)
credit<-read.csv("creditcard.csv")
str(credit)

summary(credit$Time) #2015년도 3월? 유럽 특정 카드 결제 데이터
#class : 0=정상, 1=비정상

#랜덤하게 20% 데이터만 추출
samp<-sample(1:nrow(credit), round(nrow(credit)*0.2))
samp
credit<-credit[samp,]

nrow(credit) #56961, 전체 데이터 셋으로 함
#75% 트레이닝, 25% 테스트
index<-createDataPartition(credit$Class, p=0.75, list=F)
class(index)
train<-credit[index,]
test<-credit[-index,]

dim(train) #42721    31
dim(test) #14240    31

#train->knn모델->test->평가, k 최적화
#kaggle titanic-knn 적용 -> 제출(점수 최대한 올려보세요)


























