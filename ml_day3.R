library(caret)
library(class)
set.seed(20220315)
train<-read.csv("train.csv")
test<-read.csv("test.csv")

train_label<-train$Survived
test_label<-NULL

test_label

train$Sex<-factor(train$Sex)
test$Sex<-factor(test$Sex)
train$Pclass<-factor(train$Pclass)
test$Pclass<-factor(test$Pclass)

levels(train$Sex)<-c(1,2) 
levels(test$Sex)<-c(1,2)  
levels(train$Pclass)<-c(1,2,3)
levels(test$Pclass)<-c(1,2,3)
train$Sex
train$Pclass

train_x<-train[c("Pclass","Sex")]
test_x<-test[c("Pclass","Sex")]

pred<-knn(train=train_x,
          test=test_x,
          cl=train_label,
          k=20)

submit<-data.frame(PassengerId=test$PassengerId,Survived=pred)
write.csv(submit,file="knn_titanic.csv",row.names = FALSE)
#0.78229
# Age
# Fare
# Embarked
# k값 최적화

colnames(train)
#sqrt(56961) #237

# 추천 시스템 알고리즘 종류
# 1)연관규칙(장바구니분석): 거래데이터부터 함께 구매가 발생한 규칙 추출 -> 특정 상품 구매시 연관성 있는 상품 추천 => 매출 극대화
# ex)넷플릭스-시청 영화(2/3 추천), 아마존(50%이상 추천), 월마트...
# 2)협업필터링(Collaborative Filtering) : 네이버/다음 검색 시스템, 고객a가 구입하지 않은 상품을 고객 a와 가장 상관관계가
# 큰 고객 b가 구입 완료한 상품중에 추천
# - 컨텐츠 기반 추천시스템 : 과거에 구매한 상품들의 속성과 유사한 상품중에서 아직 구매되지 않은 상품을 추천
# 3) 순차분석 : 고객의 시간의 흐름에 따른 구매 패턴을 추출 -> 고객이 특정 상품 구매시 일정 시간후 적정한 시각에 상품 추천

# 연관규칙
# 규칙(rules) : if 조건문 then 결과 if(A->B)
# 연관규칙(association rule)   : 특정 사건이(맥주 구매) 발생했을때 발생하는 또 다른 사건(안주 구매)의 규칙
# 항목집합(item set) : 전체 항목 집합으로부터 가능한 모든 부분 집합
# ex)전체항목집합={오징어, 땅콩, 담배, 커피, 맥주, 기저귀}, 부분집합의 개수?2의6승
#                    0      0     0     0      0      0
#                    1      1     1     1      1      1
# 연관규칙의 예
# {기저귀} -> {맥주} : 기저귀 구매 고객은 맥주도 같이 구매한다
# {기저귀, 땅콩, 맥주} -> {오징어, 커피}
# #{맥주} ->{} 연관규칙이 아님
# {맥주, 남성, 금요일} -> {기저귀}
# {여성, 금요일, 비, 막걸리} -> {밀가루, 식용유, 파, 달걀,...}
# 
# 연관규칙 사용 분야 : 유통업, 의료업(DNA 패턴, 증상->질병 등), 등 다양한 분야
# 순차분석 : 의료비 허위 청구 순서 패턴 등

# 지지도(support): 전체 거래 중에서 두 항목집합 X와Y를 모두 포함하는 거래 건수의 비율 = X,Y가 모두 포함된 거래수 /전체 거래수
# 지지도 범위 : 0<=지지도<=1
# -> 빈도가 높은 항목 집합 검색(연관규칙 포함)
# -> 빈도가 낮은 항목 집합 검색(연관규칙 제외)
# #전체항목집합={계란,우유,기저귀,맥주,사과,콜라}=2의6승=>64개의 항목부분집합
# 
# # 고객ID 거래ID             아이템
# # 1       A01       계란,우유
# # 2       A03       계란,기저귀,맥주,사과
# # 3       A05       우유,기저귀,맥주,콜라
# # 4       A08       계란,우유,맥주,기저귀
# # 5       A09       계란,우유,맥주,콜라
# 
# X={계란,맥주}, Y={기저귀}
# 연관규칙 X->Y의 지지도(support)?   s(X->Y) =  X를 구매한 사람은 동시에 Y도 구매한다는 규칙의 지지도
# => 2/5 = 0.4
# 
# 
# # 신뢰도(confidence) : X 항목집합을 포함하고 있는 거래중에서 Y 항목집합도 포함하고 있는 거래 비율
# # = X와Y가 모두 포함된 거래수 / X가 포함된 거래수
# # 신뢰도 범위 : 0<=신뢰도<=1
# # # 고객ID 거래ID             아이템
# # # 1       A01       계란,우유
# # # 2       A03       계란,기저귀,맥주,사과
# # # 3       A05       우유,기저귀,맥주,콜라
# # # 4       A08       계란,우유,맥주,기저귀
# # # 5       A09       계란,우유,맥주,콜라
# # 
# # X={계란,맥주}, Y={기저귀}
# # 연관규칙 X->Y의 신뢰도(confidence)? c(X->Y) 
# # = X와Y가 모두 포함된 거래수 / X가 포함된 거래수
# # => 2 / 3 = 0.6667 => 계란과 맥주를 구매한 사람은 기저귀도 구매한다 라는 연관규칙은 66.67%만큼 신뢰할만 하다
# 
# 
# 향상도(lift) : X 항목집합이 주어지지 않은 상황에서 Y 항목집합의 확률 대비 //
# X 항목집합이 주어진 상황에서 Y 항목집합의 확률의 증가 비율 
# 
# lift(X->Y) = c(X->Y) / s(Y)
# 향상도 = X가 포함된 거래 중에서 Y도 포함된 거래비율 / 전체 거래중 Y가 포함된 거래비율
# 기준값 : 1
# 향상도가 기준값 1보다 크다면 우연한 거래보다 우수하며, 1보다 작다면 우연한 거래보다 더 적다
# 향상도가 1이면 x,y는 서로 독립이다
# 
# 
# # 고객ID 거래ID             아이템
# # 1       A01       계란,우유
# # 2       A03       계란,기저귀,맥주,사과
# # 3       A05       우유,기저귀,맥주,콜라
# # 4       A08       계란,우유,맥주,기저귀
# # 5       A09       계란,우유,맥주,콜라
# # X={계란,맥주}, Y={기저귀}
# lift(X->Y)= c(X->Y) / s(Y)
#           = (2/3) / (3/5) = 0.666/0.6 = 1.1111...
# 계란과맥주를 구매한 고객(3명)중에 2명이 기저귀를 구매함  / 마트에 온 모든 고객(5명)중에 3명이 기저귀를 구매함
# 분자 > 분모 => 1보다 크다(일반 고객이 기저귀를 구매한 비율에 비해 '계란과 맥주'를 구매한 고객이 기저귀 구매 비율이 높더라)
# 분자 < 분모 => 1보다 작다
# 
# 
# lift({계란,맥주}->{기저귀})? 일반 고객들이 기저귀를 구매한 비율에 비하여,
# 계란과 맥주를 모두 구매한 고객들이 기저귀도 구매한 비율을 조사. 값이 1보다 크다면
# 계란과 맥주를 모두 구매한 고객들이 기저귀를 구매할 확율이 일반 고객에 비해 높다라고 볼 수 있음.
# 따라서, 계란과 맥주와 함께 기저귀를 배치하면 매출액 상승을 기대해볼 수 있음
# 
# ex)
# x={담배}, y={껌}
# 질문 : 담배를 사는 사람들 중에 껌을 사는 사람의 비율이, 
# 일반적인 손님들(흡연자/비흡연자 모두 포함됨) 중에 껌을 사는 사람의 비율보다 클까?
#   
#   
# 우리 마트에서 묶음 상품을 개발 -> 매출 극대화
# (밀가루, ??) -> 일반적인 고객의 밀가루 구매 대비, 특정 상품(??) 구매시 밀가루 구매 비율이 더 높은지? 1보다 크다면 묶음 상품 후보가 됨
# -> lift값이 가장 큰 ?? 상품을 묶음 상품화
# (채소, ??)
# # 1000명의 고객중 100명이 채소를 구매함 -> s(채소)=100/1000=0.1
# # 식용유를 구매한 50명의 고객중 25명이 채소를 구매함 -> c(식용유 -> 채소)= 25/50=0.5
# # lift(식용유->채소)= 0.5/0.1 = 5(일반 고객 대비 식용유 구매 고객의 채소 구매 확율이 5배 더 높다)
# 
# lift(모든상품->채소) 구하여 가장 높은 향상도에 해당되는 상품들을 조사. 묶음 상품화
#   


#groceries<-read.csv("groceries.csv") #4건씩 읽어들이므로 사용 불가

install.packages("arules")
library(arules)
groceries<-read.transactions("groceries.csv",sep=",")
groceries
summary(groceries)

print(9875*169) #169개 종류의 상품, 9875건의 거래가 발생
inspect(groceries)
inspect(groceries[10:20])

itemFrequency(groceries[,1:10])
itemFrequency(groceries)
sum(itemFrequency(groceries)) #각 항목별 빈도수

#support 값을 기준으로 시각화
itemFrequencyPlot(groceries, support=0.1)
itemFrequencyPlot(groceries, topN=30)

itemFrequency(groceries, type='absolute') #도수

itemFrequencyPlot(groceries, topN=30, type='absolute')


groceryRules<-apriori(groceries, parameter = list(support=0.005, confidence=0.2, minlen= 2))


groceryRules #872 규칙이 발견됨

inspect(groceryRules)

#lhs(left-hand side)
#'{cake bar}  => {whole milk} :cake bar 구매자가 whole milk를 구매한다' 는 연관규칙의
#support=0.005592272, confidence=0.4230769, lift=1.6557746, count=55건의 거래에서 규칙이 발견됨

#lift 열 기준 정렬

inspect(   sort(groceryRules, by="lift")      )
# [1]   {citrus fruit, other vegetables, whole milk}          => {root vegetables}       0.005795628 0.4453125  0.013014743 4.085493
# [2]   {butter, other vegetables}                            => {whipped/sour cream}    0.005795628 0.2893401  0.020030503 4.036397
# [3]   {herbs}                                               => {root vegetables}       0.007015760 0.4312500  0.016268429 3.956477
# [4]   {citrus fruit, pip fruit}                             => {tropical fruit}        0.005592272 0.4044118  0.013828165 3.854060
# [5]   {berries}                                             => {whipped/sour cream}    0.009049314 0.2721713  0.033248602 3.796886

inspect(   sort(groceryRules, by="lift")[1:10]   )

#berries
groceryRules

berryrules<-subset(groceryRules, items %in% "berries")
inspect(berryrules)

write(berryrules, sep=",", file="berryrules.csv", row.names=FALSE, quote=FALSE)

df<-as(berryrules, "data.frame")
df

help(Epub)
data(Epub)
Epub

#주석을 달아서 설명을 해주세요
#다양한 규칙 발견
#특정 도서에 대한 규칙
#시각화


























