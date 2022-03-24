sms_raw<-read.csv("smsdata_ansi.txt", stringsAsFactors = FALSE)
head(sms_raw)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

library(tm)
sms_raw$text
sms_corpus<-VCorpus(VectorSource(sms_raw$text) ) #문자열 벡터 자료를 바탕으로 코퍼스(말뭉치) 구축

print(sms_corpus)
#문서의(각각의 이메일 제목) 개수 5559개
#inspect함수 :코퍼스를 구성하는 문서(document)를 확인
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[[1]], as.character)
lapply(sms_corpus[[1]]$content, as.character)
lapply(sms_corpus[1:3], as.character)

sms_corpus
#문자열 -> 코퍼스 구축 -> 코퍼스에 저장된 문자열 전처리(공백, 특수문자, 숫자 등 제거 또는 변환) -> (DTM 변환) -> 시각화/분석
# DTM(Document Term Matrix) : 행-문서, 열-단어, 요소값-단어 등장 횟수
# TDM(TERM Document Matrix)



#모든 문자열에 대해 소문자 변환
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
lapply(sms_corpus_clean[1:5], as.character)

#숫자, 불용어, 특수문자 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) #숫자 제거
lapply(sms_corpus_clean[1:5], as.character)

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #특수문자자 제거
lapply(sms_corpus_clean[1:5], as.character)

stopwords()
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) #불용어 제거
lapply(sms_corpus_clean[1:5], as.character)

# stemming(어간 추출), lemmatization(표제어 추출)
# 어간추출 ex) fishing, fished, fisher => 어간추출 : fish
# 표제어추출 ex) are, am, is => be

install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)
lapply(sms_corpus_clean[1:5], as.character)

sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace) #여러 개의 공백문자 -> 1개의 공백문자

lapply(sms_corpus_clean[1:5], as.character)


#DTM
texts<-c("hello world world", "hello python")
texts
corpus<-Corpus(VectorSource(texts))
corpus

dtm<-DocumentTermMatrix(corpus)
dtm
inspect(dtm)

tdm<-TermDocumentMatrix(corpus)
tdm
inspect(tdm)


#sms_corpus_clean 데이터로부터 DTM 생성
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
inspect(sms_dtm)


#5559개 문서 
#베이지안필터기(이메일분류기)
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

sms_corpus_clean
install.packages("wordcloud")
library(wordcloud)

#install.packages("RColorBrewer")
library(RColorBrewer) #전체 파레트 로드
display.brewer.all() #전체 파레트트
pal<-brewer.pal(8, "Dark2") #Dark2 파레트에서 8가지 색상을 모두 사용
#pal<-brewer.pal(9, "Pastel1") #Pastel1 파레트에서 9가지 색상을 모두 사용

wordcloud(sms_corpus_clean, min.freq = 50, random.order = F, colors=pal, random.color = T, max.words=30) #최소 50번 이상 등장한 단어

wordcloud(sms_corpus_clean, min.freq = 50, random.order = F, colors=pal, random.color = T, max.words=30) #최소 50번 이상 등장한 단어


sms_raw

spam<-subset(sms_raw, type=='spam')
ham<-subset(sms_raw, type=='ham')

#spam, ham에서 자주 사용된 단어를 워드클라우드로 그리세요
spam_corpus <- Corpus(VectorSource(spam))
wordcloud(spam_corpus, min.freq = 30, random.order = F, colors = pal)

ham_corpus <- Corpus(VectorSource(ham))
wordcloud(ham_corpus, min.freq = 50, random.order = F, colors = pal)
#wordcloud(spam, min.freq = 50, random.order = F, colors=pal, random.color = T) #최소 50번 이상 등장한 단어

wordcloud(spam$text, max.words = 40, scale=c(5,1), colors = pal) #빈도가 가장 큰 단어와 작은 단어의 폰트 크기
wordcloud(ham$text, max.words = 40, scale=c(5,1), colors = pal) #빈도가 가장 큰 단어와 작은 단어의 폰트 크기

sms_freq_words<-findFreqTerms(sms_dtm_train, 5) #최소 5번 이상 등장한 단어 검색
sms_dtm_train

#sparse 는 0(등장하지 않은 단어) => sparse term을 제거해서 필요한 것만 남겨두고 처리
inspect(sms_dtm_train)
#Sparsity : 100%, 32453개 단어가 non sparse value

#Sparsity 설정 전
# <<DocumentTermMatrix (documents: 4169, terms: 6906)>>
# Non-/sparse entries: 32453/28758661
# Sparsity           : 100%

removeSparseTerms(sms_dtm_train, 0.9)# Sparsity를 설정하여 빈도수가 낮은 단어를 제거
# Sparsity값이 0.9보다 작은 단어가 딱 1개 존재함
# removeSparseTerms는 1개의 단어만 남기고 모두 제거

sms_dtm_freq_train<-removeSparseTerms(sms_dtm_train, 0.999) #6906 -> 1115개 단어
sms_dtm_freq_train

sms_dtm_train

# library(dplyr)
# sms_raw %>% 
# filter(type=="spam")
#   
# sms_raw[sms_raw$type=="spam",]
  
sms_freq_words #최소 5번이상 등장, 1151개 단어

sms_dtm_freq_train<-sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[ , sms_freq_words]

convert_counts<-function(x){
  x<-ifelse(x>0, 'Yes', 'No')
}

#0보다 크면 Yes, 아니면 No
sms_train<-apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test<-apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

str(sms_train)
inspect(sms_dtm_freq_train)

#단어의 등장 유무가 필요함. 빈도수는 필요치 않음
#install.packages("e1071")
library(e1071)

sms_classifier<-naiveBayes(sms_train, sms_train_labels)
sms_test_pred<-predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels)



sms_classifier2<-naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2<-predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels)


data(cars)
head(cars)
cars
#speed:속도(독립), dist:제동거리(종속)
plot(cars)
#선형적인 패턴 확인...
scatter.smooth(cars) #추세선

#극단치에 따라 회귀분석 결과가 왜곡될 수 있음
par(mfrow=c(1,2))
boxplot(cars$speed)
boxplot(cars$dist)

#독립변수와 종속변수가 정규분포를 따를 때 선형회귀분석 결과가 좋음
par(mfrow=c(1,2))
plot(density(cars$speed)) #밀도 그래프
plot(density(cars$dist)) #밀도 그래프

#회귀분석
#lm함수, 종속변수~독립변수
model<-lm(dist ~ speed, cars)
summary(model)

dist= w * x + b
dist = 3.9324 * speed + (-17.5791)


#Pr(>|t|) : p-value
#유의수준 5%(0.05)보다 작으면(p<0.05)면 통계적으로 유의미하다.
# speed가 증가할 때 기대되는 dist의 변화는 유의수준 5%에서
# 통계적으로 유의미하다

# Multiple R-squared : 0.6511
# 모델 적합도(설명도), 
# dist = 3.9324 * speed + (-17.5791) 모델은
# dist의 분산을 speed가 65%를 설명한다


#다중공선성
#공선성?하나의 독립변수가 다른 하나의 독립변수로 잘 예측되는 경우, 즉
#독립변수 간에 서로 상관이 높은 경우

#다중공선성?하나의 독립변수가 다른 여러 개의 독립변수로 잘 예측되는 경우

# (다중)공선성이 있다면?
# 모델을 만들게 되면 예측 결과가 잘 맞지 않고, 불안정해짐. 데이터가 약간만
# 바뀌어도 결과가 크게 달라짐. 회귀계수가 통계적으로 유의미하지 않게 나올 수 있음

# ex) 음주(독립변수, x), 학업성취도(종속변수, y)
# 일평균음주량(x1), 혈중알콜농도(x2), 학업성취도(종속변수, y) -> 다중공선성 문제
# x1,x2 독립변수
# y 종속변수
# # y=w1*x1 + w2*x2 + b
# 
# 일평균음주량(x1), 혈중알콜농도(x2) 변수사이에는 강력한 상관관계가 존재
# # y=w1*x1 + w2*x2 + b
# 
# 학생=w1*수학선생님1+w2+수학선생님2+b
#  
# 학생=w3*수학선생님3+b
#   
#   
# 따릉이  
# ... 외부온도 체감온도   자전거대여수(종속)
#       -3        -5           1
#       20        18           30
#   
#           온도
#           -4
          # 19

df<-read.csv("crab.csv")
df

#일반적으로 분산팽창계수(VIF)가 10(또는 5)보다 크면 다중공선성이 있다고 판단함

model=lm(y~sat+weight+width, df)
summary(model)
# 유의수준 5%에서 sat, width는 통계적으로 유의미하다.
# weight는 유의미하지 않다

install.packages("car")
library(car)
vif(model)
#weight와 width가 서로 상관이 높아서 vif가 높게 나옴 (다중공선성)

#예를 들어 weight 유의미한데, vif가 높게 나오면? 별도의 처리 없이 모델 만들면 됨
#예를 들어 weight 유의미하지 않고, vif가 높게 나오면? 처리를 해야 함
#처리- 변수를 추가하거나, 기존 변수를 빼거나
#(외부온도, 체감온도 => 온도), (-외부온도 or -체감온도) 

model=lm(y~sat+weight, df)
summary(model)

predict(model, data.frame(sat=7, weight=3.5))
# 
# 1~1000번까지 데이터 -> 회귀 모델( strength ~ .)
# 1001~1031번까지 데이터 -> 데이터프레임 -> 모델 입력 -> 예측 결과 출력
# 실제 결과와 비교했을때 어느정도 차이가 나는지 출력

























