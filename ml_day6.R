library(dplyr)
rain <- read.csv("train.csv")

train

train %>% 
  group_by(User_ID) %>% 
  summarise(n=n())

table(train$User_ID)

table(train$Product_ID)


#인기상품 5개#
best5 <- train %>% 
  group_by(Product_ID) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(5)
best5

user <- train %>% 
  distinct(User_ID, .keep_all = T)

nrow(user)
#5891명

user %>% 
  group_by(Gender) %>% 
  summarise(n=n())



user

user_age <- user %>% 
  group_by(Age,Gender) %>% 
  summarise(n=n())

user_age

user %>% 
  group_by(City_Category) %>% 
  summarise(n=n())


library(ggplot2)
ggplot(user_age,aes(x=Age,y=n,fill=Gender))+
  geom_col()

best5_gender <- train %>%
  filter(Product_ID %in% c("P00265242","P00025442","P00110742","P00112142","P00057642")) %>% 
  group_by(Gender,Product_ID) %>% 
  select(Product_ID,Gender,Age) %>% 
  summarise(n=n()) %>% 
  arrange(Product_ID)

best5_gender



best5

# 여자 인기상품 5개
train %>% 
  filter(Gender=='F') %>% 
  group_by(Product_ID) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(5)

# 남자 인기상품 5개
train %>% 
  filter(Gender=='M') %>% 
  group_by(Product_ID) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(5)

best5_gender %>% 
  mutate(ratio=ifelse(Gender=='F',n*100/1666,n*100/4225))

best5_age <- train %>%
  filter(Product_ID %in% c("P00265242","P00025442","P00110742","P00112142","P00057642")) %>% 
  group_by(Age,Product_ID) %>% 
  select(Product_ID,Age) %>% 
  summarise(n=n()) %>% 
  arrange(-n)


best5_age



#C도시에서 가장 많이 사는 물품
train %>% 
  filter(City_Category=="C") %>% 
  group_by(Product_ID) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(5)



#나이별 상품 거래수

train %>% 
  group_by(Age) %>% 
  summarise(n=n())

#마트를 제일 많이 사용하는 유저
train %>% 
  group_by(User_ID) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

#1001680이 제일 많이 이용함

#도시별 거래수

train %>% 
  group_by(City_Category) %>% 
  summarise(n=n())

#B도시에서 거래가 가장 많음 ( 사람은 C가 가장 많은데 비해)

#B도시 사람들은 어떤 사람이 많은가?

train %>% 
  filter(City_Category=='B') %>% 
  distinct(User_ID, .keep_all = T) %>% 
  group_by(Age,Gender) %>% 
  summarise(n=n())

#18~45세 까지가 많이 살고있음


###################################################################

#결측값 조사
#Gender 결측치 없음
table(is.na(train$Gender))
table(train$Gender)

summary(train)
#User_ID가 중복인 것은 한 사람이 여러번 구매한 이력이라고 보고 진행했습니다. 

gender_frame <- as.data.frame(prop.table(table(train$Gender)))
ggplot(gender_frame, aes(x=Var1, y = Freq, fill=Var1)) + geom_col()+xlab("성별") + ylab("비율")+coord_flip()+ggtitle("데이터 성별 인원 비율")
#남성의 자료가 더 많음을 알 수 있습니다.


train %>% 
  group_by(Gender) %>% 
  count(Age)


#Occupation 이상치나 결측치 없음
table(is.na(train$Occupation))
table(train$Occupation)
Occupation_frame <- as.data.frame(prop.table(table(train$Occupation)))
Occupation_frame
ggplot(Occupation_frame, aes(x=Var1, y = Freq, fill=Var1)) + geom_col()+xlab("직업") + ylab("비율")+coord_flip()+ggtitle("데이터 직업별 인원 비율")+coord_polar()




target<-train %>% 
  select(Gender,Age,City_Category,Marital_Status, Product_ID, Purchase ) %>% 
  filter(Gender=="M" & Age=="26-35" & Marital_Status=="0")

target_Product_ID<-target %>% 
  group_by(Product_ID) %>% 
  count(Product_ID) %>% 
  arrange(desc(n)) %>% 
  head(5)

target
target_Product_ID

###############################################################################
dataset<-read.csv("train.csv")
summary(dataset)
head(dataset)

#EDA
#1) 쇼핑객 성별 식별(5891명)
dataset_gender=dataset %>% 
  select(User_ID, Gender) %>% 
  group_by(User_ID) %>% 
  distinct()
head(dataset_gender)

summary(dataset_gender$Gender)

#2) 고객별 구매금액
total_purchase_user<-dataset %>% 
  select(User_ID, Gender, Purchase) %>% 
  group_by(User_ID) %>% 
  arrange(User_ID) %>% 
  summarise(Total_Purchase=sum(Purchase))

total_purchase_user

user_gender<-dataset %>% 
  select(User_ID, Gender, Purchase) %>% 
  group_by(User_ID) %>% 
  arrange(User_ID) %>% 
  distinct()
head(user_gender)

total_purchase_user
user_gender

# inner_join():교집합
# full_join():합집합
# left_join():왼쪽 데이터프레임 기준으로 합침
# right_join():오른쪽 데이터프레임 기준으로 합침

test1 <- data.frame(name=c('서현','민아','유진','다미'), math = c(75, 85,90,70),stringsAsFactors=FALSE)
test2 <- data.frame(name = c('규리','서현','민아','유진','민경'),english = c(80,90,95,70,85),stringsAsFactors=FALSE)

test1
test2

inner_join(test1,test2,by='name')
full_join(test1,test2,by='name')
left_join(test1,test2,by='name')
right_join(test1,test2,by='name')


user_purchase_gender<-full_join(total_purchase_user, user_gender, by='User_ID')
inner_join(total_purchase_user, user_gender, by='User_ID')
left_join(total_purchase_user, user_gender, by='User_ID')
right_join(total_purchase_user, user_gender, by='User_ID')

user_purchase_gender

#3)성별에 따른 구매금액 차이
average_spending_gender<-user_purchase_gender %>% 
  group_by(Gender) %>% 
  summarise(g_purchase=sum(Purchase), Count=n(), Average=g_purchase/Count)


#시각화
ggplot(data=average_spending_gender, aes(x=Gender, y=Average,fill=Gender))+
  geom_col()

ggplot(data=average_spending_gender,
       aes(x=Gender, y=Average, fill=Gender))+
  geom_bar(stat='identity')


#4)가장 많이 구매한 상품
top_sellers<-dataset %>% 
  count(Product_ID, sort=TRUE)

top_5<-head(top_sellers,5)

top_5

#5)가장 많이 구매된 P00265242  상품에  대해 좀 더 알아보자
best_seller<-dataset[dataset$Product_ID=='P00265242',]
head(best_seller)

ggplot(data=best_seller, 
       aes(x=Gender, fill=Gender))+
  geom_bar()

#동일한 결과
ggplot(data=best_seller)+
  geom_bar(mapping = aes(x=Gender, y=..count.., fill=Gender))

#연령대 별 고객수
customers_age<-dataset %>% 
  select(User_ID, Age) %>% 
  distinct() %>% 
  count(Age)
customers_age


#도시별 고객수
customers_location<-dataset %>% 
  select(User_ID, City_Category) %>% 
  distinct()

customers_location

ggplot(data=customers_location, 
       aes(x=City_Category, fill=City_Category))+
  geom_bar()

ggplot(data=customers_location)+
  geom_bar(mapping = aes(x=City_Category, y=..count.., fill=City_Category))

#도시별 구매 금액 -> 어느 도시 고객이 구매를 많이 했을까?
Purchases_city<-dataset %>% 
  group_by(City_Category) %>% 
  summarise(Purchases=sum(Purchase))
Purchases_city

#도시별 구매 금액이 크므로 1000$ 단위로 환산
Purchases_city_1000<-Purchases_city %>% 
  mutate(purchasesThousands=Purchases/1000)

#각 고객별 구매 횟수
customers<-dataset %>% 
  group_by(User_ID) %>% 
  count(User_ID)

customers_City<-dataset %>% 
  select(User_ID, City_Category) %>% 
  group_by(User_ID) %>% 
  distinct() %>% 
  ungroup() #그룹 해제

customers_City

customers

left_join(customers, customers_City, by='User_ID')

#베스트셀러 고객이 어느 도시에 가장 많이 있는가?  c도시
best_seller %>% 
  select(User_ID, City_Category) %>% 
  distinct() %>% 
  count(City_Category)

##############################################################################
#감성 사전 불러오기

# dic<-read.csv("knu_sentiment_lexicon.csv", encoding = 'UTF-8', fileEncoding = 'CP949')
# dic
# 인코딩 에러 발생

library(readr)
dic<-read_csv("knu_sentiment_lexicon.csv")
dic
#약 15000여개의 단어, 감정 분류
dic %>% 
  filter(polarity>=1) %>%  #긍정 4,871 단어
  arrange(word)

dic %>% 
  filter(polarity<=-1) %>%  #부정 9,829 단어
  arrange(word)

dic %>% 
  filter(polarity==0) %>%  #중립 154 단어
  arrange(word)

# 감성분석기(문장 -> 감성분석기 -> 긍/부정)
# 감성사전 기반 감성분석기 제작
# 문장(문서,문단) -> 전처리 -> 단어 분리 -> 사전에서 단어를 검색 -> polarity 값을 합산 => 긍/중/부정 판단

dic %>% 
  filter(word %in% c("좋은", "나쁜"))

dic %>% 
  filter(word %in% c("기쁜", "슬픈"))

dic %>% 
  filter(word %in% c("행복하다", "좌절하다"))

#이모티콘 문자만 추출하세요
library(stringr)

dic %>% 
  filter(grepl("[가-힣]", word)) #14,777
dic %>% 
  filter(grepl("[^가-힣]", word))

dic %>% 
  filter(!str_detect(word,"[가-힣]")) %>% 
  arrange(word)

dic %>% 
  mutate(sentiment=ifelse(polarity>=1, "pos", 
                   ifelse(polarity<=-1, "neg","nu"))) %>%  #긍정:pos, 부정:neg, 중립:neu
  count(sentiment)

sentence<-c("디자인 예쁘고 마감도 좋아서 만족스럽다.",
  "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다.")

df<-tibble(sentence) #vector -> tibble
library(tidytext)
df
df<-df %>% 
  unnest_tokens(input=sentence,
                output=word,
                token="words",
                drop=F) #본래 있던 sentence 열이 삭제 안됨
df

dic

df<-df %>% left_join(dic, by="word") %>% 
  mutate(polarity=ifelse(is.na(polarity),0, polarity))
df  

#sentence(2가지)별로 polarity 합계 출력

df %>% 
  group_by(sentence) %>% 
  summarise(score=sum(polarity))

sum(df[1:5,]$polarity)
sum(df[6:12,]$polarity)

####################################################################
#영화 기생충 코멘트 감성 분석
raw_news_comment<-read_csv("news_comment_parasite.csv")
raw_news_comment

#문자열 전처리 : tm_map, textclean 패키지들이 많이 사용
install.packages("textclean")
library(textclean)

news_comment<-raw_news_comment %>% 
  mutate(id=row_number(),
         reply=str_squish(reply))


# str_trim("  String with trailing and leading white space\t")
# str_trim("\n\nString with trailing and leading white space\n\n")
# 
# str_squish("  String with trailing,  middle, and leading white space\t")
# str_squish("\n\nString with excess,  trailing and leading white   space\n\n")

str(news_comment)
glimpse(news_comment) #level 표시 안됨

word_comment<-news_comment %>% #공백 기준으로 단어 분리
  unnest_tokens(input=reply, output=word, token='words', drop=F)

word_comment %>% 
  select(reply, word)


word_comment<-word_comment %>% 
  left_join(dic, by="word") %>% 
  mutate(polarity=ifelse(is.na(polarity), 0, polarity))
  
word_comment %>% 
  select(word, polarity)


word_comment<-word_comment %>%  #2->pos, -2->neg, else-> neu
  mutate(sentiment=ifelse(polarity==2, "pos", 
         ifelse(polarity==-2, "neg", "neu")))

word_comment

word_comment %>% 
  count(sentiment) #부정:285, 긍정:762, 중립:나머지

top10_sentiment<-word_comment %>%  #중립 제외하고 추출
  filter(sentiment!='neu') %>%  #1047건 있음
  count(sentiment,word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=10)


top10_sentiment

ggplot(top10_sentiment, aes(x=reorder(word,n), y=n, fill=sentiment))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)
word_comment

score_comment<-word_comment %>%  #38,512
  group_by(id,reply) %>%  #4149개 그룹
  summarise(score=sum(polarity)) %>%  #각 댓글에 대한 감성 점수의 총합=> 긍/부정
  ungroup()

score_comment %>% 
  select(score, reply) %>% 
  arrange(score)

score_comment %>% 
  select(score, reply) %>% 
  arrange(-score)

score_comment %>% 
  count(score) #각 댓글별 score

score_comment<-score_comment %>% 
  mutate(sentiment=ifelse(score>=1, "pos",
                          ifelse(score<=-1,"neg","neu")))
score_comment

score_comment %>% 
  count(sentiment) %>% 
  mutate(ratio=n/sum(n)*100)


df <- tibble(country = c("Korea", "Korea", "Japen", "Japen"),  # 축
             sex = c("M", "F", "M", "F"),                     # 누적 막대
             ratio = c(60, 40, 30, 70))  
df

ggplot(df, aes(x=country, y=ratio, fill=sex))+geom_col()


ggplot(df, aes(x=country, y=ratio, fill=sex))+geom_col()+
  geom_text(aes(label=paste0(ratio,"%")),
            position = position_stack(vjust=0.5)
            )


comment<-score_comment %>% 
  unnest_tokens(input=reply,
                output=word,
                token='words',
                drop=F) #38,512건

comment<-comment %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word)>=2) #35,933건 ->33,445 건

#감정 및 단어별 빈도수
frequency_word<-comment %>% 
  count(sentiment , word, sort=T)

#긍정 댓글 빈도수가 높은 단어
frequency_word %>% 
  filter(sentiment=="pos")

frequency_word %>% 
  filter(sentiment=="neg")

library(tidyr)

comment_wide<-frequency_word %>% 
  filter(sentiment!='neu') %>%  #중립 제외
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0)) #누락값은 0으로
  
comment_wide


#로그(log) 오즈(odds)비(ratio)=승산비, 교차비
# odds : 확률에서 실패에 비해 성공할 확률의 비, odds=p/(1-p)
# ex) 이길 확률 = 1/5, 질 확률 = 4/5 
# 게임에서 이길 odds비는 1/4이며, 5번 중에 4번 질 동안 1번 이긴다는 의미

comment_wide %>% 
  mutate(odds_ratio=   ((pos+1) / (sum(pos+1)))       /   ((neg+1) / (sum(neg+1)))    ,
         log_odds_ratio=log(odds_ratio))

comment_wide %>% 
  mutate(sum_neg=sum(neg+1), ratio_neg=(neg+1)/sum_neg,
         sum_pos=sum(pos+1), ratio_pos=(pos+1)/sum_pos,
         odds_ratio=ratio_pos/ratio_neg,
         log_odds_ratio=log(odds_ratio))

dic %in% c("쩐다","핵노잼") #둘 다 없는 단어
dic

#dic에 신조어 등록
newword<-tibble(word=c("쩐다","핵노잼"),
       polarity=c(2,-2))
newword
dic

newword_dic<-bind_rows(dic,newword)
newword_dic

#새 사전으로 감정 분석
#토큰화, 감정 및 단어별 빈도





