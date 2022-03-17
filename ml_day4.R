# lift(담배->껌) => 2
# 0.5(담배를 구매한 20명중 10명 껌 구매)  = c(담배->껌)
# ------------------------------------------------------------
# 0.01(100명중 1명이 껌구매) = s(껌)
# =50

help(Epub)#전자책과 관련된 데이터셋
library(arules)
data(Epub) #Epub 불러오기
Epub

#Epub데이터셋 파악
summary(Epub)
#15729 rows : 거래횟수
#936 columns : 책 종류
inspect(Epub)

itemFrequency(Epub)
itemFrequencyPlot(Epub, topN=10)

######################################################################
library(arules)
data(Epub) #Epub 불러오기
Epub
inspect(Epub[1:10])
itemFrequency(Epub[ ,1:10]) #거래품목별 구매 비율(support)
itemFrequencyPlot(Epub, support=0.01, main='item freq plot')
itemFrequencyPlot(Epub, topN=30, main='item freq plot')

image(sample(Epub, 500, replace=FALSE))

Epub_rule_2<-apriori(Epub, list(support=0.001, confidence=0.2, minlen=2))
#minlen=lhs+rhs
#{lhs} => {rhs}
summary(Epub_rule_2)
#65개 룰, lhs+rhs=2인 규칙의 개수가 62, lhs+rjs=3인 규칙이 3개
#{빵}->{우유},  {빵,우유}->{물} or {우유}->{빵,물}
#lhs->rhs              lhs+rhs=3


inspect(Epub_rule_2[1:20])

inspect(sort(Epub_rule_2[1:20],by='lift')[1:20])
inspect(sort(Epub_rule_2[1:20],by='support')[1:20])

#subset함수 : 특정 아이템과 연관된 규칙을 발견

# "doc_72f" 도서가 포함된 규칙 검색
rule_interest<-subset(Epub_rule_2, items %in% "doc_72f")
inspect(rule_interest)

# "doc_72f  or doc_4ac" 도서가 포함된 규칙 검색
rule_interest<-subset(Epub_rule_2, items %in% c("doc_72f", "doc_4ac"))
inspect(rule_interest)

#연관규칙의 lhs 또는 rhs를 기준으로 도서 검색
#"doc_72f  or doc_4ac" 도서가 lhs/rhs에 포함된 규칙 검색
rule_interest<-subset(Epub_rule_2, rhs %in% c("doc_72f", "doc_4ac"))
inspect(rule_interest)

#"60e"라는 아이템명이 포함된 모든 도서에 대한 규칙 검색
rule_interest<-subset(Epub_rule_2, items %pin% c("60e"))
inspect(rule_interest)

#규칙의 lhs/rhs에 doc_6e8과 doc_6e9를 모두 가지고 있는 규칙 검색

rule_interest<-subset(Epub_rule_2, lhs %ain% c("doc_6e8", "doc_6e9"))
inspect(rule_interest)

#신뢰도가 0.25보다 큰 모든 규칙 검색
rule_interest<-subset(Epub_rule_2, confidence>0.25)
inspect(rule_interest)

#신뢰도가 0.25보다 크면서 lhs/rhs에 '60e'라는 문자열을 포함하는 모든 규칙 검색
rule_interest<-subset(Epub_rule_2, confidence>0.25 & items %pin% c("60e"))
inspect(rule_interest)


#####################################################################################
#문자열->베이지안필터기

raw_moon<-readLines("speech_moon.txt", encoding = 'UTF-8')
head(raw_moon)

install.packages("stringr")
library(stringr)

txt="r 프로그래밍은!! 약간 쉽다. python은 어렵다!#$%"

#문자열 변경
#한글을 제외한 문자 제거
txt<-str_replace_all(txt, pattern ="[^가-힣]", replacement =" ")
txt

raw_moon
str_replace_all(raw_moon, pattern ="[^가-힣]", replacement =" ")
library(dplyr)

moon<-raw_moon %>% 
  str_replace_all("[^가-힣]", " ")
head(moon)

str_squish(txt)

moon<-moon %>% 
  str_squish()
moon

str(moon)
class(moon)
#vector -> tibble, dataframe...
as_tibble(moon)
as.data.frame(moon)

moon<-as_tibble(moon)
moon

#지금까지의 작업은 아래 코드로 표현 가능
moon<-raw_moon %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble()
  
moon


str(iris)
as_tibble(iris)

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text

#문장(공백, 글자) 단위로 문자열 데이터를 분리
install.packages("tidytext")
library(tidytext)

text %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token='sentences')

text %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token='words')

text %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token='characters')

word_space<-moon %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token='words')

word_space #2025개 단어(중복 단어 포함)

word_space %>% 
  count(word) #한글 문자 순서대로 정렬, 1440개 단어(중복 불포함)

word_space<-word_space %>% 
  count(word, sort=T) #내림차순 정렬
word_space

str_count("배") #1글자
str_count("바나나") #3글자

#2글자 이상만 추출
word_space<-word_space %>% 
  filter(str_count(word)>1)

word_space %>% 
  count(word, sort=T) %>% 
  filter(str_count(word)>1)

fruit <- c("apple", "banana", "pear", "pineapple")
str_count(fruit, "a")
str_count(fruit, "p")
str_count(fruit, "e")
str_count(fruit, c("a", "b", "p", "p"))





word_space<-moon %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token='words')
word_space

word_space<-word_space %>% 
  count(word, sort=T) %>% 
  filter(str_count(word)>1)

word_space

top20<-word_space %>% 
  head(20)

library(ggplot2)
#EDA + ARULES
top20
ggplot(top20, aes(x=reorder(word,n), y=n))+ #단어 빈도순
  geom_col()+
  coord_flip()+ #회전
  geom_text(aes(label=n), hjust=-0.7)+ #막대 외부 빈도 표시
  labs(title="문재인 대통령 출마 연설문 단어 빈도", x=NULL, y=NULL)+
  theme(title=element_text(size=12))

#워드클라우드

install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label = word, size = n, col=n)) + #빈도에 따라 색상 지정
  geom_text_wordcloud(seed = 1234)+
  scale_radius(limits=c(3,NA), #최소(3), 최대 단어 빈도
               range=c(3,30))+ #최소, 최대 글자 크기
  scale_color_gradient(low="#66aaff",
                       high="#004EAA")+
  theme_minimal()


install.packages("showtext")
library(showtext)
font_add_google(name="Nanum Gothic", family="nanumgothic") #폰트 추가
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col=n)) + #빈도에 따라 색상 지정
  geom_text_wordcloud(seed = 1234, family="nanumgothic")+
  scale_radius(limits=c(3,NA), #최소(3), 최대 단어 빈도
               range=c(3,30))+ #최소, 최대 글자 크기
  scale_color_gradient(low="#66aaff",
                       high="#004EAA")+
  theme_minimal()


font_add_google(name="Gamja Flower", family="gamjaflower") #폰트 추가
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col=n)) + #빈도에 따라 색상 지정
  geom_text_wordcloud(seed = 1234, family="gamjaflower")+
  scale_radius(limits=c(3,NA), #최소(3), 최대 단어 빈도
               range=c(3,30))+ #최소, 최대 글자 크기
  scale_color_gradient(low="#66aaff",
                       high="#004EAA")+
  labs(title="문재인 대통령 연설문", x=NULL, y=NULL)+
  theme(title=element_text(size=15),
        text=element_text(family="gamjaflower"))

#KoNLP:한국어처리 패키지
install.packages("multilinguer")
library(multilinguer)
install_jdk() #java 설치치

install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
#Checking user defined dictionary! 메시지가 나오면 설치 성공

extractNoun("멀티캠퍼스 빅데이터 분석 반입니다.")
useNIADic()

library(dplyr)

text <- tibble(
  value = c("대한민국은 민주공화국이다.",
            "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))
text
  
extractNoun(text$value)

library(tidytext)
text


text <- tibble(
  value = c("대한민국은 민주공화국이다.",
            "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))

text %>% 
  unnest_tokens(input=value, #text변수에 저장된 문자열이 value임
                output=word, #출력 변수
                token=extractNoun)


raw_moon<-readLines("speech_moon.txt", encoding='UTF-8')
raw_moon

library(stringr)

moon<-raw_moon %>% 
  str_replace_all("[^가-힣]", " ") %>%  #한글만
  str_squish() %>%  #중복 공백 제거
  as_tibble() #티블로 변환

moon

#명사 기준 분리
word_noun<-moon %>% 
  unnest_tokens(input=value,
                output=word,
                token=extractNoun)
word_noun
#형태소 분석기는 완벽하지 않음. 잘못 분류된 명사 단어도 상당수 보임

word_noun<-word_noun %>% 
  count(word, sort=T) %>%  #단어 빈도수로 내림차순 정렬
  filter(str_count(word)>1) #두 글자 이상만 추출
word_noun

top20<-word_noun %>% 
  head(20)

library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA),
               range = c(3, 15)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()







