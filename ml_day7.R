library(stringr)
R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

str_extract(R_wiki, "software environment") #벡터
str_extract_all(R_wiki, "software environment") #리스트
str_extract_all(R_wiki, "software environment", simplify = TRUE) #행렬

#str_extract_all(R_wiki, "[A-Z]+") #정규식
#[A-Z]+ : 대문자로 시작하는 경우에 매칭 
#str_extract_all(R_wiki, "^[A-Z]") #정규식 Hello
#str_extract_all(R_wiki, "[A-Z][a-z]+")
#str_extract_all(R_wiki,"[A-Z]+[a-z]+")
#str_extract_all(R_wiki,"[A-Z][a-z]*")

#첫 글자가 대문자로 시작하는 단어를 검색
#help -> regular expression 검색
str_extract_all(R_wiki,"[A-Z][A-Za-z]*")
myextract<-str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}") #[[:upper:]]=대문자
table(myextract)

#str_extract 함수와 동일
str_match(R_wiki, "software environment") #행렬
str_match_all(R_wiki, "software environment") #행렬

university_address <- c("연세대학교 주소는 서울시 서대문구 연세로 50번지다",
                        "서울대 주소: 서울시 관악구 관악로 1번지다",
                        "고려대는 서울시 성북구 안암로 145번지에 있다",
                        "카이스트 주소, 대전시 유성구 대학로 291번지",
                        "포항시 남구 청암로 77번지는 포항공과 대학교 주소임")

university_address
str_match(university_address, "[[:alpha:]]{1,}시 [[:alpha:]]{1,}구")

R_wiki
str_locate(R_wiki, "software environment")
str_locate_all(R_wiki, "software environment")

#첫글자가 대문자로 시작되는 단어들의 위치를 출력
str_locate_all(R_wiki, "[A-Z]+[a-z]*")
str_locate_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
str_locate_all(R_wiki, "[[:upper:]]{1,}")
str_locate_all(R_wiki,"[A-Z][A-Za-z]*")


mylocate<-str_locate_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
class(mylocate[[1]])

dim(mylocate[[1]])

mydata<-data.frame(mylocate[[1]])

mydata$myword.length<-mydata$end - mydata$start+1
mydata

myextract<-str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
myextract[[1]]
mydata$myword<-myextract[[1]]

mydata


R_wiki
R_wiki_para<-str_split(R_wiki, "\n") #문단으로 분리

R_wiki_para  #각각의 문단을 문장으로 분리
# [[1]] -> [1],[2]
# [[1]] : 리스트의 1번째 요소
# [1],[2] : 리스트의 1번째 요소에 저장된 길이가 2인 문자열 벡터

R_wiki_para[[1]]
#문자열 벡터에 대해 문자열 처리
R_wiki_sent<-str_split(R_wiki_para[[1]],"\\. ") #마침표+공백 있는 경우 문장의 끝으로 간주
R_wiki_sent


my2sentences<-unlist(R_wiki_sent)[c(4,7)] #list->vector

#각 문장의 단어수를 세어보자
mylength1<-length(unlist(str_split(my2sentences[1]," ")))
mylength2<-length(unlist(str_split(my2sentences[2]," ")))
mylength1;mylength2 #5, 13

my2sentences
str_split_fixed(my2sentences, " ", 5) #2행 5열의 행렬
str_split_fixed(my2sentences, " ", 13) #2행 13열의 행렬

length.sentences<-rep(NA, length(unlist(R_wiki_sent)))

str_split(unlist(R_wiki_sent)," ")

length(str_split(unlist(R_wiki_sent)," ")[[1]])
length(str_split(unlist(R_wiki_sent)," ")[[2]])
length(str_split(unlist(R_wiki_sent)," ")[[7]])

for (i in 1:length(unlist(R_wiki_sent))){
  length.sentences[i]<-length(str_split(unlist(R_wiki_sent)," ")[[i]])
}
max(length.sentences)


#특정 표현으로 시작되는 텍스트 추출(벡터의 위치)
R_wiki_sent[[2]]

grep("C", R_wiki_sent[[2]]) #동일한 표현 str_which(R_wiki_sent[[2]], "C")

R_wiki_sent[[1]]
#A or An or The 표현이 있는 문자열 검색
str_starts(R_wiki_sent[[1]], "A|An|The")

#R이라는 표현이 등장하는지 확인
R_wiki
str_count(R_wiki, "R") #9번 등장

#문자열 치환
#software environment -> software_environment
temp<-str_replace_all(R_wiki,"software environment", "software_environment")
temp
str_extract_all(temp, "software_environment|software|environment")


str_dup("soft", 5) #soft가 5번 반복된 결과가 1개의 문자열
rep("soft",5) #soft가 5번 반복되어 길이가 5인 벡터

mytext <- c("software environment","software  environment","software\tenvironment")
mytext

str_split(mytext, " ")


sapply(str_split(mytext, " "), length) #단어수
sapply(str_split(mytext, " "), str_length) #문자수

mytext <- "The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"
mytext

myword<-unlist(str_extract_all(mytext, boundary("word")))
myword
myword<-str_replace(myword, "Trump", "Trump_unique_") #고유명사화
myword
myword<-str_replace(myword, "States", "States_unique_") #고유명사화

tolower(myword)

mytext
#숫자 제거

str_replace_all(mytext, "[[:digit:]]{1,}", "")
#[0-9]
str_replace_all(mytext, "[[:digit:]]{1,}[[:space:]]{1,}", "")

#tm패키지
install.packages("tm") #text mining
library(tm)

stopwords("en") #  불용어 사전, 문맥(의미) 파악하는데 도움이 안되는 단어
stopwords("SMART")# 불용어 사전

mytext <- c("I am a boy. You are a boy. The person might be a boy. Is Jane a boy?")
mytext

#어근 동일화(am, are, was, is -> be로 통일)
#한국, 코리아, 대한민국, 남한,... -> 대한민국

various_be<-function(mytextobject){
str_replace_all(mytextobject,"(a|A)m |(a|A)re |(i|I)s |(W|w)as","be ")  
}

various_be(mytext)


#텍스트 마이닝

#자연어처리(감성분석, 챗봇, 자연어생성, 해석,...)
#Corpus(코퍼스, 말뭉치, 특정분야사전) : 특정 도메인에 속하는 단어들의 집합

# ex) 상담봇(상담챗봇) : 
# *은행 상담챗봇은 은행 업무와 관련된 대화 가능
# ex) 너 어제 잘 잤어? 너 결혼했어? 너 코로나 안 걸렸니?

#코퍼스 구축
mypaper<-VCorpus(DirSource("papers")) #33개의 문서(파일)


mypaper[[1]]
mypaper[[1]]$content #문서 내용 확인
mypaper[[33]]$content #문서 내용 확인

mypaper[[1]]$meta #문서의 메타 정보


meta(mypaper[[1]], tag='author')<-'Baek'
mypaper[[1]]$meta

mypaper

#특수문자를 전후로 어떤 단어들이 있는지 확인
#알파벳/숫자+특수문자+알파벳/숫자
str_extract_all("hello123*hi3","[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")


myfunc<-function(x){
  str_extract_all(x$content, "[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
}
#myfunc("hello123*hi3")
#myfunc(mypaper)

#VCorpus 타입의 문서 각각에 대해 특정 함수를 적용할때는 lapply함수를 사용
mypuncts<-lapply(mypaper, myfunc)
table(unlist(mypuncts)) #list -> vector -> table함수 적용


mypaper #숫자를 모두 검색
lapply(mypaper, 
       FUN = function(x){str_extract_all(x$content, '[0-9]+')})

myfunc<-function(x){
  str_extract_all(x$content,"[[:digit:]]{1,}")
}
lapply(mypaper, myfunc)

#숫자를 모두 삭제
mycorpus<-tm_map(mypaper, removeNumbers)
#특수문자 모두 삭제
mycorpus<-tm_map(mycorpus, removePunctuation)
#공백 삭제
mycorpus<-tm_map(mycorpus, stripWhitespace)
#소문자로 변환
mycorpus<-tm_map(mycorpus, content_transformer(tolower))
#불용어 사전에 등록된 단어를 삭제
mycorpus<-tm_map(mycorpus, removeWords, words=stopwords("SMART"))

mycorpus[[1]]$content
mypaper[[1]]$content

library(readr)

#기생충 댓글 데이터
raw_news_comment<-read_csv("news_comment_parasite.csv")
raw_news_comment

library(dplyr)
library(textclean)
#한글을 제외한 나머지 문자를 공백문자로 대체
news_comment<-raw_news_comment %>% 
  select(reply) %>% 
  mutate(reply=str_replace_all(reply,"[^가-힣]"," "), 
         reply=str_squish(reply),
         id=row_number())

news_comment #4150개 댓글

news_comment %>% #3899개 댓글
  distinct(reply) #중복 제외한 reply열만 출력

news_comment<-news_comment %>% #3899개 댓글
  distinct(reply, .keep_all = T) #중복 제외한 모든 열 출력

#댓글이 3 단어 이상으로 구성된 것만 추출, 3329개 댓글
news_comment %>% 
  filter(str_count(reply, boundary('word'))>2)

str_count("정말 우리 집에 좋은 일이") #14글자
str_count("정말 우리 집에 좋은 일이", boundary('word')) #5단어

library(tidytext)
library(KoNLP)
#명사 추출
comment<-news_comment %>% #32,677 -> 23,494
  unnest_tokens(input=reply,
                output=word,
                token=extractNoun,
                drop=F) %>% 
  filter(str_count(word)>1) %>% 
  #댓글 내 중복 단어 제거
  group_by(id) %>%  #3,758개 그룹(댓글)
  distinct(word, .keep_all = T) %>%  #중복단어 제거
  #select(id,word) #22,166 x 2, Groups:   id [3,758]
  ungroup() %>% #그룹 해제
  select(id,word)
  

comment


count_word<-comment %>% 
  add_count(word) %>%  #각 단어의 개수를 세어서 n이라는 열의 값으로 설정
  filter(n<=200) %>% 
  select(-n) #n열을 제외하고 추출

#count_word에는 200번 이하로 언급된 단어가 저장

count_word %>% 
  count(word, sort=T) %>% #6,171개 단어 출력
  print(n=100) #지정한 개수만큼 출력
  
#불용어 변수
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼")

#18,345단어
count_word<-count_word %>% 
  filter(!word %in% stopword) %>%  #18,345단어 -> 17,729 단어
  mutate(word=recode(word,
                     "자랑스럽습니" = "자랑",
                     "자랑스럽" = "자랑",
                     "자한" = "자유한국당",
                     "문재" = "문재인",
                     "한국의" = "한국",
                     "그네" = "박근혜",
                     "추카" = "축하",
                     "정경" = "정경심",
                     "방탄" = "방탄소년단")) 





count_word #17,729 단어


stopword <- tibble(word = c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
                            "해요", "이것", "니들", "하기", "하지", "한거", "해주",
                            "그것", "어디", "여기", "까지", "이거", "하신", "만큼"))

stopword
#불용어 사전 저장
write_csv(stopword, "stopword.csv")
#불러오기
stopword<-read_csv( "stopword.csv")
stopword

#댓글 단위 단어의 빈도수
count_word_doc<-count_word %>% 
  count(id,word, sort=T)
  
  





#범주형 변수
bloodtype<-c('A','A','B','AB','O')
#도수분포표
ft<-table(bloodtype)
#히스토그램
barplot(ft)


#연속형 변수
x<-c(1,1,1,2,3,5,5,7,8,9)
#데이터 구간 분리
h<-hist(x)
#구간 경계 확인
h$breaks
#[1]  0<  <=2<  <=4<  <=6<  <=8< <=10

h<-hist(x, breaks = c(1,3,5,7,9))
h$counts


x = c(100, 100, 200, 400, 500, 5000)
mean(x)
median(x)

ft<-table(x)
ft

which.max(ft) #최대값의 위치

ft[which.max(ft)] #100이 2번 등장(가장 많이)

x <- c(1, 1, 2, 3, 3, 3, 4, 5, 5, 7)
#범위
max(x)- min(x)

#분산:각 데이터에서 평균을 뺀 다음, 그것을 제곱하여 평균을 구함.
#평균으로부터 각 데이터가 얼마만큼 떨어져 있는지를 수치화한 값
#분산이 작다 -> 데이터가 평균 주변
var(x)
mean(x)

#표준편차 = 분산의 제곱근
sd(x)
sqrt(var(x))

#사분위간 범위(IQR):3사분위수-1사분위수
quantile(x)
IQR(x) #Q3-Q1 = 4.75-2.25 =2.5


#수
#상관분석 -> 베이즈이론 -> 베이지안필터기

#목
#회귀분석 -> 회귀모델


R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

#
# 1.정규식 이용하여 한 글자(대문자)로 구성된 단어 검색
# - 각 단어의 빈도수 조사 -> 시각화
# 
# 2. software 단어가 등장한 횟수를 출력
# 
# 3. 단어를 구성하는 글자수가 4 이상인 단어들만 추출
# 
# 4. R이라는 단어가 등장한 후 is 단어가 등장하는 빈도수 출력
# 
# 5. so로 시작하는 모든 문자열 추출












