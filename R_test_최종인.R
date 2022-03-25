#1번 문제
data(cars)
cars
model<-lm(dist~speed, cars)
summary(model)
#회귀식은 dist=3.9324*speed+(-17.5791)
speed<-c(10,14,27)
predict(model, data.frame(speed))
#속도가 10일 경우 제동거리는 21.74499
#속도가 14일 경우 제동거리는 37.47463
#속도가 27일 경우 제동거리는 88.59594

#2번 문제
library(dplyr)
library(ggplot2)
train<-read.csv("movies_train_ansi.txt")
test<-read.csv("movies_test_ansi.txt")
submission<-read.csv("submission_ansi.txt")
str(train)
str(test)
str(submission)
factor(train$genre)

mean(train$box_off_num)
#평균 관객수는 708181.8

#상위 10개 배급사별 작품수 비교시각화
dis<-train%>%
  group_by(train$distributor)%>%
  count(train$distributor)%>%
  arrange(desc(n))%>%
  head(10)
dis<-as.data.frame(dis)
dis
#CJ,롯데,... 순으로 배급사 확인
ggplot(dis, aes(x=`train$distributor`, y = n)) + geom_col()+xlab("배급사") + ylab("작품수")+coord_flip()+ggtitle("배급사별 작품수")

#장르별 작품수 비교시각화
gen<-as.data.frame(table(train$genre))
gen
#드라마가 가장 작품수가 많은 것을 확인
ggplot(gen, aes(x=Var1, y=Freq, fill=Var1)) + geom_col()+xlab("장르") + ylab("작품수")+coord_flip()+ggtitle("장르별 작품수")

#상영등급별 비교시각화
rat<-as.data.frame(table(train$screening_rat))
rat
#청소년 관람불가,15세,12세,전체 순서로 많은 것을을 확인
ggplot(rat, aes(x=Var1, y=Freq, fill=Var1)) + geom_col()+xlab("상영등급") + ylab("작품수")+coord_flip()+ggtitle("등급별 작품수")

#최고 흥행작품 10개 분석
bestmv<-train%>%
  arrange(train$box_off_num)%>%
  tail(10)
bestmv
bestmv$title
#최고 흥행작품 10개
#국제시장, 도둑들, 7번방의 선물, 암살, 광해, 변호인, 설국열차, 관상, 해적, 수상한 그녀
plot(table(bestmv$genre))
#드라마가 10개 중 4작품으로 가장 많은 장르
drama<-train%>%
  filter(train$genre=='드라마')%>%
  arrange(box_off_num)%>%
  tail(10)
drama
plot(table(bestmv$screening_rat))
#12,15세 관람가밖에 없으며, 15세관람가가 8작품임을 확인
table(bestmv$director)
#최동훈 감독이 10작품 중 2작품의 감독임
choidir<-train%>%
  filter(train$director=='최동훈')
choidir
#최동훈 감독은 2작품만 제작했지만 둘 다 흥행작품
#결론
#최동훈 감독이 15세 관람가의 드라마 작품을 제작한다면 큰 흥행을 기대할 수 있음


#3번 문제
gennum<-train%>%
  group_by(genre)%>%
  summarise(box_num=mean(box_off_num))
gennum
#장르별 평균 관객수 획득
for(i in 1:243){
  for(j in 1:12){
    ifelse(test[i,3]==gennum[j,1], submission[i,2]<-gennum[j,2], submission[i,2]<-submission[i,2])
  }
}
#장르별 평균 관객수 입력
for(i in 1:243){
  ifelse(test[i,8]>0, submission[i,2]<-test[i,8], submission[i,2]<-submission[i,2])
}
#감독별 전작 평균 관객수 입력
submission$box_off_num<-as.integer(submission$box_off_num)
#관객수 정수화
submission
