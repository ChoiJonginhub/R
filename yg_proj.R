library(haven)
library(dplyr)
library(ggplot2)
ygdata<-read_sav(file = 'ygdata.sav', encoding='UTF-8')

plot(table(ygdata$DM2))

#휴가기간 중 여가활동 상위 10개
holiyg<-ygdata %>%
  group_by(q14_3)%>%
  count(q14_3)%>%
  arrange(desc(n))%>%
  head(11)

#가장 만족하는 여가활동 상위 10개
favoryg<-ygdata %>%
  group_by(q4)%>%
  count(q4)%>%
  arrange(desc(n))%>%
  head(10)

table(ygdata$DM8)
table(ygdata$DM2)
table(ygdata$q49_1)

#개인소득 없는 사람들의 데이터 추출
nomoney<-ygdata%>% filter(q49_1==1)
table(nomoney$DM1)
#여성 전업주부들의 데이터가 많이 포함됨을 확인
table(nomoney$DM2)
plot(table(nomoney$q4))

plot(ygdata$DM8, ygdata$DM2)
cor.test(ygdata$DM8, ygdata$DM2)

#tv시청이 취미인 데이터 추출
favortv<-ygdata %>% filter(q4==74)
plot(table(favortv$DM8))
#대인관계가 취미인 데이터 추출
favormeet<-ygdata %>% filter(q4==86)
plot(table(favormeet$DM8))

golf<-ygdata %>% filter(q4==24)
plot(table(golf$DM8))

#가구소득이 100만원 미만 데이터 추출
under100<-ygdata %>% filter(DM8==1)
plot(table(under100$q4))
plot(table(under100$q2_6_1))
under100 %>%
  group_by(q4)%>%
  count(q4)%>%
  arrange(desc(n))%>%
  head(10)
# 그 외소득별 여가활동 분석
in100_200<-ygdata %>% filter(DM8==2)
plot(table(in100_200$q4))
plot(table(in100_200$q2_6_1))

in200_300<-ygdata %>% filter(DM8==3)
plot(table(in200_300$q4))
plot(table(in200_300$q2_6_1))

in300_400<-ygdata %>% filter(DM8==4)
plot(table(in300_400$q4))

over600<-ygdata %>% filter(DM8==7)
plot(table(over600$q4))
over600 %>%
  group_by(q4)%>%
  count(q4)%>%
  arrange(desc(n))%>%
  head(10)
#여가활동 분야별 분석
favorart<-ygdata %>% filter(q4<9)
plot(table(favorart$DM8))
plot(table(favorart$q4))

doart<-ygdata %>% filter(q4>8 & q4<16)
plot(table(doart$DM8))
plot(table(doart$q4))

exercise<-ygdata %>% filter(q4>15 & q4<38)
plot(table(exercise$DM8))
plot(table(exercise$q4))

tour<-ygdata %>% filter(q4>37 & q4<49)
plot(table(tour$DM8))
plot(table(tour$q4))

hobby<-ygdata %>% filter(q4>48 & q4<71)
plot(table(hobby$DM8))
plot(table(hobby$q4))

rest<-ygdata %>% filter(q4>71 & q4<80)
plot(table(rest$DM8))
plot(table(rest$q4))

social<-ygdata %>% filter(q4>79 & q4<88)
plot(table(social$DM8))
plot(table(social$q4))

#적절하다고 생각하는 여가비용별 분류
small<-ygdata%>%filter(q10<=50000)
plot(table(small$q4))
plot(table(small$DM2))

middle<-ygdata%>%filter(q10>50000 & q10<=100000)
plot(table(middle$DM2))
plot(table(middle$q4))

high<-ygdata%>%filter(q10>=400000)
plot(table(high$DM8))
plot(table(high$q4))
high %>%
  group_by(q4)%>%
  count(q4)%>%
  arrange(desc(n))%>%
  head(10)
#여가비용을 높게 잡는 사람들의 가장 흔한 취미는 골프
plot(table(ygdata$q33))
plot(table(ygdata$q30))
plot(table(ygdata$q28))
