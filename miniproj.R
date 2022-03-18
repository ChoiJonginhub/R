bf<-read.csv("blackfri.csv")
library(dplyr)
#고객별 중복주문 제거
bf2<-distinct(bf, bf$User_ID, .keep_all = TRUE)
bf2
#성별별 고객수
table(bf2$Gender)
plot(table(bf2$Gender))
#여성:1666, 남성:4225
#성별에 따른 결혼여부
prop.table(table(bf2$Gender, bf2$Marital_Status))
#여성은 1.6:1.2, 남성은 4.1:2.9(비혼:기혼)
#도시별 고객수
table(bf2$City_Category)
#A:1045, B:1707, C:3139
#도시별 남녀비율
prop.table(table(bf2$Gender, bf2$City_Category))
#A는 5:12, B는 2:5, C는 7:19의 남녀비를 확인
#고객 나이 분포 그래프
table(bf2$Age)
plot(table(bf2$Age))
#26~35세 고객이 2053명으로 제일 많은 것을 확인
bf3<-bf2 %>% filter(Age=='26-35')
#26~35세 고객들의 남녀비
table(bf3$Gender)
plot(table(bf3$Gender))
#여성 545명, 남성 1508명
#26~35세 고객들의 결혼여부
table(bf3$Gender, bf3$Marital_Status)
#비혼 여성 320명, 남성 924명
#데이터를 통해 비혼 남성을 타켓으로 마케팅을 진행할 경우 유리할 것이라고 추측됨

#연관규칙
library(tidyverse)
library(scales)
library(arules)

customers_products<-bf %>%
  select(User_ID, Product_ID) %>%
  group_by(User_ID) %>%
  arrange(User_ID) %>%
  mutate(id=row_number()) %>%
  spread(User_ID, Product_ID) %>%
  t()
write.csv(customers_products, file = 'customers_products.csv')
customersProducts<-read.transactions('customers_products.csv')
