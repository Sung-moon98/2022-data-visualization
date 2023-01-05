install.packages('tidyverse')
install.packages("igraph")
install.packages("treemap")
install.packages("aplpack")
library('aplpack')
library('treemap')
library('igraph')
library("tidyverse")

data1 <- read.csv('도로교통공단_시도_시군구별_월별_교통사고(2018).csv')
data2 <- read.csv('서울_주요구별_병원현황.csv')

fam_1 <- c('삼촌', '큰이모', '엄마', '막내이모', '나', '동생', '사촌동생')
fam_2 <- c('외할머니', '외할머니', '외할머니', '외할머니', '엄마', '엄마', '막내이모' )

df1 <- data.frame(fam_2, fam_1)

g <- graph.data.frame(df1)

plot(g,layout=layout.fruchterman.reingold,vertex.size=20,edge.arrow.size=0.5,
     vertex.label.cex = 1, edge.color='magenta', vertex.color='green')
title(main='외가 가족관계도')


data2 %>% view()

dta_1 <- data2 %>% select(강남구, 강동구, 강서구)
name01 <- data2$표시과목
stars(dta_1, draw.segments = TRUE, labels = name01, flip.labels=TRUE, frame.plot=TRUE, 
      main = "강남구, 강동구, 강서구 stars")

dta_2 <- data2 %>% select(관악구, 도봉구, 구로구)
dta_2
stars(dta_2, draw.segments = FALSE, labels = name01, frame.plot=TRUE, main = "관악구, 도봉구, 구로구 stars")


df2 <- data1 %>% group_by(시도) %>% summarise(mean1 = mean(부상자수))
mean2 <- round(df2$mean1, 0)
label01 <- paste(df2$시도,"\n",mean2)

df3 <- data.frame(df2, mean2, label01)

treemap(df3, vSize="mean2", index=c("label01"))


dta_3 <- data1 %>% group_by(시도) %>% 
  summarise_each(funs(mean), 발생건수, 사망자수, 부상자수, 중상, 경상, 부상신고)

name02 <- dta_3$시도
dta_4 <- dta_3 %>% select(발생건수, 사망자수, 부상자수, 중상, 경상, 부상신고)


faces(dta_4, face.type = 2, labels = name02, main = "시도별 교통사고 현황 체르노프 얼굴")
arrows(5,-50,0,-10)
text(5, -55, '얼굴 높이: 발생건수')
text(5, -75, '얼굴 넓이: 사망자수')
text(5, -95, '얼굴 구조: 부상자수')
text(5, -115, '입 높이: 중상')
text(5, -135, '입 넓이: 경상')
text(5, -155, '미소: 부상신고')






