library('tidyverse')


data1 <- read.csv("도로교통공단_시도_시군구별_월별_교통사고(2018).csv")
data2 <- read.csv("서울_주요구별_병원현황.csv")

data2

gangnam <- data2$강남구
gangdong <- data2$강동구
dobong <- data2$도봉구
name02 <- data2$표시과목    

plot(gangnam, ann = F, axes = F, ylim = c(0,400), type='o', lty=2, pch=18, col='blue')
axis(1, at=1:9, labels = name02)
axis(2, ylim=c(0,400), las=1)
title(main = '서울 병원 현황', line = 1)
lines(gangdong, type = 'o', lty=3, pch=19, col='red')
lines(dobong, type = 'o', lty=4, pch=20, col='green')
legend('topright', legend = c('강남구', '강동구', '도봉구'), lty=2:4, pch=19:20, col=c('blue', 'red', 'green'))


bpx <- barplot(gangnam, col=rainbow(12), ylab='갯수', names = name02, ylim = c(0, 400), las=1)
title(main = '강남구 병원 현황', line=1)
text(x = bpx, y=gangnam, pos = 3, labels = paste(gangnam,"개"))


m1 <- as.matrix(data2[, 2:11])
pct <- round(m1/sum(m1)*100, 2)
en <- barplot(m1, beside = T, col=rainbow(9), ylim = c(0,350), las=1)
title(main = '서울 주요구별 병원현황', line = 2)
abline(h=seq(0,350,10),lty=3,lwd=0.2)
abline(v=seq(0, 90, 10), lty=3,lwd=2, col="red")
legend('topright', name02, fill = rainbow(10))


pie(gangnam, col = heat.colors(9), paste(name02,'\n',gangnam,'개'))
title(main = '강남구 병원 현황', line = 1)


pct <- round(gangnam/sum(gangnam)*100, 2)
pie(gangnam, col = rainbow(9), paste(name02,'\n',pct,'%'))
title(main = '강남구 병원 현황', line = 1)



















