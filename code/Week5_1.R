library('tidyverse')


data1 <- read.csv("도로교통공단_시도_시군구별_월별_교통사고(2018).csv")
data2 <- read.csv("서울_주요구별_병원현황.csv")

gangnam <- data1 %>% filter(시군구=='강남구')
gangnam

plot(gangnam$발생건수, type='o', xlab='월', ylab='수', main='강남구 교통사고', ylim=c(0, 500), lty=2, pch=18, col='red')
lines(gangnam$사망자수, type='o',lty=3, pch=20, col='blue')
lines(gangnam$중상, type='o',lty=4, pch=21, col='magenta')
legend('topright', c('발생건수', '사망자수', '부상자수'), lty=2:4, pch=c(18,20,21), col=c('red', 'blue', 'magenta'))

name01 <- gangnam$월

bpx <- barplot(gangnam$발생건수, col=rainbow(12), ylab='발생건수', names = name01, ylim = c(0, 450), las=1)
title(main = '강남구 월별 교통사고 발생건수', line=1)
text(x = bpx, y=gangnam$발생건수*1.07, labels = paste(gangnam$발생건수,"건"))

gdf <- gangnam %>% select(발생건수, 부상자수, 중상)
m3 <- as.matrix(gdf)
pct <- round(m3/sum(m3)*100, 2)
bpx <- barplot(m3, beside=T, col = rainbow(12), ylim = c(0,600))
title(main='강남구 교통사고', line=2)
abline(h=seq(0,600, 50), lty=2, col="red")
abline(v=seq(0,36, 13), lty=3,lwd=2, col="blue")
text(x = bpx, y=m3*1.07, labels = paste(pct,'%'), cex=0.7)
legend('topright', legend = name01, fill = rainbow(12))

v1 = gangnam$부상자수
pie(v1, col=rainbow(12), main='강남구 월별 교통사고 부상자수', paste(v1,'명'))
legend('bottomleft', legend = name01, fill = rainbow(12))

v2 = gangnam$발생건수
pct <- round(v2/sum(v2)*100, 2)
pie(v2, main='강남구 월별 교통사고 발생건수', col = rainbow(12), labels = paste(v2,'건\n',pct,'%'))
legend('left', legend = name01, fill = rainbow(12))
