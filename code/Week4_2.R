library('tidyverse')

tat <- read.csv('도로교통공단_시도_시군구별_월별_교통사고(2018).csv') %>% filter(시도 == '서울') %>% group_by(시군구) %>%
  summarise(mean1 = mean(중상))

tat[,2] <- round(tat[,2], 3)      # 소수점 아래 3자리까지만

View(tat)


plot(tat$mean1, ann = F, axes=F, ylim=c(0,100), col='red', type='b')

axis(1, at=1:25, labels = tat$시군구)
axis(2, ylim=c(0,100), las=1)


pie(tat$mean1, labels = tat$mean1, col=rainbow(25), main='서울시군구별 중상자 수')
legend('topright', tat$시군구, cex=0.6, col = rainbow(25), lty=1)


pie(tat$mean1, labels = tat$시군구, col=heat.colors(25), main='서울시군구별 중상자 수')


par(mfrow=c(1,3))

pie(tat$mean1, radius=1, labels = tat$시군구, col=rainbow(25), main='서울시군구별 중상자 수')

plot(tat$mean1, pch=25, bg='red', ann = F, axes=F, ylim=c(0,100), col='red', type='o')
axis(1, at=1:25, labels = tat$시군구)
axis(2, ylim=c(0,100))

pie(tat$mean1, radius=1, labels = tat$mean1, col= terrain.colors(25), main='서울시군구별 중상자 수')
legend('topright', tat$시군구, cex=0.3, col =  terrain.colors(25), lty=1)

par(mfrow=c(1,1))


barplot(tat$mean1, names.arg = tat$시군구, las=1, col=heat.colors(12), ylim=c(0,100), ylab='인원 수(명)', main='서울시군구별 중상자 수')


par(bg='yellow')
plot(tat$mean1, ann = F, axes=F, pch=23, bg='blue', ylim=c(0,100), col='blue', type='o', las=1)

axis(1, at=1:25, labels = tat$시군구)
axis(2, ylim=c(0,100), las=1)


par(bg='white')


cols <- colors()
bcol <- grep('blue', cols, value=T)

barplot(tat$mean1, names.arg = tat$시군구, las=1, col=bcol[1:25], ylim=c(0,100), ylab='인원 수(명)', main='서울시군구별 중상자 수')


gcol <- grep('green', cols, value = T)
pie(tat$mean1, radius=1, labels = tat$시군구, col=gcol[1:25], main='서울시군구별 중상자 수')


pie(tat$mean1, radius=0.7, labels = tat$mean1, col=bcol[1:25], main='서울시군구별 중상자 수')
legend('topright', tat$시군구, cex=0.5, col =bcol[1:25], lty=1)


barplot(tat$mean1, names.arg = tat$시군구, las=1, col=rainbow(12), ylim=c(0,100), ylab='인원 수(명)', main='서울시군구별 평균 중상자 수')



