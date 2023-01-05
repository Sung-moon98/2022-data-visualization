library('tidyverse')

tat <- read.csv('도로교통공단_시도_시군구별_월별_교통사고(2018).csv')
tat %>% View()


df <- tat %>% filter(시군구=='남양주시') %>% select(발생건수, 부상자수, 중상, 경상, 부상신고)
df
name <- tat[1:12,]$월

plot(df$발생건수, col = 'red', type='l', xlab = '월', ylab='인원 수(명)', ylim = c(0, 500), main='남양주시 월별 교통사고')
lines(df$부상자수, col='blue', type='l')
lines(df$중상, col='yellow', type='l')
lines(df$경상, col='magenta', type='l')
lines(df$부상신고, type='l')

legend('topright', c('발생건수', '부상자수', '중상', '경상', '부상신고'),cex=0.9, col=c('red', 'blue','yellow','magenta','black'), lty=1)


plot(df$발생건수, pch = 25, bg = 'red', col = 'red', type='o', xlab = '월', ylab='인원 수(명)', ylim = c(0, 500), main='남양주시 월별 교통사고')
lines(df$부상자수, pch = 24, bg = 'blue', col='blue', type='o')
lines(df$중상, pch = 23, bg = 'yellow', col='yellow', type='o')
lines(df$경상, pch = 22, bg = 'magenta', col='magenta', type='o')
lines(df$부상신고, pch = 21, bg = 'black', type='o')

legend('topright', c('발생건수', '부상자수', '중상', '경상', '부상신고'),cex=0.9, col=c('red', 'blue','yellow','magenta','black'), lty=1)


plot(df$발생건수, bty='l', col='green', main='남양주시 월별 교통사고 발생건수', type = 'l', xlab='월', ylab='발생건수')


par(mfrow=c(2,2))
plot(df$부상자수, col='magenta', main='남양주시 월별 교통사고 부상자 수', type = 'l', xlab='월', ylab='부상자 수')
plot(df$중상, col='green', main='남양주시 월별 교통사고 중상자 수', type = 'b', xlab='월', ylab='중상자 수')
plot(df$경상, col='red', main='남양주시 월별 교통사고 경상자 수', type = 'o', xlab='월', ylab='경상자 수')
plot(df$부상신고, pch=15, col='blue', main='남양주시 월별 교통사고 부상신고', type = 'o', xlab='월', ylab='부상신고')


par(mfrow=c(1,1))

barplot(df$발생건수,names.arg = name, col=rainbow(12), ylim=c(0,300), ylab='인원 수(명)', main='남양주시 월별 교통사고 발생건수')


mat <- as.matrix(df)
mat

barplot(mat,names.arg = c('발생건수', '부상자수', '중상', '경상', '부상신고'), col=rainbow(12), ylim=c(0,5000), ylab='인원 수(명)', main='월별 교통사고')
legend(locator(1), name, cex=0.9, bg = 'yellow', fill=rainbow(12))


t_mat <- t(mat)

barplot(t_mat, names.arg = name, col=heat.colors(5), ylim=c(0,1500), ylab='수', main='월별 교통사고')
legend('topright', c('발생건수', '부상자수', '중상', '경상', '부상신고'), cex=0.9, col = rainbow(12), lty=1)


pie(df$부상자수, col=terrain.colors(12), label=df[,3] ,main='남양주시 월별 교통사고 부상자수')
legend('topright', name, cex=0.8, col = terrain.colors(12), lty=1)



mat01 <- as.matrix(df)
mat01 <- t(mat01)

barplot(mat01, beside = T, names.arg = name, col=rainbow(5), ylim=c(0,500), ylab='수', main='남양주시 월별 교통사고')
legend('topright', c('발생건수', '부상자수', '중상', '경상', '부상신고'), cex=0.9, col = rainbow(5), lty=1)







