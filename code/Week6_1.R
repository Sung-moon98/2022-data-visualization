install.packages("animation")
install.packages("png")
install.packages("RColorBrewer")
library('RColorBrewer')
library('animation')
library('png')
library('tidyverse')

data1 <- read.csv('도로교통공단_시도_시군구별_월별_교통사고(2018).csv')
data2 <- read.csv('서울_주요구별_병원현황.csv')
data1

d <- data1 %>% group_by(시도) %>% summarise_each(funs(sum), 발생건수, 사망자수, 부상자수)
d

m3 <- as.matrix(d[ ,2:4])

tm <- t(m3) %>% view()
tm

barplot(tm, names.arg = d$시도, col = 1:3, beside = T)
legend('topright', legend = c('발생건수', '사망자수', '부상자수'), fill = 1:3)

mycol <- brewer.pal(12, 'Set3')
df1 <- data1 %>% filter(시군구 == '남양주시') %>% select(월, 발생건수, 사망자수, 부상자수, 중상, 경상)
name_lab <- colnames(df1)[2:6]
df1
name_lab

gangnam <- data2$강남구
name02 <- data2$표시과목

barplot(gangnam, density=30, angle=70, col = mycol, names.arg = name02, ylim = c(0, 350))
title(main = '강남구 병원현황', line = 2)
legend('topright', legend = name02, fill = mycol, density = 30, angle = 70)
arrows(2, 300, 4, 200)
text(2, 330, '서울시 전체 병원 중\n 최댓값', cex = 1.5, col = 'green')

mat1 <- as.matrix(df1[2:6])
barplot(mat1, beside=T, density=60, angle=70, names.arg = c('발생건수', '사망자수', '부상자수', '중상', '경상'), 
        col = mycol, ylim=c(0,500))
legend('topright', legend = df1$월, fill = mycol, density=60, angle=70, cex = 0.6)
title(main = '남양주시 교통사고 현황', line = 2)
arrows(22, 450, 33, 350, code = 0)
text(22, 460, '최댓값', col = 'red')

barplot(mat[,1], density=60, angle=70, names.arg = df1$월, col = mycol, ylim=c(0,350))
title(main = '남양주시 교통사고 월별 발생건수', line = 1)
arrows(4, 300, 8, 200, code = 0)
text(4, 310, '최댓값', col = 'red', cex = 1.5)
arrows(1, 250, 3, 150)
text(1, 260, '최솟값', cex = 1.5)

df2 <- data1 %>% filter(시군구 == '강서구' & 시도 == '서울') %>% 
  select(월, 발생건수, 사망자수, 부상자수, 중상, 경상)
name02 <- df2$월
plot(df2$발생건수, axes=F, xlab='월', ylab='건 수', pch=2, col = 4, type = 'o', lty = 2, ylim = c(100, 160))
axis(1, labels = name02, at=1:12)
axis(2, las = 2)
title(main = '서울시 강서구 월별 교통사고 발생건수', line = 2)
arrows(8, 103, 5, 103)
text(9, 103, '급격히\n줄어들었음', cex = 1.3, col = 'green')

pct <- round(df2$부상자수/sum(df2$부상자수)*100, 2)
mycol <- brewer.pal(12, 'Set2')
pie(df2$부상자수, col = mycol, density = 70, angle = 70, paste(name02,'\n',pct,'%'))
title(main = '서울시 강서구 월별 교통사고 부상자수', line = 1)
legend('topright', legend = name02, fill = mycol, density = 70, angle = 70)


df1 <- data1 %>% filter(시군구 == '남양주시') %>% select(월, 발생건수, 사망자수, 부상자수, 중상, 경상)
df1
name_lab <- colnames(df1)[2:6]
d1 <- df1$발생건수
d2 <- df1$사망자수
d3 <- df1$부상자수
d4 <- df1$중상
d5 <- df1$경상

plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(1, 210, '발생건수', col = 1)
lines(d2, type='o', col=2, pch=16)
text(3, 20, '사망자수', col = 2)
lines(d3, type='o', col=3, pch=17, lwd=2)
text(1, 360, '부상자수', col = 3)
lines(d4, type='o', col=4, pch=18, lwd=2)
text(1, 80, '중상자수', col = 4)
lines(d5, type='o', col=5, pch=19, lwd=2)
text(5, 280, '경상자수', col = 5)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)

png(filename = "dance1.png")
plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(2, 210, '발생건수', col = 1)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)
dev.off()

png(filename = "dance2.png")
plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(2, 210, '발생건수', col = 1)
lines(d2, type='o', col=2, pch=16)
text(3, 20, '사망자수', col = 2)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)
dev.off()

png(filename = "dance3.png")
plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(2, 210, '발생건수', col = 1)
lines(d2, type='o', col=2, pch=16)
text(3, 20, '사망자수', col = 2)
lines(d3, type='o', col=3, pch=17, lwd=2)
text(2, 360, '부상자수', col = 3)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)
dev.off()

png(filename = "dance4.png")
plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(2, 210, '발생건수', col = 1)
lines(d2, type='o', col=2, pch=16)
text(3, 20, '사망자수', col = 2)
lines(d3, type='o', col=3, pch=17, lwd=2)
text(2, 360, '부상자수', col = 3)
lines(d4, type='o', col=4, pch=18, lwd=2)
text(2, 80, '중상자수', col = 4)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)
dev.off()

png(filename = "dance5.png")
plot(d1, main="남양주시 월별 교통사고 통계(2018)", xlab="월", ylab="건수", 
     type='o', col=1, ylim = c(0,400), pch=15, lwd=2)
text(2, 210, '발생건수', col = 1)
lines(d2, type='o', col=2, pch=16)
text(3, 20, '사망자수', col = 2)
lines(d3, type='o', col=3, pch=17, lwd=2)
text(2, 360, '부상자수', col = 3)
lines(d4, type='o', col=4, pch=18, lwd=2)
text(2, 80, '중상자수', col = 4)
lines(d5, type='o', col=5, pch=19, lwd=2)
text(5, 280, '경상자수', col = 5)
legend(10, 150, name_lab, cex=0.6, col=c(1,2,3,4,5), lty=1, pch=1:5)
dev.off()



ani.options(interval = 1)
plot.new()

for (i in 1:5) {
  imgFileNames <- paste("dance",i,".png", sep = "") 
  img <- readPNG(imgFileNames)
  rasterImage(img,0,0,1,1)
  ani.pause()
}







