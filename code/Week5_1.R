library('tidyverse')


data1 <- read.csv("���α������_�õ�_�ñ�����_����_������(2018).csv")
data2 <- read.csv("����_�ֿ䱸��_������Ȳ.csv")

gangnam <- data1 %>% filter(�ñ���=='������')
gangnam

plot(gangnam$�߻��Ǽ�, type='o', xlab='��', ylab='��', main='������ ������', ylim=c(0, 500), lty=2, pch=18, col='red')
lines(gangnam$����ڼ�, type='o',lty=3, pch=20, col='blue')
lines(gangnam$�߻�, type='o',lty=4, pch=21, col='magenta')
legend('topright', c('�߻��Ǽ�', '����ڼ�', '�λ��ڼ�'), lty=2:4, pch=c(18,20,21), col=c('red', 'blue', 'magenta'))

name01 <- gangnam$��

bpx <- barplot(gangnam$�߻��Ǽ�, col=rainbow(12), ylab='�߻��Ǽ�', names = name01, ylim = c(0, 450), las=1)
title(main = '������ ���� ������ �߻��Ǽ�', line=1)
text(x = bpx, y=gangnam$�߻��Ǽ�*1.07, labels = paste(gangnam$�߻��Ǽ�,"��"))

gdf <- gangnam %>% select(�߻��Ǽ�, �λ��ڼ�, �߻�)
m3 <- as.matrix(gdf)
pct <- round(m3/sum(m3)*100, 2)
bpx <- barplot(m3, beside=T, col = rainbow(12), ylim = c(0,600))
title(main='������ ������', line=2)
abline(h=seq(0,600, 50), lty=2, col="red")
abline(v=seq(0,36, 13), lty=3,lwd=2, col="blue")
text(x = bpx, y=m3*1.07, labels = paste(pct,'%'), cex=0.7)
legend('topright', legend = name01, fill = rainbow(12))

v1 = gangnam$�λ��ڼ�
pie(v1, col=rainbow(12), main='������ ���� ������ �λ��ڼ�', paste(v1,'��'))
legend('bottomleft', legend = name01, fill = rainbow(12))

v2 = gangnam$�߻��Ǽ�
pct <- round(v2/sum(v2)*100, 2)
pie(v2, main='������ ���� ������ �߻��Ǽ�', col = rainbow(12), labels = paste(v2,'��\n',pct,'%'))
legend('left', legend = name01, fill = rainbow(12))