if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}
install.packages('tidyverse')
library('tidyverse')
install.packages("foreign")  # foreign 패키지 설치
install.packages("readxl")  # 엑셀파일 읽기     read_excel("Codebook.xlsx", col_names = T, sheet = 2)
library(foreign)             # SPSS 파일 로드
library(ggplot2)             # 시각화
library(readxl)              # 엑셀 파일 불러오기


if (!require(quantmod)) {             # quantmod 패키지는 시계열 모델링을 위한 패키지
  install.packages("quantmod")    # 미국 주가를 가져오는 getSymbols()함수가 포함됨
  require(quantmod)
}

getSymbols("TSLA", from=as.Date("2021-01-01"),to=as.Date("2021-03-31")) 

p <- ggplot(TSLA, aes(x=index(TSLA), y=TSLA.Close))    
p <- p + geom_ribbon(aes(min=TSLA.Low, max=TSLA.High), fill="yellow", colour="red", alpha=0.3)
p <- p + geom_point(aes(y=TSLA.Close), colour="black", size=5, alpha=0.6)
p <- p + geom_line(aes(y=TSLA.Close), colour="blue", lwd=1)
p <- p + stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.3) 
p





raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
welfare <- raw_welfare
welfare <- rename(welfare,
                  sex = h10_g3,            # 성별
                  birth = h10_g4,          # 태어난 연도
                  marriage = h10_g10,      # 혼인 상태
                  religion = h10_g11,      # 종교
                  income = p1002_8aq1,     # 월급
                  code_job = h10_eco9,     # 직종 코드
                )


# 이상치 확인
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, 
                      "male", "female")
table(welfare$sex)

welfare_sex <- table(welfare$sex)
welfare_sex

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)


age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))


ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line() + geom_point() +
  stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.3)


ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_area() + geom_point() +
  stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.3)


sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))


ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line() + geom_point() +
  stat_smooth(method="loess", se=FALSE, colour="yellow", lwd=0.8)



# 연령대별 건강상태

welfare <- rename(welfare, health_code = h10_med2)
welfare$health_code

list_health <- data.frame(health_code = c(1:5),
                          health = c("아주 건강하다",
                                     "건강한 편이다",
                                     "보통이다",
                                     "건강하지 않은 편이다",
                                     "건강이 아주 안 좋다"))

welfare <- left_join(welfare, list_health, id = "health_code")

health_ageg <- welfare %>%
  group_by(health, ageg) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 2))


ggplot(data = health_ageg, aes(x = health,  y = pct, fill = ageg)) +
  geom_col() +
  facet_wrap(~ ageg)












