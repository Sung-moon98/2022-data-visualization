install.packages('foriegn')

library('tidyverse')
library(foreign)            
library(dplyr)              
library(ggplot2)            
library(readxl)              

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

welfare <- raw_welfare





# 전처리
welfare <- rename(welfare,
                  sex = h10_g3,            # 성별
                  birth = h10_g4,          # 태어난 연도
                  marriage = h10_g10,      # 혼인 상태
                  religion = h10_g11,      # 종교
                  income = p1002_8aq1,     # 월급
                  code_job = h10_eco9,     # 직종
                  code_region = h10_reg7   # 지역
                  )     
            

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
table(is.na(welfare$income))
welfare$age <- 2015 - welfare$birth + 1


# 연령대에 따른 월급 차이(미성년자(underage), 청년(youth), 중년(middle), 노년(old))

welfare <- welfare %>%
  mutate(ageg = ifelse(age <= 17, "underage",
                       ifelse(age <= 65, "youth", ifelse(age <= 79, "middle", "old"))))

underage <- welfare %>% filter(welfare$ageg == 'underage')
underage$income

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))


ageg_income

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()+
  scale_x_discrete(limits = c("youth", "middle", "old"))



# 연령대 및 성별 월급 차이

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("youth", "middle", "old"))




# 연령대 및 종교 유무에 따른 이혼율 분석

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))


ageg_divorce <- ageg_marriage %>%
  filter(ageg != "underage" & group_marriage == "divorce") %>%
  select(ageg, pct)

ageg_divorce
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()+
  scale_x_discrete(limits = c("youth", "middle", "old"))



# 지역별 연령대 비율

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
welfare$code_region

welfare <- left_join(welfare, list_region, id = "code_region")

region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 2))


list_order_old <- region_ageg %>%
  filter(ageg == "youth") %>%
  arrange(pct)

order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region,  y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)



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

list_order_youth <- health_ageg %>%
  filter(ageg == "youth") %>%
  arrange(pct)

order <- list_order_youth$health
order

ggplot(data = health_ageg, aes(x = health,  y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

































