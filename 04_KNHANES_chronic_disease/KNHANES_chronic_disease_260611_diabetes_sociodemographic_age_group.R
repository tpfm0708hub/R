rm(list=ls())
library(fs)# dir_ls()
library(psych)# describe()
library(dplyr)
library(haven)
library(doParallel)#  foreach()
# 활용자료: "국민건강영양조사 제9기(2022-2024)
# URL: https://knhanes.kdca.go.kr/knhanes/rawDataDwnld/rawDataDwnld.do

# 병렬처리 코어 설정
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# 파일 경로 설정
path_001 <- 'D:/github'

# 경로 내 파일 형식 지정
df_list_001 <- dir_ls(path = path_001, regexp = 'hn.*\\.sas7bdat$')

# 경로 내 해당 파일 불러온 후 병합
df_001 <- foreach(f = df_list_001, .combine = bind_rows, .packages = c('haven')) %dopar% {
  df_01 <- read_sas(f)
  return(df_01)
}

# 병렬처리 종료
stopCluster(cl)

# 연령(age) 요인 분포 확인
df_001 %>% pull(age) %>% describe(.)
#   vars     n  mean    sd median trimmed  mad min max range  skew kurtosis   se
#X1    1 20191 47.46 22.15     51   48.59 25.2   1  80    79 -0.39    -0.93 0.16

# 20세 이상 성인 대상 분석 진행
df_001 <- df_001 %>% filter(age >= 20)

# 연령집단 분류 기준: 20~39세, 40~59세, 60~79세, 80세 이상
# 기준 자료: Cha, J. E., & Yun, S. N. (2015). The comparison of health behaviors, use of health services, and health expenditures among diabetic patients according to the practice of exercise. Journal of Korean Academy of Community Health Nursing, 26(1), 31-41.
df_002 <- df_001 %>% mutate(
  age_group = case_when(
    age >= 80 ~ 3,
    age >= 60 ~ 2,
    age >= 40 ~ 1,
    age >= 20 ~ 0,
    TRUE      ~ 99
  )
)

# 전체 분석 집단 연령집단 빈도 확인
df_002 %>% count(age_group)
#  age_group     n  내용
#1         0  3812  20세 이상 39세 이하
#2         1  5954  40세 이상 59세 이하
#3         2  6355  60세 이상 79세 이하
#4         3  1026  80세 이상

# 당뇨병 진단 연령집단 빈도 확인
df_002 %>% filter(DE1_dg == 1) %>% count(age_group)
#  age_group     n  내용
#1         0    46  20세 이상 39세 이하
#2         1   489  40세 이상 59세 이하
#3         2  1407  60세 이상 79세 이하
#4         3   250  80세 이상