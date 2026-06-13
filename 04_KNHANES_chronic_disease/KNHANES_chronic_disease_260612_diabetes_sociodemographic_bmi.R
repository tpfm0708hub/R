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

# 신장(HE_ht) 및 체중(HE_wt) 요인 분포 확인
df_002 %>% pull(HE_ht) %>% describe(.)
#   vars     n   mean   sd median trimmed  mad   min max range skew kurtosis   se
#X1    1 16826 163.39 9.27  162.9  163.27 9.79 127.9 194  66.1 0.11    -0.38 0.07

df_002 %>% pull(HE_wt) %>% describe(.)
#   vars     n  mean    sd median trimmed  mad min max range skew kurtosis  se
#X1    1 17033 64.63 13.25   62.8   63.61 12.6  25 147   122 0.89      1.5 0.1

# 신장 및 체중 결측 제거
df_003 <- df_002 %>% filter(!is.na(HE_ht) & !is.na(HE_wt))

# 신장 및 체중 결측 제거 확인
df_003 %>% filter(is.na(HE_ht)) %>% nrow(.)
# [1] 0
df_003 %>% filter(is.na(HE_wt)) %>% nrow(.)
# [1] 0

# BMI(set_bmi) 지표 생성
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_003 <- df_003 %>% mutate(set_bmi = HE_wt / (HE_ht / 100)^2)

# 기존 생성 변수(HE_BMI)와 일치여부 비교
sum(df_003 %>% pull(set_bmi) != df_003 %>% pull(HE_BMI))
# [1] 0
