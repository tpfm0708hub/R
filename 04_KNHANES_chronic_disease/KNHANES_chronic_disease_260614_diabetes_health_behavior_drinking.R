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

# 신장 및 체중 결측 제거
df_003 <- df_002 %>% filter(!is.na(HE_ht) & !is.na(HE_wt))

# BMI(set_bmi) 지표 생성
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_003 <- df_003 %>% mutate(set_bmi = HE_wt / (HE_ht / 100)^2)

# 흡연여부 변수 생성
# 기준 자료: Lee, D. Y. (2024). Prevalence and risk factors of osteoarthritis in Korea: a cross-sectional study. Medicina, 60(4), 665.
# 평생 일반담배(궐련) 흡연 여부(BS1_1) 및 현재 일반담배(궐련) 흡연 여부(BS3_1)
# 궐련형 전자담배 평생사용여부(BS12_37) 및 궐련형 전자담배 현재사용여부(BS12_47)

# 담배 평생사용여부 무응답 및 결측 제거
df_004 <- df_003 %>% filter(
  BS1_1 != 9 & BS12_37 != 9 &
  !is.na(BS1_1) & !is.na(BS12_37)
  )

# 현재 흡연여부(current_smoking) 변수 생성
df_004 <- df_004 %>% mutate(
  current_smoking = case_when(
    # 현재흡연자(2): 담배관련 '매일피움'이거나 '가끔피움'에 해당
    BS3_1 %in% c(1, 2) | BS12_47 %in% c(1, 2) ~ 2,
    # 과거흡연자(1): 담배관련 '과거엔 피웠으나, 현재 피우지 않음' 응답에 해당
    BS3_1 == 3 | BS12_47 == 3 ~ 1,
    # 비흡연자(0): 담배 관련 '피운적 없음' 응답에 해당
    BS3_1 == 8 | BS12_47 == 8 ~ 0,
    TRUE ~ NA)
  )

# 음주여부 변수 생성
# 기준 자료: Lee, D. Y. (2024). Prevalence and risk factors of osteoarthritis in Korea: a cross-sectional study. Medicina, 60(4), 665.

# 평생음주경험(BD1) 빈도 확인
df_004 %>% count(BD1)
#    BD1     n
#1     1  1761
#2     2 14668

# 1년간 음주빈도(BD1_11) 빈도 확인
df_004 %>% count(BD1_11)
#  BD1_11     n 내용
#1      1  3085 최근 1년간 전혀 마시지 않았다
#2      2  3192 월1회미만
#3      3  1791 월1회정도
#4      4  3424 월2-4회
#5      5  2256 주2-3회정도
#6      6   919 주4회이상
#7      8  1761 비해당(평생음주경험 없음)
#8      9     1 모름, 무응답

# 1년간 음주빈도 무응답 제거
df_005 <- df_004 %>% filter(BD1_11 != 9)

# 현재 음주여부(current_drinking) 변수 생성
df_005 <- df_005 %>% mutate(
  current_drinking = case_when(
    # 현재음주자(1): 음주빈도 월1회이상
    BD1_11 %in% c(3, 4, 5, 6) ~ 1,
    # 비음주자(0): 평생음주경험 없음 및 음주빈도 월1회미만
    BD1_11 %in% c(1, 2, 8) ~ 0,
    TRUE ~ NA)
  )

# 현재 음주여부 빈도 확인
df_005 %>% count(current_drinking)
#  current_drinking     n 내용
#1                0  8038 비음주자
#2                1  8390 음주자