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

# 하루평균 일반담배 흡연량 및 전자담배 흡연량 비해당(888) 응답: 0으로 변환
df_005 <- df_005 %>% mutate(
  BS3_2 = ifelse(BS3_2 == 888, 0, BS3_2),
  BS12_47_1 = ifelse(BS12_47_1 == 888, 0, BS12_47_1)
)

df_006 <- df_005 %>% 
  # 현재 흡연여부 변수명: smoke_current로 변경(규칙성 통일)
  rename(smoke_current = current_smoking) %>%
  # 현재 흡연량(smoke_amt) 변수 생성: 하루평균 일반담배 흡연량 및 전자담배 흡연량 합산
  mutate(smoke_amt = rowSums(across(c(BS3_2, BS12_47_1))))

# 한 번에 마시는 음주량(BD2_1) 빈도 확인
df_006 %>% count(BD2_1)
#  BD2_1     n  내용
#1     1  4344  1-2잔
#2     2  2342  3-4잔
#3     3  1320  5-6잔
#4     4  2007  7-9잔
#5     5  1569  10잔 이상
#6     8  4846  비해당(술을 마셔본 적 없음)

# 한 번에 마시는 음주량 비해당(8) 응답: 0으로 변환
df_006 <- df_006 %>% mutate(
  BD2_1 = ifelse(BD2_1 == 8, 0, BD2_14)
  )

df_007 <- df_006 %>%
  # 현재 음주여부 변수명: drink_current로 변경(규칙성 통일)
  rename(drink_current = current_drinking) %>%
  # 현재 음주량 변수 생성(drink_amt):
  # 10잔이상: 직접 기입값[한 번에 마시는 음주량(잔)(BD2_14)]
  # 9잔이하:  각 문항 내용의 중앙값 출력
  mutate(drink_amt = case_when(
    BD2_1 == 5 ~ BD2_14,
    BD2_1 == 4 ~ 8,
    BD2_1 == 3 ~ 5.5,
    BD2_1 == 2 ~ 3.5,
    BD2_1 == 1 ~ 1.5,
    TRUE       ~ NA
  ))

# 현재 음주량 분포 확인
df_007 %>% pull(drink_amt) %>% describe(.)
#   vars     n mean   sd median trimmed  mad min max range skew kurtosis   se
#X1    1 11582 5.17 4.39    3.5    4.36 2.97 1.5  42  40.5 1.61     3.38 0.04