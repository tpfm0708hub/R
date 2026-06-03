rm(list=ls())
library(fs)# dir_ls()
library(psych)# describe()
library(dplyr)
library(haven)
library(doParallel)#  foreach()
# 활용자료: "국민건강영양조사 제9기(2022-2024)
# URL: https://knhanes.kdca.go.kr/knhanes/rawDataDwnld/rawDataDwnld.do

# 병렬처리 코어 설정
cl <- makeCluster(parallel::detectCores()-1)
registerDoParallel(cl)

# 파일 경로 설정
path_001 <- 'D:/github'
# 경로 내 파일 형식 지정
df_list_001 <- dir_ls(path_001, regexp = 'hn.*\\.sas7bdat$')

# 경로 내 해당 파일 불러온 후 병합
df_001 <- foreach(f = df_list_001, .combine = bind_rows, .packages = c('dplyr', 'haven', 'stringr')) %dopar% {
  df <- read_sas(f)
  return(df)
}

# 병렬처리 종료
stopCluster(cl)

# 1. 고혈압 경험집단 관련 변수 전처리 진행
# 1)'고혈압 의사진단 여부' 변수 빈도 확인
# 2)'2차 수축기 혈압' 및  '2차 이완기 혈압' 변수 분포 확인
# 3)'3차 수축기 혈압' 및  '3차 이완기 혈압' 변수 분포 확인

# 1)'고혈압 의사진단 여부' 변수 빈도 확인
#df_001 %>% count(DI1_dg)

# 고혈압 의사진단 여부(DI1_dg)' 결측 처리
df_002 <- df_001 %>% filter(!is.na(DI1_dg) & !DI1_dg %in% c(8, 9))

# 2)'2차 수축기 혈압(HE_sbp2)' 및  '2차 이완기 혈압(HE_dbp2)' 변수 분포 확인
#nrow(df_002)

#df_002 %>% select(HE_sbp2) %>% describe(.)
#df_002 %>% select(HE_sbp3) %>% describe(.)

# '2차 수축기 혈압' 및  '3차 수축기 혈압' 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_sbp2) & !is.na(HE_sbp3))

# 3)'3차 수축기 혈압(HE_sbp3)' 및  '3차 이완기 혈압(HE_dbp3)' 변수 분포 확인
# 분석 데이터프레임 개수 확인
#nrow(df_003)

#df_003 %>% select(HE_sbp3) %>% describe(.)
#df_003 %>% select(HE_dbp3) %>% describe(.)

# 최종 수축기 혈압(cal_sbp) 및 최종 이완기 혈압(cal_dbp) 생성
df_003 <- df_003 %>% mutate(
  cal_sbp = rowSums(across(c(HE_sbp2, HE_sbp3))) / 2,
  cal_dbp = rowSums(across(c(HE_dbp2, HE_dbp3))) / 2
)

# 국민건강양양조사 내 기존 생성 변수(HE_sbp, HE_dbp)와 계산값 비교
sum(df_003 %>% pull(HE_sbp) == df_003 %>% pull(cal_sbp))
#[1] 16964
sum(df_003 %>% pull(HE_dbp) == df_003 %>% pull(cal_dbp))
#[1] 16964
