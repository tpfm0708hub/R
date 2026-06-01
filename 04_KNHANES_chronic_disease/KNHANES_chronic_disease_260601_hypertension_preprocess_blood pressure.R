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
# 분석 데이터프레임 개수 확인
nrow(df_002)
# [1] 17255

df_002 %>% select(HE_sbp2) %>% describe(.)
#        vars     n   mean   sd median trimmed   mad min max range skew kurtosis   se
#HE_sbp2    1 16964 120.62 16.6    119  119.49 16.31  73 238   165 0.84      1.5 0.13
df_002 %>% select(HE_sbp3) %>% describe(.)
#        vars     n   mean   sd median trimmed   mad min max range skew kurtosis   se
#HE_sbp3    1 16964 119.47 16.2    118   118.4 14.83  70 237   167 0.83     1.58 0.12

# '2차 수축기 혈압' 및  '2차 이완기 혈압' 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_sbp2) & !is.na(HE_sbp3))

# 3)'3차 수축기 혈압(HE_sbp3)' 및  '3차 이완기 혈압(HE_dbp3)' 변수 분포 확인
# 분석 데이터프레임 개수 확인
nrow(df_003)
#[1] 16964

df_003 %>% select(HE_sbp3) %>% describe(.)
#        vars     n   mean   sd median trimmed   mad min max range skew kurtosis   se
#HE_sbp3    1 16964 119.47 16.2    118   118.4 14.83  70 237   167 0.83     1.58 0.12
df_003 %>% select(HE_dbp3) %>% describe(.)
#        vars     n  mean   sd median trimmed   mad min max range skew kurtosis   se
#HE_dbp3    1 16964 74.13 9.85     73   73.61 10.38  40 130    90 0.56     0.66 0.08