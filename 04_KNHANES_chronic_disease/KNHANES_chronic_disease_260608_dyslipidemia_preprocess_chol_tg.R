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
df_001 <- foreach(f = df_list_001, .combine = bind_rows, .packages = c('haven', 'dplyr')) %dopar%{
  df_01 <- read_sas(f)
  return(df_01)
  }

# 병렬처리 종료
stopCluster(cl)

# 1. 이상지질혈증 경험집단 관련 변수 전처리 진행
# 1)'이상지질혈증 의사진단 여부'(DI2_dg) 변수 빈도 확인
# 2)'총콜레스테롤'(HE_chol) 및 '중성지방'(HE_TG) 변수 분포 확인
# 3) '고콜레스테롤혈증 유병여부' 및 '고중성지방혈증 유병여부' 변수 생성

# 1)'이상지질혈증 의사진단 여부' 변수 빈도 확인
#df_001 %>% count(DI2_dg)

# 이상지질혈증 의사진단 여부(DI2_dg)' 결측 처리
df_002 <- df_001 %>% filter(!is.na(DI2_dg) & !DI2_dg %in% c(8, 9))

# 결측처리 확인
#df_002 %>% count(DI2_dg)

# 2)'총콜레스테롤'(HE_chol) 및 '중성지방'(HE_TG) 변수 분포 확인
df_002 %>% pull(HE_chol) %>% describe(.)
#   vars     n   mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 16674 186.57 40.41    185  185.43 40.03  70 489   419 0.37     0.58 0.31

df_002 %>% pull(HE_TG) %>% describe(.)
#   vars     n   mean    sd median trimmed   mad min  max range skew kurtosis   se
#X1    1 16674 126.82 96.53    104  111.71 50.41  18 3367  3349 6.82   118.09 0.75

# 2)'총콜레스테롤'(HE_chol) 및 '중성지방'(HE_TG) 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_chol) & !is.na(HE_TG))