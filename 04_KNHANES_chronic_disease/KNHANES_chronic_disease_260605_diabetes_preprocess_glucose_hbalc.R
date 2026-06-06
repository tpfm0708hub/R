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

# 1. 당뇨병 경험집단 관련 변수 전처리 진행
# 1)'당뇨병 의사진단 여부' 변수 빈도 확인
# 2)'공복혈당' 변수 분포 확인
# 3)'당화혈색소' 변수 분포 확인
# 4) '당뇨병 유병여부' 변수 생성

# 1)'당뇨병 의사진단 여부' 변수 빈도 확인
#df_001 %>% count(DE1_dg)

# 당뇨병 의사진단 여부(DE1_dg)' 결측 처리
df_002 <- df_001 %>% filter(!is.na(DE1_dg) & !DE1_dg %in% c(8, 9))

# 결측 처리 확인
#df_002 %>% nrow(.)

# 2) '공복혈당'(HE_glu) 및 3) '당화혈색소'(HE_HbA1c) 변수 분포 확인
df_002 %>% select(HE_glu) %>% describe(.)
#       vars     n  mean    sd median trimmed   mad min max range skew kurtosis   se
#HE_glu    1 16674 101.4 22.67     96   97.71 10.38  47 460   413 4.22    30.55 0.18
df_002 %>% select(HE_HbA1c) %>% describe(.)
#         vars     n mean   sd median trimmed  mad min  max range skew kurtosis   se
#HE_HbA1c    1 16649 5.64 0.78    5.5    5.52 0.44 3.3 17.6  14.3 3.55    23.49 0.01

# '공복혈당' 및 '당화혈색소' 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_glu) & !is.na(HE_HbA1c))
