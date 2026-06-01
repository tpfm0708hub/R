rm(list=ls())
library(fs)# dir_ls()
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
df_001 %>% count(DI1_dg)
#  DI1_dg     n 내용
#   <dbl> <int>
#1      0 12300 없음
#2      1  4955 있음
#3      8  2916 비해당(소아, 청소년)
#4      9     2 모름, 무응답
#5     NA    18

# 고혈압 의사진단 여부(DI1_dg)' 결측 처리
df_002 <- df_001 %>% filter(!is.na(DI1_dg) & !DI1_dg %in% c(8, 9))

# 결측 처리 확인
df_002 %>% nrow(.)
# [1] 17255