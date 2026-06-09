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
#df_002 %>% pull(HE_chol) %>% describe(.)
#df_002 %>% pull(HE_TG) %>% describe(.)

# 2)'총콜레스테롤'(HE_chol) 및 '중성지방'(HE_TG) 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_chol) & !is.na(HE_TG))

# 3) '고콜레스테롤혈증 유병여부' 및 '고중성지방혈증 유병여부' 변수 생성
df_003 <- df_003 %>% mutate(
  # 고콜레스테롤혈증: 총콜레스테롤 240mg/dL 이상
  # 분석에서 제외: '현재 콜레스테롤강하제'
  cls_hchol = case_when(
    HE_chol >= 240 ~ 1,
    TRUE ~ 0
  ),
  # 고중성지방혈증: 중성지방 200mg/dL 이상
  cls_htg = case_when(
    HE_TG >= 200 ~ 1,
    TRUE ~ 0
    )
  )

# 전체집단 고콜레스테롤혈증 유병여부(cls_hchol) 빈도 확인
prop.table(df_003 %>% count(cls_hchol))
#  cls_hchol n
#1         0 0.90428786
#2         1 0.09565217

# 전체집단 고중성지방혈증 유병여부(cls_htg) 빈도 확인
prop.table(df_003 %>% count(cls_htg))
#  cls_htg n
#1       0 0.8807796
#2       1 0.1191604


# 이상지질혈증 경험자(의사진단자) 중 고콜레스테롤혈증 유병여부(cls_hchol) 빈도 확인
prop.table(df_003 %>% filter(DI2_dg == 1) %>% count(cls_hchol))
# cls_hchol n
#1        0 0.9340067
#2        1 0.0657688

# 이상지질혈증 경험자(의사진단자) 중 고중성지방혈증 유병여부(cls_htg) 빈도 확인
prop.table(df_003 %>% filter(DI2_dg == 1) %>% count(cls_htg))
# cls_htg n
#1      0 0.8751964
#2      1 0.1245791