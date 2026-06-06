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
#df_002 %>% select(HE_glu) %>% describe(.)
#df_002 %>% select(HE_HbA1c) %>% describe(.)

# '공복혈당' 및 '당화혈색소' 결측 처리
df_003 <- df_002 %>% filter(!is.na(HE_glu) & !is.na(HE_HbA1c))

# 당뇨병 유병여부 변수(BS_BG) 생성
# ('의사진단 여부' 및 '혈당강하제 복용' 및 '인슐린 주사 사용' 분석에서 제외)
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_003 <- df_003 %>% mutate(BS_BG = case_when(
  # 당뇨병:  공복혈당 : 126mg/dL 이상
  #         당화혈색소: 6.5% 이상
  # 분석 제외: '당뇨병 의사진단 여부' 및 '혈당강하제 복용' 및 '인슐린 주사 사용'
  HE_glu   >= 126 |
  HE_HbA1c >= 6.5 ~ 2,
  # 당뇨병 전단계:  공복혈당 : 100mg/dL 이상 및 125mg/dL 이하
  #                당화혈색소: 5.7% 이상 및 6.4% 이하
  HE_glu   >= 100 |
  HE_HbA1c >= 5.7 ~ 1,
  # 정상:  공복혈당 : 100mg/dL 미만
  #       당화혈색소: 5.7% 미만
  HE_glu   < 100 |
  HE_HbA1c < 5.7 ~ 0,
  )
)

# 전체집단 당뇨병 유병여부(BS_BG) 빈도 확인
prop.table(df_003 %>% count(BS_BG))
# BS_BG         n
#1 0 0.5179580
#2 1 0.3603003
#3 2 0.1215616

# 당뇨병 경험자(의사진단자) 중 당뇨병 유병여부 빈도(BS_BG) 확인
prop.table(df_003 %>% filter(DE1_dg == 1) %>% count(BS_BG))
# BS_BG          n
#1 0 0.02250958
#2 1 0.29406130
#3 2 0.68199234