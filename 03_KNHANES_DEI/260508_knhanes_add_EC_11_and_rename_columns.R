rm(list=ls())
library(fs)# dir_ls()
library(dplyr)
library(haven)
library(doParallel)#  foreach()
library(psych)# describe()
library(formattable)# comma()
# 활용자료: "국민건강영양조사 제9기(2022-2024)
# URL: https://knhanes.kdca.go.kr/knhanes/rawDataDwnld/rawDataDwnld.do

# 병렬처리 코어 설정
cl <- makeCluster(parallel::detectCores()-1)
registerDoParallel(cl)

# 데이터 폴더 경로 설정
path_001<-'D:/github'
# 경로 내 hn으로 시작하는 SAS 파일 목록 추출
df_list_01<-dir_ls(path_001, regexp="hn.*\\.sas7bdat$")

# 해당 SAS 파일을 병렬로 불러온 뒤 결합
df_001<-foreach(f=df_list_01, .combine=bind_rows, .packages=c('dplyr', 'haven'))%dopar%{
  df<-read_sas(f)
  return(df)}

# 병렬처리 클러스터 종료
stopCluster(cl)

# 당뇨병 의사진단 여부(DE1_dg) 없음(0), 있음(1) 외 제거  
df_002 <- df_001 %>% filter(DE1_dg %in% c(0, 1))

# 연령 분포 확인
#df_002 %>% select(age) %>% describe(.)

# 노인 외 65세 미만 집단 추출
df_003 <- df_002 %>% filter(age < 65)

# 노인 제외 전후 표본 수 변화 확인
#print(
#  paste0('노인제외 전 표본수 : ', comma(nrow(df_002),digits=0), '명 / ',
#         '노인제외 후 표본수 : ', comma(nrow(df_003),digits=0), '명')
#)

# 'age'(연령) 분포 재확인
#print(df_003 %>% select(age) %>% describe(.))

# 'sex'(성별) 빈도 확인
#df_003 %>% group_by(sex) %>% summarise(n=n())

# 'edu'(교육수준) 빈도 확인
#df_003 %>% count(edu)

# 'edu' 변수 결측치(NA) 제거
df_003 <- df_003 %>% filter(!is.na(edu))

# 'marri_1'(결혼여부) 빈도  확인
df_003 %>% count(marri_1)

# 'edu' 변수 이분형 범주로 재조정 후 edu_binary 변수 생성
# 0 = 고졸이하, 1 = 대졸이상
df_003 <- df_003 %>% mutate(
  edu_binary = if_else(edu != 4, 0, 1)
)

# 'town_t'(동/읍면 구분) 빈도 확인
#df_003 %>% count(town_t)

# 'EC1_1'(경제활동 상태) 빈도 확인
df_003 %>% count(EC1_1)
#EC1_1     n
#<dbl> <int>
#1     1  7803
#2     2  3072
#3     9   719

# 'EC1_1' 변수 '모름, 무응답'(9) 제거
df_003 <- df_003 %>% filter(EC1_1 != 9)

# 분석용 데이터프레임 생성('연령' 및 '성별', '교육수준_이분형', 
# '결혼여부', '동/읍면 구분', '경제활동 상태', '당뇨병 의사진단 여부')
df_004 <- df_003 %>% select(age, sex, edu_binary, marri_1, town_t, EC1_1, DE1_dg)

# 각 변수에 범주명 부여
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_fit_001 <- df_004 %>% mutate(
  sex = factor(sex, levels = c(1, 2), labels = c('남자', '여자')),
  edu_binary = factor(edu_binary, levels = c(0, 1), labels = c('고졸이하', '대졸이상')),
  marri_1 = factor(marri_1, levels = c(1, 2), labels = c('기혼', '미혼')),
  town_t = factor(town_t, levels = c(1, 2), labels = c('동', '읍·면')),
  EC1_1 = factor(EC1_1, levels = c(1, 2), labels = c('예', '아니요')),
  DE1_dg = factor(DE1_dg, levels = c(0, 1), labels = c('없음', '있음'))
) %>% rename(
  성별 = sex,
  교육수준 = edu_binary,
  결혼여부 = marri_1,
  거주지역 = town_t,
  경제활동상태 = EC1_1,
  당뇨병의사진단여부 = DE1_dg
)