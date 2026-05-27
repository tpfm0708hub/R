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
#df_003 %>% count(marri_1)

# 'edu' 변수 이분형 범주로 재조정 후 edu_binary 변수 생성
# 0 = 고졸이하, 1 = 대졸이상
df_003 <- df_003 %>% mutate(
  edu_binary = if_else(edu != 4, 0, 1)
)

# 'town_t'(동/읍면 구분) 빈도 확인
#df_003 %>% count(town_t)

# 'EC1_1'(경제활동 상태) 빈도 확인
#df_003 %>% count(EC1_1)

# 'EC1_1' 변수 '모름, 무응답'(9) 제거
df_003 <- df_003 %>% filter(EC1_1 != 9)

# 'ainc'(월평균 가구총소득) 분포 확인
df_003 %>% pull(ainc) %>% describe(.)

# 'ainc' 변수 결측 제거
df_003 <- df_003 %>% filter(!is.na(ainc))

# 월평균 가구총소득 중위소득 설정
ainc_mid <- median(df_003$ainc); print(ainc_mid)
# 출력 결과
# 566.6667

# 'ainc' 변수 이분형 범주로 재조정 후 ainc_binary 변수 생성
# 0 = 중위소득 미만, 1 = 중위소득 이상
df_003 <- df_003 %>% mutate(
  ainc_binary = if_else(ainc >= ainc_mid, 1, 0)
)

# ainc_binary 빈도 확인
#df_003 %>% count(ainc_binary)

# BS3_1(현재 궐련형 일반담배 흡연여부) 빈도 확인
#df_003 %>% count(BS3_1)

# BS12_47(현재 궐련형 전자담배 사용여부) 빈도 확인
#df_003 %>% count(BS12_47)

# BS3_1 및 BS12_47 모름·무응답 제거
df_003 <- df_003 %>% filter(BS3_1 != 9 & BS12_47 != 9)

# smoker(흡연자) 변수 생성(0: 비흡연자, 1: 흡연자)
# 기준: BS3_1 및 BS12_47 1(매일피움) 및 2(가끔피움) 응답자 합산
df_003 <- df_003 %>% mutate(
  smoker = ifelse(BS3_1 %in% c(1, 2) | BS12_47 %in% c(1, 2), 1, 0)
)

# smoker(흡연자) 빈도 확인
#df_003 %>% count(smoker)

# BD1_11(1년간 음주빈도) 빈도 확인
#df_003 %>% count(BD1_11)

# BD2_1(한 번에 마시는 음주량) 빈도 확인
#df_003 %>% count(BD2_1)

# dr_high(고위험음주여부) 변수 생성(0: 비해당, 1: 해당)
# 지표정의: 1회 평균 음주량(남자: 7잔이상, 여자: 5잔 이상) + 주 2회이상 음주하는 분율
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_003 <- df_003 %>% mutate(
  dr_high = ifelse((sex == 1 & BD1_11 %in% c(5, 6) & BD2_1 %in% c(4, 5))|
                     (sex == 2 & BD1_11 %in% c(5, 6) & BD2_1 %in% c(3, 4, 5)), 1, 0)
) 

# dr_high(고위험음주여부) 빈도 확인
#df_003 %>% count(dr_high)

# BE3_71(고강도 신체활동 여부: 일) 빈도 확인
#df_003 %>% count(BE3_71)

# BE3_71(고강도 신체활동 여부: 일) 모름, 무응답 제거
df_003 <- df_003 %>% filter(BE3_71 != 9)

# BE3_72(고강도 신체활동 일수: 일) 빈도 확인
#df_003 %>% count(BE3_72)

# BE3_73(고강도 신체활동 시간(시간): 일) 분포 확인
#df_003 %>% pull(BE3_73) %>% describe(.)
# BE3_74(고강도 신체활동 시간(분): 일) 분포 확인
#df_003 %>% pull(BE3_74) %>% describe(.)

# BE3_73 및 BE3_74 '해당없음' 처리 및
# vig_t1(일 관련 고강도 신체활동 시간) 변수 생성
df_003 <- df_003 %>% mutate(
  BE3_73 = if_else(BE3_73 == 88, 0 , BE3_73),
  BE3_74 = if_else(BE3_74 == 88, 0 , BE3_74),
  vig_t1 = (BE3_73 * 60 + BE3_74 * 1) * BE3_72
)

# BE3_81(중강도 신체활동 여부: 일) 빈도 확인
#df_003 %>% count(BE3_81)

# BE3_82(중강도 신체활동 일수: 일) 빈도 확인
#df_003 %>% count(BE3_82)

# BE3_83(중강도 신체활동 시간(시간): 일) 분포 확인
#df_003 %>% pull(BE3_83) %>% describe(.)
# BE3_84(중강도 신체활동 시간(분): 일) 분포 확인
#df_003 %>% pull(BE3_84) %>% describe(.)

# BE3_83 및 BE3_84 '해당없음' 처리 및
# mod_t1(일 관련 중강도 신체활동 시간) 변수 생성
df_003 <- df_003 %>% mutate(
  BE3_83 = if_else(BE3_83 == 88, 0 , BE3_83),
  BE3_84 = if_else(BE3_84 == 88, 0 , BE3_84),
  mod_t1 = (BE3_83 * 60 + BE3_84 * 1) * BE3_82
)

# BE3_75(고강도 신체활동 여부: 여가) 빈도 확인
#df_003 %>% count(BE3_75)

# BE3_75(고강도 신체활동 여부: 여가) 모름·무응답 제거
df_003 <- df_003 %>% filter(BE3_75 != 9)

# BE3_76(고강도 신체활동 일수: 여가) 빈도 확인
#df_003 %>% count(BE3_76)

# BE3_76(고강도 신체활동 일수: 여가) 모름·무응답 제거
df_003 <- df_003 %>% filter(BE3_76 != 9)

# BE3_77(고강도 신체활동 시간(시간): 여가) 분포 확인
#df_003 %>% pull(BE3_77) %>% describe(.)
# BE3_78(고강도 신체활동 시간(분): 여가) 분포 확인
#df_003 %>% pull(BE3_78) %>% describe(.)

# BE3_77 및 BE3_78 '해당없음' 처리 및
# vig_t2(여가 관련 고강도 신체활동 시간) 변수 생성
df_003 <- df_003 %>% mutate(
  BE3_77 = if_else(BE3_77 == 88, 0 , BE3_77),
  BE3_78 = if_else(BE3_78 == 88, 0 , BE3_78),
  vig_t2 = (BE3_77 * 60 + BE3_78 * 1) * BE3_76
)

# BE3_85(중강도 신체활동 여부: 여가) 빈도 확인
#df_003 %>% count(BE3_85)

# BE3_86(중강도 신체활동 일수: 여가) 빈도 확인
#df_003 %>% count(BE3_86)

# BE3_87(중강도 신체활동 시간(시간): 여가) 분포 확인
#df_003 %>% pull(BE3_87) %>% describe(.)
# BE3_88(중강도 신체활동 시간(분): 여가) 분포 확인
#df_003 %>% pull(BE3_88) %>% describe(.)

# BE3_87 및 BE3_88 '해당없음' 처리 및
# mod_t2(여가 관련 중강도 신체활동 시간) 변수 생성
df_003 <- df_003 %>% mutate(
  BE3_87 = if_else(BE3_87 == 88, 0 , BE3_87),
  BE3_88 = if_else(BE3_88 == 88, 0 , BE3_88),
  mod_t2 = (BE3_87 * 60 + BE3_88 * 1) * BE3_86
)

# BE3_91(신체활동 여부: 장소이동) 빈도 확인
#df_003 %>% count(BE3_91)

# BE3_91(신체활동 여부: 장소이동) 모름·무응답 처리
df_003 <- df_003 %>% filter(BE3_91 != 9)

# BE3_92(신체활동 일수: 장소이동) 빈도 확인
#df_003 %>% count(BE3_92)

# BE3_92(신체활동 일수: 장소이동) 모름·무응답 제거
df_003 <- df_003 %>% filter(BE3_92 != 9)

# BE3_93(신체활동 시간(시간): 장소이동) 분포 확인
#df_003 %>% pull(BE3_93) %>% describe(.)
# BE3_94(신체활동 시간(분): 장소이동) 분포 확인
#df_003 %>% pull(BE3_94) %>% describe(.)

# BE3_93 및 BE3_94 '무응답' 제거 및 해당없음' 처리
# walk_t2(장소이동 신체활동 시간) 변수 생성
df_003 <- df_003 %>% filter(BE3_93 != 99 & BE3_94 != 99) %>% mutate(
  BE3_93 = if_else(BE3_93 == 88, 0 , BE3_93),
  BE3_94 = if_else(BE3_94 == 88, 0 , BE3_94),
  walk_t2 = (BE3_93 * 60 + BE3_94 * 1) * BE3_92
)

# pa_aerobic(유산소실천여부) 변수 생성(0: 비실천, 1: 실천)
# 기준: 중강도 신체활동 150분 이상, 고강도 신체활동 75분 이상,
#       고강도 1분을 중강도 2분으로 환산한 총 신체활동량이 150분 이상
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_003 <- df_003 %>% mutate(
  vig_t = rowSums(across(c(vig_t1, vig_t2))),
  vig_tt2 = vig_t * 2,
  sum_mw = rowSums(across(c(mod_t1, walk_t2, mod_t2))),
  hour_vm = rowSums(across(c(vig_tt2, sum_mw))),
  pa_aerobic = if_else(sum_mw >= 150 | vig_t >= 75 | hour_vm >= 150, 1, 0)
) 

# BP1(평소 스트레스 인지 정도) 빈도 확인
df_003 %>% count(BP1)

# 'BP1' 변수 이분형 범주로 재조정 후 BP1_binary 변수 생성
# 0 = 적게인지, 1 = 많이인지
df_003 <- df_003 %>% mutate(
  BP1_binary = ifelse(BP1 >= 3, 0, 1)
)

# 범불안장애 선별도구(GAD-7) 관련 빈도 확인
#df_003 %>% count(BP_GAD_1)

# '모름, 무응답'(9) 제거
df_003 <- df_003 %>% filter(BP_GAD_1 != 9)

#df_003 %>% count(BP_GAD_2)
#df_003 %>% count(BP_GAD_3)

# '모름, 무응답'(9) 제거
df_003 <- df_003 %>% filter(BP_GAD_3 != 9)

#df_003 %>% count(BP_GAD_4)
#df_003 %>% count(BP_GAD_5)
#df_003 %>% count(BP_GAD_6)
#df_003 %>% count(BP_GAD_7)

# GAD-7 점수(mh_GAD_S) 변수 생성
# GAD-7 점수 이분형 범주로 재조정 후 GAD_binary 변수 생성
# (이번 연구에서는 6점 이상을 위험군으로 정의)
# 0 = 양호(5점 이하), 1 = 위험(6점 이상)
df_003 <- df_003 %>% mutate(
  mh_GAD_S = BP_GAD_1 + BP_GAD_2 + BP_GAD_3 + 
    BP_GAD_4 + BP_GAD_5 + BP_GAD_6 + 
    BP_GAD_7,
    mh_GAD_binary = if_else(mh_GAD_S < 6, 0, 1)
)

# 체질량지수(HE_BMI) 분포 확인 
#df_003 %>% pull(HE_BMI) %>% describe(.)

# 체질량지수(HE_BMI) 결측치 제거
df_003 <- df_003 %>% filter(!is.na(HE_BMI))

# obesity_binary(비만여부) 변수 생성
# 0. 비비만(BMI<25), 1.비만(BMI>=25)
# 지표생성 활용자료: "국민건강영양조사 제9기(2022-2024)
df_003 <- df_003 %>% mutate(
  obesity_binary = if_else(HE_BMI<25, 0, 1)
)

# obesity_binary(비만여부) 빈도 확인
#df_003 %>% count(obesity_binary)

# 고혈압 의사진단 여부(DI1_dg) 빈도 확인
#df_003 %>% count(DI1_dg)

# 이상지질혈증 의사진단 여부(DI2_dg) 빈도 확인
#df_003 %>% count(DI2_dg)

# 주관적 체형인식(BO1) 빈도 확인
# 설문: 현재 본인의 체형이 어떻다고 생각하십니까?
#df_003 %>% count(BO1)

# 주관적 체형인식 model1 및 model2 생성
# model1 - 0(마른편): (BO1 응답 1, 2, 3), 1(뚱뚱한편): (BO1 응답: 4, 5)
# model2 - 0(마른편): (BO1 응답 1, 2), 1(뚱뚱한편): (BO1 응답: 3, 4, 5)
df_003 <- df_003 %>% mutate(
  model1 = ifelse(BO1 %in% c(1,2,3), 0, 1),
  model2 = ifelse(BO1 %in% c(1,2), 0, 1)
)

# model1 및 model2 빈도 확인
#df_003 %>% count(model1); df_003 %>% count(model2)

# model1 및 model2 별 연령별 분포 확인
#df_003 %>% group_by(model1) %>% summarise(describe(age))
#df_003 %>% group_by(model2) %>% summarise(describe(age))

# age_binary 생성
# 0 = 45세 이하, 1 = 46세 이상상
df_003 <- df_003 %>% mutate(age_binary = if_else(age >= 46, 1, 0))

# 분석용 데이터프레임 생성('성별', '교육수준_이분형', 
# '결혼여부', '동/읍면 구분', '경제활동 상태', '월평균 가구총소득', 
# '흡연자', '고위험음주여부', '유산소신체활동여부', '스트레스인지_이분형',
# 'GAD_이분형', '비만여부', '고혈압 의사진단 여부', '이상지질혈증 의사진단 여부', 
# '연령구분', '당뇨병 의사진단 여부', 'model1', 'model2'
# '건강설문-검진조사 가중치')
df_004 <- df_003 %>% select(sex, edu_binary, marri_1, town_t, EC1_1, ainc_binary, 
                            smoker, dr_high, pa_aerobic, BP1_binary, mh_GAD_binary, 
                            obesity_binary, DI1_dg, DI2_dg, age_binary, DE1_dg, model1, model2,
                            wt_itvex)

# 각 변수에 범주명 부여
# 참고자료: 국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서
df_fit_001 <- df_004 %>% mutate(
  sex = factor(sex, levels = c(1, 2), labels = c('남자', '여자')),
  edu_binary = factor(edu_binary, levels = c(0, 1), labels = c('고졸이하', '대졸이상')),
  marri_1 = factor(marri_1, levels = c(1, 2), labels = c('기혼', '미혼')),
  town_t = factor(town_t, levels = c(1, 2), labels = c('동', '읍·면')),
  EC1_1 = factor(EC1_1, levels = c(1, 2), labels = c('예', '아니요')),
  ainc_binary = factor(ainc_binary, levels = c(0, 1), labels = c('미만', '이상')),
  smoker = factor(smoker, levels = c(0, 1), labels = c('비흡연자', '흡연자')),
  dr_high = factor(dr_high, levels = c(0, 1), labels = c('정상군', '고위험군')),
  pa_aerobic = factor(pa_aerobic, levels = c(0, 1), labels = c('비실천', '실천')),
  BP1_binary = factor(BP1_binary, levels = c(0, 1), labels = c('적게인지', '많이인지')),
  mh_GAD_binary = factor(mh_GAD_binary, levels = c(0, 1), labels = c('양호', '위험')),
  obesity_binary = factor(obesity_binary, levels= c(0, 1), labels = c('비만전', '비만')),
  DI1_dg = factor(DI1_dg, levels = c(0, 1), labels = c('없음', '있음')),
  DI2_dg = factor(DI2_dg, levels = c(0, 1), labels = c('없음', '있음')),
  age_binary = factor(age_binary, levels = c(0, 1), labels = c('45이하', '46이상')),
  DE1_dg = factor(DE1_dg, levels = c(0, 1), labels = c('없음', '있음')),
  model1 = factor(model1, levels = c(0, 1), labels = c('비비만', '비만')),
  model2 = factor(model2, levels = c(0, 1), labels = c('마른편', '보통이상'))
) %>% rename(
  성별 = sex,
  교육수준 = edu_binary,
  결혼여부 = marri_1,
  거주지역 = town_t,
  경제활동상태 = EC1_1,
  중위가구소득 = ainc_binary,
  흡연자 = smoker,
  고위험음주여부 = dr_high,
  유산소실천여부 = pa_aerobic,
  스트레스인지 = BP1_binary,
  범불안장애위험 = mh_GAD_binary,
  비만여부 = obesity_binary,
  고혈압의사진단여부 = DI1_dg,
  이상지질혈증의사진단여부 = DI2_dg,
  연령구분 = age_binary,
  당뇨병의사진단여부 = DE1_dg
)

# 분석 데이터 전처리 후, 분석집단 데이터 출력
library(readr)

write_excel_csv(df_fit_001, file.path(path_001,paste0('analyze_target_',format(Sys.Date(),format='%y%m%d'),'.csv')))
