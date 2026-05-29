rm(list=ls())
library(fs)
library(car)# vif()
library(survey)#  svydesign
library(psych)# corr.test()
library(vroom)
library(broom)# tidy()
library(stringr)
library(tidyverse)
library(formattable)

# 분석용 csv파일 불러오기
path_001 <- 'D:/github'
df_list_001 <- dir_ls(path = path_001, regexp = 'analyze_target.*\\.csv$')

df_001 <- vroom(df_list_001)

# '당뇨병의사진단여부'에 따른 분석집단 구분
# '당뇨병의사진단여부'에 따른 응답 빈도 비교
#df_001 %>% count(당뇨병의사진단여부)

# df_diabetes_001: 당뇨병의사진단여부="있음" 응답자
df_diabetes_001 <- 
  df_001 %>% filter(당뇨병의사진단여부=="있음") %>%
# '당뇨병의사진단여부' 열 제거
  select(-당뇨병의사진단여부)
#nrow(df_diabetes_001)

# df_non_diabetes_001: 당뇨병의사진단여부="없음" 응답자
df_non_diabetes_001 <- df_001 %>% filter(당뇨병의사진단여부=="없음") %>%
# '당뇨병의사진단여부' 열 제거
  select(-당뇨병의사진단여부)
#nrow(df_non_diabetes_001)

# 변수 및 요인별 빈도수 정리 함수 생성
summary_freq <- function(col_01, col_02, 
                         group_01, group_02,
                         df_01, df_02, df_03){
  df_freq_01 <- df_01 %>% count(.data[[col_01]]) %>% mutate(
                !!group_01 := paste0(comma(n, digits = 0), '(', round(n/sum(n)*100, 2), '%)')
                 ) %>% select(-n)
  df_freq_02 <- df_02 %>% count(.data[[col_01]]) %>% mutate(
                !!group_02 := paste0(comma(n, digits = 0), '(', round(n/sum(n)*100, 2), '%)')
                 ) %>% select(-n)
  df_freq_03 <- df_freq_01 %>% left_join(df_freq_02, by=col_01) %>% 
                rename(요인 = all_of(col_01)) %>% mutate(변수명 = col_01) %>% 
                select(변수명, 요인, all_of(c(group_01, group_02)))
  df_chidq_01 <- df_03 %>% select(all_of(c(col_01, col_02))) %>% table() %>%
                 # pearson 카이제곱 적용
                 chisq.test(correct = FALSE) 
  df_chidq02 <- df_chidq_01 %>% tidy() %>% select(`X-squared`='statistic', `p-value` = `p.value`) %>%
                 mutate(
                   `X-squared` = sprintf('%.2f', `X-squared`),
                   `p-value` = case_when(
                   `p-value` >= 0.05 ~ '> 0.05',
                   `p-value` >= 0.01 ~ '< 0.05',
                   TRUE ~ '< 0.01'
                   ))
  df_fit_01 <- bind_cols(df_freq_03, df_chidq02)
  
  return(df_fit_01)
}

col_list_01 <- c(
  '성별', '연령구분', '교육수준', '결혼여부', '거주지역',
  '경제활동상태', '중위가구소득', '흡연자', '고위험음주여부',
  '유산소실천여부', '스트레스인지', '범불안장애위험', '비만여부',
  '고혈압의사진단여부', '이상지질혈증의사진단여부'
)

# 각 변수별 교차분석 진행
result_chisq_01 <- data.frame(
  map_dfr(col_list_01, function(x){
    summary_freq(x, '당뇨병의사진단여부', 
                 '당뇨경험집단', '일반집단',
                 df_diabetes_001, df_non_diabetes_001, df_001)
  }), check.names = FALSE)
#print(result_chisq_01)

# model1에 따른 변수별 교차분석 진행
# (model1: 주관적 체형인식: 비만(응답 4,5)/비비만(응답 1,2,3))

# 당뇨경험집단
df_diabetes_md1_fat <- df_diabetes_001 %>% filter(model1 == '비만')
df_diabetes_md1_not <- df_diabetes_001 %>% filter(model1 == '비비만')

result_chisq_02 <- data.frame(
  map_dfr(col_list_01, function(x){
    summary_freq(x, 'model1', 
                 '체형인식_비만', '체형인식_비비만',
                 df_diabetes_md1_fat, df_diabetes_md1_not, df_diabetes_001)
  }), check.names = FALSE)
#print(result_chisq_02)

# 일반집단
df_non_diabetes_md1_fat <- df_non_diabetes_001 %>% filter(model1 == '비만')
df_non_diabetes_md1_not <- df_non_diabetes_001 %>% filter(model1 == '비비만')

result_chisq_03 <- data.frame(
    map_dfr(col_list_01, function(x){
    summary_freq(x, 'model1',
                 '체형인식_비만', '체형인식_비비만',
                 df_non_diabetes_md1_fat, df_non_diabetes_md1_not, df_non_diabetes_001
                 )
    }), check.names = FALSE)
#print(result_chisq_03)

# model2에 따른 변수별 교차분석 진행
# (model2: 주관적 체형인식: 보통이상(응답 3,4,5)/마른편(응답 1,2,3))

# 당뇨경험집단
df_diabetes_md2_norm <- df_diabetes_001 %>% filter(model2 == '보통이상')
df_diabetes_md2_thin <- df_diabetes_001 %>% filter(model2 == '마른편')

result_chisq_04 <- data.frame(
  map_dfr(col_list_01, function(x){
  summary_freq(x, 'model2',
               '체형인식_보통이상', '체형인식_마른편',
               df_diabetes_md2_norm, df_diabetes_md2_thin, df_diabetes_001
               )
  }), check.names = FALSE)
#print(result_chisq_04)

# 일반집단
df_non_diabetes_md2_norm <- df_non_diabetes_001 %>% filter(model2 == '보통이상')
df_non_diabetes_md2_thin <- df_non_diabetes_001 %>% filter(model2 == '마른편')

result_chisq_05 <- data.frame(
  map_dfr(col_list_01, function(x){
  summary_freq(x, 'model2',
               '체형인식_보통이상', '체형인식_마른편',
               df_non_diabetes_md2_norm, df_non_diabetes_md2_thin, df_non_diabetes_001
              )
  }), check.names = FALSE)
#print(result_chisq_05)

# 싱관분석 진행 위한 함수 생성
summary_cor <- function(df_01, col_01){
  df_02 <- df_01[, c(col_01, col_list_01)] %>%
           mutate(
             성별 = as.numeric(ifelse(성별 == '남자', 0, 1)),
             교육수준 = as.numeric(ifelse(교육수준 == '고졸이하', 0, 1)),
             결혼여부 = as.numeric(ifelse(결혼여부=='미혼', 0, 1)),
             거주지역 = as.numeric(ifelse(거주지역=='동', 0, 1)),
             경제활동상태 = as.numeric(ifelse(경제활동상태=='아니요', 0, 1)),
             중위가구소득 = as.numeric(ifelse(중위가구소득=='미만', 0, 1)),
             흡연자 = as.numeric(ifelse(흡연자=='비흡연자', 0, 1)),
             고위험음주여부 = as.numeric(ifelse(고위험음주여부=='정상군', 0, 1)),
             유산소실천여부 = as.numeric(ifelse(유산소실천여부=='비실천', 0, 1)),
             스트레스인지 = as.numeric(ifelse(스트레스인지=='적게인지', 0, 1)),
             범불안장애위험 = as.numeric(ifelse(범불안장애위험=='양호', 0, 1)),
             비만여부 = as.numeric(ifelse(비만여부=='비만전', 0, 1)),
             고혈압의사진단여부 = as.numeric(ifelse(고혈압의사진단여부=='없음', 0, 1)),
             이상지질혈증의사진단여부 = as.numeric(ifelse(이상지질혈증의사진단여부=='없음', 0, 1)),
             연령구분 = as.numeric(ifelse(연령구분=='45이하', 0, 1)),
             )
  if (col_01 == 'model1'){
    df_02 <- df_02 %>% mutate(model1 = as.numeric(ifelse(model1=='비비만', 0, 1)))
  } else if (col_01 == 'model2'){
    df_02 <- df_02 %>% mutate(model2 = as.numeric(ifelse(model2=='마른편', 0, 1)))
  }
  
    # 상관분석 진행을 위해 숫자형으로 변환
#    mutate(across(everything(), ~as.numeric(as.factor(.))))
  
  # corr.test(): 상관분석 진행행
  # suppressWarnings(): 한글 열 이름으로 인한 경고문 처리
  df_03 <- suppressWarnings(corr.test(df_02))
  
  df_r_01 <- round(df_03$r, 2)
  df_p_01 <- df_03$p
  
  df_p_02 <- ifelse(
    df_p_01 < 0.01, '**',
    ifelse(df_p_01 < 0.05, '*', '')
    )
  
  df_result_01 <- matrix(
    paste0(df_r_01, df_p_02),
    nrow = nrow(df_r_01),
    ncol = ncol(df_p_01)
  )
  
  rownames(df_result_01) <- rownames(df_r_01)
  colnames(df_result_01) <- colnames(df_r_01)
  
  df_result_02 <- data.frame(df_result_01)

  return(df_result_02)}

# model1에 따른 변수별 상관분석 진행
# 당뇨경험집단
cor_df_diabetes_md1_fat<-summary_cor(df_diabetes_001, 'model1')
#print(data.frame(cor_df_diabetes_md1_fat) %>% select(1))

# 일반집단
cor_df_non_diabetes_md1_not <-summary_cor(df_non_diabetes_001, 'model1')
#print(data.frame(cor_df_non_diabetes_md1_not) %>% select(1))

# model1에 따른 변수별 상관분석 진행
# 당뇨경험집단
cor_df_diabetes_md2_fat <- summary_cor(df_diabetes_001, 'model2')
#print(data.frame(cor_df_diabetes_md2_fat) %>% select(1))

# 일반집단
cor_df_non_diabetes_md2_not <-summary_cor(df_non_diabetes_001, 'model2')
#print(data.frame(cor_df_non_diabetes_md2_not) %>% select(1))

# 로지스틱 다중공선성 및 로지스틱 회귀분석 진행
# 분석용 데이터 프레임 생성
set_df <- function(df_01, col_01){
  df_02 <- df_01 %>% mutate(
    성별 = factor(성별, levels = c('남자', '여자')),
    교육수준 = factor(교육수준, levels = c('고졸이하', '대졸이상')),
    결혼여부 = factor(결혼여부, levels = c('미혼', '기혼')),
    거주지역 = factor(거주지역, levels = c('동', '읍·면')),
    경제활동상태 = factor(경제활동상태, levels = c('아니요', '예')),
    중위가구소득 = factor(중위가구소득, levels = c('미만', '이상')),
    흡연자 = factor(흡연자, levels = c('비흡연자', '흡연자')),
    고위험음주여부 = factor(고위험음주여부, levels = c('정상군', '고위험군')),
    유산소실천여부 = factor(유산소실천여부, levels = c('비실천', '실천')),
    스트레스인지 = factor(스트레스인지, levels= c('적게인지', '많이인지')),
    범불안장애위험 = factor(범불안장애위험, levels = c('양호', '위험')),
    비만여부 = factor(비만여부, levels= c('비만전', '비만')),
    고혈압의사진단여부 = factor(고혈압의사진단여부, levels = c('없음', '있음')),
    이상지질혈증의사진단여부 = factor(이상지질혈증의사진단여부, levels = c('없음', '있음')),
    연령구분 = factor(연령구분, levels = c('45이하', '46이상'))
  )
  if(col_01 == 'model1') {
    df_02 <-  df_02 %>% mutate(model1 = factor(model1, levels = c('비비만', '비만')))
  } else if(col_01 == 'model2') {
    df_02 <-  df_02 %>% mutate(model2 = factor(model2, levels = c('마른편', '보통이상')))
  }
  return(df_02)
}

# 다중공선성 기준(VIF 5이상)
summary_vif <- function(df_01, col_01){
  df_02 <- formula(paste(col_01 ,'~', paste(col_list_01, collapse = ' + ')))  
  sum_cov_01 <- glm(df_02, data = df_01, family = quasibinomial(link = 'logit'), weights = wt_itvex)
  car::vif(sum_cov_01)
}

# 모델에 따른 당뇨경험집단 다중공선성 분석
# model1
# 당뇨경험집단
#summary_vif(set_df(df_diabetes_001, 'model1'), 'model1')
# 일반집단
#summary_vif(df_non_diabetes_001, 'model1')

# model2
# 당뇨경험집단
#summary_vif(df_diabetes_001, 'model2')
# 일반집단
#summary_vif(df_non_diabetes_001, 'model2')

# 로지스틱 회귀분석
# 비만여부 제거: 체형인식(model1 및 model2)와 개념적으로 밀접
summary_glm <- function(df_01, col_01){
    # 로지스틱 회귀분석: 가중치 반영
    md_svy <- svydesign(ids = ~1, weights = ~wt_itvex, data = df_01)
    df_02 <- formula(paste(col_01, '~', paste(col_list_01[-match('비만여부', col_list_01)], collapse = '+')))
    
    sum_glm_01 <- svyglm(df_02, design = md_svy, family = quasibinomial(link = 'logit'))
    sum_glm_02 <- tidy(sum_glm_01) %>% mutate(
                  OR = exp(estimate),
                  p_value = case_when(
                    p.value < 0.001 ~ '< 0.001',
                    p.value < 0.01  ~ sprintf('%.3f', p.value),
                    TRUE ~ sprintf('%.2f', p.value)),
                  CI_low = exp(estimate - 1.96 * std.error),
                  CI_high = exp(estimate + 1.96 * std.error)
                  ) %>% select('Independent variables' = term, OR, p_value, CI_high, CI_low)
    return(sum_glm_02)
}

# model1 로지스틱 회귀분석(비만여부 제외)
# 당뇨경험집단
summary_glm(set_df(df_diabetes_001, 'model1') %>% select(-비만여부), 'model1')

# 일반 집단
summary_glm(set_df(df_non_diabetes_001, 'model1') %>% select(-비만여부), 'model1')

# model2 로지스틱 회귀분석(비만여부 제외)
# 당뇨경험집단
summary_glm(set_df(df_diabetes_001, 'model2') %>% select(-비만여부), 'model2')
#   `Independent variables`          OR p_value CI_high CI_low
#   <chr>                         <dbl> <chr>     <dbl>  <dbl>
# 1 (Intercept)                  32.4   < 0.001 209.    5.01  
# 2 성별여자                      2.76  0.01      6.15  1.24  
# 3 연령구분46이상                0.186 0.05      0.980 0.0351
# 4 교육수준대졸이상              1.61  0.11      2.89  0.902 
# 5 결혼여부기혼                  0.291 0.07      1.11  0.0761
# 6 거주지역읍·면                 0.966 0.91      1.79  0.523 
# 7 경제활동상태예                1.94  0.08      4.03  0.934 
# 8 중위가구소득이상              1.02  0.96      1.84  0.561 
# 9 흡연자흡연자                  0.828 0.54      1.50  0.457 
#10 고위험음주여부고위험군        1.11  0.79      2.39  0.515 
#11 유산소실천여부실천            0.957 0.88      1.69  0.541 
#12 스트레스인지많이인지          1.07  0.86      2.10  0.540 
#13 범불안장애위험위험            0.898 0.83      2.40  0.336 
#14 고혈압의사진단여부있음        2.43  0.002     4.26  1.38  
#15 이상지질혈증의사진단여부있음  1.21  0.51      2.11  0.692 

# 일반 집단
summary_glm(set_df(df_non_diabetes_001, 'model2') %>% select(-비만여부), 'model2')
#   `Independent variables`         OR p_value CI_high CI_low
#   <chr>                        <dbl> <chr>     <dbl>  <dbl>
# 1 (Intercept)                  3.28  < 0.001   4.13   2.60 
# 2 성별여자                     1.30  < 0.001   1.52   1.12 
# 3 연령구분46이상               0.805 0.01      0.954  0.679
# 4 교육수준대졸이상             1.08  0.26      1.25   0.941
# 5 결혼여부기혼                 1.60  < 0.001   1.90   1.35 
# 6 거주지역읍·면                1.12  0.28      1.38   0.911
# 7 경제활동상태예               1.29  0.001     1.51   1.11 
# 8 중위가구소득이상             1.06  0.39      1.23   0.924
# 9 흡연자흡연자                 0.806 0.01      0.956  0.680
#10 고위험음주여부고위험군       1.03  0.74      1.26   0.849
#11 유산소실천여부실천           1.21  0.007     1.40   1.05 
#12 스트레스인지많이인지         0.991 0.91      1.17   0.840
#13 범불안장애위험위험           1.05  0.66      1.29   0.853
#14 고혈압의사진단여부있음       2.62  < 0.001   3.53   1.94 
#15 이상지질혈증의사진단여부있음 1.39  0.01      1.78   1.08 