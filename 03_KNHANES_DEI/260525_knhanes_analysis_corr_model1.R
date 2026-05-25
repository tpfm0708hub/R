rm(list=ls())
library(fs)
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
library(psych)# corr.test()

summary_cor <- function(df_01, col_01){
  df_02 <- df_01[, c(col_01, col_list_01)] %>%
    # 상관분석 진행을 위해 숫자형으로 변환
    mutate(across(everything(), ~as.numeric(as.factor(.))))
  
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
cor_df_non_diabetes_md1_fat<-summary_cor(df_diabetes_001, 'model1')
print(data.frame(cor_df_non_diabetes_md1_fat) %>% select(1))
#                          model1
#model1                       1**
#성별                     -0.11**
#연령구분                  0.17**
#교육수준                   -0.03
#결혼여부                 -0.13**
#거주지역                    0.06
#경제활동상태                0.01
#중위가구소득               -0.02
#흡연자                      0.02
#고위험음주여부              0.03
#유산소실천여부              0.03
#스트레스인지              0.09**
#범불안장애위험            -0.07*
#비만여부                   0.6**
#고혈압의사진단여부       -0.13**
#이상지질혈증의사진단여부 -0.09**

# 일반집단
cor_df_non_diabetes_md1_not <-summary_cor(df_non_diabetes_001, 'model1')
print(data.frame(cor_df_non_diabetes_md1_not) %>% select(1))
#                          model1
#model1                       1**
#성별                           0
#연령구분                       0
#교육수준                       0
#결혼여부                  0.06**
#거주지역                 -0.03**
#경제활동상태              -0.02*
#중위가구소득                0.01
#흡연자                      0.02
#고위험음주여부            0.04**
#유산소실천여부            0.03**
#스트레스인지              0.06**
#범불안장애위험           -0.03**
#비만여부                   0.6**
#고혈압의사진단여부       -0.13**
#이상지질혈증의사진단여부  -0.1**