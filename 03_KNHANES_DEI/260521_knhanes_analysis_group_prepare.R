rm(list=ls())
library(fs)
library(vroom)
library(tidyverse)

# 분석용 csv파일 불러오기
path_001 <- 'D:/github'
df_list_001 <- dir_ls(path = path_001, regexp = 'analyze_target.*\\.csv$')

df_001 <- vroom(df_list_001)

# '당뇨병의사진단여부'에 따른 분석집단 구분
# '당뇨병의사진단여부'에 따른 응답 빈도 비교
df_001 %>% count(당뇨병의사진단여부)
# A tibble: 2 × 2
#  당뇨병의사진단여부     n
#  <chr>              <int>
#1 없음                9980
#2 있음                 806

# df_diabetes_001: 당뇨병의사진단여부="있음" 응답자
df_diabetes_001 <- 
  df_001 %>% filter(당뇨병의사진단여부=="있음") %>%
# '당뇨병의사진단여부' 열 제거
  select(-당뇨병의사진단여부)
nrow(df_diabetes_001)
# [1] 806

# df_non_diabetes_001: 당뇨병의사진단여부="없음" 응답자
df_non_diabetes_001 <- df_001 %>% filter(당뇨병의사진단여부=="없음") %>%
# '당뇨병의사진단여부' 열 제거
  select(-당뇨병의사진단여부)

nrow(df_non_diabetes_001)
# [1] 9980