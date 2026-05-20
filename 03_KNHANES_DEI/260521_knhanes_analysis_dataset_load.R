rm(list=ls())
library(tidyverse)
library(fs)
library(vroom)

# 분석용 csv파일 불러오기
path_001 <- 'D:/github'
df_list_001 <- dir_ls(path = path_001, regexp = 'analyze_target.*\\.csv$')

df_001 <- vroom(df_list_001)

# 분석용 데이터 프레임 생성
md_001 <- df_001 %>% select(-model2)
md_002 <- df_001 %>% select(-model1)