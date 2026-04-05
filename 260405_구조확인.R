rm(list=ls())
library(dplyr)
library(stringr)

# d드라이브 내 "260403_파일출력.R" 불러오기
df_001 <- readRDS(file.path('D:/github/260403_파일출력.R'))

# 데이터 구조 확인
str(df_001)