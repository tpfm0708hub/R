rm(list=ls())
library(dplyr)
library(stringr)

# d드라이브 내 "260403_파일출력.R" 불러오기
df_001 <- readRDS(file.path('D:/github/260403_파일출력.R'))

# 데이터 구조 확인
str(df_001)

# 결츨 미포함
table(df_001$주소)
# 분당구 수정구 중원구 
# 337    292    346
# 결측 포함
df_001%>%group_by(주소)%>%summarise(n=n())
# 주소       n
# <chr>  <int>
# 1 분당구   337
# 2 수정구   292
# 3 중원구   346
# 4 NA        25

# 필요 library 호출
library(psych)
# '수입' 변수 기술통계량 확인
describe(df_001$수입)
