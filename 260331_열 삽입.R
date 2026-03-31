rm(list=ls())
library(dplyr)
library(stringr)

#  임의의 표본 500개 생성
idx_01<-c(
  1:500
  )

# seed값 설정
set.seed(1234)

#  주소구성: "수정구", "분당구", "중원구"
address_01<-sample(
  c("수정구","분당구","중원구"),size=length(idx_01),replace=TRUE
  )

# seed값 설정
set.seed(2345)

#  수입범위: 240~500
income_01<-sample(
  c(240:500),size=length(idx_01),replace=TRUE
  )

df_01<-data.frame(
  순번=idx_01,주소=address_01,수입=income_01
  )

df_02<-df_01%>%mutate(
  # 주소: 25개 행(500*5%)에 결측 삽입
  주소=replace(주소,sample(
    n(),size=round(n()*0.05)),NA
  ),
  # 수입: 30개 행(500*6%)에 결측 삽입
  수입=replace(수입,sample(
    n(),size=round(n()*0.06)),NA
  )
)

# df_01과 동일한 구조 생성
tbl_01<-tibble(
  순번=c(1:500),
  주소=address_01,수입=income_01
  )

# df_02와 같은 열 기준으로 병합 
df_03<-bind_rows(df_02,tbl_01)

set.seed(3456)

# df_03 행수 만큼의 성별 열 생성
tbl_02<-tibble(
  성별=sample(
    c('남성','여성'),size=nrow(df_03),replace=TRUE
  )
)

# 기존 데이터와 열병합
df_04<-bind_cols(df_03,tbl_02)
