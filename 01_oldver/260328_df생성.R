rm(list=ls())
library(dplyr)

#  임의의 표본 500개 생성
idx_01<-c(
  1:500
  )
#  주소구성: "수정구", "분당구", "중원구"
address_01<-sample(
  c("수정구","분당구","중원구"),size=length(idx_01),replace=TRUE
  )
#  수입범위: 240~500
income_01<-sample(
  c(240:500),size=length(idx_01),replace=TRUE
  )

df_01<-data.frame(
  식별=idx_01,주소=address_01,수입=income_01
  )
