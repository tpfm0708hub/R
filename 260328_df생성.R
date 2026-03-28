rm(list=ls())
library(dplyr)

idx_01<-c(
  1:500
  )
address_01<-sample(
  c("수정구","분당구","중원구"),size=length(idx_01),replace=TRUE
  )
income_01<-sample(
  c(240:500),size=length(idx_01),replace=TRUE
  )

df_01<-data.frame(
  순번=idx_01,주소=address_01,수입=income_01
  )
