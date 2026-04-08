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

# 순번 열에 대해 속성 변환 후 '순번_01'로 변환
df_05<-df_04%>%mutate(순번_01=as.character(순번))

# 각 열값 속성 확인
class(df_05$순번);class(df_05$순번_01)
# 출력결과
#[1] "integer"
#[1] "character"

# 주소가 '분당구'인 데이터 추출
df_06<-df_05%>%filter(주소 == "분당구")

# 출력 확인
head(df_06)
#순번   주소 수입 성별 순번_01
#1    1 분당구  450 여성       1
#2    7 분당구  337 남성       7
#3    8 분당구  388 여성       8
#4   10 분당구  324 남성      10
#5   11 분당구  470 여성      11
#6   12 분당구  403 남성      12
