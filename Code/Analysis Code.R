options("scipen"= 100)

library("readxl")
library("dplyr")
library(foreign)
library(MASS)
library(dplyr)


##### 0. 데이터 로드


### 0-1. 학교정보공시 에듀데이터 로드(15년~19년)

data <- read_excel(path = "0218. 학교용지현황(15-19)(70%) 수정.xlsx", col_names = T)

data1 <- read_excel(path = "0222. 학교폭력가해학생선도교육조치현황(15-19)(70%) 수정.xlsx", col_names = T)

data2 <- read_excel(path = "0223. 학교폭력가해학생특별교육현황(15-19) 수정.xlsx", col_names = T)


data5 <- read_excel(path = "0232. 학교폭력예방교육실적_학생대상(15-19)(70%) 수정.xlsx", col_names = T)

data7 <- read_excel(path = "0237. 학년별학급별학생현황_고(15-19)(70%) 수정.xlsx", col_names = T)

data8 <- read_excel(path = "0242. 학생학부모상담실적(15-19)(70%) 수정.xlsx", col_names = T)

data9 <- read_excel(path = "0313. 전출입및학업중단현황_고특(15-19)(70%) 수정.xlsx", col_names = T)

data10 <- data10 %>% select(공시년도, 층화추출학교ID, 학교급명) %>% 
  group_by(공시년도, 층화추출학교ID) %>% filter(층화추출학교ID %in% f4$층화추출학교ID)


nrow(data) ; nrow(data1) ; nrow(data2) ; nrow(data5) ; nrow(data7) ; nrow(data8)



da0 <- read_excel(path = "0230. 학교폭력실태조사피해현황(15)(70%).xlsx", col_names = T)

da1 <- read_excel(path = "0230. 학교폭력실태조사피해현황(16)(70%).xlsx", col_names = T)

da2 <- read_excel(path = "0230. 학교폭력실태조사피해현황(17)(70%).xlsx", col_names = T)

da3 <- read_excel(path = "0230. 학교폭력실태조사피해현황(18)(70%).xlsx", col_names = T)

da4 <- read_excel(path = "0230. 학교폭력실태조사피해현황(19)(70%).xlsx", col_names = T)



# 로드한 위 da0, da1, da2, da3, da4 데이터들을 행 결합

data4 <- rbind(da0, da1, da2, da3, da4)




### 0-2. 학업 성취도 관련 나이스 에듀데이터 로드(15년~19년)

dt1 <- read_excel(path = "0305. 교과별학업성취도_학년별(15)(70%).xlsx", col_names = T)

dt2 <- read_excel(path = "0305. 교과별학업성취도_학년별(16)(70%).xlsx", col_names = T)

dt3 <- read_excel(path = "0305. 교과별학업성취도_학년별(17)(70%).xlsx", col_names = T)

dt4 <- read_excel(path = "0305. 교과별학업성취도_학년별(18)(70%).xlsx", col_names = T)

dt5 <- read_excel(path = "0305. 교과별학업성취도_학년별(19)(70%).xlsx", col_names = T)




### 0-3. 대학수학능력 성취도 데이터 로드(15년~19년)

gr1 <- read_excel(path = "대학수학능력시험_학교(15)(70%).xlsx", col_names = T)

gr2 <- read_excel(path = "대학수학능력시험_학교(16)(70%).xlsx", col_names = T)

gr3 <- read_excel(path = "대학수학능력시험_학교(17)(70%).xlsx", col_names = T)

gr4 <- read_excel(path = "대학수학능력시험_학교(18)(70%).xlsx", col_names = T)

gr5 <- read_excel(path = "대학수학능력시험_학교(19)(70%).xlsx", col_names = T)




grade <- rbind(gr1, gr2, gr3, gr4, gr5)




##### 1. 데이터 전처리


### 1-1. 데이터 전처리 - 파생변수 생성

# 전체 처벌 종류별 처벌 횟수를 합하여 가해학생처벌수 생성

data1$가해학생처벌수 <- data1$가해학생출석정지건수 + data1$선도교육조치_가해학생학급교체건수 + 
  data1$가해학생접촉협박금지건수 + data1$가해학생서면사죄건수 +data1$가해학생전출건수 + 
  data1$가해학생심리치료건수 + data1$가해학생학교봉사건수 + data1$가해학생사회봉사건수 +  
  data1$가해학생퇴학처분건수



# 가해학생 재발 방지 교육별 횟수를 통합하여 가해방지교육건수 변수 생성

data2$가해방지교육건수 <- data2$가해학생보호자교육실시건수 + data2$가해학생교육대상건수 + 
  data2$가해학생교육실시건수 + data2$학폭가해학생_학교수



# 학교 정규, 비정규 수업시간을 통합하여 교육시간 변수 생성

data5$교육시간 <- data5$'비정규교과수업시간수(분)' + data5$'정규교과수업시간수(분)' 



# 고등학생에 재학 중인 남학생 및 여학생 수를 합하여 총 학생수 변수 생성

data7$총학생수 <- data7$고등_남학생수 + data7$고등_여학생수


# 부적응 종류별 학생수를 합하여 전체 학생 수로 나누어 부적응 비율 변수 생성

data9$부적응비율 <- (data9$고특_부적응학업중단학생수 + data9$유예학업중단학생수 +
                  data9$고특_품행학업중단학생수)/data9$전출입학업중단_고특_학생수


# 학업성취도 데이터 변수선택 및 각 성취도 등급별 학생 수를 총합하여 총학생수 생성

dt1 <- dt1 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, '학년별 성취도평가F등급학생수', '학년별 성취도평가E등급학생수',
                      '학년별 성취도평가D등급학생수', '학년별 성취도평가C등급학생수', 
                      '학년별 성취도평가B등급학생수', '학년별 성취도평가A등급학생수', 
                      '교과학년별 성취도평가평균점수')

dt2 <- dt2 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, '학년별 성취도평가F등급학생수', '학년별 성취도평가E등급학생수',
                      '학년별 성취도평가D등급학생수', '학년별 성취도평가C등급학생수', 
                      '학년별 성취도평가B등급학생수', '학년별 성취도평가A등급학생수', 
                      '교과학년별 성취도평가평균점수')

dt3 <- dt3 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, '학년별 성취도평가F등급학생수', '학년별 성취도평가E등급학생수',
                      '학년별 성취도평가D등급학생수', '학년별 성취도평가C등급학생수', 
                      '학년별 성취도평가B등급학생수', '학년별 성취도평가A등급학생수', 
                      '교과학년별 성취도평가평균점수')

dt4 <- dt4 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, '학년별 성취도평가F등급학생수', '학년별 성취도평가E등급학생수',
                      '학년별 성취도평가D등급학생수', '학년별 성취도평가C등급학생수', 
                      '학년별 성취도평가B등급학생수', '학년별 성취도평가A등급학생수', 
                      '교과학년별 성취도평가평균점수')

dt5 <- dt5 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, '학년별 성취도평가F등급학생수', '학년별 성취도평가E등급학생수',
                      '학년별 성취도평가D등급학생수', '학년별 성취도평가C등급학생수', 
                      '학년별 성취도평가B등급학생수', '학년별 성취도평가A등급학생수', 
                      '교과학년별 성취도평가평균점수')


dt1$총학생수 <- dt1$`학년별 성취도평가F등급학생수` + dt1$`학년별 성취도평가E등급학생수` + 
  dt1$`학년별 성취도평가D등급학생수` + dt1$`학년별 성취도평가C등급학생수` + 
  dt1$`학년별 성취도평가B등급학생수` + dt1$`학년별 성취도평가A등급학생수`

dt2$총학생수 <- dt2$`학년별 성취도평가F등급학생수` + dt2$`학년별 성취도평가E등급학생수` + 
  dt2$`학년별 성취도평가D등급학생수` + dt2$`학년별 성취도평가C등급학생수` + 
  dt2$`학년별 성취도평가B등급학생수` + dt2$`학년별 성취도평가A등급학생수`

dt3$총학생수 <- dt3$`학년별 성취도평가F등급학생수` + dt3$`학년별 성취도평가E등급학생수` + 
  dt3$`학년별 성취도평가D등급학생수` + dt3$`학년별 성취도평가C등급학생수` + 
  dt3$`학년별 성취도평가B등급학생수` + dt3$`학년별 성취도평가A등급학생수`

dt4$총학생수 <- dt4$`학년별 성취도평가F등급학생수` + dt4$`학년별 성취도평가E등급학생수` + 
  dt4$`학년별 성취도평가D등급학생수` + dt4$`학년별 성취도평가C등급학생수` + 
  dt4$`학년별 성취도평가B등급학생수` + dt4$`학년별 성취도평가A등급학생수`

dt5$총학생수 <- dt5$`학년별 성취도평가F등급학생수` + dt5$`학년별 성취도평가E등급학생수` + 
  dt5$`학년별 성취도평가D등급학생수` + dt5$`학년별 성취도평가C등급학생수` + 
  dt5$`학년별 성취도평가B등급학생수` + dt5$`학년별 성취도평가A등급학생수`



# 위에서 생성한 총학생수를 이용하여 각 성취도 등급 비율 변수 생성

dt1$'F비율' <-  dt1$`학년별 성취도평가F등급학생수` / dt1$총학생수
dt1$'E비율' <-  dt1$`학년별 성취도평가E등급학생수` / dt1$총학생수
dt1$'D비율' <-  dt1$`학년별 성취도평가D등급학생수` / dt1$총학생수
dt1$'C비율' <-  dt1$`학년별 성취도평가C등급학생수` / dt1$총학생수
dt1$'B비율' <-  dt1$`학년별 성취도평가B등급학생수` / dt1$총학생수
dt1$'A비율' <-  dt1$`학년별 성취도평가A등급학생수` / dt1$총학생수

dt2$'F비율' <-  dt2$`학년별 성취도평가F등급학생수` / dt2$총학생수
dt2$'E비율' <-  dt2$`학년별 성취도평가E등급학생수` / dt2$총학생수
dt2$'D비율' <-  dt2$`학년별 성취도평가D등급학생수` / dt2$총학생수
dt2$'C비율' <-  dt2$`학년별 성취도평가C등급학생수` / dt2$총학생수
dt2$'B비율' <-  dt2$`학년별 성취도평가B등급학생수` / dt2$총학생수
dt2$'A비율' <-  dt2$`학년별 성취도평가A등급학생수` / dt2$총학생수

dt3$'F비율' <-  dt3$`학년별 성취도평가F등급학생수` / dt3$총학생수
dt3$'E비율' <-  dt3$`학년별 성취도평가E등급학생수` / dt3$총학생수
dt3$'D비율' <-  dt3$`학년별 성취도평가D등급학생수` / dt3$총학생수
dt3$'C비율' <-  dt3$`학년별 성취도평가C등급학생수` / dt3$총학생수
dt3$'B비율' <-  dt3$`학년별 성취도평가B등급학생수` / dt3$총학생수
dt3$'A비율' <-  dt3$`학년별 성취도평가A등급학생수` / dt3$총학생수

dt4$'F비율' <-  dt4$`학년별 성취도평가F등급학생수` / dt4$총학생수
dt4$'E비율' <-  dt4$`학년별 성취도평가E등급학생수` / dt4$총학생수
dt4$'D비율' <-  dt4$`학년별 성취도평가D등급학생수` / dt4$총학생수
dt4$'C비율' <-  dt4$`학년별 성취도평가C등급학생수` / dt4$총학생수
dt4$'B비율' <-  dt4$`학년별 성취도평가B등급학생수` / dt4$총학생수
dt4$'A비율' <-  dt4$`학년별 성취도평가A등급학생수` / dt4$총학생수

dt5$'F비율' <-  dt5$`학년별 성취도평가F등급학생수` / dt5$총학생수
dt5$'E비율' <-  dt5$`학년별 성취도평가E등급학생수` / dt5$총학생수
dt5$'D비율' <-  dt5$`학년별 성취도평가D등급학생수` / dt5$총학생수
dt5$'C비율' <-  dt5$`학년별 성취도평가C등급학생수` / dt5$총학생수
dt5$'B비율' <-  dt5$`학년별 성취도평가B등급학생수` / dt5$총학생수
dt5$'A비율' <-  dt5$`학년별 성취도평가A등급학생수` / dt5$총학생수



# 위에서 생성한 변수들을 바탕으로 학업성취도 데이터 변수 재선택

dt1 <- dt1 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, 'F비율', 'E비율', 'D비율', 'C비율', 'B비율', 'A비율', 
                      '교과학년별 성취도평가평균점수')
dt2 <- dt2 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, 'F비율', 'E비율', 'D비율', 'C비율', 'B비율', 'A비율', 
                      '교과학년별 성취도평가평균점수')
dt3 <- dt3 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, 'F비율', 'E비율', 'D비율', 'C비율', 'B비율', 'A비율', 
                      '교과학년별 성취도평가평균점수')
dt4 <- dt4 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, 'F비율', 'E비율', 'D비율', 'C비율', 'B비율', 'A비율', 
                      '교과학년별 성취도평가평균점수')
dt5 <- dt5 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명, 
                      학기명, 편제명, 'F비율', 'E비율', 'D비율', 'C비율', 'B비율', 'A비율', 
                      '교과학년별 성취도평가평균점수')



# 편제명이 '국어, 수학, 영어' 에 속하는 데이터만 선택

dt1 <- dt1 %>% filter(편제명 %in% c('국어', '수학', '영어'))

dt2 <- dt2 %>% filter(편제명 %in% c('국어', '수학', '영어'))

dt3 <- dt3 %>% filter(편제명 %in% c('국어', '수학', '영어'))

dt4 <- dt4 %>% filter(편제명 %in% c('국어', '수학', '영어'))

dt5 <- dt5 %>% filter(편제명 %in% c('국어', '수학', '영어'))



# 고등학교에 속하는 학교들에 대한 데이터만 선택

dt1 <- dt1 %>% filter(학교급명 %in% '고등학교')

dt2 <- dt2 %>% filter(학교급명 %in% '고등학교')

dt3 <- dt3 %>% filter(학교급명 %in% '고등학교')

dt4 <- dt4 %>% filter(학교급명 %in% '고등학교')

dt5 <- dt5 %>% filter(학교급명 %in% '고등학교')



# 과목별 등급 비율에 가중치를 부여하여 학교별 각 과목 점수 변수 생성

gr_1 <- gr1[,c(1,2,3)]

gr_1$국어A점수변환 <- 9*gr1[,83] + 8*gr1[,84] + 7*gr1[,85] + 6*gr1[,86] + 5*gr1[,87] + 4*gr1[,88] + 
  3*gr1[,89] + 2*gr1[,90] + 1*gr1[,91]

gr_1$국어B점수변환 <- 9*gr1[,92] + 8*gr1[,93] + 7*gr1[,94] + 6*gr1[,95] + 5*gr1[,96] + 4*gr1[,97] + 
  3*gr1[,98] + 2*gr1[,99] + 1*gr1[,100]

gr_1$수학A점수변환 <- 9*gr1[,101] + 8*gr1[,102] + 7*gr1[,103] + 6*gr1[,104] + 5*gr1[,105] + 4*gr1[,106] + 
  3*gr1[,107] + 2*gr1[,108] + 1*gr1[,109]

gr_1$수학B점수변환 <- 9*gr1[,110] + 8*gr1[,111] + 7*gr1[,112] + 6*gr1[,113] + 5*gr1[,114] + 4*gr1[,115] + 
  3*gr1[,116] + 2*gr1[,117] + 1*gr1[,118]

gr_1$영어A점수변환 <- 9*gr1[,119] + 8*gr1[,120] + 7*gr1[,121] + 6*gr1[,122] + 5*gr1[,123] + 4*gr1[,124] + 
  3*gr1[,125] + 2*gr1[,126] + 1*gr1[,127]

gr_1$영어B점수변환 <- 9*gr1[,128] + 8*gr1[,129] + 7*gr1[,130] + 6*gr1[,131] + 5*gr1[,132] + 4*gr1[,133] + 3*gr1[,134] + 2*gr1[,135] + 1*gr1[,136]



gr_2 <- gr2[,c(1,2,3)]

gr_2$국어점수변환 <- 9*gr2[,86] + 8*gr2[,87] + 7*gr2[,88] + 6*gr2[,89] + 5*gr2[,90] + 4*gr2[,91] + 
  3*gr2[,92] + 2*gr2[,93] + 1*gr2[,94]

gr_2$수학가형점수변환 <- 9*gr2[,95] + 8*gr2[,96] + 7*gr2[,97] + 6*gr2[,98] + 5*gr2[,99] + 4*gr2[,100] + 
  3*gr2[,101] + 2*gr2[,102] + 1*gr2[,103]

gr_2$수학나형점수변환 <- 9*gr2[,104] + 8*gr2[,105] + 7*gr2[,106] + 6*gr2[,107] + 5*gr2[,108] + 4*gr2[,109] +
  3*gr2[,110] + 2*gr2[,111] + 1*gr2[,112]

gr_2$영어점수변환 <- 9*gr2[,113] + 8*gr2[,114] + 7*gr2[,115] + 6*gr2[,116] + 5*gr2[,117] + 4*gr2[,118] + 
  3*gr2[,119] + 2*gr2[,120] + 1*gr2[,121]



gr_3 <- gr3[,c(1,2,3)]

gr_3$국어점수변환 <- 9*gr3[,86] + 8*gr3[,87] + 7*gr3[,88] + 6*gr3[,89] + 5*gr3[,90] + 4*gr3[,91] + 
  3*gr3[,92] + 2*gr3[,93] + 1*gr3[,94]

gr_3$수학가형점수변환 <- 9*gr3[,95] + 8*gr3[,96] + 7*gr3[,97] + 6*gr3[,98] + 5*gr3[,99] + 4*gr3[,100] + 
  3*gr3[,101] + 2*gr3[,102] + 1*gr3[,103]

gr_3$수학나형점수변환 <- 9*gr3[,104] + 8*gr3[,105] + 7*gr3[,106] + 6*gr3[,107] + 5*gr3[,108] + 4*gr3[,109] +
  3*gr3[,110] + 2*gr3[,111] + 1*gr3[,112]

gr_3$영어점수변환 <- 9*gr3[,113] + 8*gr3[,114] + 7*gr3[,115] + 6*gr3[,116] + 5*gr3[,117] + 4*gr3[,118] + 
  3*gr3[,119] + 2*gr3[,120] + 1*gr3[,121]



gr_4 <- gr4[,c(1,4,2)]

gr_4$국어점수변환 <- 9*gr4[,86] + 8*gr4[,87] + 7*gr4[,88] + 6*gr4[,89] + 5*gr4[,90] + 4*gr4[,91] + 
  3*gr4[,92] + 2*gr4[,93] + 1*gr4[,94]

gr_4$수학가형점수변환 <- 9*gr4[,95] + 8*gr4[,96] + 7*gr4[,97] + 6*gr4[,98] + 5*gr4[,99] + 4*gr4[,100] + 
  3*gr4[,101] + 2*gr4[,102] + 1*gr4[,103]

gr_4$수학나형점수변환 <- 9*gr4[,104] + 8*gr4[,105] + 7*gr4[,106] + 6*gr4[,107] + 5*gr4[,108] + 4*gr4[,109] +
  3*gr4[,110] + 2*gr4[,111] + 1*gr4[,112]

gr_4$영어점수변환 <- 9*gr4[,113] + 8*gr4[,114] + 7*gr4[,115] + 6*gr4[,116] + 5*gr4[,117] + 4*gr4[,118] + 
  3*gr4[,119] + 2*gr4[,120] + 1*gr4[,121]



gr_5 <- gr5[,c(1,4,2)]

gr_5$국어점수변환 <- 9*gr5[,17] + 8*gr5[,16] + 7*gr5[,15] + 6*gr5[,40] + 5*gr5[,41] + 4*gr5[,66] + 
  3*gr5[,65] + 2*gr5[,64] + 1*gr5[,63]

gr_5$수학가형점수변환 <- 9*gr5[,56] + 8*gr5[,55] + 7*gr5[,54] + 6*gr5[,53] + 5*gr5[,52] + 4*gr5[,51] + 
  3*gr5[,50] + 2*gr5[,49] + 1*gr5[,48]

gr_5$수학나형점수변환 <- 9*gr5[,46] + 8*gr5[,45] + 7*gr5[,44] + 6*gr5[,43] + 5*gr5[,42] + 4*gr5[,127] + 
  3*gr5[,126] + 2*gr5[,125] + 1*gr5[,124]

gr_5$영어점수변환 <- 9*gr5[,74] + 8*gr5[,73] + 7*gr5[,72] + 6*gr5[,71] + 5*gr5[,70] + 4*gr5[,69] + 
  3*gr5[,68] + 2*gr5[,67] + 1*gr5[,39]



# 변환된 각 과목 점수 변수에 가점을 부여하여 학교별 수능 총점 점수 변수 생성


gr_1$총점 <- (gr_1$국어A점수변환 * 0.4 + gr_1$국어B점수변환 * 0.6 + gr_1$수학A점수변환 * 0.4 +
              gr_1$수학B점수변환 * 0.6 + gr_1$영어A점수변환 * 0.4 + gr_1$영어B점수변환 * 0.6)/3

gr_2$총점 <- (gr_2$국어점수변환 + gr_2$수학가형점수변환 * 0.6 + gr_2$수학나형점수변환 * 0.4 +
              gr_2$영어점수변환)/3

gr_3$총점 <- (gr_3$국어점수변환 + gr_3$수학가형점수변환 * 0.6 + gr_3$수학나형점수변환 * 0.4 +
              gr_3$영어점수변환)/3

gr_4$총점 <- (gr_4$국어점수변환 + gr_4$수학가형점수변환 * 0.6 + gr_4$수학나형점수변환 * 0.4 +
              gr_4$영어점수변환)/3

gr_5$총점 <- (gr_5$국어점수변환 + gr_5$수학가형점수변환 * 0.6 + gr_5$수학나형점수변환 * 0.4 +
              gr_5$영어점수변환)/3



# 위의 총점 데이터를 이용해 순서형 범주를 갖는 1,2,3등급 변수 생성

gr_1$등급 <- ifelse(gr_1$총점 >= quantile(as.matrix(gr_1$총점), 0.6666) , 1, 
                  ifelse(gr_1$총점 >= quantile(as.matrix(gr_1$총점), 0.3333), 2, 3))

gr_2$등급 <- ifelse(gr_2$총점 >= quantile(as.matrix(gr_2$총점), 0.6666) , 1, 
                  ifelse(gr_2$총점 >= quantile(as.matrix(gr_2$총점), 0.3333), 2, 3))

gr_3$등급 <- ifelse(gr_3$총점 >= quantile(as.matrix(gr_3$총점), 0.6666) , 1, 
                  ifelse(gr_3$총점 >= quantile(as.matrix(gr_3$총점), 0.3333), 2, 3))

gr_4$등급 <- ifelse(gr_4$총점 >= quantile(as.matrix(gr_4$총점), 0.6666) , 1, 
                  ifelse(gr_4$총점 >= quantile(as.matrix(gr_4$총점), 0.3333), 2, 3))

gr_5$등급 <- ifelse(gr_5$총점 >= quantile(as.matrix(gr_5$총점), 0.6666) , 1, 
                  ifelse(gr_5$총점 >= quantile(as.matrix(gr_5$총점), 0.3333), 2, 3))



quantile(as.matrix(gr_1$총점), 0.3333)
quantile(as.matrix(gr_1$총점), 0.6666)

quantile(as.matrix(gr_2$총점), 0.3333)
quantile(as.matrix(gr_2$총점), 0.6666)

quantile(as.matrix(gr_3$총점), 0.3333)
quantile(as.matrix(gr_3$총점), 0.6666)

quantile(as.matrix(gr_4$총점), 0.3333)
quantile(as.matrix(gr_4$총점), 0.6666)

quantile(as.matrix(gr_5$총점), 0.3333)
quantile(as.matrix(gr_5$총점), 0.6666)



# 위  1,2,3등급을 데이터로 갖는 변수를 상,중,하를 범주로 갖는 범주형 변수로 변환

grade <- grade %>% mutate(등급 = factor(등급, unique(등급), labels = c('하', '중', '상')))



# 최종적으로 전처리한 수능 성취도 데이터를 열 기준으로 행 병합

grade <- rbind(gr_1[,c(1,2,3,11)], gr_2[,c(1,2,3, 9)], gr_3[,c(1,2,3, 9)], gr_4[,c(1,2,3, 9)], 
               gr_5[,c(1,2,3, 9)])





### 1-2. 데이터 전처리 - 결측치 처리

# 각 등급별 비율에 대한 결측치를 0으로 추정

dt1$'F비율'[is.na(dt1$'F비율')] <-  0
dt1$'E비율'[is.na(dt1$'E비율')] <-  0
dt1$'D비율'[is.na(dt1$'D비율')] <-  0
dt1$'C비율'[is.na(dt1$'C비율')] <-  0
dt1$'B비율'[is.na(dt1$'B비율')] <-  0
dt1$'A비율'[is.na(dt1$'A비율')] <-  0

dt2$'F비율'[is.na(dt2$'F비율')] <-  0
dt2$'E비율'[is.na(dt2$'E비율')] <-  0
dt2$'D비율'[is.na(dt2$'D비율')] <-  0
dt2$'C비율'[is.na(dt2$'C비율')] <-  0
dt2$'B비율'[is.na(dt2$'B비율')] <-  0
dt2$'A비율'[is.na(dt2$'A비율')] <-  0

dt3$'F비율'[is.na(dt3$'F비율')] <-  0
dt3$'E비율'[is.na(dt3$'E비율')] <-  0
dt3$'D비율'[is.na(dt3$'D비율')] <-  0
dt3$'C비율'[is.na(dt3$'C비율')] <-  0
dt3$'B비율'[is.na(dt3$'B비율')] <-  0
dt3$'A비율'[is.na(dt3$'A비율')] <-  0

dt4$'F비율'[is.na(dt4$'F비율')] <-  0
dt4$'E비율'[is.na(dt4$'E비율')] <-  0
dt4$'D비율'[is.na(dt4$'D비율')] <-  0
dt4$'C비율'[is.na(dt4$'C비율')] <-  0
dt4$'B비율'[is.na(dt4$'B비율')] <-  0
dt4$'A비율'[is.na(dt4$'A비율')] <-  0

dt5$'F비율'[is.na(dt5$'F비율')] <-  0
dt5$'E비율'[is.na(dt5$'E비율')] <-  0
dt5$'D비율'[is.na(dt5$'D비율')] <-  0
dt5$'C비율'[is.na(dt5$'C비율')] <-  0
dt5$'B비율'[is.na(dt5$'B비율')] <-  0
dt5$'A비율'[is.na(dt5$'A비율')] <-  0



# 각 등급별 비율을 합하여 점수합산 변수 생성

dt1$점수합산 <- 1* dt1$'F비율' + 2* dt1$'E비율' + 3* dt1$'D비율' + 4* dt1$'C비율' + 5* dt1$'B비율' + 
  6* dt1$'A비율'

dt2$점수합산 <- 1* dt2$'F비율' + 2* dt2$'E비율' + 3* dt2$'D비율' + 4* dt2$'C비율' + 5* dt2$'B비율' + 
  6* dt2$'A비율'

dt3$점수합산 <- 1* dt3$'F비율' + 2* dt3$'E비율' + 3* dt3$'D비율' + 4* dt3$'C비율' + 5* dt3$'B비율' + 
  6* dt3$'A비율'

dt4$점수합산 <- 1* dt4$'F비율' + 2* dt4$'E비율' + 3* dt4$'D비율' + 4* dt4$'C비율' + 5* dt4$'B비율' + 
  6* dt4$'A비율'

dt5$점수합산 <- 1* dt5$'F비율' + 2* dt5$'E비율' + 3* dt5$'D비율' + 4* dt5$'C비율' + 5* dt5$'B비율' + 
  6* dt5$'A비율'





### 1-3. 데이터 전처리 - 그룹화

# 각 피해유형 종류별 발생건수 총합 그룹화

d0 <- da0 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>% 
  summarize(합계 = sum(총응답건수))

d1 <- da1 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>% 
  summarize(합계 = sum(총응답건수))

d2 <- da2 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>% 
  summarize(합계 = sum(총응답건수))

d3 <- da3 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>% 
  summarize(합계 = sum(총응답건수))

d4 <- da4 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>%
  summarize(합계 = sum(총응답건수))



dd <- data4 %>% filter(설문항목명 == '피해유형') %>% group_by(설문응답명) %>% 
  summarize(합계 = sum(총응답건수))


data_trans <- data4 %>% filter(설문항목명 == "피해유형") %>% select(공시년도, 층화추출학교ID, 총응답건수)



# 공시년도별 학교별 피해건수 총합 그룹화

f <- data_trans %>% group_by(공시년도, 층화추출학교ID)  %>% filter(층화추출학교ID %in% f4$층화추출학교ID) %>% summarize(sum(총응답건수))

nrow(f)



# 공시년도별 학교별 전체 교육시간 총합 그룹화

f1 <- data5 %>% select(공시년도, 층화추출학교ID, 교육시간) %>% group_by(공시년도, 층화추출학교ID) %>% 
  filter(층화추출학교ID %in% f4$층화추출학교ID) %>% summarize(sum(교육시간))

nrow(f1)


# 공시년도별 학교별 부적응비율 평균 그룹화

f2 <- data9 %>% select(공시년도, 층화추출학교ID, 부적응비율) %>% group_by(공시년도, 층화추출학교ID) %>%
  summarize(mean(부적응비율)*100)

nrow(f2)


# 공시년도별 학교별 진로 상담 교사수 그룹화

f3 <- data8 %>% select(공시년도, 층화추출학교ID, 진로상담전문교사수) %>% group_by(공시년도, 층화추출학교ID) %>% filter(층화추출학교ID %in% f4$층화추출학교ID)

nrow(f3)


# 공시년도별 학교별 총학생수 그룹화

f4 <- data7 %>% select(공시년도, 층화추출학교ID, 총학생수) %>% group_by(공시년도, 층화추출학교ID) %>% summarize(sum(총학생수))

nrow(f4)



# 공시년도별 학교별 가해학생 처벌수 그룹화

f5 <- data1 %>% select(공시년도, 층화추출학교ID, 가해학생처벌수)  %>% filter(층화추출학교ID %in% f4$층화추출학교ID) %>% group_by(공시년도, 층화추출학교ID) %>% summarize(sum(가해학생처벌수))

nrow(f5)



# 공시년도별 학교별 가해방지교육건수 그룹화

f6 <- data2 %>% select(공시년도, 층화추출학교ID, 가해방지교육건수)  %>% filter(층화추출학교ID %in% f4$층화추출학교ID) %>% group_by(공시년도, 층화추출학교ID) %>% summarize(sum(가해방지교육건수))

nrow(f6)



# 성취도 데이터 학교별, 학년별 점수합산 변수 그룹화하여 총점 변수 생성

dtt1 <- dt1 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명,
                       학기명, 편제명, '점수합산') %>% group_by(층화추출학교ID, 공시학년명) %>% 
  summarize(총점 = sum(점수합산))

dtt2 <- dt2 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명,
                       학기명, 편제명, '점수합산') %>% group_by(층화추출학교ID, 공시학년명) %>% 
  summarize(총점 = sum(점수합산))

dtt3 <- dt3 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명,
                       학기명, 편제명, '점수합산') %>% group_by(층화추출학교ID, 공시학년명) %>% 
  summarize(총점 = sum(점수합산))

dtt4 <- dt4 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명,
                       학기명, 편제명, '점수합산') %>% group_by(층화추출학교ID, 공시학년명) %>% 
  summarize(총점 = sum(점수합산))

dtt5 <- dt5 %>% select(공시년도, 층화추출학교ID, 학교급명, 남녀공학구분명, 주야과정구분명, 공시학년명,
                       학기명, 편제명, '점수합산') %>% group_by(층화추출학교ID, 공시학년명) %>% 
  summarize(총점 = sum(점수합산))



# 성취도 데이터 학교별 그룹화 된 점수합산 변수를 합하여 학교별 총점 변수 그룹화

dttt1 <- dtt1 %>% group_by(층화추출학교ID) %>% summarize(총점 = sum(총점))

dttt2 <- dtt2 %>% group_by(층화추출학교ID) %>% summarize(총점 = sum(총점))

dttt3 <- dtt3 %>% group_by(층화추출학교ID) %>% summarize(총점 = sum(총점))

dttt4 <- dtt4 %>% group_by(층화추출학교ID) %>% summarize(총점 = sum(총점))

dttt5 <- dtt5 %>% group_by(층화추출학교ID) %>% summarize(총점 = sum(총점))



# 위의 그룹화된 총점 변수를 3분위수로 분할하여 1,2,3 을 범주로 갖는 순위형 범주 변수인 등급 변수 생성

dttt1$등급 <- ifelse(dttt1$총점 >= quantile(as.matrix(dttt1$총점), 0.6666) , 1, 
                   ifelse(dttt1$총점 >= quantile(as.matrix(dttt1$총점), 0.3333), 2, 3))

dttt2$등급 <- ifelse(dttt2$총점 >= quantile(as.matrix(dttt2$총점), 0.6666) , 1, 
                   ifelse(dttt2$총점 >= quantile(as.matrix(dttt2$총점), 0.3333), 2, 3))

dttt3$등급 <- ifelse(dttt3$총점 >= quantile(as.matrix(dttt3$총점), 0.6666) , 1, 
                   ifelse(dttt3$총점 >= quantile(as.matrix(dttt3$총점), 0.3333), 2, 3))

dttt4$등급 <- ifelse(dttt4$총점 >= quantile(as.matrix(dttt4$총점), 0.6666) , 1, 
                   ifelse(dttt4$총점 >= quantile(as.matrix(dttt4$총점), 0.3333), 2, 3))

dttt5$등급 <- ifelse(dttt5$총점 >= quantile(as.matrix(dttt5$총점), 0.6666) , 1, 
                   ifelse(dttt5$총점 >= quantile(as.matrix(dttt5$총점), 0.3333), 2, 3))



# 더 이상 사용하지 않는 학교ID 변수를 제거 및 등급 변수를 범주화

ddd1 <- dt1[!duplicated(dt1$층화추출학교ID),]
ddd2 <- dt2[!duplicated(dt2$층화추출학교ID),]
ddd3 <- dt3[!duplicated(dt3$층화추출학교ID),]
ddd4 <- dt4[!duplicated(dt4$층화추출학교ID),]
ddd5 <- dt5[!duplicated(dt5$층화추출학교ID),]


dttt1 <- dttt1 %>% mutate(등급 = factor(등급, unique(등급), labels = c('상', '중', '하')))
dttt2 <- dttt2 %>% mutate(등급 = factor(등급, unique(등급), labels = c('상', '중', '하')))
dttt3 <- dttt3 %>% mutate(등급 = factor(등급, unique(등급), labels = c('하', '중', '상')))
dttt4 <- dttt4 %>% mutate(등급 = factor(등급, unique(등급), labels = c('하', '중', '상')))
dttt5 <- dttt5 %>% mutate(등급 = factor(등급, unique(등급), labels = c('하', '중', '상')))


# 전처리한 성취도 데이터 통합 

tt1 <- merge(ddd1, dttt1, by = '층화추출학교ID', all = T)
tt2 <- merge(ddd2, dttt2, by = '층화추출학교ID', all = T)
tt3 <- merge(ddd3, dttt3, by = '층화추출학교ID', all = T)
tt4 <- merge(ddd4, dttt4, by = '층화추출학교ID', all = T)
tt5 <- merge(ddd5, dttt5, by = '층화추출학교ID', all = T)

tt <- rbind(tt1, tt2, tt3, tt4, tt5)




### 1-4. 번외

# 직위별 자격별 교원 현황 데이터 로드

js1 <- read_excel(path = "0408. 직위별자격별교원현황(09-21)(70%).xlsx", col_names = T)



# 공시년도별, 학교별 전문상담 교사수와 전문상담 교사 자격 교원수를 그룹화

f7 <- js1 %>% select(공시년도, 층화추출학교ID, 전문상담교사자격교원수, 전문상담교사수) %>% filter(공시년도 %in% c(2015, 2016, 2017, 2018, 2019)) %>% filter(층화추출학교ID %in% f4$층화추출학교ID) %>% group_by(공시년도, 층화추출학교ID) %>% summarize( sum(전문상담교사수), sum(전문상담교사자격교원수) )

nrow(f7)



# 학교 정보 관련 데이터 통합

final <- merge(f, f1, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f2, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f3, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f4, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f5, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f6, by = c('공시년도','층화추출학교ID'), all = T)

final <- merge(final, f7, by = c('공시년도','층화추출학교ID'), all = T)



# 통합된 학교 정보에서 성취도 데이터에 존재하는 학교들에 대해 학교 ID와 고등학교명 변수 선택

high <- final %>% select(층화추출학교ID,고등학교구분명) %>% filter(층화추출학교ID %in% tt$층화추출학교ID)



# 위에서 선택된 데이터를 전처리한 년도별 성취도 데이터와 통합

tt1 <- merge(tt1, high, by = '층화추출학교ID', all = T)
tt2 <- merge(tt2, high, by = '층화추출학교ID', all = T)
tt3 <- merge(tt3, high, by = '층화추출학교ID', all = T)
tt4 <- merge(tt4, high, by = '층화추출학교ID', all = T)
tt5 <- merge(tt5, high, by = '층화추출학교ID', all = T)



# 다시 성취도 데이터 통합

tt <- rbind(tt1, tt2, tt3, tt4, tt5)



# 통합된 학교 관련 정보 데이터와 위의 성취도 데이터를 통합 후 저장 

final_1 <- merge(final, tt, by = c('공시년도','층화추출학교ID'), all = T)

nrow(final_1)

write.csv(final_1, file = "학교정보공시합본.csv")



# 수능성취도 데이터 저장

write.csv(grade, file = "학교별 수능성적 등급 변환표.csv")



# 전처리한 수능성취도 데이터에서 공시년도와 학교명, 학교별 수능성취도 등급 변수를 선택

grade <- grade[,2:5]



# final_1 데이터와 수능성취도 데이터를 통합 및 분석에 필요한 변수만 선택

final_2 <- merge(final_1, grade, by = c('공시년도','층화추출학교ID'), all = T)

final_2 <- final_2 %>% select(공시년도,층화추출학교ID, 'sum(총응답건수)', 'sum(교육시간)', 
                          'mean(부적응비율) * 100' , '진로상담전문교사수' , 'sum(총학생수)' ,
                          'sum(가해학생처벌수)' , 'sum(가해방지교육건수)' , '학업중단학생수' ,
                          '소재지구분명' , '설립구분명' , '고등학교구분명' , '고등학교일반실업구분명' ,
                          '남녀공학구분명.x' ,   '주야구분명' ,   '계열구분명' , 'sum(전문상담교사수)' ,
                          'sum(전문상담교사자격교원수)' , '등급.x' , '시도교육청명' , '등급.y' )




final$mean.부적응비율....100[is.na(final$mean.부적응비율....100)] <-  0

final$'학업중단학생수'[is.na(final$'학업중단학생수')] <-  0

final$이탈률 <- final$학업중단학생수 / final$sum.총학생수.

final$등급.y  <- as.factor(final$등급.y)

final %>% rename(sum(총응답건수) = 총응답건수)

colnames(final)[4]<-'총교육시간'



# 최종 전처리 데이터를 저장

write.csv(final_2, file = "데이터 최종합본.csv")






##### 2. 시각화

gt1 <- xtabs(~고등학교구분명+등급, data=tt1)
gt1[2:5,]
mosaic(gt1[2:5,])

gt1_1 <- xtabs(~남녀공학구분명+등급, data=tt1)
gt1_1
mosaic(gt1_1)

gt2 <- xtabs(~고등학교구분명+등급, data=tt2)
gt2[2:5,]
mosaic(gt2[2:5,])

gt2_1 <- xtabs(~남녀공학구분명+등급, data=tt2)
gt2_1
mosaic(gt2_1)

gt3 <- xtabs(~고등학교구분명+등급, data=tt3)
gt3
mosaic(gt3)

gt3_1 <- xtabs(~남녀공학구분명+등급, data=tt3)
gt3_1
mosaic(gt3_1)

gt4 <- xtabs(~고등학교구분명+등급, data=tt4)
gt4[2:5,]
mosaic(gt4[2:5,])

gt4_1 <- xtabs(~남녀공학구분명+등급, data=tt4)
gt4_1
mosaic(gt4_1)

gt5 <- xtabs(~고등학교구분명+등급, data=tt5)
gt5[2:5,]
mosaic(gt5[2:5,])

gt5_1 <- xtabs(~남녀공학구분명+등급, data=tt5)
gt5_1
mosaic(gt5_1)


gt <- xtabs(~고등학교구분명+등급, data=tt)
gt[2:5,]
mosaic(gt[2:5,])

gt_1 <- xtabs(~남녀공학구분명+등급, data=tt)
gt_1
mosaic(gt_1)



pt1 <- final %>% filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>% summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))

pt2 <- final %>% filter(공시년도 == 2015) %>% 
  filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>%
  summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))

pt3 <- final %>% filter(공시년도 == 2016) %>%
  filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>%
  summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))

pt4 <- final %>% filter(공시년도 == 2017) %>% 
  filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>%
  summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))

pt5 <- final %>% filter(공시년도 == 2018) %>% 
  filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>%
  summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))

pt6 <- final %>% filter(공시년도 == 2019) %>% 
  filter(고등학교구분명 %in% c('일반고', '자율고', '특목고', '특성화고')) %>% group_by(고등학교구분명) %>%
  summarize(이탈율 = mean(`mean(부적응비율) * 100`, na.rm = T))



barplot(height = pt1$이탈율, names.arg = pt1$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)',main = '2015~2019년도 학교별 이탈률', col = 4)

barplot(height = pt2$이탈율, names.arg = pt2$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)', main = '2015년도 학교별 이탈률', col = 4)

barplot(height = pt3$이탈율, names.arg = pt3$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)', main = '2016년도 학교별 이탈률', col = 4)

barplot(height = pt4$이탈율, names.arg = pt4$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)', main = '2017년도 학교별 이탈률', col = 4)

barplot(height = pt5$이탈율, names.arg = pt5$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)', main = '2018년도 학교별 이탈률', col = 4)

barplot(height = pt6$이탈율, names.arg = pt6$고등학교구분명, xlab = '고등학교구분명', ylab = '이탈률(%)', main = '2019년도 학교별 이탈률', col = 4)



g15 <- final %>% filter(공시년도 == 2015)

g16 <- final %>% filter(공시년도 == 2016)

g17 <- final %>% filter(공시년도 == 2017)

g18 <- final %>% filter(공시년도 == 2018)

g19 <- final %>% filter(공시년도 == 2019)



gg1 <- xtabs(~고등학교구분명+등급, data=final)
gg1[2:5,]
mosaic(gg1[2:5,])

gg2 <- xtabs(~남녀공학구분명+등급, data=final)
gg2[1:3,]
mosaic(gg2[1:3,])

gg15_1 <- xtabs(~고등학교구분명+등급, data=g15)
gg15_1
mosaic(gg15_1)

gg15_2 <- xtabs(~남녀공학구분명+등급, data=g15)
gg15_2
mosaic(gg15_2)

gg16_1 <- xtabs(~고등학교구분명+등급, data=g16)
gg16_1[2:5,]
mosaic(gg16_1[2:5,])

gg16_2 <- xtabs(~남녀공학구분명+등급, data=g16)
gg16_2[1:3,]
mosaic(gg16_2[1:3,])

gg17_1 <- xtabs(~고등학교구분명+등급, data=g17)
gg17_1[2:5,]
mosaic(gg17_1[2:5,])

gg17_2 <- xtabs(~남녀공학구분명+등급, data=g17)
gg17_2[1:3,]
mosaic(gg17_2[1:3,])

gg18_1 <- xtabs(~고등학교구분명+등급, data=g18)
gg18_1[2:5,]
mosaic(gg18_1[2:5,])

gg18_2 <- xtabs(~남녀공학구분명+등급, data=g18)
gg18_2[1:3,]
mosaic(gg18_2[1:3,])

gg19_1 <- xtabs(~고등학교구분명+등급, data=g19)
gg19_1[2:5,]
mosaic(gg19_1[2:5,])

gg19_2 <- xtabs(~남녀공학구분명+등급, data=g19)
gg19_2[1:3,]
mosaic(gg19_2[1:3,])



barplot(height = d0$합계, names.arg = d0$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2015년도 피해유형별 건수', col = 4)

barplot(height = d1$합계, names.arg = d1$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2016년도 피해유형별 건수', col = 4)

barplot(height = d2$합계, names.arg = d2$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2017년도 피해유형별 건수', col = 4)

barplot(height = d3$합계, names.arg = d3$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2018년도 피해유형별 건수', col = 4)

barplot(height = d4$합계, names.arg = d4$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2019년도 피해유형별 건수', col = 4)

barplot(height = dd$합계, names.arg = dd$설문응답명, xlab = '피해유형', ylab = '발생건수', 
        main = '2015~2019년도 피해유형별 건수', col = 4)





##### 3. 순서형 범주 모델 적합 및 검정


# 모델 적용 전 계산

final <- final_2[-7760,]
final <- final_2[-c(1015, 7292, 7585),]

final$이탈률[is.na(final$이탈률)] <- 0

final$이탈률 <- ifelse(final$이탈률 <= quantile(as.matrix(final$이탈률), 0.3333) , 1, 
                    ifelse(final$이탈률 <= quantile(as.matrix(final$이탈률), 0.6666), 2, 3))

final$이탈률 <- as.factor(final$이탈률)



m <- polr(formula = 등급.y ~ final[,3] + final[,4] + final[,6] + final[,8] + final[,9] + final[,18] + 
            final[,19], data = final,Hess = TRUE, method = "logistic")

fit <- polr(final$이탈률 ~ final[,3] + final[,4] + final[,6] + final[,8] + final[,9] + final[,18] + 
              final[,19], data = final,Hess = TRUE, method = "logistic")

fit1 <- polr(final$이탈률 ~ final[,3] + final[,4] + final[,6] + final[,8] + final[,9], 
             data = final,Hess = TRUE, method = "logistic")
fit2 <- polr(final$이탈률 ~ final[,3] + final[,4] + final[,6] + final[,8] + final[,9] + 
               final[,3]*final[,18] + final[,3]*final[,19], data = final,Hess = TRUE, method = "logistic")




# 모델의 회귀 계수 값 산출

m$coefficients

exp(coef(m))



boxplot(final$이탈률)


# 모델 검정

summary(m)
summary(fit)
summary(fit1)
summary(fit2)
plot(fit)

shapiro.test(residuals(fit))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
hist(final$이탈률)

final[which(cooks.distance(fit)) ,]
influence.measures(fit)

sum(final[,5] == 0)
final$이탈률