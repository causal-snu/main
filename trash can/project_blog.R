##221101 정연호(ps matching - logistic regression으로 해보기)
## ps matching_blog
dat <- read.csv('C:/Users/sarah/Desktop/대학원 2학기/응용통계세미나/프로젝트/data.csv')
summary(dat)

## 데이터셋 조작 : sex(성), drk_st(음주여부), HBV(B형 간염), HCV(C형 간염), LC(간경변증,liver cirrhosis)
cols_fac <- c('sex', 'drk_st', 'HBV', 'HCV', 'LC') #factor type으로 바꿔줄 column
dat[,cols_fac] <- lapply(dat[,cols_fac], as.factor) #factor type으로 바꿔줌

cols_fac
dat[,cols_fac]

install.packages('moonBook')
library(moonBook)
mytable(drk_st ~., data = dat) #음주상태여부 : drk_st, 0:비음주, 1:음주
#drk_st ~. : drk_st와 . 변수에서 어떤 차이가 있는가?? data=dat에서라는 의미, 이때 .은 drk_st를 제외한 모든 변수를 사용해라, Y~X의 관계라고 생각하자

## mytable에서 p-value 값을 좀더 develop 해보기 : 범주형 변수의 경우 chi-square test, 연속형변수의 경우 t-test를 통해 계산된다.
mytable(drk_st ~., data = dat, method=3, catMethod=0) #P-VALUE보게되면 sex, age, drk_freq(음주빈도), exercise(운동횟수)에서 비음주/음주여부의 차이가 있음!! 

## ps-matching해보기 
# 1) 음주상태에 영향을 미치는 요인들이 무엇인지 logistic regression 해보자
mod <- glm(drk_st ~ sex + age + drk_freq + HBV + HCV + exercise, family = binomial, data = dat)
#이떄 ~.을 사용하면 제한되는게 id,year는 필요없기에! 그리고 음주가 간암에 미치는 영향이므로 lc도 제외 
summary(mod)

mod_ex <- glm(drk_st ~ . -ID - year - LC, family = binomial, data = dat) #mod와 mod_ex는 같은 식 : 즉 필요변수만 쓸것이냐... 모든 .에서 -변수s 할것이냐
summary(mod_ex)

socre.table <- data.frame(ps=predict(mod, type = 'response'), drk_st = mod$model$drk_st)
library(dplyr)
library(ggplot2) ## 참고 ) %>%는 체인연산자, 파이프라고 하는데 데이터와 데이터를 연결하는 dplyr 패키지의 핵심 연산자(데이터를 전달한다)
socre.table %>% mutate(drk_st = ifelse(drk_st == 1, 'Drinking', 'Non-drinking'))%>%
  ggplot(aes(x=ps, fill = drk_st)) + geom_histogram(color = 'black', position = 'dodge') + theme_bw() + ylim(c(0,50)) #ps에 대해서 많이 겹친다? 비슷한 ps끼리 매칭이 가능하다

## 2) 매칭시켜보기(비슷한 ps끼리)
install.packages('MatchIt')
library(MatchIt)
match_fit <- matchit(drk_st ~ sex + age + drk_freq + HBV + HCV + exercise, method = 'nearest' , data = dat) #'nearest' : 점수가 가장 가까운 것
matched_dat <- match.data(match_fit)
matched_dat
table(matched_dat$drk_st) #wow.... 비음주(0)과 음주(1)이 ps에 따라 각 군에서 609명씩 1:1 매칭이 되었다. 여기서 비음주(0) 609명이고, 음주(1)이 5369명임은 알자

## 3) matched set으로 두 그룹의 특성 비교하기 : 매칭이 끝난 2)의 데이터로 음주상태에 따른 두 그룹의 특성을 비교해보자!! 
mytable(drk_st ~ ., data = matched_dat, method = 3, catMethod = 0) #데이터를 matched_dat로 바꿔준다 (매칭 후 데이터의 분석 결과)

## 교훈
#1. p<0.05인 변수가 존재한다(HBV, HCV, LC의 경우), score가 가장 비슷한 것끼리 매칭이 되긴 했다.
#2. ps matching은 matching 되지 않은 데이터는 버려지게 된다. 데이터 손실이 발생할 수 있다. 매칭하려는 변수가 많을수록 데이터 손실이 커질 가능성 있다.

## 참고기능
## csv파일 저장하기
matched_dat #이걸 저장해야겠지?
write.csv(matched_dat, file='matched_dat.csv')












