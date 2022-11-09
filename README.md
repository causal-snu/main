# Causal Inference 
 인과추론 수업 프로젝트 팀입니다. 2022-10-31 ~
 - 주제: 8. Machine learning methods for causal inference

## 스터디 방식
- ⏰ 스터디 시간 : 매주 수 오후 3시
- 📗 스터디 자료 : [성향점수매칭 예시](https://m.blog.naver.com/paperfactor_ceo/222098513280), [국민건강영양조사 원시데이터](https://knhanes.kdca.go.kr/knhanes/sub03/sub03_02_05.do)
  
## 스터디 과정
- 코드
  - 블로그에 나온 propensity score matching
  - Logistic regression 대신 CART, Random forest 적용하기
  - Raw data 전처리하고 적용하기
- [논문](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2807890/pdf/nihms153529.pdf) 읽기
- 강의록: propensity score 내용 읽기
- [propensity score matching&weighting in python](https://matheusfacure.github.io/python-causality-handbook/11-Propensity-Score.html)
- [A Practical Guide for Using Propensity Score Weighting in R](https://www.math.umd.edu/~slud/s818M-MissingData/PropensityScoreWeightingR.pdf)
- [cart 이용한 ps score 추정](https://arxiv.org/pdf/1807.09462.pdf)
- [propensity score weighting](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3069059/pdf/pone.0018174.pdf
)
- [propensity score github](https://github.com/ygeunkim/propensityml)
--------------------------
## Propensity score analysis 요약
  - propensity score를 활용하는 방식에 따라 분류.
1. propensity score weighting
  - treatment group과 control group의 propensity score가 같아지도록 weight를 부여.
  - Inverse probability of treatment weighting, IPW.

2. propensity score matching
  - treatment group의 unit과 비슷한 propensity score를 가진 control group unit을 매칭.
  
3. propensity score subclassification
  - treatment group과 control group의 unit들을 propensity score가 비슷한 k개의 집단으로 subclassification.
----------------------------
## 스터디 일정
- 2022-10-31-월-4시반
  - 앞으로 뭘 해야하는지 확인하고 과정 정리

- 2022-11-9-수-3시
  - 각자 공부해온 내용 공유
- 2022-11-4-금 00시
  - 응통세미나 1번 과제 제출 ( a번 : 연호, b번 : 경선 )

- 2022-11-15-화-5시
  - 깃허브 코드 구현, 데이터 전처리, 과제3 해결 해서 가져와서 짬뽕.
