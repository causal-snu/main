## 패키지 설치

```
install.packages("remotes")
remotes::install_github("ygeunkim/propensityml")

```

## 라이브러리 부착

```
library(propensityml)

```

## R 폴더에서 weighting.R 함수 내용 정리
add_propensity(data, object = NULL, formula = NULL, method = c("logit", "rf", "cart", "SVM"), var = "propensity", mc_col = NULL, sc_col = NULL, parallel = FALSE, ...)
 - Propensity score를 원래 dataframe에 new column으로 추가해주는 함수. method를 뭘로 지정하는지에 따라 어떤 모델 기반으로 propensity score를 추정할지 정할 수 있음.

add_ipw_wt(data, treatment, trt_indicator = 1, object = NULL, formula = NULL, method = c("logit", "rf", "cart", "SVM"), mc_col = NULL, sc_col = NULL, parallel = FALSE, ...
 - Data를 넣으면 ATE_ipw의 weight를 추정해주고 이를 dataframe의 new column으로 추가해주는 함수. 위의 propensity score추정하는 함수가 들어가있어서 ps 추정까지 자동으로 해준다. 따라서 이 함수의 method도 위와 같이 propensity score 추정에 쓰이는 모델을 고르는 parameter임.

compute_ipw(data, treatment, treat_indicator, outcome, method= ...)
 - 아예 data와 treatment, outcome 넣으면 ATE_ipw를 추정해주는 함수. 위의 add_propensity 와 add_ipw_wt 가 들어가므로 두 개의 new column 생성, output은 ATE_ipw.

compute_sipw (data, treatment, treat_indicator, outcome, method= ...)
 - 위와 input 같고 output이 ATE_sipw임.

add_weighting(data, treatment, treat_indicator, outcome, method= ...)
 - ATE_ inverse probability treatment weighting 줄여서 ATE_iptw 새로운 개념 등장. 이건 안배운 내용인듯. 이때 필요한 weight를 계산해줌. ipw와 거의 똑같은데 t/ps - (1-t)/(1-ps) 로 중간 부호가 마이너스로 바뀜.
