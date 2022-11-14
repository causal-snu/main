## 패키지 설치

```
install.packages("remotes")
remotes::install_github("ygeunkim/propensityml")

```

## 라이브러리 부착

```
library(propensityml)

```

## weighting.R 함수 내용 정리
<span style="color:blue">add_propensity</span>(data, object = NULL, formula = NULL, method = c("logit", "rf", "cart", "SVM"), var = "propensity", mc_col = NULL, sc_col = NULL, parallel = FALSE, ...)
 - Propensity score를 원래 dataframe에 new column으로 추가해주는 함수. 
 - method를 뭘로 지정하는지에 따라 어떤 모델 기반으로 propensity score를 추정할지 정할 수 있음.

<span style="color:blue">add_ipw_wt</span>(data, treatment, trt_indicator = 1, object = NULL, formula = NULL, method = c("logit", "rf", "cart", "SVM"), mc_col = NULL, sc_col = NULL, parallel = FALSE, ...
 - Data를 넣으면 ATE_ipw의 weight를 추정해주고 이를 dataframe의 new column으로 추가해주는 함수. 
 - 위의 propensity score추정하는 함수가 들어가있어서 ps 추정까지 자동으로 해준다. 
 - 따라서 이 함수의 method도 위와 같이 propensity score 추정에 쓰이는 모델을 고르는 parameter임.

<span style="color:blue">compute_ipw</span>(data, treatment, treat_indicator, outcome, method= ...)
 - 아예 data와 treatment, outcome 넣으면 ATE_ipw를 추정해주는 함수. 
 - 위의 add_propensity 와 add_ipw_wt 가 들어가므로 두 개의 new column 생성, output은 ATE_ipw.

<span style="color:blue">compute_sipw </span>(data, treatment, treat_indicator, outcome, method= ...)
 - 위와 input 같고 output이 ATE_sipw임.

<span style="color:blue">add_weighting</span>(data, treatment, treat_indicator, outcome, method= ...)
 - ATE_ inverse probability treatment weighting 줄여서 ATE_iptw 새로운 개념 등장. 이건 안배운 내용인듯. 
 - 이 때 필요한 weight를 계산해줌. 
 - ipw와 거의 똑같은데 t/ps - (1-t)/(1-ps) 로 중간 부호가 마이너스로 바뀜.

## propensity.R 함수 내용 정리
### propensity score를 위한 모델 적합

<span style="color:blue">ps_glm</span>(formula, data, ...)
 - propensity score 추정 전에 y를 treatment로 하는 **logistic regression** 모델에 데이터를 적합하는 합수.
 - 결과로 glm 함수 돌린 값을 result로 반환

<span style="color:blue">ps_rf</span>(formula, data, ...)
 - 위와 같은데 이번에는 y를 treatment로 하는 **random forest** 모델에 데이터를 적합하는 함수.

<span style="color:blue">ps_cart</span>(formula, data, ...)
 - 위와 같은데 이번에는 y를 treatment로 하는 **classification tree** 모델에 데이터를 적합하는 함수.

<span style="color:blue">ps_svm</span>(formula, data, scale = FALSE, kernel = c("radial", "linear", "polynomial", "sigmoid"), cost = 1, gamma, .....)
 - 위와 같은데 이번에는 y를 treatment로 하는 **support vestor machine** 모델에 데이터를 적합하는 함수.
 > 저희가 하는 모델중에 포함 안시켜도 되지 않나요?? 
 > > A: 맞아요.

<span style="color:blue">plot_cp</span>(object, ...)
 - Before pruning, plot complexity parameter from CART.
 > plot_cp에 대한 부분이 이해가 안되는데 설명해주세요~ 이것도 pruning을 위한 과정인데 저희 내용에 포함 안시켜도 되지 않나요?
 > > A: prune한다는건 CART가 의사결정나무인데 의사결정나무의 최대 단점이 과적합입니다. 그래서 가지 중에서(의사결정 node 중에서) 너무 과적합하는 의사결정 가지를 잘라서 가지치기하는걸 말합니다. 그래서 과적합을 방지할 수 있고 보통은 교차검증한 오류를 제일 줄이는 방향으로 가지치기를 진행합니다.

<span style="color:blue">ps_prune</span>(object,cp, ...)
 - prunes the result of \code{\link{ps_cart}}.
 > **prune 한다는게 뭔지 아직 모르겠음 **
 > 이 부분은 안해도 될것 같은데요?? 
 > > A: 맞아요.

### propensity score 추정

<span style="color:blue">estimate_ps</span>(object, ...)
 - object로는 위의 모델 적합한 변수가 들어감.
 - object$name== "glm" or "rf" or "cart" or "SVM" 
 - return 으로 모델 적합 후 predict 한 값이 나옴.
 
 > 함수에서 trt_lev가 무엇인가요? type='prob' 는 ?? 
 > > A: type='prob'는 예측 이후 matrix of class probabilities를 반환하는 argument입니다. 'prob'도 있고 'response'도 있는데 glm의 예측된 확률을 반환하는 argument가 'response'이고 다른 것들은 'prob' 입니다.
 > > A: trt_level은 말 그대로 treat가 1인 행의 class값입니다.
 
 > #propensity socre model class의 'promod' class는 머야요
 > > A: 이거 쓰면 굳이 저렇게 계산 안해도 ps score를 반환해준다고 합니다. 


## evaluate.R 함수 내용 정리
<span style="color:blue">compute_balance</span>(data, col_name = "balance", treatment, trt_indicator = 1, outcome, exclude = NULL)
 - gives covariate balance summary

compute_moment(x)
 - compute_balance 부속 함수

tidy_moment(data, treatment, with_melt = NULL, col_exclude)
 - compute_balance 부속 함수
 
<span style="color:blue">compute_asam</span>(data, treatment, trt_indicator = 1, outcome, exclude = NULL,
                         object = NULL, formula = NULL, method = c("logit", "rf", "cart", "SVM"), weighting = c("IPW", "SIPW"), mc_col = NULL, sc_col = NULL, parallel = FALSE, ...)
 - computes average standardized absolute mean distance (ASAM)
 
## Readme에 있는 내용.
``` sim_outcome ```
 - 논문에 나온 dataset을 시뮬레이션 해볼 수 있게 dataset을 생성해주는 함수
 
 ``` 
 (fit_rf <- 
  x %>% 
  ps_rf(exposure ~ . - y - exposure_prob, data = .))
  
 estimate_ps(fit_rf)
 
 ```
 - 이 과정으로 propensity score 추정.
                     
                         

