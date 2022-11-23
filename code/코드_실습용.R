#1. propensity.R : 
##Fitting glm for propensity score
data
##1 ps_glm 부터 해보자
fit1 <- ps_glm(poisox ~ age + sex, data = data)
fit1

#2. Estimation of propensity score
##1 estimation of propensity score using glm, rf
est_ps_glm <- estimate_ps(fit1)
summary(est_ps_glm)

#3. propensity scoremodel class : propmod class
print.propmod(fit1)


#4 Weighing.R :
##1 add_propensity : Add estimated propensity score to a data frame
add_ps_glm <- add_propensity(data, fit1, poisox ~ age + sex, method = 'logit', var = "propensity")
add_ps_glm

##2 add_ipw_wt
ipw_wt_glm <- add_ipw_wt(add_ps_glm, treatment = 'poisox', trt_indicator = 1, 
                         object = fit1, formula = poisox ~ age + sex, method = 'logit')
ipw_wt_glm

##3 Estimation of Inverse Probability Weighting : ipw , sipw 추정
compute_ipw_glm <- compute_ipw(data = ipw_wt_glm, treatment = 'poisox', trt_indicator = 1, outcome = 'blood', weight = 'ipw_wt',
                               object = fit1, formula = poisox ~ age + sex, method = 'logit')

compute_sipw_glm <- compute_sipw(data = ipw_wt_glm, treatment = 'poisox', trt_indicator = 1, outcome = 'blood', weight = 'ipw_wt',
                                 object = fit1, formula = poisox ~ age + sex, method = 'logit')
ipw_sipw_glm <- c(compute_ipw_glm, compute_sipw_glm)
ipw_sipw_glm


##4 Treatment effect estimation using propensity scores, ATE 대신 ATT사용

add_weighting_glm <- add_weighting(data , treatment = 'poisox', trt_indicator = 1, 
                                   object= fit1, formula = poisox ~ age + sex, method = 'logit')
add_weighting_glm #trt == 1 이면 propwt 1을 부여하고 0이면 (pi / (1-pi)) 를 부여

##5 Covariate balance
compute_balance_glm <- compute_balance(data = data, col_name = 'balance', treatment = 'poisox', 
                                       trt_indicator = 1,outcome = 'blood')
compute_balance_glm
##6 ASAM : computes average standardized absolute mean distance (ASAM)

asam_glm_ipw <- compute_asam(data, treatment = 'poisox', trt_indicator = 1, outcome = 'blood',
                             object = fit1, formula = poisox ~ age + sex, method = 'logit', weighting = 'IPW')

asam_glm_sipw <- compute_asam(data = data, treatment = 'poisox', trt_indicator = 1, outcome = 'blood',
                             object = fit1, formula = poisox ~ age + sex, method = 'logit', weighting = 'SIPW')
asam_collect <- c(asam_glm_ipw, asam_glm_sipw)
asam_collect ##엥 왜 똑같지??
#-------------------------------------------------------------------------------#
## randomforest 를 이용한 propensity score 추정... 전체적으로 결과를 보면 shit 하다

data
fit2 <- ps_rf(poisox ~ age + sex, data = data)
fit2

est_ps_rf <- estimate_ps(fit2)
summary(est_ps_rf)

print.propmod(fit2)

add_ps_rf1<- add_propensity(data, object = fit2, formula = poisox ~ age + sex, method = 'rf', var = 'propensity')
add_ps_rf1

ipw_wt_rf <- add_ipw_wt(data = add_ps_rf1, treatment = 'poisox', trt_indicator = 1, object = fit2, 
                        formula = poisox ~ age + sex, method = 'rf')
ipw_wt_rf

compute_ipw_rf <- compute_ipw(data = ipw_wt_rf, treatment = 'poisox', trt_indicator = 1, outcome = 'blood', weight = 'ipw_wt',
                              object = fit2, formula = poisox ~ age + sex, method = 'rf')
compute_ipw_rf

compute_sipw_rf <- compute_sipw(data = ipw_wt_rf, treatment = 'poisox', trt_indicator = 1, outcome = 'blood', weight = 'ipw_wt',
                                object = fit2, formula = poisox ~ age + sex, method = 'rf')
compute_sipw_rf

add_weighting_rf <- add_weighting(data , treatment = 'poisox', trt_indicator = 1, 
                                   object= fit2, formula = poisox ~ age + sex, method = 'rf')
add_weighting_rf














