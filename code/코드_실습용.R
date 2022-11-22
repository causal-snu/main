#1. propensity.R : 
##Fitting 000000 for propensity score

##1 ps_glm
fit1 <- ps_glm(poisox ~ age + sex, data = chemical)
fit1

##2 ps_rf
fit2 <- ps_rf(poisox ~ age + sex, data = chemical)
fit2

##3 ps_cart
fit3 <- ps_cart(poisox ~ age + sex, data = chemical)
fit3


#2. Estimation of propensity score
##1 estimation of propensity score using glm
est_ps_glm <- estimate_ps(fit1)
summary(est_ps_glm)

##2 estimation of propensity score using random forest

est_ps_rf <- estimate_ps(fit2)
summary(est_ps_rf)


##3 estimation of propensity score using cart
est_ps_cart <- estimate_ps(fit3)
summary(est_ps_cart)


#3. propensity scoremodel class : propmod class
print.propmod(fit1)
print.propmod(fit2)
print.propmod(fit3)


#2. Weighing.R :
##1 add_propensity : Add estimated propensity score to a data frame
add_ps_glm <- add_propensity(chemical, fit1, poisox ~ age + sex, method = 'logit')

add_ps_rf <- add_propensity(chemical, fit2, poisox ~ age + sex, method = 'rf')

add_ps_cart <- add_propensity(chemical, fit3, poisox ~ age + sex, method = 'cart')

##2 add_ipw_wt

ipw_wt_glm <- add_ipw_wt(chemical, treatment = 'poisox', trt_indicator = 1, object = fit1, formula = poisox ~ age + sex, method = 'logit')
ipw_wt_rf <- add_ipw_wt(chemical, treatment = 'poisox', trt_indicator = 1, object = fit2, formula = poisox ~ age + sex, method = 'rf')
ipw_wt_cart <- add_ipw_wt(chemical, treatment = 'poisox', trt_indicator = 1, object = fit3, formula = poisox ~ age + sex, method = 'cart')

##3 Estimation of Inverse Probability Weighting : ipw 추정
compute_ipw_glm <- compute_ipw(ipw_wt_glm, treatment = 'poisox', trt_indicator = 1, outcome = 'mortal', weight = 'ipw_wt',
                               object = fit1, formula = poisox ~ age + sex, method = 'logit') 

