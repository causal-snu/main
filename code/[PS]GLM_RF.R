data
formula = poisox ~ age + sex + mortal


## Fitting model for Propensity Score
ps_glm <- ps_glm(formula = formula, data = data)

ps_rf <- ps_rf(formula = formula, data = data)

## Estimation of Propensity Score
estimate_ps_glm <- estimate_ps(ps_glm)

estimate_ps_rf <- estimate_ps(ps_rf)


## propensity score model class
print.propmod <- print.propmod(x= ps_glm)

print.propmod <- print.propmod(x =ps_rf)

## Add estimated propensity score to a data frame

add_ps_glm <- add_propensity(data = data, object = ps_glm, formula = formula,
                             method = 'logit', var = 'propensity')

add_ps_rf <- add_propensity(data = data, object = ps_rf, formula = formula,
                            method = 'rf', var = 'propensity')

## weighting

add_ipw_wt_glm <- add_ipw_wt(data = add_ps_glm, treatment = 'poisox', trt_indicator = 1,
                             object = ps_glm, formula=formula, method = 'logit')
add_ipw_wt_rf <- add_ipw_wt(data = add_ps_rf, treatment = 'poisox', trt_indicator = 1,
                            object = ps_rf, formula=formula, method = 'rf')

## Estimation of Inverse Probability Weighting
compute_ipw_glm <- compute_ipw(data = add_ipw_wt_glm, treatment = 'poisox', trt_indicator = 1,
                               outcome = 'blood', weight = 'ipw_wt', 
                               obejct = ps_glm, formula = formula, method = 'glm'
                               )
compute_sipw_glm <- compute_sipw(data = add_ipw_wt_glm, treatment = 'poisox', trt_indicator = 1,
                                outcome = 'blood', weight = 'ipw_wt', 
                                obejct = ps_rf, formula = formula, method = 'rf'
                                )
compute_ipw_rf <- compute_ipw(data = add_ipw_wt_rf, treatment ='poisox', trt_indicator = 1,
                              outcome = 'blood', weight = 'ipw_wt', object = ps_rf, formula = formula,
                              method = 'rf')

compute_sipw_rf <- compute_sipw(data = add_ipw_wt_rf, treatment ='poisox', trt_indicator = 1,
                              outcome = 'blood', weight = 'ipw_wt', object = ps_rf, formula = formula,
                              method = 'rf')

## Treatment Effect Estimation Using Propensity Scores
add_weighting_glm <- add_weighting(data = data, treatment = 'poisox', formula = formula, 
                                   method = 'logit')

add_weighting_rf <- add_weighting(data= data, treatment = 'poisox', formula = formula,
                                  method= 'rf')












