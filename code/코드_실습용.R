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

#3. propensity score model class
print.propmod(fit1)
print.propmod(fit2)
print.propmod(fit3)


#2. Weighing.R :
