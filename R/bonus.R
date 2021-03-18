# code for bonus question analysis

library(lmtest)
library(sandwich)
library(here)
library(tidyverse)

D <- read_csv(here("data","example_dat.csv"))

mod_bonus1 <- glm(Z ~ X + Xm1, data=D, family=binomial("logit"))

coeftest(mod_bonus1, vcov = vcov(mod_bonus1))


D$pscore <- glm(X ~ as.factor(int) + Xm1 + Z + Zm1,data=D,family=binomial(link="logit"))$fitted.values
D$p_num <- glm(X ~ as.factor(int),data=D,family=binomial(link="logit"))$fitted.values
D <- D %>% group_by(ID) %>% 
  mutate(num = X*p_num + (1-X)*(1-p_num),
         den = X*pscore + (1-X)*(1-pscore),
         sw = cumprod(num/den),
         w = cumprod(1/den)) %>% 
  ungroup(ID)

mod_bonus2 <- glm(Z ~ X + Xm1, data=D, family=quasibinomial("logit"), weights=sw)

coeftest(mod_bonus2, vcov = vcovCL(mod_bonus2, cluster=D$ID, type = "HC1"))