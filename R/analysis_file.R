packages <- c("data.table","tidyverse","skimr","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

D$pscore <- glm(X ~ as.factor(int) + Xm1 + C + Cm1,data=D,family=binomial(link="logit"))$fitted.values
D$p <- glm(X ~ as.factor(int),data=D,family=binomial(link="logit"))$fitted.values

D <- D %>% group_by(ID) %>% 
  mutate(num = X*p + (1-X)*(1-p),
         den = X*pscore + (1-X)*(1-pscore),
         sw = cumprod(num/den))

D

mod1 <- glm(Y ~ X, data=D, family = quasibinomial(link = "logit"), weights=sw)

exp(mod1$coefficients)

plot_dat <- D %>% 
  group_by(ID) %>% 
  mutate(cumexp = cumsum(X),
         expratio = cumexp/int) %>% 
  filter(last_flag == 1)

ggplot(plot_dat) + geom_histogram(aes(expratio))

D %>% group_by(int) %>% summarize(meanSW = mean(sw),
                                  medianSW = median(sw),
                                  minSW = min(sw),
                                  maxSW = max(sw))

ggplot(D) + 
  geom_density(aes(x=pscore,fill=as.factor(X)),alpha=.5,bw=.1) +
  facet_wrap(~int)
