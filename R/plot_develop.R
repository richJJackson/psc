### Develop plotting output


library(psc)
library(survival)
library(enrichwith)
library(ggplot2)
library(survminer)

data <- psc::data
surv.mod <- psc::surv.mod
bin.mod <- psc::bin.mod
cont.mod <- psc::cont.mod
count.mod <- psc::count.mod


pssur <- pscfit(surv.mod,data,trt=data$trt)
psbin <- pscfit(bin.mod,data,trt=data$trt)
pscou <- pscfit(count.mod,data,trt=data$trt)
pscon <- pscfit(cont.mod,data,trt=data$trt)


plot(pssur)
plot(psbin)
plot(pscou)
plot(pscon)

print(psbin)
coef(psbin)
summary(psbin)

plot_ite(pssur)
plot_ite(psbin)
plot_ite(pscou)
plot_ite(pscon)




















