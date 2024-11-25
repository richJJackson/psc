### Muliple Treatment Comparisons


library(psc)
library(survival)
library(enrichwith)


data <- psc::data
surv.mod <- psc::surv.mod
bin.mod <- psc::bin.mod
cont.mod <- psc::cont.mod
count.mod <- psc::count.mod


pssur <- pscfit(surv.mod,data,trt=data$trt)
pssur_null <- pscfit(surv.mod,data)
psbin <- pscfit(bin.mod,data,trt=data$trt)
psbin_null <- pscfit(bin.mod,data)
pscon <- pscfit(cont.mod,data,trt=data$trt)
pscou <- pscfit(count.mod,data,trt=data$trt)


summary(pssur)
summary(pssur_null)
summary(psbin)
summary(pscou)

print(pssur)
print(psbin)
print(pscon)
print(pscou)

plot(pssur)
plot(psbin)
plot(pscon)
plot(pscou)

?psc


