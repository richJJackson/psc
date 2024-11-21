### Muliple Treatment Comparisons


library(psc)
library(survival)

data <- psc::data
surv.mod <- psc::surv.mod
bin.mod <- psc::bin.mod
cont.mod <- psc::cont.mod
count.mod <- psc::count.mod


pssur <- pscfit(surv.mod,data,trt=data$trt)
psbin <- pscfit(bin.mod,data,trt=data$trt)
pscon <- pscfit(cont.mod,data,trt=data$trt)
pscou <- pscfit(count.mod,data,trt=data$trt)


summary(pssur)
summary(psbin)
summary(pscon)
summary(pscou)

print(pssur)
print(psbin)
print(pscon)
print(pscou)

plot(pssur)
plot(psbin)
plot(pscon)
plot(pscou)


data$trt

