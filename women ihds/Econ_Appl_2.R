
##################################################

##https://cran.r-project.org/web/packages/MatchIt/vignettes/sampling-weights.html


m.data <- match.data(m.out4)


library("marginaleffects")

fit <- lm(WSEARNHOURLY ~ GR18A * (EW5 + EW6 + EW10 + ID11 + COPC + 
                                    NPERSONS + GR46 +MH2 ), data = m.data)

avg_comparisons(fit,
                variables = "GR18A",
                vcov = ~subclass,
                newdata = subset(m.data, GR18A == 1))

################################################

data("lalonde")

P<-glm(women_IHDS$treat~join$q104+join$area_D+join$q114_D+join$q117_D
       +join$q201A_D+join$q201B_D+join$q301_D+join$q303_D,
       family=binomial)$fitted.values



join$pscore <- NA
join$pscore<-glm(join$treat~join$q104+join$area_D+join$q114_D+join$q117_D
                 +join$q201A_D+join$q201B_D+join$q301_D+join$q303_D,
                 family=binomial)$fitted.values

library(dplyr)
covariate<- join[, c("q104","area_D","q114_D","q117_D","q201A_D","q201B_D","q301_D","q303_D")]

library(DOS)
dmat<-smahal(join$treat,covariate)

dmat<-addcaliper(dmat,join$treat,p)

dim(dmat)

library(optmatch)

m<-fullmatch(dmat,min.controls=2,max.controls=2,
             omit.fraction=1379/2689)

1023-2*1468

length(m)
m[1:10]

matched(m)[1:10]

sum(matched(m))









