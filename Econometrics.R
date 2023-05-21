help.search("match")
library("MatchIt")




########################################




library(haven)
women_IHDS <- read_dta("E:/CNNS/women_IHDS.dta")
View(women_IHDS)
library("MatchIt")


women_IHDS<-zap_formats(women_IHDS)
women_IHDS<-zap_label(women_IHDS)
women_IHDS<-zap_labels(women_IHDS)
#women_IHDS <- women_IHDS %>% mutate_all(labelled::zap_labels)


women_IHDS_df<-data.frame(women_IHDS)



columns_to_clean <- c("GR18A", "EW5", "EW6", "EW10", "ID11", "COPC", "NPERSONS", "GR46", "MH2")
women_IHDS_df_clean <- women_IHDS_df[complete.cases(women_IHDS_df[, columns_to_clean]), ]

####optmatch

## controls - relationship to head, age, years of education, general health, religion, household exp,household number of people, ever worked for wages, marriage status
P<-glm(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
       +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2,
       family=binomial)$fitted.values

library(dplyr)
covariate<- women_IHDS_df_clean[, c("EW5", "EW6", "EW10", "ID11", "COPC", "NPERSONS", "GR46", "MH2" )]
GR18A <- women_IHDS_df_clean$GR18A

library(DOS)
dmat<-smahal(GR18A,covariate)


dmat<-addcaliper(dmat,GR18A,P)

dim(dmat)

library(optmatch)

m<-fullmatch(dmat,min.controls=2,max.controls=2,
             omit.fraction=1379/2689)


####matchit

m.out0 <- matchit(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
                  +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2, data = women_IHDS_df_clean,
                  method = NULL, distance = "glm")


# Checking balance prior to matching
summary(m.out0)

m.out1 <- matchit(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
                  +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2, data = women_IHDS_df_clean,
                  method = "nearest", distance = "glm")


# Checking balance prior to matching
summary(m.out1)


m.out2 <- matchit(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
                  +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2, data = women_IHDS_df_clean,
                  method = "full", distance = "glm", link = "probit")

# Optimal full Mahalanobis distance matching within a PS caliper
m.out2 <- matchit(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
                  +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2, data = women_IHDS_df_clean,
                  method = "full", caliper = .01,
                  mahvars = ~ women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6)


plot(m.out1, type = "jitter", interactive = FALSE)

# optimal matching with many controls
m.out4 <- matchit(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
                  +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2, data = women_IHDS_df_clean,
                  method = "nearest", ratio = 2,
                  min.controls = 1, max.controls = 12)
plot(m.out4, type = "histogram")



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









