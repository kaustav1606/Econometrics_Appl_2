

#### Effect of Mahila Mandal on Women Empowerment #######

#### 10th June 2023 ##########

library("MatchIt")
library(haven)
library("MatchIt")
library(dplyr)
library(DOS)
library(bigmatch)
library(optmatch)

########################################





women_IHDS <- read_dta("E:/CNNS/women_IHDS.dta")
View(women_IHDS)



women_IHDS<-zap_formats(women_IHDS)
women_IHDS<-zap_label(women_IHDS)
women_IHDS<-zap_labels(women_IHDS)
#women_IHDS <- women_IHDS %>% mutate_all(labelled::zap_labels)


women_IHDS_df<-data.frame(women_IHDS)



columns_to_clean <- c("GR18A", "EW5", "EW6", "EW10", "ID11", "COPC", "NPERSONS", "GR46", "MH2")
women_IHDS_df_clean <- women_IHDS_df[complete.cases(women_IHDS_df[, columns_to_clean]), ]

#### optmatch -- create propensity score to create counterfactuals using matching ###

## controls - relationship to head, age, years of education, general health, religion, household exp,household number of people, ever worked for wages, marriage status

P<-glm(women_IHDS_df_clean$GR18A~women_IHDS_df_clean$EW5+women_IHDS_df_clean$EW6+women_IHDS_df_clean$EW10+women_IHDS_df_clean$ID11
       +women_IHDS_df_clean$COPC+women_IHDS_df_clean$NPERSONS+women_IHDS_df_clean$GR46+women_IHDS_df_clean$MH2,
       family=binomial)$fitted.values


covariate<- women_IHDS_df_clean[, c("EW5", "EW6", "EW10", "ID11", "COPC", "NPERSONS", "GR46", "MH2" )]
GR18A <- women_IHDS_df_clean$GR18A


dmat<-smahal(GR18A,covariate)


dmat<-addcaliper(dmat,GR18A,P)

dim(dmat)



m<-fullmatch(dmat,min.controls=2,max.controls=2,
             omit.fraction=1379/2689)


####matchit

### using different methods of matching with propensity scoring and mahalnobis matching methods

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


m.data <- match.data(m.out4)

##### average treatment effects #####

library("marginaleffects")

fit <- lm(WSEARNHOURLY ~ GR18A * (EW5 + EW6 + EW10 + ID11 + COPC + 
                                    NPERSONS + GR46 +MH2 ), data = m.data)

avg_comparisons(fit,
                variables = "GR18A",
                vcov = ~subclass,
                newdata = subset(m.data, GR18A == 1),
                wts= "FWETE")
