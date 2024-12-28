library(quantreg)


############## IHDS Education and wages
women_IHDS <- read_dta("E:/CNNS/women_IHDS.dta")
data<-zap_formats(data)
data<-zap_label(data)
data<-zap_labels(data)

data<-data.frame(data)


# AGE MARRIAGE STATUS MAJOMORBITIDT (COST OF MEDICINE)
columns_to_clean <- c("EDUC7", "RO5", "WSEARN", "RO6", "MB27", "NPERSONS", "ID11", "ASSETS")
data <- data[complete.cases(data[, columns_to_clean]), ]


# various quantile regression
wageeducfit <- summary(rq(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS,tau=c(.05, .25, .5, .75, .95)))


# OLS regression
wageeducfitolsreg <- lm(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS)
summary(wageeducfitolsreg)

# Quantile regression
wageeducfit25 <- rq(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS, tau=0.25)
summary(wageeducfit25)
wageeducfit50 <- rq(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS, tau=0.5)
summary(quantreg50)
wageeducfit75 <- rq(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS, tau=0.75)
summary(wageeducfit75)
# Difference in coefficients
anova(wageeducfit25,wageeducfit50, wageeducfit75)
# Plot coefficients
wageeducfitquantreg.all <- rq(data$WSEARN ~ data$EDUC7  +data$RO5 +data$RO6 + data$MB27+data$NPERSONS +data$ID11 + data$ASSETS, tau = seq(0.05, 0.95, by = 0.05))
wageeducfitquantreg.plot <- summary(wageeducfitquantreg.all)
plot(wageeducfitquantreg.plot)









############## RBC folate and BMI
columns_to_clean <- c("q104","gender_recode","area_recode", "zbfa", "rbcf_1unic")
WB_clean <- WB[complete.cases(WB[, columns_to_clean]), ]

WB$area_recode <- ifelse(WB$area == "rural", 0, 1)
WB$gender_recode <- ifelse(WB$q102 == "female", 0, 1)


fit1 <- rq(WB_clean$rbcf_1unic ~ WB_clean$zbfa +WB_clean$area_recode +WB_clean$gender_recode + WB_clean$q104 , tau = .1, data =WB_clean)

summary(fit2,se = "nid")
summary(fit2)


fit1 <- summary(rq(foodexp~xx,tau=2:98/100))
fit2 <- summary(rq(WB_clean$rbcf_1unic ~ WB_clean$zbfa +WB_clean$area_recode +WB_clean$gender_recode + WB_clean$q104,tau=c(.05, .25, .5, .75, .95)))



fitquantreg.all <- rq(WB_clean$rbcf_1unic ~ WB_clean$zbfa +WB_clean$area_recode +WB_clean$gender_recode + WB_clean$q104, tau = seq(0.05, 0.95, by = 0.05))
fitquantreg.plot <- summary(fitquantreg.all)
plot(fitquantreg.plot)


















