
library(dplyr)

columns_to_clean <- c("q104","gender_recode","area_recode", "zbfa", "rbcf_1unic")
WB_clean <- WB[complete.cases(WB[, columns_to_clean]), ]

WB$area_recode <- ifelse(WB$area == "rural", 0, 1)
WB$gender_recode <- ifelse(WB$q102 == "female", 0, 1)

new_df <- select(WB,q104,gender_recode,area_recode, zbfa )

par_df <- select(WB_clean ,q104,area_recode )
nonl_df <- select(WB_clean ,zbfa,gender_recode)



######## Partial Linear Model


X <- subset(WB_clean, select = c("gender_recode","zbfa"))
Z <- subset(WB_clean,select=c( "area_recode","q104"))
Y <- subset(WB_clean,select=c("rbcf_1unic"))
Yv <- Y[["rbcf_1unic"]]
Zv <- Z[["area_recode"]]
Xv <- X[["gender_recode"]]
Zv1 <- Z[["q104"]]
Xv1 <- X[["zbfa"]]



plspm <- npplregbw(formula=Yv~Xv1+factor(Xv)|factor(Zv)+Zv1)
pl <- npplreg(bws=plspm)
summary(pl)
plot(plspm)
plot(pl)
#bw <- npplregbw(xdat=X, zdat=Z, ydat=Yv)

#bw_smooth <- npscoefbw(xdat=X, ydat=Y, zdat=Z)

#################### ichimura estimator of single index model
ichbw <- npindexbw(xdat=new_df , ydat=WB$rbcf_1unic)

summary(ichbw)

ichmodel <- npindex(bws=ichbw, gradients=TRUE)

summary(ichmodel)
plot(ichbw, gradients=TRUE)
plot(ichbw)














#################### partially linear model
plbw <- npplregbw(xdat = nonl_df, ydat =vec3, zdat = par_df)
pl <- npplreg(bws=plbw)
summary(pl)



####################
bw <- npindexbw(xdat=new_df , ydat=WB$rbcf_1unic, method="kleinspady")

summary(bw)

model <- npindex(bws=bw, gradients=TRUE)

summary(model)




