

********** data without household information *******
use "E:\econometrics\data\IHDS DATA\working\women_indi.dta", clear

********** data with household information *******
use "E:\econometrics\data\IHDS DATA\working\women_indi_house.dta", clear





**** base regression **** without controls
xi: reg GR27B i.treat*i.SURVEY [pweight= FWTEW] i.village_id_str, cluster(ID) 
xi: reg GR11F i.treat*i.SURVEY [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg GR29B i.treat*i.SURVEY [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg AI1 i.treat*i.SURVEY [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg WSEARN i.treat*i.SURVEY [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID)
xi: reg GR37 i.treat*i.SURVEY [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID)  


***** controls *****


macro define control "EW6 GR3A MH2 MH9 ED11 FAMCAT"


**** base regression **** with controls

xi: reg GR27B i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg GR11F i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg GR29B i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg AI1 i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID) 
xi: reg WSEARN i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID)
xi: reg GR37 i.treat*i.SURVEY $control [pweight= FWTEW] i.PSUID i.DISTID, cluster(ID)  







































replace MM12Y = 0  if SURVEY == 1
xi: reg GR27B i.MM12Y*i.SURVEY [pweight= FWTEW]
tab MM12Y
tab GR27B
tab GR27B
tab GR27B by SURVEY
tab GR27B, SURVEY
TAB GR11F
tab GR11F
replace GR11F = 0  if GR11F == .
xi: reg GR11F i.MM12Y*i.SURVEY [pweight= FWTEW]
replace GR27B = 0  if GR11F == .
xi: reg GR27B i.MM12Y*i.SURVEY [pweight= FWTEW]
gen var = 0
replace var = 1 if survey == 2 & MM12Y == 1
replace var = 1 if SURVEY == 2 & MM12Y == 1
xi: reg GR27B MM12Y SURVEY var [pweight= FWTEW]
egen concatenated = concat( HHBASE PBASE ), punc("_")
tab concatenated
recast long HHBASE
gen year =0
repalce year = 1 if SURVEY ==2
replace year = 1 if SURVEY ==2
xi: reg GR11F i.MM12Y*i.SURVEY [pweight= FWTEW]
pwcorr _IMM12Y_1 _ISURVEY_2 _IMM1XSUR_1_2
do "C:\Users\user\AppData\Local\Temp\STD00000000.tmp"
do "C:\Users\user\AppData\Local\Temp\STD00000000.tmp"
keep if year year <1987
keep if year <1987
keep if bf15==1
xi: reg lnr i.repeal*i.year acc ir pi alcohol crack poverty income ur [aweight=totpop], cluster(fip)
use "C:\Users\user\Desktop\Stata14\merged.dta", clear
do "C:\Users\user\AppData\Local\Temp\STD00000000.tmp"
replace MM12Y if SURVEY ==1
replace MM12Y = . if SURVEY ==1
replace GR27B = 0  if GR27B == .
xi: reg GR27B i.MM12Y*i.SURVEY [pweight= FWTEW]
replace GR11F = 0  if GR11F == .
xi: reg GR11F i.MM12Y*i.SURVEY [pweight= FWTEW]
replace GR29B = 0  if GR29B == .
xi: reg GR29B i.MM12Y*i.SURVEY [pweight= FWTEW]
replace Al1 = 0  if Al1 == .
replace AI1 = 0  if AI1 == .
xi: reg  AI1 i.MM12Y*i.SURVEY [pweight= FWTEW]
xi: reg  WSEARN i.MM12Y*i.SURVEY [pweight= FWTEW]
logit GR2B EW18D EW18D HB1 HB1
xi: logit   AI1 i.MM12Y*i.SURVEY [pweight= FWTEW]
