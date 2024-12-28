
****** Women empowerment and 

***** create dataset *****


**** check indi data ***
use "E:\CNNS\IHDS_DATA\ICPSR_37382\DS0003\37382-0003-Data.dta", clear
tab SURVEY

**** check women data ***
use "E:\CNNS\IHDS_DATA\ICPSR_37382\DS0017\37382-0017-Data.dta", clear

tab SURVEY

**** merge individual data to women data ********
sort SURVEY STATEID DISTID PSUID HHID HHSPLITID
merge 1:1 SURVEY STATEID DISTID PSUID HHID HHSPLITID PERSONID using "E:\CNNS\IHDS_DATA\ICPSR_37382\DS0005\37382-0005-Data.dta"
save "E:\econometrics\data\IHDS DATA\working\women_indi.dta", replace

use "E:\econometrics\data\IHDS DATA\working\women_indi.dta", clear

tab _merge
keep if _merge ==3

tab MM12Y, missing

tab SURVEY, gen(SURVEY_D)

sort HHBASE PBASE SURVEY_D1
*** create new column for treatment fo mobile owning ***

gen treat = MM12Y
tab treat, missing
*** identfy individuals with mobile ***

by HHBASE PBASE: replace treat = treat[_n-1] if SURVEY_D1 ==1

*** check ****
by HHBASE PBASE: list SURVEY_D1 MM12Y treat
save "E:\econometrics\data\IHDS DATA\working\women_indi.dta", replace

**** merge household level data ********

sort SURVEY HHBASE
merge m:1 SURVEY STATEID DISTID PSUID HHID HHSPLITID using "E:\CNNS\IHDS_DATA\ICPSR_37382\DS0011\37382-0011-Data.dta"
save "E:\econometrics\data\IHDS DATA\working\women_indi_house.dta", replace


tab _merge
keep if _merge ==3
save "E:\econometrics\data\IHDS DATA\working\women_indi_house.dta", replace
tab treat, missing


*** one unique ID for each person ***
gen double ID = PBASE * 10^(ceil(log10(HHBASE + 1))+1) + HHBASE

drop ID

******* unique ID for village ****
tostring PSUID, gen(PSUID_str)
tostring DISTID, gen(DISTID_str)
tostring STATEID, gen(STATEID_str)
gen village_id_str = PSUID_str + DISTID_str + STATEID_str

replace ED11 = 0 if ED11 ==.



