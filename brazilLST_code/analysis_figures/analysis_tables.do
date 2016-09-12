clear
********************************************************************************
*SET FILEPATH
********************************************************************************

/*
IF USING DIFFERENT HALOS THAN 1 - 2 - 4 
line 279 - 311 *COMBINATION OF HALOS - if changing halo number or radii the inputs to lincomest
will need to be changed following the format of the code written - a commented example using 4 halos with 
a 10 km halo added is left as an example

At the end of the script Table_S3 will need to have its dimensions adjusted to account for the change in the number / radii
of halos - line 404 - end
*/



*Change working directory as per user
global path E:\brazil_lst\data
cd "$path"


********************************************************************************
*READ IN DATA
********************************************************************************
use dailypanel_mhv_4_nw, clear

********************************************************************************
*REGRESSION MODEL
********************************************************************************

global daily_controls hfa* rmjm2 ppt i.qcbin3 i.month_hemi i.week_id i.year 


********************************************************************************
*SET VARIABLES
********************************************************************************

/* halolist_comp = number of halos to compute*/
*put halo sizes in matrix
matrix halolist_comp = [2, 4]

*put halo size in global variable
global halolist_comp 2 4

*set number of halos - should equal number of columns in matrix
global halonum_comp 2

/* halolist = full number of halos for output*/
*put halo sizes in matrix
matrix halolist = [1, 2, 4]

*put halo size in global variable
global halolist 1 2 4

*set number of halos - should equal number of columns in matrix
global halonum 3

/*TABLE 1 - set number of rows in table output - this should = the number estimates 
stored +1 as we esimtate a model on mean change in NFB with LST*/
global regnumber 3


********************************************************************************

*drop incomplete LST years
drop if year == 2002 | year == 2013
drop if nfa1_2km == . | nfa1_4km == . | nfa1_10km == . 

*merge in s_id (0.25 deg grid cell id)
merge m:1 u_id using gfc_universe_s_id
drop _merge

*rename variables for consistency
rename monthid month
rename weekid week_id

*month*hemisphere fixed effect
gen byte north=0
replace north=1 if y>0
egen month_hemi=group (north month)


*convert nfc into area of forest cover - km
foreach y in "nfa1" {
foreach i in "2" "4" "10" {

replace `y'_`i'km =`y'_`i'km*0.01
replace `y'_`i'km = `y'_`i'km*`i'*`i'*_pi


}
}

*convert pixel forest cover into km

replace nfa1_1 = nfa1_1*0.01
replace nfa1_1 = nfa1_1*0.9266254330554165*0.9266254330554165

*compute halos
local halonum $halonum_comp

foreach y in "f" {
foreach j in "a" {
forval i = 1/`halonum' {

if `i' == 1 {
scalar idx = halolist_comp[1, `i']
local zz = idx
gen h`y'`j'`zz' = n`y'`j'1_`zz'km 
} 
else {
scalar idx = halolist_comp[1, `i']
local zz = idx
local i2 = `i'-1
scalar idx2 = halolist_comp[1, `i2']
local zz2 = idx2
gen h`y'`j'`zz' = n`y'`j'1_`zz'km -n`y'`j'1_`zz2'km 
}

} 
}
}

rename nfa1_1 hfa1


**** Do prep for table 1 now so estimates are stored

gen qnt_id = 1

save dailypanel_qntid, replace

use panel025_ag, replace

*rename location forest cover to match halo convention and convert to km
rename pfa1_2002 hfa1_2002
rename pfa1_2011 hfa1_2011


replace hfa1_2002 = hfa1_2002*0.01
replace hfa1_2002 = hfa1_2002*0.9266254330554165*0.9266254330554165


replace hfa1_2011 = hfa1_2011*0.01
replace hfa1_2011 = hfa1_2011*0.9266254330554165*0.9266254330554165


*convert nfc into area of forest cover
foreach y in "nfa1" {
foreach i of global halolist_comp  {
foreach q in "2002" "2011"  {

replace `y'_`i'km_`q' =`y'_`i'km_`q'*0.01
replace `y'_`i'km_`q'= `y'_`i'km_`q'*`i'*`i'*_pi

}
}
}

*compute halos
local halonum $halonum_comp

foreach y in "f" {
foreach j in "a" {
foreach q in "2002" "2011" {
forval i = 1/`halonum' {

if `i' == 1 {
scalar idx = halolist_comp[1, `i']
local zz = idx
gen h`y'`j'`zz'_`q' = n`y'`j'1_`zz'km_`q' 
} 
else {
scalar idx = halolist_comp[1, `i']
local zz = idx
local i2 = `i'-1
scalar idx2 = halolist_comp[1, `i2']
local zz2 = idx2
gen h`y'`j'`zz'_`q' = n`y'`j'1_`zz'km_`q' -n`y'`j'1_`zz2'km_`q' 
}

}
} 
}
}

*compute 25th percentile, 75th percentile, and mean change in forest cover between 2002 and 2012
keep u_id25 hfa*

collapse hfa*, by(u_id25)

foreach i of global halolist {

by u_id25: gen change_h`i' = hfa`i'_2011 - hfa`i'_2002
egen pct25_`i' = pctile(change_h`i'), p(25)
egen pct75_`i' = pctile(change_h`i'), p(75)
egen mn_`i' = mean(change_h`i')

}

*now make change at pixel location (hfa1) the maximum possible 50% change (equivalent of 25th to 75th percentile) - this is the pixel resolution from GEE
replace pct25_1 = (0.9266254330554165*0.9266254330554165*-1)/2

gen qnt_id = 1

collapse pct* mn* qnt_id 

save qnt_change_ag, replace

use dailypanel_qntid, clear

merge m:1 qnt_id using qnt_change_ag
drop _merge


/*estimate regression models and store estimates*/

areg lst $daily_controls , absorb(u_id) robust

est sto B1

areg tmaxb $daily_controls , absorb(u_id) robust

est sto B2

********************************************************************************
*FIGURES AND TABLES
********************************************************************************

*Table 1

*compute maximum possible change between 2002 and 2012
local halonum $halonum

foreach y in "f" {
foreach j in "a" {
forval i = 1/`halonum' {

scalar idx = halolist[1, `i']
local zz = idx
gen h`y'`j'`zz'ch = pct25_`zz'-pct75_`zz'

}
}
}



*INDIVIDUAL HALOS


foreach y in "f" {
foreach j in "a" {
foreach zz of global halolist {
*LST all year
estimates restore B1
scalar chg = h`y'`j'`zz'ch[1]
lincomest _b[h`y'`j'`zz']*chg
parmest,label saving(h`y'`j'`zz', replace)

*BEST all year
estimates restore B2
scalar chg = h`y'`j'`zz'ch[1]
lincomest _b[h`y'`j'`zz']*chg
parmest,label saving(h`y'`j'`zz'_best, replace)


*MEAN LST
estimates restore B1
scalar chg = mn_`zz'[1]
lincomest _b[h`y'`j'`zz']*chg
parmest,label saving(mn_`zz', replace)

}
}
}

*COMBINATION OF HALOS

local regnumber $regnumber

foreach y in "f" {
foreach j in "a" {
forval i = 1/`regnumber' { 


if `i' != 3 {

estimates restore B`i'

*lincomest `=hfa1ch'*_b[hfa1]+`=hfa2ch'*_b[hfa2]+`=hfa4ch'*_b[hfa4]+`=hfa10ch'*_b[hfa10]
lincomest `=hfa1ch'*_b[hfa1]+`=hfa2ch'*_b[hfa2]+`=hfa4ch'*_b[hfa4]
parmest,label saving(ch_all_`y'`j'`i', replace)

}

else if `i' == 3 {

estimates restore B1


*lincomest `=mn_1'*_b[hfa1]+`=mn_2'*_b[hfa2]+`=mn_4'*_b[hfa4]+`=mn_10'*_b[hfa10]
lincomest `=mn_1'*_b[hfa1]+`=mn_2'*_b[hfa2]+`=mn_4'*_b[hfa4]
parmest,label saving(ch_all_`y'`j'`i', replace)

}

}
}
}


********************************************************************************
*Table S2

esttab B* using Table_S2_year_lag.csv, b(%9.4g) se(%9.4g) nonumbers nonumbers mtitles("LST" "BEST") drop(*.month_hemi *.week_id *.year 0.qcbin3) nogaps noomitted ///
coeflabels(rmjm2 "Extraterrestrial Radiation" pfb1 "pixel forest cover" ppt "precipitation" 1.qcbin3 "QC") replace

********************************************************************************
*Table S3
* get LST model estimates
estimates restore B1

matrix BB1 = e(b)

local halonum $halonum

forval i = 1/`halonum' {

local halo = halolist[1, `i']
local idx = `i'
gen h_lst`halo' = BB1[1,`idx']

}

* get BEST model estimates
estimates restore B2

matrix BB2 = e(b)

local halonum $halonum

forval i = 1/`halonum' {

local halo = halolist[1, `i']
local idx = `i'
gen h_best`halo' = BB2[1,`idx']

}

keep if year == 2012

collapse h_lst* h_best* , by(year)

drop year

********************************************************************************
*ESTIMATE COEFFICIENT PERCENTAGES OF INNER HALO
********************************************************************************
*LST percentage coefficients

local halonum $halonum

forval i = 1/`halonum' {

if `i' == 1 {

local halo = halolist[1, `i']
local idx = `i'+1
gen pct_h_lst`halo' = 100
}
else {
local halo = halolist[1, `i']
local halo1 = halolist[1,1]
local idx = `i'+1
gen pct_h_lst`halo' = h_lst`halo'/h_lst`halo1'
replace pct_h_lst`halo' = pct_h_lst`halo'*100
}
}

*BEST percentage coefficients

local halonum $halonum

forval i = 1/`halonum' {

if `i' == 1 {

local halo = halolist[1, `i']
local idx = `i'+1
gen pct_hp_best`halo' = 100
}
else {
local halo = halolist[1, `i']
local halo1 = halolist[1,1]
local idx = `i'+1
gen pct_h_best`halo' = h_best`halo'/h_best`halo1'
replace pct_h_best`halo' = pct_h_best`halo'*100
}
}

********************************************************************************
*WRITE OUT RESULTS TABLE

putexcel set Table_S3
*set table row names
putexcel A2 = ("Coefficients") using Table_S3, modify
putexcel A3 = ("% of inner halo") using Table_S3, modify

*set column headers
putexcel B1 = ("location") using Table_S3, modify
putexcel C1 = ("halo 2km") using Table_S3, modify
putexcel D1 = ("halo 2-4km") using Table_S3, modify
*putexcel E1 = ("halo 4-10km") using Table_S3, modify

putexcel B2 = (h_lst1) using Table_S3, modify
putexcel B3 = (pct_h_lst1) using Table_S3, modify

putexcel C2 = (h_lst2) using Table_S3, modify
putexcel C3 = (pct_h_lst2) using Table_S3, modify

putexcel D2 = (h_lst4) using Table_S3, modify
putexcel D3 = (pct_h_lst4) using Table_S3, modify

*putexcel E2 = (h_lst10) using Table_S3, modify
*putexcel E3 = (pct_h_lst10) using Table_S3, modify


