clear
********************************************************************************
*SET FILEPATH
********************************************************************************

*Change working directory as per user
global path E:\brazil_lst\data
cd "$path"


/* if halos / radii change then lines 176 - 214, 301 - 306 and 316 - 321 will need to be
edited to reflect this change. This is where DDW and the temperature value of
remaining forest is estimated - an example of how to make this change - adding in an 
extra halo at 10 km - has been commented out */

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

/*estimate regression models and store estimates*/
areg lst $daily_controls , absorb(u_id) robust

est sto B1

areg tmaxb $daily_controls , absorb(u_id) robust

est sto B2

********************************************************************************
*ESTIMATE STATION REGRESSION MODEL
********************************************************************************

use stpanel.dta, clear

*predict AT from near forest LST
* first catch missing data 9999 farenheit = 5538.17 deg C
drop if tmaxst == 5538.17

rename LON lon
rename LAT lat
rename weekid week_id

reg tmaxst lst rmjm2 qcbin* ppt ele lon lat wdist cdist nfa1_1 nfa1_100km i.year i.week_id#c.lat c.slope##c.aspect##i.week_id

est sto station_model

esttab B* using Table_station_AT_regressions.csv, b(%9.4g) se(%9.4g) nonumbers nonumbers mtitles("LST" "BEST") drop(*.month_hemi *.week_id *.year) nogaps noomitted replace


********************************************************************************
*PREDICT AT NEAR AG
********************************************************************************
estimates restore station_model

use panel025_ag, clear
*to avoid confusion always use u_id25 with this panel025_ag
drop u_id 

*remove capital letters from variable labels
foreach v of varlist _all{ 

	rename `v' `=lower("`v'")' 
}


gen lon = x
gen lat = y
rename weekid week_id

gen nfa1_100km = nfa1_100km_2011
gen nfa1_1 = pfa1_2011

* Predict air temperature
predict tmaxlst

save panel025_ATpred, replace

********************************************************************************
*MAKE COEFFICIENT VARIABLES
********************************************************************************

estimates restore B1

global lst_h1 _b[hfa1]
global lst_h2 _b[hfa2]
global lst_h4 _b[hfa4]
*global lst_h10 _b[hfa10]


gen lst_h1 = $lst_h1
gen lst_h2 = $lst_h2
gen lst_h4 = $lst_h4
*gen lst_h10 = $lst_h10

*All year - BEST

estimates restore B2

global best_h1 _b[hfa1]
global best_h2 _b[hfa2]
global best_h4 _b[hfa4]
*global best_h10 _b[hfa10]

gen best_h1 = $best_h1
gen best_h2 = $best_h2
gen best_h4 = $best_h4
*gen best_h10 = $best_h10


keep lst_h* best_h*

gen coef_id = 1

collapse lst_h* best_h* coef_id

save coefs_daily_panel_gfc, replace

********************************************************************************
*PREPARE DATA FOR FIGS AND EDD 
********************************************************************************

*Read in full 0.25 deg forest samples
*read in data
use panel025_ATpred, clear

*convert nfc into area of forest cover
foreach y in "nfa1" {
foreach i of global halolist_comp  {
foreach q in "2002" "2011"  {

replace `y'_`i'km_`q' =`y'_`i'km_`q'*0.01
replace `y'_`i'km_`q'= `y'_`i'km_`q'*`i'*`i'*_pi

}
}
}

*convert pixel location into forest area 
replace pfa1_2002 = pfa1_2002*0.01
replace pfa1_2002 = pfa1_2002*0.9266254330554165*0.9266254330554165

replace pfa1_2011 = pfa1_2011*0.01
replace pfa1_2011 = pfa1_2011*0.9266254330554165*0.9266254330554165


rename pfa1_2002 hfa1_2002 
rename pfa1_2011 hfa1_2011


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

save panel025_ATpred_v1, replace

*compute forest loss by u_id25

keep u_id25 hfa* 

collapse hfa*, by(u_id25)

sort u_id25

/*change in forest cover between 2002 to 2011 - use these years as EDD is predicted
for 2012 calendar year*/

foreach i of global halolist {
by u_id25: gen change_h`i' = hfa`i'_2011 - hfa`i'_2002
}

joinby u_id25 using panel025_ATpred, unmatched(both)
drop _merge

gen coef_id = 1

*add in coefs to predict change in temp due to deforestation out of sample
joinby coef_id using coefs_daily_panel_gfc, unmatched(master)
drop _merge

*estimte DDW
*gen ltcsum = change_h1*lst_h1 + change_h2*lst_h2 + change_h4*lst_h4 + change_h10*lst_h10
gen ltcsum = change_h1*lst_h1 + change_h2*lst_h2 + change_h4*lst_h4 
*gen btcsum = change_h1*best_h1 + change_h2*best_h2 + change_h4*best_h4 + change_h10*best_h10
gen btcsum = change_h1*best_h1 + change_h2*best_h2 + change_h4*best_h4 
gen cdiff = ltcsum - btcsum

*adjust predicted ATmax to ATmax as if no deforestation since 2002
gen lstadj = tmaxlst-ltcsum 

keep tmaxb lstadj tminb tminb2 tmaxlst ltcsum btcsum cdiff u_id25 yeardoy rmjm2 year gsyear hfa* lst_h* best_h* x y

*save output to compute EDD in R
save edd_new25, replace

*now estimate the remaining 'temperature value of forest' as left in the ground in 2012
*gen lfcsum = hfa1_2011*lst_h1 + hfa2_2011*lst_h2 + hfa4_2011*lst_h4 + hfa10_2011*lst_h10 
gen lfcsum = hfa1_2011*lst_h1 + hfa2_2011*lst_h2 + hfa4_2011*lst_h4  
*gen bfcsum = hfa1_2011*best_h1 + hfa2_2011*best_h2 + hfa4_2011*best_h4 + hfa10_2011*best_h10
gen bfcsum = hfa1_2011*best_h1 + hfa2_2011*best_h2 + hfa4_2011*best_h4
gen fdiff = lfcsum - bfcsum

collapse tmaxb tminb tminb2 tmaxlst ltcsum btcsum cdiff yeardoy rmjm2 year gsyear hfa* lst_h* best_h* lfcsum bfcsum fdiff x y, by(u_id25)

*use this output to make DDW figures
save ddw_fig_data, replace


********************************************************************************
*MAKE LST AVAILABILITY FIGURES
********************************************************************************
use dailypanel_mhv_4_nw, clear


****Keep variables needed****
keep u_id year lst qcbin1 qcbin2 qcbin3 x y

gen lstavail = 1 if lst != .
gen qc1avail = 1 if qcbin1 == 1
gen qc2avail = 1 if qcbin2 == 1
gen qc3avail = 1 if qcbin3 == 1

gen lon  = x
gen lat = y

sort u_id year

by u_id year: egen lstsum = sum(lstavail)
by u_id year: egen qcbin1sum = sum(qc1avail)
by u_id year: egen qcbin2sum = sum(qc2avail)
by u_id year: egen qcbin3sum = sum(qc3avail)

collapse lstsum qcbin1sum qcbin2sum qcbin3sum lon lat x y, by(u_id)

save lst_availability, replace


