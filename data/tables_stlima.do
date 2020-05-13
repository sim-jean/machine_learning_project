clear all 
cap log close
set more off


*	CHANGE DIRECTORY
cd "/Users/simonjean/Projects/machine_learning_project/data"
	


****************************
*	1) GENERATE MAIN DATASET
****************************
use "Intermedias/input_r1.dta",clear
joinby codest using "Intermedias/input_r2.dta", unm(m)
recode _merge (1=0) (3=1), gen(follow_up)
drop _merge

joinby codest using "Auxiliares/listas_final.dta", unm(m)
tab _merge
drop _merge

joinby codmod using "Auxiliares/school_pairs_final.dta", unm(m)
tab _merge
drop _merge

keep if grado_r1>=2

cap joinby codest using "Intermedias/ece_r2.dta", unm(m)  /*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
cap tab _merge
cap drop _merge

replace grado_r2=2 if grado_r1==2 & grado_r2==.

save "Finales/tables_stlima.dta", replace


***************************************************
*	2) GENERATE MEAN AND SD FOR SCORES FROM ROUND 1
***************************************************
foreach y in raven pcotskill pcsrskill {
	use "Finales/tables_stlima.dta", clear
	keep if participated_in_lottery==1 & won_lottery==0
	keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
	keep if pchome_r1~=. & pchome_r2~=.
	keep if `y'_r1~=. & `y'_r2~=.
	collapse (mean) `y'_r1 (sd) sd_`y'_r1=`y'_r1, by (grado_r1)
	rename (`y'_r1) (mean_`y'_r1)
	save "Auxiliares/stscore_`y'_r1.dta", replace
	}


use "Finales/tables_stlima.dta", clear
keep if participated_in_lottery==1 & won_lottery==0
cap keep if math_r1~=. & m500_m_11~=.							/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
collapse (mean) math_r1 (sd) sd_math_r1=math_r1, by (grado_r1)
rename (math_r1) (mean_math_r1)
save "Auxiliares/stscore_math2_r1.dta", replace

use "Finales/tables_stlima.dta", clear
keep if participated_in_lottery==1 & won_lottery==0
cap keep if read_r1~=. & m500_c_11~=.							/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
collapse (mean) read_r1 (sd) sd_read_r1=read_r1, by (grado_r1)
rename (read_r1) (mean_read_r1)
save "Auxiliares/stscore_read2_r1.dta", replace


merge 1:1 grado_r1 using "Auxiliares/stscore_raven_r1.dta", nogen
merge 1:1 grado_r1 using "Auxiliares/stscore_pcsrskill_r1.dta", nogen
merge 1:1 grado_r1 using "Auxiliares/stscore_pcotskill_r1.dta", nogen
merge 1:1 grado_r1 using "Auxiliares/stscore_math2_r1.dta", nogen update
save "Auxiliares/standardscores_r1.dta", replace


***************************************************
*	3) GENERATE MEAN AND SD FOR SCORES FROM ROUND 2
***************************************************
foreach y in raven pcotskill pcsrskill {
	use "Finales/tables_stlima.dta", clear
	keep if participated_in_lottery==1 & won_lottery==0
	keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
	keep if pchome_r1~=. & pchome_r2~=.
	keep if `y'_r1~=. & `y'_r2~=.
	collapse (mean) `y'_r2 (sd) sd_`y'_r2=`y'_r2, by (grado_r2)
	rename (`y'_r2) (mean_`y'_r2)
	save "Auxiliares/stscore_`y'_r2.dta", replace
	}

cap use "Finales/tables_stlima.dta", clear									/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
cap keep if participated_in_lottery==1 & won_lottery==0
cap keep if math_r1~=. & m500_m_11~=.
cap collapse (mean) m500_m_11 (sd) sd_mece_r2=m500_m_11, by (grado_r2)
cap rename (m500_m_11) (mean_mece_r2)
cap save "Auxiliares/stscore_mece_r2.dta", replace

cap use "Finales/tables_stlima.dta", clear									/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
cap keep if participated_in_lottery==1 & won_lottery==0
cap keep if read_r1~=. & m500_c_11~=.
cap collapse (mean) m500_c_11 (sd) sd_rece_r2=m500_c_11, by (grado_r2)
cap rename (m500_c_11) (mean_rece_r2)
cap save "Auxiliares/stscore_rece_r2.dta", replace

use "Finales/tables_stlima.dta", clear
keep if participated_in_lottery==1 & won_lottery==0
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if pchome_r1~=. & pchome_r2~=.
keep if xo_r2~=. 
collapse (mean) xo_r2 (sd) sd_xo_r2=xo_r2, by (grado_r2)
rename (xo_r2) (mean_xo_r2)
save "Auxiliares/stscore_xo_r2.dta", replace

merge 1:1 grado_r2 using "Auxiliares/stscore_raven_r2.dta", nogen 
merge 1:1 grado_r2 using "Auxiliares/stscore_pcsrskill_r2.dta", nogen
merge 1:1 grado_r2 using "Auxiliares/stscore_pcotskill_r2.dta", nogen
cap merge 1:1 grado_r2 using "Auxiliares/stscore_mece_r2.dta", nogen		/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
cap merge 1:1 grado_r2 using "Auxiliares/stscore_rece_r2.dta", nogen		/*(WARNING: Due to the confidentiality of the ECE the do file will not read these lines)*/
save "Auxiliares/standardscores_r2.dta", replace

cd "/Users/simonjean/Projects/machine_learning_project/data/Auxiliares"	
!erase stscore_*.dta

	
	
**************
*	4) TABLE 1
**************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if participated_in_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
gen tcol= participated_in_lottery==1 & won_lottery==1

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable1_stlima.xls
!erase ntable1_stlima.txt 

foreach y in age male sibling fathlivh fathwout mothwout  phone electri car  {
quiet reg `y'_r2 if tcol==1
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet areg `y'_r2  tcol, a(cfixeff) cl(codmod)
outreg2 tcol using ntable1_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in pchome inthome {
quiet reg `y'_r1 if tcol==1& `y'_r2~=.
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet reg `y'_r1 if tcol==0& `y'_r2~=.
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet areg `y'_r1  tcol if `y'_r2~=., a(cfixeff) cl(codmod)
outreg2 tcol using ntable1_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in stpcotskill stpcsrskill straven {
quiet reg `y'_r1 if tcol==1& `y'_r2~=.
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet reg `y'_r1 if tcol==0& `y'_r2~=.
outreg2 using ntable1_stlima.xls, noas dec(2)
quiet areg `y'_r1  tcol if `y'_r2~=., a(cfixeff) cl(codmod)
outreg2 tcol using ntable1_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


*Add balance for 2nd grade math and reading (WARNING: Due to the confidentiality of the ECE the do file will not estimate these results)
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
	
	*Keep relevant observations
	keep if grado_r1==2
	keep if participated_in_lottery==1
	gen tcol= participated_in_lottery==1 & won_lottery==1
	
	*Generate standardized scores
	gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
	gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
	
	*Table
	egen cfixeff=group(codmod grado_r1 seccion_r1)
	cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	

	cap foreach y in stmath {
	cap quiet reg `y'_r1 if tcol==1 &  m500_m_11~=.
	cap outreg2 using ntable1_stlima.xls, noas dec(2)
	cap quiet reg `y'_r1 if tcol==0 &  m500_m_11~=.
	cap outreg2 using ntable1_stlima.xls, noas dec(2)
	cap quiet areg `y'_r1  tcol if m500_m_11~=., a(cfixeff) cl(codmod)
	cap outreg2 tcol using ntable1_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
	cap }

	cap foreach y in stread {
	cap quiet reg `y'_r1 if tcol==1 &  m500_c_11~=.
	cap outreg2 using ntable1_stlima.xls, noas dec(2)
	cap quiet reg `y'_r1 if tcol==0 &  m500_c_11~=.
	cap outreg2 using ntable1_stlima.xls, noas dec(2)
	cap quiet areg `y'_r1  tcol if m500_c_11~=., a(cfixeff) cl(codmod)
	cap outreg2 tcol using ntable1_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
	cap }
	cap clear


**************
*	5) TABLE 2
**************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if participated_in_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
gen tcol= participated_in_lottery==1 & won_lottery==1
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable2_stlima.xls
!erase ntable2_stlima.txt 

foreach y in pchome pcuweek pcuyest pcuwsch pcuwhome pcuwcafe pcuwfri pcuytwsch pcuytwhome pcuytwcafe pcuytwfri pcthwork pctgame pctmusic pctvideo inthome intuweek {
quiet reg `y'_r2 if tcol==1& `y'_r1~=.
outreg2 using ntable2_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0& `y'_r1~=.
outreg2 using ntable2_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  tcol $covariates				, a(cfixeff) cl(codmod)
outreg2 tcol using ntable2_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


********************************************************************************
*	6) TABLE 3
********************************************************************************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if participated_in_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
gen tcol= participated_in_lottery==1 & won_lottery==1
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"


*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"
!erase ntable3_stlima.xls
!erase ntable3_stlima.txt 

foreach y in stxo {
quiet reg `y'_r2 if tcol==1
outreg2 using ntable3_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0
outreg2 using ntable3_stlima.xls, noas dec(2)
quiet areg `y'_r2   tcol $covariates, a(cfixeff) cl(codmod)
outreg2 tcol using ntable3_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in stpcotskill stpcsrskill straven friendh efforth eduexpt {
quiet reg `y'_r2 if tcol==1& `y'_r1~=.
outreg2 using ntable3_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0& `y'_r1~=.
outreg2 using ntable3_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  tcol $covariates, a(cfixeff) cl(codmod)
outreg2 tcol using ntable3_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}
clear

*Now insert results from 2011 ECE for 2nd graders 					(WARNING: Due to confidentiality of the ECE the do file will not estimate these results)
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

	*Keep relevant observations
	keep if grado_r1==2
	keep if participated_in_lottery==1
	gen tcol= participated_in_lottery==1 & won_lottery==1
	
	*Generate standardized scores
	gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
	gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
	cap gen stmath_r2=(m500_m_11-mean_mece_r2)/sd_mece_r2
	cap gen stread_r2=(m500_c_11-mean_rece_r2)/sd_rece_r2

	*Table
	egen cfixeff=group(codmod grado_r1 seccion_r1)
	cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	

	cap foreach y in stmath stread{
	cap quiet reg `y'_r2 if tcol==1 &  `y'_r1~=.
	cap outreg2 using ntable3_stlima.xls, noas dec(2)
	cap quiet reg `y'_r2 if tcol==0 &  `y'_r1~=.
	cap outreg2 using ntable3_stlima.xls, noas dec(2)
	cap quiet areg `y'_r2  tcol if `y'_r1~=., a(cfixeff) cl(codmod)
	cap outreg2 tcol using ntable3_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
	cap }
	clear

********************************************************************************
*	7) TABLE 4
********************************************************************************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if participated_in_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
gen tcol= participated_in_lottery==1 & won_lottery==1
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"

egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"
!erase ntable4_stlima.xls
!erase ntable4_stlima.txt 

foreach y in uthelpho utcaring utshopp utwstree utwstore uthwork utplay utwattv utread wfrndbest wfrndvist wfrndwork wfrndtot {
quiet reg `y'_r2 if tcol==1& `y'_r1~=.
outreg2 using ntable4_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0& `y'_r1~=.
outreg2 using ntable4_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  tcol $covariates, a(cfixeff) cl(codmod)
outreg2 tcol using ntable4_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}



**************
*	8) TABLE 5
**************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=. &  wfrndtotplott_r2~=.
keep if participated_in_lottery==1 & won_lottery==0
keep if pchome_r1~=. & pchome_r2~=.
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"

gen loswfnd= (participated_in_lottery==1 & won_lottery==0) & (wfrndtotwlott_r1>=1 & wfrndtotwlott_r1!=.)
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable5_stlima.xls
!erase ntable5_stlima.txt 

foreach y in pchome pcuweek pcuyest {
quiet reg `y'_r2  if `y'_r1!=. &  loswfnd==1 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet reg `y'_r2  if `y'_r1!=. &  loswfnd==0 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  loswfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 loswfnd  using ntable5_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}      

foreach y in stxo {
quiet reg `y'_r2  if loswfnd==1 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet reg `y'_r2  if loswfnd==0 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet areg `y'_r2  loswfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 loswfnd  using ntable5_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


foreach y in stpcotskill stpcsrskill straven friendh efforth eduexpt {
quiet reg `y'_r2  if `y'_r1!=. &  loswfnd==1 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet reg `y'_r2  if `y'_r1!=. &  loswfnd==0 
outreg2 using ntable5_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  loswfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 loswfnd  using ntable5_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


**************
*	9) TABLE 6
**************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=. &  wfrndtotplott_r2~=.
keep if participated_in_lottery==1 & won_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"

gen winwfnd= (participated_in_lottery==1 & won_lottery==1) & (wfrndtotwlott_r1>=1 & wfrndtotwlott_r1!=.)
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable6_stlima.xls
!erase ntable6_stlima.txt 

foreach y in pchome pcuweek pcuyest {
quiet reg `y'_r2  if `y'_r1!=. &  winwfnd==1 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet reg `y'_r2  if `y'_r1!=. &  winwfnd==0 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  winwfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 winwfnd  using ntable6_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in stxo {
quiet reg `y'_r2  if winwfnd==1 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet reg `y'_r2  if winwfnd==0 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet areg `y'_r2  winwfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 winwfnd  using ntable6_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


foreach y in stpcotskill stpcsrskill straven friendh efforth eduexpt {
quiet reg `y'_r2  if `y'_r1!=. &  winwfnd==1 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet reg `y'_r2  if `y'_r1!=. &  winwfnd==0 
outreg2 using ntable6_stlima.xls, noas dec(2)
quiet areg `y'_r2  `y'_r1  winwfnd  wfrndtotplott_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 winwfnd  using ntable6_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

**************
*	10) TABLE 7
**************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
set more off

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if pchome_r1~=. & pchome_r2~=.

joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2


*Generate weights for spillovers on classmates
global dep_t7 "pchome pcuweek pcuyest stpcotskill stpcsrskill straven friendh efforth eduexpt"
global dep_t7a "stxo"

egen class=group( codmod grado_r1 seccion_r1)

foreach x in $dep_t7{
gen factor_`x'=1 if participated_in_lottery==0
egen part_`x'=sum(participated_in_lottery) if `x'_r1~=. & `x'_r2~=., by(class)
egen obt_`x'=sum(won_lottery)if `x'_r1~=. & `x'_r2~=., by(class)
g no_obt_`x'=part_`x'-obt_`x'
g lost_`x'=part_`x'/no_obt_`x'
replace factor_`x'=lost_`x' if participated_in_lottery==1
drop part_* obt_* no_obt_* lost_*
}

foreach x in $dep_t7a {
gen factor_`x'=1 if participated_in_lottery==0
egen part_`x'=sum(participated_in_lottery) if  `x'_r2~=., by(class)
egen obt_`x'=sum(won_lottery)if `x'_r2~=., by(class)
g no_obt_`x'=part_`x'-obt_`x'
g lost_`x'=part_`x'/no_obt_`x'
replace factor_`x'=lost_`x' if participated_in_lottery==1
drop part_* obt_* no_obt_* lost_*
}


*Identify kids that reply to each outcome in both rounds
foreach x in $dep_t7 {
gen reg_`x'=`x'_r1~=. & `x'_r2~=.
}

foreach x in $dep_t7a {
gen reg_`x'=`x'_r2~=.
}

gen stxo_r1=.

*Rename relevant outcomes to make reshape easier
foreach x in $dep_t7 $dep_t7a{
ren `x'_r1 `x'1
ren `x'_r2 `x'2
}

*Reshape data to panel format (long)
reshape long pchome pcuweek pcuyest stpcotskill stpcsrskill straven friendh efforth eduexpt stxo, i( codest pair factor_pchome reg_pchome treatment_school) j(round)

*Keep only kids that: (a) participated and did not win; (b) did not participate; (c) study in control schools 
keep if won_lottery==0

*Keep only relevant variables
keep codest codmod class pair factor_* reg_* treatment_school round $dep_t7 $dep_t7a

*dDo diff in diff regressions
gen post=round==2
gen DD=post*treatment_school
xi i.pair 

*Table
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable7_stlima.xls
!erase ntable7_stlima.txt
set more off
foreach x in  $dep_t7 {
quiet reg `x' if post==0 & treatment_school==1 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
quiet reg `x' if post==1 & treatment_school==1 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
quiet reg `x' if post==0 & treatment_school==0 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
quiet reg `x' if post==1 & treatment_school==0 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet cgmwildboot `x' treatment_school post DD _Ipair* if  reg_`x'==1 [aw=factor_`x'] , cluster(codmod) bootcluster(codmod) reps(1000) seed(999) null(. . 0 . . . . . . . . . . . . .) 
outreg2 DD using ntable7_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach x in  $dep_t7a {
quiet reg `x' if  treatment_school==1 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
quiet reg `x' if treatment_school==0 & reg_`x'==1
outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet cgmwildboot `x' treatment_school _Ipair* if  reg_`x'==1 [aw=factor_`x'] , cluster(codmod) bootcluster(codmod) reps(1000) seed(999) null(0 . . . . . . . . . . . . .) 
outreg2 treatment_school using ntable7_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}
clear

*Add math and read for 2nd graders  
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

set more off
keep if grado_r1==2
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge
gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
cap gen stmath_r2=(m500_m_11-mean_mece_r2)/sd_mece_r2 				/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/
cap gen stread_r2=(m500_c_11-mean_rece_r2)/sd_rece_r2 				/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/

*Generate weights for spillovers on classmates
cap global dep_t7b "stmath stread"									/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/

egen class=group( codmod grado_r1 seccion_r1)

cap foreach x in $dep_t7b {
cap gen factor_`x'=1 if participated_in_lottery==0
cap egen part_`x'=sum(participated_in_lottery) if `x'_r1~=. & `x'_r2~=., by(class)
cap egen obt_`x'=sum(won_lottery)if `x'_r1~=. & `x'_r2~=., by(class)
cap g no_obt_`x'=part_`x'-obt_`x'
cap g lost_`x'=part_`x'/no_obt_`x'
cap replace factor_`x'=lost_`x' if participated_in_lottery==1
cap drop part_* obt_* no_obt_* lost_*
cap }

*Identify kids that reply to each outcome in both rounds
cap foreach x in $dep_t7b {
cap gen reg_`x'=`x'_r1~=. & `x'_r2~=.
cap }

*Rename relevant outcomes to make reshape easier
cap foreach x in $dep_t7b{
cap ren `x'_r1 `x'1
cap ren `x'_r2 `x'2
cap }

*Reshape data to panel format (long)
cap  reshape long stmath stread, i( codest pair treatment_school) j(round)

*Keep only kids that: (a) participated and did not win; (b) did not participate; (c) study in control schools 
keep if won_lottery==0

*Keep only relevant variables
cap keep codest codmod class pair factor_* reg_* treatment_school round $dep_t7b

*Do diff in diff regressions
gen post=round==2
gen DD=post*treatment_school
xi i.pair

cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"
set more off	
cap foreach x in  $dep_t7b {
cap quiet reg `x' if post==0 & treatment_school==1 & reg_`x'==1
cap outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet reg `x' if post==1 & treatment_school==1 & reg_`x'==1
cap outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet reg `x' if post==0 & treatment_school==0 & reg_`x'==1
cap outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet reg `x' if post==1 & treatment_school==0 & reg_`x'==1
cap outreg2 using ntable7_stlima.xls, noas dec(2)
cap quiet cgmwildboot `x' treatment_school post DD _Ipair* if  reg_`x'==1 [aw=factor_`x'] , cluster(codmod) bootcluster(codmod) reps(1000) seed(999) null(. . 0 . . . . . . . . . . . . .) 
cap outreg2 DD using ntable7_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
cap }



***************
*	11) TABLE 8
***************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear

joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=.
keep if participated_in_lottery==1
keep if pchome_r1~=. & pchome_r2~=.

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2

*Generate dummy variables 
gen tcol= participated_in_lottery==1 & won_lottery==1
gen female=male_r2==0
replace female=. if male_r2==.
gen g34=grado_r1>=3 & grado_r1<=4
gen g56=grado_r1>=5 & grado_r1<=6
gen npc= pchome_r1==0
replace npc=. if  pchome_r1==.
gen nint=inthome_r1==0
replace nint=. if inthome_r1==.

*Generate Academic Achievement Groups
egen avmr_r1=rowmean(math_r1 read_r1)
replace avmr_r1=. if (math_r1==. | read_r1==.)
gen bmavmr_r1=.
gen bmmath_r1=.
gen bmread_r1=.
foreach x in math_r1 read_r1 avmr_r1 {
	forv i=3/6 {
		qui summ `x' if grado_r1==`i', d
		local `x'_`i'=r(p50)
		display ``x'_`i''
		replace bm`x'=1 if `x'<=``x'_`i'' & grado_r1==`i'
		replace bm`x'=0 if `x'>``x'_`i'' & grado_r1==`i' & `x'!=.
		}
	}
gen ammath_r1=bmmath_r1==0
replace ammath_r1=. if bmmath_r1==.
gen amread_r1=bmread_r1==0
replace amread_r1=. if bmread_r1==.
gen amavmr_r1=bmavmr_r1==0
replace amavmr_r1=. if bmavmr_r1==.

*generate interactions
foreach x in male_r2 female g34 g56 pchome_r1 npc inthome_r1 nint bmmath_r1 bmread_r1 bmavmr_r1 ammath_r1 amread_r1 amavmr_r1{
gen t_`x'=tcol*`x'
}

*Table:
*------
global covariates  "male_r2 age_r2 sibling_r2 ysibling_r2 fathlivh_r2 fathwout_r2 mothwout_r2"
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntable8_stlima.xls
!erase ntable8_stlima.txt 

foreach y in pchome pcuweek pcuyest {
quiet areg `y'_r2  `y'_r1  t_male_r2 t_female $covariates , a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 g34  t_g34 t_g56 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 pchome_r1 t_pchome_r1 t_npc $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 bmavmr_r1 t_bmavmr_r1 t_amavmr_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in stxo {
quiet areg `y'_r2   t_male_r2 t_female $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2   g34  t_g34 t_g56 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2   pchome_r1 t_pchome_r1 t_npc $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  bmavmr_r1 t_bmavmr_r1 t_amavmr_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


foreach y in stpcotskill stpcsrskill straven friendh efforth eduexpt {
quiet areg `y'_r2  `y'_r1  t_male_r2 t_female $covariates 			  , a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 g34  t_g34 t_g56 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 pchome_r1 t_pchome_r1 t_npc $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
quiet areg `y'_r2  `y'_r1 bmavmr_r1 t_bmavmr_r1 t_amavmr_r1 $covariates, a(cfixeff) cl(codmod)
outreg2 t_* using ntable8_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}
clear


****************
*	12) TABLE A1
****************
do "/Users/simonjean/Projects/machine_learning_project/data/table_stlima_logs.do"


****************
*	13) TABLE A2
****************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=. &  wfrndtotplott_r2~=.
keep if participated_in_lottery==1 & won_lottery==0
keep if pchome_r1~=. & pchome_r2~=.
gen tcol=wfrndtotwlott_r1>=1 & wfrndtotwlott_r1!=. 

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntablea2_stlima.xls
!erase ntablea2_stlima.txt 

foreach y in age male sibling fathlivh fathwout mothwout  phone electri car {
quiet reg `y'_r2 if tcol==1
outreg2 using ntablea2_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0
outreg2 using ntablea2_stlima.xls, noas dec(2)
quiet areg `y'_r2  tcol  wfrndtotplott_r1, a(cfixeff) cl(codmod)
outreg2 tcol using ntablea2_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in pchome inthome stpcotskill stpcsrskill straven{
quiet reg `y'_r1 if tcol==1& `y'_r2~=.
outreg2 using ntablea2_stlima.xls, noas dec(2)
quiet reg `y'_r1 if tcol==0& `y'_r2~=.
outreg2 using ntablea2_stlima.xls, noas dec(2)
quiet areg `y'_r1  tcol  wfrndtotplott_r1 if `y'_r2~=., a(cfixeff) cl(codmod)
outreg2 tcol using ntablea2_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


****************
*	14) TABLE A3
****************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if male_r2~=.& age_r2~=.& sibling_r2~=.& ysibling_r2~=.& fathlivh_r2~=.& fathwout_r2~=.& mothwout_r2~=. &  wfrndtotplott_r2~=.
keep if participated_in_lottery==1 & won_lottery==1
keep if pchome_r1~=. & pchome_r2~=.
gen tcol=wfrndtotwlott_r1>=1 & wfrndtotwlott_r1!=. 

*Table
egen cfixeff=group(codmod grado_r1 seccion_r1)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntablea3_stlima.xls
!erase ntablea3_stlima.txt 

foreach y in age male sibling fathlivh fathwout mothwout  phone electri car {
quiet reg `y'_r2 if tcol==1
outreg2 using ntablea3_stlima.xls, noas dec(2)
quiet reg `y'_r2 if tcol==0
outreg2 using ntablea3_stlima.xls, noas dec(2)
quiet areg `y'_r2  tcol  wfrndtotplott_r1, a(cfixeff) cl(codmod)
outreg2 tcol using ntablea3_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in pchome inthome stpcotskill stpcsrskill straven{
quiet reg `y'_r1 if tcol==1& `y'_r2~=.
outreg2 using ntablea3_stlima.xls, noas dec(2)
quiet reg `y'_r1 if tcol==0& `y'_r2~=.
outreg2 using ntablea3_stlima.xls, noas dec(2)
quiet areg `y'_r1  tcol  wfrndtotplott_r1 if `y'_r2~=., a(cfixeff) cl(codmod)
outreg2 tcol using ntablea3_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}


********************
*	15) TABLE A4
********************
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
set more off

*Keep relevant observations
keep if follow_up==1
keep if grado_r1>=3 & grado_r1<=6
keep if pchome_r1~=. & pchome_r2~=.
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stpcotskill_r1=(pcotskill_r1-mean_pcotskill_r1)/sd_pcotskill_r1
gen stpcsrskill_r1=(pcsrskill_r1-mean_pcsrskill_r1)/sd_pcsrskill_r1
gen straven_r1=(raven_r1-mean_raven_r1)/sd_raven_r1 
gen stpcotskill_r2=(pcotskill_r2-mean_pcotskill_r2)/sd_pcotskill_r2
gen stpcsrskill_r2=(pcsrskill_r2-mean_pcsrskill_r2)/sd_pcsrskill_r2
gen straven_r2=(raven_r2-mean_raven_r2)/sd_raven_r2 
gen stxo_r2=(xo_r2-mean_xo_r2)/sd_xo_r2

*Generate weights 
global dep_t7 "pchome inthome stpcotskill stpcsrskill straven"
global dep_t7a "age male sibling fathlivh fathwout mothwout  phone electri car"

egen class=group( codmod grado_r1 seccion_r1)

foreach x in $dep_t7{
gen factor_`x'=1 if participated_in_lottery==0
egen part_`x'=sum(participated_in_lottery) if `x'_r1~=. & `x'_r2~=., by(class)
egen obt_`x'=sum(won_lottery)if `x'_r1~=. & `x'_r2~=., by(class)
g no_obt_`x'=part_`x'-obt_`x'
g lost_`x'=part_`x'/no_obt_`x'
replace factor_`x'=lost_`x' if participated_in_lottery==1
drop part_* obt_* no_obt_* lost_*
}

foreach x in $dep_t7a {
gen factor_`x'=1 if participated_in_lottery==0
egen part_`x'=sum(participated_in_lottery) if  `x'_r2~=., by(class)
egen obt_`x'=sum(won_lottery)if `x'_r2~=., by(class)
g no_obt_`x'=part_`x'-obt_`x'
g lost_`x'=part_`x'/no_obt_`x'
replace factor_`x'=lost_`x' if participated_in_lottery==1
drop part_* obt_* no_obt_* lost_*
}

*Identify kids that reply to each outcome in both rounds
foreach x in $dep_t7 {
gen reg_`x'=`x'_r1~=. & `x'_r2~=.
}

foreach x in $dep_t7a {
gen reg_`x'=`x'_r2~=.
}

*Keep only kids that: (a) participated and did not win; (b) did not participate; (c) study in control schools 
keep if won_lottery==0

*Table
egen cfixeff=group(pair)
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
!erase ntablea4a_stlima.xls
!erase ntablea4a_stlima.txt 

foreach y in $dep_t7a {
quiet reg `y'_r2 if treatment_school==1 & reg_`y'==1
outreg2 using ntablea4a_stlima.xls, noas dec(2)
quiet reg `y'_r2 if treatment_school==0 & reg_`y'==1
outreg2 using ntablea4a_stlima.xls, noas dec(2)
quiet areg `y'_r2  treatment_school if reg_`y'==1 [aw=factor_`y'], a(cfixeff) cl(codmod)
outreg2 treatment_school using ntablea4a_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

foreach y in $dep_t7 {
quiet reg `y'_r1 if treatment_school==1 & reg_`y'==1
outreg2 using ntablea4a_stlima.xls, noas dec(2)
quiet reg `y'_r1 if treatment_school==0 & reg_`y'==1
outreg2 using ntablea4a_stlima.xls, noas dec(2)
quiet areg `y'_r1  treatment_school  if reg_`y'==1 [aw=factor_`y'], a(cfixeff) cl(codmod)
outreg2 treatment_school using ntablea4a_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
}

*Add math and read for 2nd graders
cd "/Users/simonjean/Projects/machine_learning_project/data"	
use "Finales/tables_stlima.dta",clear
set more off

*Keep relevant observations
keep if grado_r1==2
joinby grado_r1 using "Auxiliares/standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares/standardscores_r2.dta", unm(m)
drop _merge

*Generate standardized scores
gen stmath_r1=(math_r1-mean_math_r1)/sd_math_r1
gen stread_r1=(read_r1-mean_read_r1)/sd_read_r1
cap gen stmath_r2=(m500_m_11-mean_mece_r2)/sd_mece_r2					/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/
cap gen stread_r2=(m500_c_11-mean_rece_r2)/sd_rece_r2					/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/

*Generate weights 
cap global dep_t7b "stmath stread"											/*(WARNING: Due to confidentiality of the ECE the do file will not read these lines)*/

egen class=group( codmod grado_r1 seccion_r1)

cap foreach x in $dep_t7b {
cap gen factor_`x'=1 if participated_in_lottery==0
cap egen part_`x'=sum(participated_in_lottery) if `x'_r1~=. & `x'_r2~=., by(class)
cap egen obt_`x'=sum(won_lottery)if `x'_r1~=. & `x'_r2~=., by(class)
cap g no_obt_`x'=part_`x'-obt_`x'
cap g lost_`x'=part_`x'/no_obt_`x'
cap replace factor_`x'=lost_`x' if participated_in_lottery==1
cap drop part_* obt_* no_obt_* lost_*
cap }

*Identify kids that reply to each outcome in both rounds
cap foreach x in $dep_t7b {
cap gen reg_`x'=`x'_r1~=. & `x'_r2~=.
cap }

*Keep only kids that: (a) participated and did not win; (b) did not participate; (c) study in control schools 
keep if won_lottery==0

*Table
cd "/Users/simonjean/Projects/machine_learning_project/data/Resultados"	
egen cfixeff=group(pair)
cap foreach y in $dep_t7b {
cap quiet reg `y'_r1 if treatment_school==1 & reg_`y'==1
cap outreg2 using ntablea4a_stlima.xls, noas dec(2)
cap quiet reg `y'_r1 if treatment_school==0 & reg_`y'==1
cap outreg2 using ntablea4a_stlima.xls, noas dec(2)
cap quiet areg `y'_r1  treatment_school  if reg_`y'==1 [aw=factor_`y'], a(cfixeff) cl(codmod)
cap outreg2 treatment_school using ntablea4a_stlima.xls, alpha(0.01, 0.05, 0.1) symbol(***,**,*) dec(2) asterisk(se)
cap }
clear






