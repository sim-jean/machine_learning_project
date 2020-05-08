set more off
clear all

use "C:\data\Finales\table_stlima_logs.dta", clear

****************************************
* 1) CATEGORIES: 3-4 GRADE and 5-6 GRADE
****************************************
drop if mes==.
g g34=.
replace g34=1 if GRADO==3 | GRADO==4
replace g34=0 if GRADO==5 | GRADO==6


********************
* 2) OVERALL USE
********************
g n_min_dia=n_min_mes/30
sum n_min_dia

bysort sexo_hombre: sum n_min_dia
reg n_min_dia sexo_hombre  if sexo_hombre!=., cl(codmod)

bysort g34: sum n_min_dia 
reg n_min_dia g34 if g34!=. , cl(codmod)  

bysort pc_home_abril: sum n_min_dia 
reg n_min_dia pc_home_abril if pc_home_abril!=. , cl(codmod)  


*****************
* 3) USE BY MONTH
******************
table mes , c(mean n_min_dia)

bysort sexo_hombre: table mes , c(mean n_min_dia)
foreach x in 7 8 9 10{
reg n_min_dia sexo_hombre  if sexo_hombre!=. & mes==`x', cl(codmod) 
}

bysort g34: table mes , c(mean n_min_dia)
foreach x in 7 8 9 10{
reg n_min_dia g34 if mes==`x' & g34!=., cl(codmod)  
}

bysort pc_home_abril: table mes , c(mean n_min_dia)
foreach x in 7 8 9 10{
reg n_min_dia pc_home_abril  if pc_home_abril!=. & mes==`x', cl(codmod)  
}


****************************
* 4) USE BY TYPE OF ACTIVITY
****************************
foreach x in Musica Cognicion Utilitarios Lectura Programacion Otros Matematicas Medicion    {
g n_min_dia_`x'=n_min_mes_`x'/30
reg n_min_dia_`x' sexo_hombre if sexo_hombre!=., cl(codmod)
}
bysort sexo_hombre: sum n_min_dia_Musica n_min_dia_Cognicion n_min_dia_Utilitarios n_min_dia_Lectura n_min_dia_Programacion n_min_dia_Otros n_min_dia_Matematicas n_min_dia_Medicion     


foreach x in Musica Cognicion Utilitarios Lectura Programacion Otros Matematicas Medicion {
reg n_min_dia_`x' g34 if g34!=., cl(codmod)
}
bysort g34: sum n_min_dia_Musica n_min_dia_Cognicion n_min_dia_Utilitarios n_min_dia_Lectura n_min_dia_Programacion n_min_dia_Otros n_min_dia_Matematicas n_min_dia_Medicion 


foreach x in Musica Cognicion Utilitarios Lectura Programacion Otros Matematicas Medicion {
reg n_min_dia_`x' pc_home_abril if pc_home_abril!=., cl(codmod)
}
bysort pc_home_abril: sum n_min_dia_Musica n_min_dia_Cognicion n_min_dia_Utilitarios n_min_dia_Lectura n_min_dia_Programacion n_min_dia_Otros n_min_dia_Matematicas n_min_dia_Medicion 

sum n_min_dia_Musica n_min_dia_Cognicion n_min_dia_Utilitarios n_min_dia_Lectura n_min_dia_Programacion n_min_dia_Otros n_min_dia_Matematicas n_min_dia_Medicion 


****************
* 5) SAMPLE SIZE
****************
bysort mes: sum codest
bysort sexo_hombre : sum codest if mes==9
bysort g34: sum codest if mes==9
bysort pc_home_abril: sum codest if mes ==9


*************************************
* 6) BY BASELINE ACADEMIC ACHIEVEMENT
*************************************
cd "C:\data"
use "Finales\tables_stlima.dta",clear

joinby grado_r1 using "Auxiliares\standardscores_r1.dta", unm(m) 
drop _merge
joinby grado_r2 using "Auxiliares\standardscores_r2.dta", unm(m)
drop _merge

merge 1:m codest using "Finales\table_stlima_logs.dta", keep(match)
drop _merge

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

g n_min_dia=n_min_mes/30

bysort bmavmr_r1: sum n_min_dia
reg n_min_dia bmavmr_r1 if bmavmr_r1!=. , cl(codmod) 

bysort bmavmr_r1: table mes , c(mean n_min_dia)
foreach x in 7 8 9 10{
reg n_min_dia bmavmr_r1  if bmavmr_r1!=. & mes==`x', cl(codmod) 
}

foreach x in Musica Cognicion Utilitarios Lectura Programacion Otros Matematicas Medicion    {
g n_min_dia_`x'=n_min_mes_`x'/30
reg n_min_dia_`x' bmavmr_r1 if bmavmr_r1!=., cl(codmod)
}
bysort bmavmr_r1: sum n_min_dia_Musica n_min_dia_Cognicion n_min_dia_Utilitarios n_min_dia_Lectura n_min_dia_Programacion n_min_dia_Otros n_min_dia_Matematicas n_min_dia_Medicion     

bysort bmavmr_r1: sum codest if mes ==9
