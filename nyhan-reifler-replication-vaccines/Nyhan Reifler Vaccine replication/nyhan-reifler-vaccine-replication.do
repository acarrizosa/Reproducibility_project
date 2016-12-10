*********************************************************************
*Replication data for												*
*Brendan Nyhan and Jason Reifler									*
*Does correcting myths about the flu vaccine work?					*
*An experimental evaluation of the effects of corrective information*
*Vaccine, forthcoming												*
*********************************************************************

/*predicted probability calculations require installing SPost*/
/*run commented command below if you do not have it installed*/
*net install spost9_ado

/*some graphics require grc1leg*/
/*run commented commands below if you do not have it installed*/
*net from http://www.stata.com
*net cd users
*net cd vwiggins
*net install grc1leg

use "nyhan-reifler-vaccine-replication.dta", clear

/*use YouGov survey weights*/
svyset V101 [pweight=V102]

/*Table 1 - putexcel command requires Stata 13*/

putexcel B1=("Control") C1=("Danger") D1=("Correction") E1=("Total") using fluvaxtable1, replace

putexcel A2=("Age") A3=("18-29") A4=("30-44") A5=("45-59") A6=("60+") using fluvaxtable1, modify

svy,subpop(control): tab agecat4
matrix AgeControl = e(b)'
putexcel B3=matrix(AgeControl) using fluvaxtable1, modify

svy,subpop(danger): tab agecat4
matrix AgeDanger = e(b)'
putexcel C3=matrix(AgeDanger) using fluvaxtable1, modify

svy,subpop(correction): tab agecat4
matrix AgeCorrection = e(b)'
putexcel D3=matrix(AgeCorrection) using fluvaxtable1, modify

svy: tab agecat4
matrix AgeTotal = e(b)'
putexcel E3=matrix(AgeTotal) using fluvaxtable1, modify

putexcel A8=("Gender") A9=("Male") A10=("Female") using fluvaxtable1, modify

svy,subpop(control): tab gender
matrix GenderControl = e(b)'
putexcel B9=matrix(GenderControl) using fluvaxtable1, modify

svy,subpop(danger): tab gender
matrix GenderDanger = e(b)'
putexcel C9=matrix(GenderDanger) using fluvaxtable1, modify

svy,subpop(correction): tab gender
matrix GenderCorrection = e(b)'
putexcel D9=matrix(GenderCorrection) using fluvaxtable1, modify

svy: tab gender
matrix GenderTotal = e(b)'
putexcel E9=matrix(GenderTotal) using fluvaxtable1, modify

putexcel A12=("Education") A13=("High School or less") A14=("Some college") A15=("College grad") A16=("Post-grad") using fluvaxtable1, modify

svy,subpop(control): tab educat4
matrix EducControl = e(b)'
putexcel B13=matrix(EducControl) using fluvaxtable1, modify

svy,subpop(danger): tab educat4
matrix EducDanger = e(b)'
putexcel C13=matrix(EducDanger) using fluvaxtable1, modify

svy,subpop(correction): tab educat4
matrix EducCorrection = e(b)'
putexcel D13=matrix(EducCorrection) using fluvaxtable1, modify

svy: tab educat4
matrix EducTotal = e(b)'
putexcel E13=matrix(EducTotal) using fluvaxtable1, modify

putexcel A18=("Race/Ethnicity") A19=("White") A20=("Black") A21=("Hispanic") A22=("Other") using fluvaxtable1, modify

svy,subpop(control): tab racecat4
matrix RaceControl = e(b)'
putexcel B19=matrix(RaceControl) using fluvaxtable1, modify

svy,subpop(danger): tab racecat4
matrix RaceDanger = e(b)'
putexcel C19=matrix(RaceDanger) using fluvaxtable1, modify

svy,subpop(correction): tab racecat4
matrix RaceCorrection = e(b)'
putexcel D19=matrix(RaceCorrection) using fluvaxtable1, modify

svy: tab racecat4
matrix RaceTotal = e(b)'
putexcel E19=matrix(RaceTotal) using fluvaxtable1, modify

putexcel A44=("Concerned about side effects") A45=("Extremely concerned") A46=("Very concerned") A47=("Somewhat concerned") A48=("Not too concerned") A49=("Not at all concerned")  using fluvaxtable1, modify

svy,subpop(control): tab DAR300
matrix Dar300Control = e(b)'
putexcel B45=matrix(Dar300Control) using fluvaxtable1, modify

svy,subpop(danger): tab DAR300
matrix Dar300Danger = e(b)'
putexcel C45=matrix(Dar300Danger) using fluvaxtable1, modify

svy,subpop(correction): tab DAR300
matrix Dar300Correction = e(b)'
putexcel D45=matrix(Dar300Correction) using fluvaxtable1, modify

svy: tab DAR300
matrix Dar300Total = e(b)'

putexcel E45=matrix(Dar300Total) using fluvaxtable1, modify

sum DAR333_35_treat if DAR333_35_treat==1
putexcel B51=(r(N)) using fluvaxtable1, modify

sum DAR333_35_treat if DAR333_35_treat==2
putexcel C51=(r(N)) using fluvaxtable1, modify

sum DAR333_35_treat if DAR333_35_treat==3
putexcel D51=(r(N)) using fluvaxtable1, modify

sum DAR333_35_treat 
putexcel E51=(r(N)) using fluvaxtable1, modify

svy: tab agecat4 DAR333_35_treat
svy: tab racecat4 DAR333_35_treat
svy: tab educat4 DAR333_35_treat
svy: tab gender DAR333_35_treat
svy: tab DAR300 DAR333_35_treat

/*Figure 1*/

svy: tab vaxgivesflu
svy: tab vaxunsafe
svy: tab getvax

preserve

svy: tab vaxgivesflu
matrix A=e(b)'
svy: tab vaxunsafe
matrix B=e(b)'
svy: tab getvax
matrix C=e(b)'

svy,subpop(control): tab vaxgivesflu 
svy,subpop(control): tab vaxunsafe 
svy,subpop(control): tab getvax 

svmat A
svmat B
svmat C
keep A1 B1 C1 
rename B1 A2 
rename C1 A3
keep if _n<7

gen cat=_n
reshape long A, i(cat) j(dv)

rename A prop
drop if (cat==5 | cat==6) & dv<3

graph bar prop if dv==1, outergap(0) over(cat, gap(5) relabel(4 `" "Very" "accurate" "' 3 `" "Somewhat" "accurate" "' 2 `" "Somewhat" "inaccurate" "' 1 `" "Very" "inaccurate" "')) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) scheme(s2mono) yscale(r(0 .5)) ylabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%",nogrid angle(0) labsize(*.9)) ytitle("") title("Vaccine can give you the flu",size(*1.1)) saving("dvmarg1.gph",replace)

graph bar prop if dv==2, outergap(0) over(cat, gap(5) relabel(1 `" "Very" "safe" "' 2 `" "Somewhat" "safe" "' 3 `" "Not very" "safe" "' 4 `" "Not at all" "safe" "')) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) scheme(s2mono) yscale(r(0 .5)) ylabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%",nogrid angle(0) labsize(*.9)) ytitle("") title("Perceived safety of flu vaccine",size(*1.1)) saving("dvmarg2.gph",replace)

graph bar prop if dv==3, outergap(0) over(cat, gap(5) relabel(6 `" "Very" "likely" "' 5 `" "Somewhat" "likely" "' 4 `" "Slightly" "likely" "' 3 `" "Slightly" "unlikely" "' 2 `" "Somewhat" "unlikely" "' 1 `" "Very" "unlikely" "')) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) scheme(s2mono) yscale(r(0 .5)) ylabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%",nogrid angle(0) labsize(*.9)) ytitle("") title("Likelihood of vaccination",size(*1.1)) saving("dvmarg3.gph",replace)

graph combine "dvmarg1.gph" "dvmarg2.gph" "dvmarg3.gph", ycommon row(1) ysize(4) xsize(13) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none))
graph export "fig1.eps", replace

restore

/*Table 2*/

svy: oprobit vaxgivesflu danger correction
svy,subpop(lowconcern): oprobit vaxgivesflu danger correction
svy,subpop(highconcern): oprobit vaxgivesflu danger correction 

svy: oprobit vaxunsafe danger correction
svy,subpop(lowconcern): oprobit vaxunsafe danger correction 
svy,subpop(highconcern): oprobit vaxunsafe danger correction 

/*Figure 2 - bootstrapped to combine categories, have to use if rather than subpop for SPost to work properly (SEs virtually identical)*/

preserve

rename V101 id
rename V102 weight

svyset id [pweight=weight]

svy: oprobit vaxgivesflu danger correction if lowconcern==1
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(vaxgivesflu1.dta,replace)

svy: oprobit vaxgivesflu danger correction if lowconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(vaxgivesflu2.dta,replace)

svy: oprobit vaxgivesflu danger correction if lowconcern==1
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(vaxgivesflu3.dta,replace)

svy: oprobit vaxgivesflu danger correction if highconcern==1
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(vaxgivesflu4.dta,replace)

svy: oprobit vaxgivesflu danger correction if highconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(vaxgivesflu5.dta,replace)

svy: oprobit vaxgivesflu danger correction if highconcern==1
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(vaxgivesflu6.dta,replace)

/*combine categories*/
forval i=1/6 {
use "vaxgivesflu`i'.dta",clear
gen prob=(b_p3+b_p4) 
gen ll=prob
gen ul=prob
collapse (mean) prob (p5) ll (p95) ul
save "vaxgivesflu`i'b.dta",replace
}

use "vaxgivesflu1b.dta",clear
append using "vaxgivesflu2b.dta"
append using "vaxgivesflu3b.dta"
append using "vaxgivesflu4b.dta"
append using "vaxgivesflu5b.dta"
append using "vaxgivesflu6b.dta"
gen at=.
replace at=1 if _n==1
replace at=1.95 if _n==2
replace at=2.9 if _n==3
replace at=4.1 if _n==4
replace at=5.05 if _n==5
replace at=6 if _n==6

twoway (rspike ll ul at if at<4,lcolor(black) scheme(s1mono) graphregion(fcolor(white) ifcolor(none) margin(t=10 b=10)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) xscale(r(.75 6.25)) ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%",angle(0) labsize(*.6)) ymtick(.1(.2).7) yscale(r(0 .8))  title("Vaccine can give you flu",size(*.5)) xtitle("") xlabel(1 `" "Control" "' 1.95 `" "Danger" "' 2.9 `" "Correction" "' 4.1 `" "Control" "' 5.05 `" "Danger" "' 6 `" "Correction" "',labsize(*.6)))(scatter prob at if at<4,saving("giveflu.gph",replace) mcolor(black)  msymbol(o) msize(*.6))(rspike ll ul at if at>3,lcolor(gs8))(scatter prob at if at>3,mcolor(gs8) msymbol(o) msize(*.6) legend(lab(5 "Low side effects concern") lab(6 "High side effects concern") order(5 6) symxsize(*1.1) symysize(*1.1) size(*1.1))) (connected ul at if at>6,lcolor(black) mcolor(black) lpattern(solid) msymbol(O)) (connected ul at if at>6,lcolor(gs8) mcolor(gs8) lpattern(solid) msymbol(O)) 

restore

preserve

rename V101 id
rename V102 weight

svyset id [pweight=weight]

svy: oprobit vaxunsafe danger correction if lowconcern==1 
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(vaxunsafe1.dta,replace)

svy: oprobit vaxunsafe danger correction if lowconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(vaxunsafe2.dta,replace)

svy: oprobit vaxunsafe danger correction if lowconcern==1
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(vaxunsafe3.dta,replace)

svy: oprobit vaxunsafe danger correction if highconcern==1
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(vaxunsafe4.dta,replace)

svy: oprobit vaxunsafe danger correction if highconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(vaxunsafe5.dta,replace)

svy: oprobit vaxunsafe danger correction if highconcern==1
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(vaxunsafe6.dta,replace)

/*combine categories*/
forval i=1/6 {
use "vaxunsafe`i'.dta",clear
gen prob=(b_p3+b_p4) 
gen ll=prob
gen ul=prob
collapse (mean) prob (p5) ll (p95) ul
save "vaxunsafe`i'b.dta",replace
}

use "vaxunsafe1b.dta",clear
append using "vaxunsafe2b.dta"
append using "vaxunsafe3b.dta"
append using "vaxunsafe4b.dta"
append using "vaxunsafe5b.dta"
append using "vaxunsafe6b.dta"
gen at=.
replace at=1 if _n==1
replace at=1.95 if _n==2
replace at=2.9 if _n==3
replace at=4.1 if _n==4
replace at=5.05 if _n==5
replace at=6 if _n==6

twoway (rspike ll ul at if at<4,lcolor(black) scheme(s1mono) graphregion(fcolor(white) ifcolor(none) margin(t=10 b=10)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) xscale(r(.75 6.25)) ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%",angle(0) labsize(*.6)) ymtick(.1(.2).7) yscale(r(0 .8))  title("Flu vaccine unsafe",size(*.5)) xtitle("") xlabel(1 `" "Control" "' 1.95 `" "Danger" "' 2.9 `" "Correction" "' 4.1 `" "Control" "' 5.05 `" "Danger" "' 6 `" "Correction" "',labsize(*.6)))(scatter prob at if at<4,saving("safety.gph",replace) mcolor(black)  msymbol(o) msize(*.6) legend(off))(rspike ll ul at if at>3,lcolor(gs8))(scatter prob at if at>3, mcolor(gs8)  msymbol(o) msize(*.6)) (connected ul at if at>6,lcolor(black) mcolor(black) lpattern(solid) msymbol(O)) (connected ul at if at>6,lcolor(gs8) mcolor(gs8) lpattern(solid) msymbol(O)) 

grc1leg "giveflu.gph" "safety.gph", rows(1) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) imargin(0 0 0 3) iscale(*2.5) legendfrom("giveflu.gph") name(f2,replace) ycommon
graph display f2,xsize(12) ysize(4)
graph export "fig2.eps", replace

restore

/*Table 3*/

svy: oprobit getvax danger correction
svy,subpop(lowconcern): oprobit getvax danger correction 
svy,subpop(highconcern): oprobit getvax danger correction 

/*Figure 3 - bootstrapped to combine categories, have to use if rather than subpop for SPost to work properly (SEs virtually identical)*/

preserve

rename V101 id
rename V102 weight

svyset id [pweight=weight]

svy: oprobit getvax danger correction if lowconcern==1
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(getvax1.dta,replace)

svy: oprobit getvax danger correction if lowconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(getvax2.dta,replace)

svy: oprobit getvax danger correction if lowconcern==1
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(getvax3.dta,replace)

svy: oprobit getvax danger correction if highconcern==1
prvalue,x(danger==0 correction==0) bootstrap reps(1000) saving(getvax4.dta,replace)

svy: oprobit getvax danger correction if highconcern==1
prvalue,x(danger==1 correction==0) bootstrap reps(1000) saving(getvax5.dta,replace)

svy: oprobit getvax danger correction if highconcern==1 
prvalue,x(danger==0 correction==1) bootstrap reps(1000) saving(getvax6.dta,replace)

/*combine categories*/
forval i=1/6 {
use "getvax`i'.dta",clear
gen prob=(b_p4+b_p5+b_p6) 
gen ll=prob
gen ul=prob
collapse (mean) prob (p5) ll (p95) ul
save "getvax`i'b.dta",replace
}

use "getvax1b.dta",clear
append using "getvax2b.dta"
append using "getvax3b.dta"
append using "getvax4b.dta"
append using "getvax5b.dta"
append using "getvax6b.dta"
gen at=.
replace at=1 if _n==1
replace at=1.95 if _n==2
replace at=2.9 if _n==3
replace at=4.1 if _n==4
replace at=5.05 if _n==5
replace at=6 if _n==6

twoway (rspike ll ul at if at<4,lcolor(black) scheme(s1mono) graphregion(fcolor(white) ifcolor(none) margin(t=10 b=10)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) xscale(r(.75 6.25)) ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%",angle(0) labsize(*1)) ymtick(.1(.2).7) yscale(r(0 .8))  title("Likely to get flu vaccine",size(*.9)) xtitle("") xlabel(1 `" "Control" "' 1.95 `" "Danger" "' 2.9 `" "Correction" "' 4.1 `" "Control" "' 5.05 `" "Danger" "' 6 `" "Correction" "',labsize(*1)))(scatter prob at if at<4,saving("giveflu.gph",replace) mcolor(black)  msymbol(o) msize(*1))(rspike ll ul at if at>3,lcolor(gs8))(scatter prob at if at>3,mcolor(gs8) msymbol(o) msize(*1) legend(lab(5 "Low side effects concern") lab(6 "High side effects concern") order(5 6) symxsize(*1) symysize(*1) size(*1))) (connected ul at if at>6,lcolor(black) mcolor(black) lpattern(solid) msymbol(O)) (connected ul at if at>6,lcolor(gs8) mcolor(gs8) lpattern(solid) msymbol(O) xsize(6) ysize(4)) 
graph export "fig3.eps", replace

restore

/*Appendix table 1 - putexcel requires Stata 13*/

putexcel B1=("Control") C1=("Danger") D1=("Correction") E1=("Total") using fluvaxtableA1, replace

putexcel A2=("Age") A3=("18-29") A4=("30-44") A5=("45-59") A6=("60+") using fluvaxtableA1, modify

svy,subpop(control): tab agecat4
matrix AgeControl = e(b)'
putexcel B3=matrix(AgeControl) using fluvaxtableA1, modify

svy,subpop(danger): tab agecat4
matrix AgeDanger = e(b)'
putexcel C3=matrix(AgeDanger) using fluvaxtableA1, modify

svy,subpop(correction): tab agecat4
matrix AgeCorrection = e(b)'
putexcel D3=matrix(AgeCorrection) using fluvaxtableA1, modify

svy: tab agecat4
matrix AgeTotal = e(b)'
putexcel E3=matrix(AgeTotal) using fluvaxtableA1, modify

putexcel A8=("Gender") A9=("Male") A10=("Female") using fluvaxtableA1, modify

svy,subpop(control): tab gender
matrix GenderControl = e(b)'
putexcel B9=matrix(GenderControl) using fluvaxtableA1, modify

svy,subpop(danger): tab gender
matrix GenderDanger = e(b)'
putexcel C9=matrix(GenderDanger) using fluvaxtableA1, modify

svy,subpop(correction): tab gender
matrix GenderCorrection = e(b)'
putexcel D9=matrix(GenderCorrection) using fluvaxtableA1, modify

svy: tab gender
matrix GenderTotal = e(b)'
putexcel E9=matrix(GenderTotal) using fluvaxtableA1, modify

putexcel A12=("Education") A13=("High School or less") A14=("Some college") A15=("College grad") A16=("Post-grad") using fluvaxtableA1, modify

svy,subpop(control): tab educat4
matrix EducControl = e(b)'
putexcel B13=matrix(EducControl) using fluvaxtableA1, modify

svy,subpop(danger): tab educat4
matrix EducDanger = e(b)'
putexcel C13=matrix(EducDanger) using fluvaxtableA1, modify

svy,subpop(correction): tab educat4
matrix EducCorrection = e(b)'
putexcel D13=matrix(EducCorrection) using fluvaxtableA1, modify

svy: tab educat4
matrix EducTotal = e(b)'
putexcel E13=matrix(EducTotal) using fluvaxtableA1, modify

putexcel A18=("Race/Ethnicity") A19=("White") A20=("Black") A21=("Hispanic") A22=("Other") using fluvaxtableA1, modify

svy,subpop(control): tab racecat4
matrix RaceControl = e(b)'
putexcel B19=matrix(RaceControl) using fluvaxtableA1, modify

svy,subpop(danger): tab racecat4
matrix RaceDanger = e(b)'
putexcel C19=matrix(RaceDanger) using fluvaxtableA1, modify

svy,subpop(correction): tab racecat4
matrix RaceCorrection = e(b)'
putexcel D19=matrix(RaceCorrection) using fluvaxtableA1, modify

svy: tab racecat4
matrix RaceTotal = e(b)'
putexcel E19=matrix(RaceTotal) using fluvaxtableA1, modify

putexcel A44=("Concerned about side effects") A45=("Extremely concerned") A46=("Very concerned") A47=("Somewhat concerned") A48=("Not too concerned") A49=("Not at all concerned")  using fluvaxtableA1, modify

svy,subpop(control): tab DAR300
matrix Dar300Control = e(b)'
putexcel B45=matrix(Dar300Control) using fluvaxtableA1, modify

svy,subpop(danger): tab DAR300
matrix Dar300Danger = e(b)'
putexcel C45=matrix(Dar300Danger) using fluvaxtableA1, modify

svy,subpop(correction): tab DAR300
matrix Dar300Correction = e(b)'
putexcel D45=matrix(Dar300Correction) using fluvaxtableA1, modify

svy: tab DAR300
matrix Dar300Total = e(b)'
putexcel E45=matrix(Dar300Total) using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==1
putexcel B51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==2
putexcel C51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==3
putexcel D51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat 
putexcel E51=(r(N)) using fluvaxtableA1, modify

putexcel G1=("ControlW2") H1=("DangerW2") I1=("CorrectionW2") J1=("TotalW2") using fluvaxtableA1, modify 

svy,subpop(wave2control): tab agecat4
matrix AgeControlW2 = e(b)'
putexcel G3=matrix(AgeControlW2) using fluvaxtableA1, modify

svy,subpop(wave2danger): tab agecat4
matrix AgeDangerW2 = e(b)'
putexcel H3=matrix(AgeDangerW2) using fluvaxtableA1, modify

svy,subpop(wave2correction): tab agecat4
matrix AgeCorrectionW2 = e(b)'
putexcel I3=matrix(AgeCorrectionW2) using fluvaxtableA1, modify

svy,subpop(wave2): tab agecat4
matrix AgeTotalW2 = e(b)'
putexcel J3=matrix(AgeTotalW2) using fluvaxtableA1, modify

svy,subpop(wave2control): tab gender
matrix GenderControlW2 = e(b)'
putexcel G9=matrix(GenderControlW2) using fluvaxtableA1, modify

svy,subpop(wave2danger): tab gender
matrix GenderDangerW2 = e(b)'
putexcel H9=matrix(GenderDangerW2) using fluvaxtableA1, modify

svy,subpop(wave2correction): tab gender
matrix GenderCorrectionW2 = e(b)'
putexcel I9=matrix(GenderCorrectionW2) using fluvaxtableA1, modify

svy,subpop(wave2): tab gender
matrix GenderTotalW2 = e(b)'
putexcel J9=matrix(GenderTotalW2) using fluvaxtableA1, modify

svy,subpop(wave2control): tab educat4
matrix EducControlW2 = e(b)'
putexcel G13=matrix(EducControlW2) using fluvaxtableA1, modify

svy,subpop(wave2danger): tab educat4
matrix EducDangerW2 = e(b)'
putexcel H13=matrix(EducDangerW2) using fluvaxtableA1, modify

svy,subpop(wave2correction): tab educat4
matrix EducCorrectionW2 = e(b)'
putexcel I13=matrix(EducCorrectionW2) using fluvaxtableA1, modify

svy,subpop(wave2): tab educat4
matrix EducTotalW2 = e(b)'
putexcel J13=matrix(EducTotalW2) using fluvaxtableA1, modify

svy,subpop(wave2control): tab racecat4
matrix RaceControlW2 = e(b)'
putexcel G19=matrix(RaceControlW2) using fluvaxtableA1, modify

svy,subpop(wave2danger): tab racecat4
matrix RaceDangerW2 = e(b)'
putexcel H19=matrix(RaceDangerW2) using fluvaxtableA1, modify

svy,subpop(wave2correction): tab racecat4
matrix RaceCorrectionW2 = e(b)'
putexcel I19=matrix(RaceCorrectionW2) using fluvaxtableA1, modify

svy,subpop(wave2): tab racecat4
matrix RaceTotalW2 = e(b)'
putexcel J19=matrix(RaceTotalW2) using fluvaxtableA1, modify

svy,subpop(wave2control): tab DAR300
matrix Dar300ControlW2 = e(b)'
putexcel G45=matrix(Dar300ControlW2) using fluvaxtableA1, modify

svy,subpop(wave2danger): tab DAR300
matrix Dar300DangerW2 = e(b)'
putexcel H45=matrix(Dar300DangerW2) using fluvaxtableA1, modify

svy,subpop(wave2correction): tab DAR300
matrix Dar300CorrectionW2 = e(b)'
putexcel I45=matrix(Dar300CorrectionW2) using fluvaxtableA1, modify

svy,subpop(wave2): tab DAR300
matrix Dar300TotalW2 = e(b)'
putexcel J45=matrix(Dar300TotalW2) using fluvaxtableA1, modify

/*note: retention rate not included in table*/
putexcel L1=("Retention Rate") M1=("Attrition Rate") using fluvaxtableA1, modify 

svy,subpop(if agecat4==1): tab attrition
matrix Agecat4_1 = e(b)
putexcel L3=matrix(Agecat4_1) using fluvaxtableA1, modify colwise

svy,subpop(if agecat4==2): tab attrition
matrix Agecat4_2 = e(b)
putexcel L4=matrix(Agecat4_2) using fluvaxtableA1, modify colwise

svy,subpop(if agecat4==3): tab attrition
matrix Agecat4_3 = e(b)
putexcel L5=matrix(Agecat4_3) using fluvaxtableA1, modify colwise

svy,subpop(if agecat4==4): tab attrition
matrix Agecat4_4 = e(b)
putexcel L6=matrix(Agecat4_4) using fluvaxtableA1, modify colwise

svy,subpop(if gender==1): tab attrition
matrix Gender_1 = e(b)
putexcel L9=matrix(Gender_1) using fluvaxtableA1, modify colwise

svy,subpop(if gender==2): tab attrition
matrix Gender_2 = e(b)
putexcel L10=matrix(Gender_2) using fluvaxtableA1, modify colwise

svy,subpop(if educat4==1): tab attrition
matrix Educat4_1 = e(b)
putexcel L13=matrix(Educat4_1) using fluvaxtableA1, modify colwise

svy,subpop(if educat4==2): tab attrition
matrix Educat4_2 = e(b)
putexcel L14=matrix(Educat4_2) using fluvaxtableA1, modify colwise

svy,subpop(if educat4==3): tab attrition
matrix Educat4_3 = e(b)
putexcel L15=matrix(Educat4_3) using fluvaxtableA1, modify colwise

svy,subpop(if educat4==4): tab attrition
matrix Educat4_4 = e(b)
putexcel L16=matrix(Educat4_4) using fluvaxtableA1, modify colwise

svy,subpop(if racecat4==1): tab attrition
matrix Racecat4_1 = e(b)
putexcel L19=matrix(Racecat4_1) using fluvaxtableA1, modify colwise

svy,subpop(if racecat4==2): tab attrition
matrix Racecat4_2 = e(b)
putexcel L20=matrix(Racecat4_2) using fluvaxtableA1, modify colwise

svy,subpop(if racecat4==3): tab attrition
matrix Racecat4_3 = e(b)
putexcel L21=matrix(Racecat4_3) using fluvaxtableA1, modify colwise

svy,subpop(if racecat4==4): tab attrition
matrix Racecat4_4 = e(b)
putexcel L22=matrix(Racecat4_4) using fluvaxtableA1, modify colwise

svy,subpop(if DAR300==1): tab attrition
matrix Dar300_1 = e(b)
putexcel L45=matrix(Dar300_1) using fluvaxtableA1, modify colwise

svy,subpop(if DAR300==2): tab attrition
matrix Dar300_2 = e(b)
putexcel L46=matrix(Dar300_2) using fluvaxtableA1, modify colwise

svy,subpop(if DAR300==3): tab attrition
matrix Dar300_3 = e(b)
putexcel L47=matrix(Dar300_3) using fluvaxtableA1, modify colwise

svy,subpop(if DAR300==4): tab attrition
matrix Dar300_4 = e(b)
putexcel L48=matrix(Dar300_4) using fluvaxtableA1, modify colwise

svy,subpop(if DAR300==5): tab attrition
matrix Dar300_5 = e(b)
putexcel L49=matrix(Dar300_5) using fluvaxtableA1, modify colwise
putexcel A51=("Number of observations") using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==1
putexcel B51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==2
putexcel C51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if DAR333_35_treat==3
putexcel D51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat 
putexcel E51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if wave2control==1
putexcel G51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if wave2danger==1
putexcel H51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if wave2correction==1
putexcel I51=(r(N)) using fluvaxtableA1, modify

sum DAR333_35_treat if wave2==1
putexcel J51=(r(N)) using fluvaxtableA1, modify

svy,subpop(wave2): tab agecat4 DAR333_35_treat
svy,subpop(wave2): tab racecat4 DAR333_35_treat
svy,subpop(wave2): tab educat4 DAR333_35_treat
svy,subpop(wave2): tab gender DAR333_35_treat
svy,subpop(wave2): tab DAR300 DAR333_35_treat

/*Appendix table 2*/

svy: oprobit vaxgivesflu danger correction highconcern dangerXhighconcern correctionXhighconcern
svy: oprobit vaxunsafe danger correction highconcern dangerXhighconcern correctionXhighconcern
svy: oprobit getvax danger correction highconcern dangerXhighconcern correctionXhighconcern

/*Appendix table 3*/

svy,subpop(lowconcern): oprobit vaxgivesflu2 danger correction 
svy,subpop(highconcern): oprobit vaxgivesflu2 danger correction 
svy,subpop(lowconcern): oprobit vaxunsafe2 danger correction 
svy,subpop(highconcern): oprobit vaxunsafe2 danger correction 
svy,subpop(lowconcern): oprobit getvax2 danger correction 
svy,subpop(highconcern): oprobit getvax2 danger correction 

/*robustness: OLS results for Tables 2 and 3*/

svy: reg vaxgivesflu danger correction
svy,subpop(lowconcern): reg vaxgivesflu danger correction
svy,subpop(highconcern): reg vaxgivesflu danger correction 

svy: reg vaxunsafe danger correction
svy,subpop(lowconcern): reg vaxunsafe danger correction 
svy,subpop(highconcern): reg vaxunsafe danger correction 

svy: reg getvax danger correction
svy,subpop(lowconcern): reg getvax danger correction 
svy,subpop(highconcern): reg getvax danger correction 

/*robustness: binary results for Table 2*/

svy: probit givesflubinary danger correction
svy,subpop(lowconcern): probit givesflubinary danger correction 
svy,subpop(highconcern): probit givesflubinary danger correction 

svy: probit unsafebinary danger correction
svy,subpop(lowconcern): probit unsafebinary danger correction 
svy,subpop(highconcern): probit unsafebinary danger correction 

/*Clean up*/

rm "vaxunsafe1.dta"
rm "vaxunsafe1b.dta"
rm "vaxunsafe2.dta"
rm "vaxunsafe2b.dta"
rm "vaxunsafe3.dta"
rm "vaxunsafe3b.dta"
rm "vaxunsafe4.dta"
rm "vaxunsafe4b.dta"
rm "vaxunsafe5.dta"
rm "vaxunsafe5b.dta"
rm "vaxunsafe6.dta"
rm "vaxunsafe6b.dta"

rm "vaxgivesflu1.dta"
rm "vaxgivesflu1b.dta"
rm "vaxgivesflu2.dta"
rm "vaxgivesflu2b.dta"
rm "vaxgivesflu3.dta"
rm "vaxgivesflu3b.dta"
rm "vaxgivesflu4.dta"
rm "vaxgivesflu4b.dta"
rm "vaxgivesflu5.dta"
rm "vaxgivesflu5b.dta"
rm "vaxgivesflu6.dta"
rm "vaxgivesflu6b.dta"

rm "getvax1.dta"
rm "getvax1b.dta"
rm "getvax2.dta"
rm "getvax2b.dta"
rm "getvax3.dta"
rm "getvax3b.dta"
rm "getvax4.dta"
rm "getvax4b.dta"
rm "getvax5.dta"
rm "getvax5b.dta"
rm "getvax6.dta"
rm "getvax6b.dta"

rm "dvmarg1.gph"
rm "dvmarg2.gph"
rm "dvmarg3.gph"
rm "giveflu.gph"
rm "safety.gph"
