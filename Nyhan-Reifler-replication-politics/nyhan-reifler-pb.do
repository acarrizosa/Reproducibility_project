/*STUDY 1*/clear allset more offuse "fall05-data.dta",clear/*Table 1*/reg wmds iraqcorr ideolcen know msreg wmds iraqcorr ideolcen iraqcorrXideolcen know ms/*Figure 1*/generate MV=_n-4replace MV=. if _n>7matrix b=e(b)matrix V=e(V)scalar b1=b[1,1]scalar b2=b[1,2]scalar b3=b[1,3]scalar varb1=V[1,1]scalar varb2=V[2,2]scalar varb3=V[3,3]scalar covb1b3=V[1,3]scalar covb2b3=V[2,3]scalar list b1 b2 b3 varb1 varb2 varb3 covb1b3 covb2b3gen conb=b1+b3*MV if _n<8gen conse=sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV) if _n<8gen a=1.96*consegen upper=conb+agen lower=conb-agen zero=0 if _n<8twoway (rarea upper lower MV,bfcolor(gs12)) (line zero MV) (connected conb MV), scheme(s1mono) graphregion(fcolor(white) ifcolor(none)) xscale(r(-3.5 3.5)) xmtick(-2 -1 1 2) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) legend(order(3 1) label(3 "WMD correction") label (1 "95% confidence interval")) ytitle("Marginal effect on misperception") xtitle("") xlabel(-3 "Very liberal" 0 "Centrist" 3 "Very conservative",labsize(small)) title("Effect of correction on WMD misperception") subtitle("Estimated marginal effect by ideology: Fall 2005")  graph export "figure1.eps", as(eps) replace/*raw data*/bysort roc: tab binarywmd iraqcorr,col/*correlation: Iraq was right thing & WMD misperception*/corr iraqdecision wmds if roc==1bysort iraqcorr: corr iraqdecision wmds if roc==1/*STUDY 2*//*Iraq WMD*/clear alluse "spring06-data.dta"/*Table 2*/reg iraqwmd iraqcorr ideolcen know reg iraqwmd iraqcorr ideolcen know iraqcorrXideolcen reg iraqwmd iraqcorr ideolcen issue_iraq iraqcorrXideolcen iraqcorrXissue_iraq issue_iraqXideolcen iraqcorrXissue_iraqXideolcen know/*correlation: Iraq was right thing & WMD misperception*/corr iraqdecision iraqwmd if roc==1bysort iraqcorr: corr iraqdecision iraqwmd if roc==1/*Figure 2*/generate MV=_n-4replace MV=. if _n>7scalar W0=0 scalar W1=1matrix b=e(b) matrix V=e(V) scalar b1=b[1,1] scalar b2=b[1,2] scalar b3=b[1,3] scalar b4=b[1,4] scalar b5=b[1,5] scalar b6=b[1,6] scalar b7=b[1,7] scalar varb1=V[1,1] scalar varb2=V[2,2] scalar varb3=V[3,3] scalar varb4=V[4,4] scalar varb5=V[5,5] scalar varb6=V[6,6] scalar varb7=V[7,7] scalar covb1b4=V[1,4] scalar covb1b5=V[1,5] scalar covb1b7=V[1,7] scalar covb4b5=V[4,5] scalar covb4b7=V[4,7] scalar covb5b7=V[5,7] gen conb0=b1+b4*MV+b5*W0+b7*(MV*W0) if _n<8 gen conb1=b1+b4*MV+b5*W1+b7*(MV*W1) if _n<8 gen conse0=sqrt(varb1 + varb4*(MV^2) + varb5*(W0^2) + varb7*(MV^2)*(W0^2) + 2*MV*covb1b4 + 2*W0*covb1b5 + 2*MV*W0*covb1b7 + 2*MV*W0*covb4b5 + 2*W0*(MV^2)*covb4b7 + 2*(W0^2)*MV*covb5b7) if _n<8gen conse1=sqrt(varb1 + varb4*(MV^2) + varb5*(W1^2) + varb7*(MV^2)*(W1^2) + 2*MV*covb1b4 + 2*W1*covb1b5 + 2*MV*W1*covb1b7 + 2*MV*W1*covb4b5 + 2*W1*(MV^2)*covb4b7 + 2*(W1^2)*MV*covb5b7) if _n<8gen t0=conb0/conse0gen t1=conb1/conse1gen consb0=conb0gen consb1=conb1replace consb0=. if abs(t0)<1.96replace consb1=. if abs(t1)<1.96generate str1 txt="*"gen ar0=1.96*conse0gen upper0=conb0+ar0gen lower0=conb0-ar0gen ar1=1.96*conse1gen upper1=conb1+ar1gen lower1=conb1-ar1gen ar2=1.64*conse1gen upper2=conb1+ar2gen lower2=conb1-ar2gen zero=0 if _n<8twoway (rarea upper0 lower0 MV,bfcolor(gs12)) (line zero MV) (connected conb0 MV), ymtick(-3 -1 1 3) scheme(s1mono) graphregion(fcolor(white) ifcolor(none)) xscale(r(-3.5 3.5)) xmtick(-2 -1 1 2) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) legend(off) ytitle("Marginal effect on misperception") xtitle("") xlabel(-3 "Very liberal" 0 "Centrist" 3 "Very conservative",labsize(small)) title("Iraq not most important") name(zero) legend(order(3 1) label(3 "WMD correction") rows(1) label (1 "95% confidence interval") cols(1)) nodraw twoway (rarea upper1 lower1 MV,bfcolor(gs12)) (line zero MV) (connected conb1 MV), ymtick(-3 -1 1 3) scheme(s1mono) graphregion(fcolor(white) ifcolor(none)) xscale(r(-3.5 3.5)) xmtick(-2 -1 1 2) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) legend(off) ytitle("Marginal effect on misperception") xtitle("") xlabel(-3 "Very liberal" 0 "Centrist" 3 "Very conservative",labsize(small)) title("Iraq most important") name(one) nodrawgrc1leg zero one, ycommon title("Effect of correction on WMD misperception") subtitle("Marginal effect by ideology/issue importance: Spring 2006") legendfrom(zero) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) graph export "figure2.eps", as(eps) replace/*estimated marginal effect & 90% CI for most strongly committed conservatives*/list MV conb1 lower2 upper2 if _n==7/*t-test of marginal effect among liberals*/ttest iraqwmd if ideolcen<0,by(iraqcorr) unequal/*Tax cuts/revenue*/use "spring06-data.dta",clear/*Table 3*/reg taxcutrev taxcutcorr ideolcen know reg taxcutrev taxcutcorr ideolcen taxcutcorrXideolcen know /*Figure 3*/generate MV=_n-4replace MV=. if _n>7matrix b=e(b)matrix V=e(V)scalar b1=b[1,1]scalar b2=b[1,2]scalar b3=b[1,3]scalar varb1=V[1,1]scalar varb2=V[2,2]scalar varb3=V[3,3]scalar covb1b3=V[1,3]scalar covb2b3=V[2,3]scalar list b1 b2 b3 varb1 varb2 varb3 covb1b3 covb2b3gen conb=b1+b3*MV if _n<8gen conse=sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV) if _n<8gen a=1.96*consegen upper=conb+agen lower=conb-agen zero=0 if _n<8twoway (rarea upper lower MV,bfcolor(gs12)) (line zero MV) (connected conb MV), scheme(s1mono) graphregion(fcolor(white) ifcolor(none)) xscale(r(-3.5 3.5)) xmtick(-2 -1 1 2) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) legend(order(3 1) label(3 "Tax/revenue correction") label (1 "95% confidence interval")) ytitle("Marginal effect on misperception") xtitle("") xlabel(-3 "Very liberal" 0 "Centrist" 3 "Very conservative",labsize(small)) title("Effect of correction on tax/revenue misperception") subtitle("Estimated marginal effect by ideology: Spring 2006")  graph export "figure3.eps", as(eps) replace/*raw data*/bysort roc: tab binarytc taxcutcorr,col/*Stem cell ban*/use "spring06-data.dta",clear/*Table 4*/reg stemcellban sccorr ideolcen know reg stemcellban sccorr ideolcen sccorrXideolcen know/*Figure 4*/generate MV=_n-4replace MV=. if _n>7matrix b=e(b)matrix V=e(V)scalar b1=b[1,1]scalar b2=b[1,2]scalar b3=b[1,3]scalar varb1=V[1,1]scalar varb2=V[2,2]scalar varb3=V[3,3]scalar covb1b3=V[1,3]scalar covb2b3=V[2,3]scalar list b1 b2 b3 varb1 varb2 varb3 covb1b3 covb2b3gen conb=b1+b3*MV if _n<8gen conse=sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV) if _n<8gen a=1.96*consegen upper=conb+agen lower=conb-agen zero=0 if _n<8twoway (rarea upper lower MV,bfcolor(gs12)) (line zero MV) (connected conb MV), scheme(s1mono) graphregion(fcolor(white) ifcolor(none)) xscale(r(-3.5 3.5)) xmtick(-2 -1 1 2) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) legend(order(3 1) label(3 "Stem cell correction") label (1 "95% confidence interval")) ytitle("Marginal effect on misperception") xtitle("") xlabel(-3 "Very liberal" 0 "Centrist" 3 "Very conservative",labsize(small)) title("Effect of correction on stem cell ban misperception") subtitle("Estimated marginal effect by ideology: Spring 2006")  graph export "figure4.eps", as(eps) replace