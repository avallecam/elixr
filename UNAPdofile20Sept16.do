///Labeling variables and creating new variables
label define comlabel1 1 "Zungarococha" 2 "Puerto Almendra" 3 "Ninarrumi" 4 "Llanchama" 
label values community comlabel1
label define posnegmix 0 "Negative" 1 "P vivax" 2 "P falciparum" 3 "Pv/Pf mixed infection"
label define posnegmix1 0 "Negative" 1 "P vivax" 2 "P falciparum"
label values micro1 posnegmix
label values micro2 posnegmix
label values pcrrubio1 posnegmix1
label values pcrrubio2 posnegmix1
label values pcrrubio posnegmix1

label define sexlabel 1 "Female" 2 "Male" 
label values sex sexlabel

generate age1=.
replace age1=1 if age<=5
replace age1=2 if age>5 & age<=18 
replace age1=3 if age>18 & age<100

generate age2=.
replace age2=1 if age<=5
replace age2=2 if age>5 & age<=12
replace age2=3 if age>12 & age<=18
replace age2=4 if age>18 & age<=40
replace age2=5 if age>40 & age <100

generate age3=.
replace age3=1 if age<=5
replace age3=2 if age>5 & age<=18 
replace age3=3 if age>18 & age<=40
replace age3=4 if age>40 & age<=100



label define agerange1 1 "<=5" 2 "6-18" 3 ">18"
label values age1 agerange1 
label define agerange3 1 "<=5" 2 "6-18" 3 "19-40" 4 ">40"
label values age3 agerange3

label define agerange2 1 "<=5" 2 "5-12" 3 "13-18" 4 "19-40" 5 ">40"
label values age2 agerange2

generate pcrrubio=.
replace pcrrubio=1 if pcrrubio1==1
replace pcrrubio=2 if pcrrubio2==2
replace pcrrubio=3 if pcrrubio1==1 & pcrrubio2==2
replace pcrrubio=0 if pcrrubio1==0 & pcrrubio2==0

generate seroposall=.
replace seroposall=0 if pvmsp1pos==0 | pfmsp1pos==0
replace seroposall=1 if pvmsp1pos==1 | pfmsp1pos==1
replace seroposall=. if pvmsp1pos==. | pfmsp1pos==.



//Defining cutoff values based on finite mixture model//
///cutoff = component 1 + 3* sigma1
sysdir set PLUS "C:\Users\Christian.Baldeviano\Documents\NAMRU6 FILES\ASTMH\ASTMH 2016\UNAP"
fmm pvmsp1, mixture(normal) component(2)
generate cutoffpvmsp1= 0.106+(3*0.045)
display cutoffpvmsp1
.241

fmm pfmsp1, mixture(normal) component(2)
generate cutoffpfmsp1=0.066 + (3*0.016)
display cutoffpfmsp1
.114

//Defining cutoff using 5 SDs
generate cutoffpvmsp1_1=0.106 + (5*0.045)
display cutoffpvmsp1_1
.331

//Defining cutoff 5 SDs for Pfmsp1 based on finite mixture model 
generate cutoffpfmsp1_1=0.066 + (5*0.016)
display cutoffpfmsp1_1
0.146

//Generate/define seropositive and seronegatives using 3 SD as cutoff
generate pvmsp1pos=.
replace pvmsp1pos=0 if pvmsp1<=cutoffpvmsp1
replace pvmsp1pos=1 if pvmsp1>cutoffpvmsp1
replace pvmsp1pos=. if pvmsp1==.

//Generate/define pfmsp1pos
generate pfmsp1pos=.
replace pfmsp1pos=0 if pfmsp1<=cutoffpfmsp1
replace pfmsp1pos=1 if pfmsp1>cutoffpfmsp1
replace pfmsp1pos=. if pfmsp1==.

//Generate/define seropositive and seronegatives using 5 SD as cutoff
generate pvmsp1npos1=.
replace pvmsp1npos1=0 if pvmsp1n<=cutoffpvmsp1
replace pvmsp1npos1=1 if pvmsp1n>cutoffpvmsp1
replace pvmsp1npos1=. if pvmsp1n==.

generate pfmsp1pos1=.
replace pfmsp1pos1=0 if pfmsp1<=cutoffpfmsp1_1
replace pfmsp1pos1=1 if pfmsp1>cutoffpfmsp1_1
replace pfmsp1pos1=. if pfmsp1==.

///population description
tab age1 community, col exact
tab sex community, col exact

///Estimating prevalence values
tab micro1 community, col exact
tab pcrrubio community, col exact
tab pvmsp1pos community, col exact
tab pfmsp1pos community, col exact
tab pvmsp1pos1 community, col exact
tab pfmsp1pos1 community, col exact
bysort micro1: tab pvmsp1pos community, col exact
bysort pcrrubio: tab pvmsp1pos community, col exact

scatter pvmsp1pos pcrrubio, jitter(35)
tab pvmsp1pos pcrrubio


//correlations between antibodies
pwcorr pvmsp1 pvmsp1n, sig star(.05) obs

///Age and seropositivity
tab pvmsp1pos age2, row exact
tab pfmsp1pos age2, row exact
tab pvmsp1pos pcrrubio, col

bysort community: tab pvmsp1pos age2, row exact
bysort community: tab pfmsp1pos age2, row exact

hist pvmsp1, percent bin(200) start(0.04) by(pvmsp1pos)
hist pfmsp1, percent bin(200) start(0.04) by(pfmsp1pos)
hist pvmsp1, percent bin(200) start(0.04) by(pcrrubio)

//serology by age
scatter pvmsp1 age
scatter pv_msp1 pv_msp1

//boxplot serology by community for pvmsp1
sort community
by community: egen med=median(pvmsp1)
by community: egen lqt=pctile(pvmsp1), p(25)
by community: egen uqt=pctile(pvmsp1), p(75)
twoway rbar lqt med community, barw(.4) fcolor(gs13) lcolor(black) yline(0.241, lpattern(dash)) ti({bf:anti-PvMSP1 IgG}, color(black))|| ///
	rbar med uqt community, barw(.4) fcolor(gs13) lcolor(black) || ///
	scatter pvmsp1 community, graphregion(fcolor(white)) mcolor(black) msymbol(oh) msize (large) jitter(5) ///
	legend(off) xlabel(1 "ZG(n=442)" 2 "PA(n=107)" 3 "NR(n=292)" 4 "LL(n=119)", labsize(medium)) ///
	ytitle(IgG (OD{subscript:490nm}), size(5)) type(bold) ///
	text(1.4 1 "34%" 1.4 2 "59%" 1.4 3 "31%" 1.4 4 "36%", size(5) color(blue))
drop med lqt uqt

//statistical differences by community
kwallis pvmsp1, by(community)
ranksum pvmsp1 if community==1 | community==2, by(community)
ranksum pvmsp1 if community==1 | community==3, by(community)
ranksum pvmsp1 if community==1 | community==4, by(community)
ranksum pvmsp1 if community==2 | community==3, by(community)
ranksum pvmsp1 if community==2 | community==4, by(community)
ranksum pvmsp1 if community==3 | community==4, by(community)

//Create a graph for pfmsp1 by community
sort community
by community: egen med=median(pfmsp1)
by community: egen lqt=pctile(pfmsp1), p(25)
by community: egen uqt=pctile(pfmsp1), p(75)
twoway rbar lqt med community, barw(.4) fcolor(gs13) lcolor(black) yline(0.114, lpattern(dash)) ti({bf:anti-PfMSP1 IgG}, color(black))|| ///
	rbar med uqt community, barw(.4) fcolor(gs13) lcolor(black) || ///
	scatter pfmsp1 community, graphregion(fcolor(white)) mcolor(black) msymbol(oh) msize (large) jitter(5) ///
	legend(off) xlabel(1 "ZG(n=442)" 2 "PA(n=107)" 3 "NR(n=292)" 4 "LL(n=119)", labsize(medium)) ///
	ytitle(IgG (OD{subscript:490nm}), size(5)) type(bold) ///
	text(.99 1 "16%" .99 2 "40%" .99 3 "34%" .99 4 "42%", size(5) color(blue))
drop med lqt uqt

kwallis pfmsp1, by(community)
ranksum pf_msp1qtsr if community==1 | community==2, by(community)
ranksum pf_msp1qtsr if community==1 | community==3, by(community)
ranksum pf_msp1qtsr if community==1 | community==4, by(community)
ranksum pf_msp1qtsr if community==2 | community==3, by(community)
ranksum pf_msp1qtsr if community==2 | community==4, by(community)
ranksum pf_msp1qtsr if community==3 | community==4, by(community)

//Create a graph for pvmsp1 by age2
sort age2
by age2: egen med=median(pvmsp1)
by age2: egen lqt=pctile(pvmsp1), p(25)
by age2: egen uqt=pctile(pvmsp1), p(75)

*Create graph bar for pvmsp1
twoway rbar lqt med age2, barw(.5) fcolor(gs12) lcolor(black) yline(0.194)|| ///
	rbar med uqt age2, barw(.5) fcolor(gs12) lcolor(black) || ///
	scatter pvmsp1 age2, graphregion(fcolor(gs15)) mcolor(black) msymbol(Oh) jitter(8) ///
	legend(off) xlabel(1 "[2-5]" 2 "<5-12]" 3 "<12-18]" 4 "<18-40]" 5 "<40-80]") ///
	ytitle(anti-PvMSP1 antibody level (OD490)) 
drop med lqt uqt

//Create a graph for pvmsp1 by age1
sort age1
by age1: egen med=median(pvmsp1)
by age1: egen lqt=pctile(pvmsp1), p(25)
by age1: egen uqt=pctile(pvmsp1), p(75)

*Create graph bar for pvmsp1 by age1
twoway rbar lqt med age1, barw(.5) fcolor(gs12) lcolor(black) yline(0.194)|| ///
	rbar med uqt age1, barw(.5) fcolor(gs12) lcolor(black) || ///
	scatter pvmsp1 age1, graphregion(fcolor(gs15)) mcolor(black) msymbol(Oh) jitter(8) ///
	legend(off) xlabel(1 "[2-5]" 2 "<5-18]" 3 ">18") ///
	ytitle(anti-PvMSP1 antibody level (OD490)) 
drop med lqt uqt

//Create a graph for pfmsp1 by age1
sort age1
by age1: egen med=median(pf_msp1qtsr)
by age1: egen lqt=pctile(pf_msp1qtsr), p(25)
by age1: egen uqt=pctile(pf_msp1qtsr), p(75)

*Create graph bar for pvmsp1
twoway rbar lqt med age1, barw(.5) fcolor(gs12) lcolor(black) yline(0.111)|| ///
	rbar med uqt age1, barw(.5) fcolor(gs12) lcolor(black) || ///
	scatter pf_msp1qtsr age1, graphregion(fcolor(gs15)) mcolor(black) msymbol(Oh) jitter(8) ///
	legend(off) xlabel(1 "[2-5]" 2 "<5-18]" 3 ">18") ///
	ytitle(anti-PfMSP1 antibody level (OD490)) 
drop med lqt uqt

// lambda is seroconversion rate, rho is reversion rate (in 1/units of age, e.g. per year here)
sysdir set PLUS "C:\Users\Christian.Baldeviano\Documents\NAMRU6 FILES\ASTMH\ASTMH 2016\UNAP"
revcat pvmsp1pos age

// plot or generate predicted probability by age for pvmsp1, with 95% confidence limits
revcat pvmsp1pos age, plot ///
text(.2 35 "{&lambda}=0.06(0.05-0.08)".14 35 "{&rho}=0.07(0.04-0.11)" 0.6 40 "Age-stratified probability for PvMSP1 IgG seropositivity" , size(4) color(black))
label (off)

revcat pvmsp1pos age, pred(pred_msp) 
line pred_msp l_pred_msp u_pred_msp age, sort  lp(l "##." "##.") lc(blue...) legend(off)
line pred_msp age, sort 

drop pred_msp l_pred_msp u_pred_msp

// plot or generate predicted probability by age for pvmsp1pos1, with 95% confidence limits
revcat pvmsp1pos1 age, plot 
revcat pvmsp1pos age, pred(pred_msp) 
line pred_msp l_pred_msp u_pred_msp age, sort  lp(l "##." "##.") lc(blue...) legend(off)
line pred_msp age, sort 

// plot or generate predicted probability by age for pfmsp1pos1, with 95% confidence limits
revcat pfmsp1pos age, plot ///
text(.08 38 "{&lambda}=0.02(0.01-0.03)".04 38 "{&rho}=0.04(0.02-0.07)" 0.44 40 "Age-stratified probability for PfMSP1 IgG seropositivity" , size(4) color(black))
label (off)


revcat pvmsp1pos age, pred(pred_msp) 
line pred_msp l_pred_msp u_pred_msp age, sort  lp(l "##." "##.") lc(blue...) legend(off)
line pred_msp age, sort 

//plot predicted probability for pvmsp1 by age, with 95% confidence limits by community
revcat pvmsp1pos age if community==1, plot 
revcat pvmsp1pos age if community==2, plot 
revcat pvmsp1pos age if community==3, plot 
revcat pvmsp1pos age if community==4, plot 

//plot predicted probability for pvmsp1 by age, with 95% confidence limits by community

revcat pfmsp1pos age if community==1, plot 
revcat pfmsp1pos age if community==2, plot 
revcat pfmsp1pos age if community==3, plot 
revcat pfmsp1pos age if community==4, plot 
 
// estimate 2 x lambda, assuming that lambda changed 10 years previously  (from lambda1 to lambda2)
revcat_change  pvmsp1pos age , change(10)
revcat_change  pvmsp1pos age , change(10) plot
revcat_change  pvmsp1pos age , change(10) pred(pred_10)

// fit the model for a range of values of the time of change (c, 2 to 18 years age here), store the log-likelihood
gen c=1.5+_n/2 in 1/37
ll_c age c pvmsp1pos, replace

// ll is sum of log-likelihoods from each separate antigen, then all lls are rescaled to have a maximum of 0
// the region with log-likelihood above -2 is an approximate 95% CI for when the change occurred
scatter ll ll_pvmsp1 c , yline(-2)


// change in transmission at a certain age (from lambda1 to lambda2) instead of at a certain point in time 
revcat_change  pvmsp1pos age, change(10) age
ll_c age c pvmsp1pos, age gen(ll_age) replace

scatter ll ll_age c

// lambda may depend on covariates, with a single rho 

gen byte x=0 if community==1
replace x=1 if community==2
revcat  pvmsp1pos age 
revcat  pvmsp1pos age , lambda(x)

// or any of lambda, lambda1, lambda2, rho may depend on covariates through options lambda1(...) etc.



