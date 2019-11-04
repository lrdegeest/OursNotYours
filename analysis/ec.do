*===============================================================================
* estimate expected costs to noncompliance 
* produces Figures 4 and 5 in manuscript
* @author: lrdegeest
*===============================================================================


*===============================================================================
use data/ar_er_targeting.dta, clear
encode type, gen(type_n)
drop type
rename type_n type
encode treatment, gen(treatment_n)
drop treatment
rename treatment_n treatment
gen uniquesubject = 1000*treatment + 100*group + 10*session + sender
gen uniquegroup = 100*group + 10*session 
rename type target_type
rename points sanctions
rename sender_invest self_invest
rename sender_ip self_ip
rename receiver target
rename receiver_invest target_invest
rename receiver_ip target_ip 
rename id target_id
* ztree recorded outsiders in the data (even though they couldn't play this stage) 
* so we need to drop their records 
* unfortunately I didn't record each sender's type
* but each subject has five records per period (the other members of group)
* and each record is a target and has their target_type: 1 for insider, 2 for outsider
* so if we sum the records for a given subject in a given period, the "total target type" will be:
** 6, meaning the sender is an outsider
** or 7, meaning the sender is an insider
** so we can drop all records where the "total target type" is 6
egen total_target_type = sum(target_type), by(uniquesubject period)
drop if total_target_type == 6
replace group1_id = 0 if group2_id != 0 // ztree added group1_ids to Outsiders for some reason so need to reset
egen total_sanctions = sum(sanctions), by(uniquesubject period)
sort treatment group uniquesubject period target
order date treatment uniquesubject uniquegroup period
gen lag_total_sanctions = total_sanctions[_n-5] if period > 1
xtset uniquesubject
*===============================================================================

*===============================================================================
* EXPECTED COST (with bootstrapped 95% bias-corrected CIs)
* Figure 4

*================================
* program to calculate expected for a given target and treatment
capture program drop expectedcost
 program expectedcost, eclass
	version 15
	syntax [if]
	tempname em im b ec
	preserve
	keep `if' 
	local controls self_invest target_invest lag_total_sanctions  
	// P(sanction) by average insider
	qui xtprobit sanctions `controls', re vce(cluster uniquegroup)
	qui margins, at(target_invest = (0(1)50)) post
	matrix `em' = e(b)
	// E[sanction | sanction > 0] by average insider
	qui xtpoisson sanctions `controls' if sanctions > 0, re vce(cluster uniquegroup) iter(1000)
	qui margins, at(target_invest = (0(1)50)) predict(xb) post
	matrix `im' = e(b)
	mata : st_matrix("`b'", 3*exp(st_matrix("`im'")))
	// expected cost: P(sanction) * E[sanction | sanction > 0]
	local dim `= colsof(`em')'
	matrix `ec' = J(1,`dim',0)
	qui levelsof target_type, local(t)
	if `t' == 1 {
		local N 3
	}
	else {
		local N 4
	}
	forvalues i = 1/`dim' {
		matrix `ec'[1,`i'] = `N' * (`em'[1,`i'] * `b'[1,`i'])
	}
	ereturn post `ec'
	ereturn local cmd="bootstrap"
	restore	
end
*================================

*================================
* run sim
qui levelsof target_type, local(t)
qui levelsof treatment, local(T)
foreach i in `t' {
	nois _dots 0, title("TARGET: `i'"), 
	foreach j in `T' {
		di "TREATMENT: 	`j'"
		bootstrap, seed(1234) reps(1000) nodrop notable noheader: expectedcost if target_type == `i' & treatment == `j'
		matrix ec`i'`j' = e(b)', e(ci_normal)', e(ci_bc)'
		matrix colnames ec`i'`j' = ec`i'`j' ll`i'`j' ul`i'`j' llbc`i'`j' ulbc`i'`j'
		nois _dots `i' 0
	}
}
*================================

*================================
* sim data
clear
forvalues i=1/2 {
	forvalues j=1/2 {
		svmat ec`i'`j', names(col) 
	}
}
gen invest = _n-1
*================================

*================================
* plot expected costs
tw  (line ec11 invest, lcolor(blue)  lw(1) lpattern(solid)) ///
	(line ec12 invest, lcolor(orange) lw(1) lpattern(solid)) ///
	(rarea llbc11 ulbc11 invest, color("blue%10")) ///
	(rarea llbc12 ulbc12 invest, color("orange%10")), ///
		title("Insiders") xtitle("Harvest") ytitle("Expected punishment") ///
		legend(order(1 "Assigned" 2 "Earned") cols(2)) ///
		name(insiders, replace) nodraw
tw  (line ec21 invest, lcolor(blue) lw(1) lpattern(solid)) ///
	(line ec22 invest, lcolor(orange) lw(1) lpattern(solid)) ///
	(rarea llbc21 ulbc21 invest, color("blue%10")) ///
	(rarea llbc22 ulbc22 invest, color("orange%10")), ///	
		title("Outsiders") xtitle("Poaching") ytitle("Expected punishment") ///
		legend(order(1 "Assigned" 2 "Earned") cols(2)) ///
		name(outsiders, replace) nodraw		
* combine plot		
grc1leg insiders outsiders, legendfrom(insiders) cols(2) ycommon
*================================

*================================
* plot expected  payoffs
* Figure 5

* insider payoff if n-1 play social optimum (invest=25)
gen pi_in = (50 - invest) + (invest / (invest + 3*25))*(6*(invest+3*25) - 0.025*((invest+3*25)^2))
* add expected cost
forvalues i=1/2 {
	gen pi_in_ec1`i' = pi_in - ec1`i'
	gen pi_in_llbc1`i' = pi_in - llbc1`i'
	gen pi_in_ulbc1`i' = pi_in - ulbc1`i'
}

* outsider payoff if n play social optimum (surplus=350)
gen pi_out = (50 - invest) + invest*(0.5*350)*0.01 // 0.01 is w (disutility of effort), 0.5 is experiment constraint
* add expected cost
forvalues i=1/2 {
	gen pi_out_ec2`i' = pi_out - ec2`i'
	gen pi_out_llbc2`i' = pi_out - llbc2`i'
	gen pi_out_ulbc2`i' = pi_out - ulbc2`i'
}

tw  (line pi_in_ec11 invest, lcolor(blue)  lpattern(solid)) ///
	(line pi_in_ec12 invest, lcolor(orange) lpattern(solid)) ///
	(line pi_in invest, lcolor(black) lpattern(solid)) ///
	(rarea pi_in_llbc11 pi_in_ulbc11 invest, color("blue%10")) ///
	(rarea pi_in_llbc12 pi_in_ulbc12 invest, color("orange%10")), ///
		title("Insiders") xtitle("Harvest") ytitle("Expected payoff") ///
		legend(order(1 "Assigned" 2 "Earned" 3 "No punishment") cols(3)) ylabel(0(25)150) yline(50,lp(dash)) yline(112.5, lp(dash)) ///
		text(47 7 "{it:Reservation payoff}", size(small)) text(110 9 "{it:Social optimum payoff}", size(small)) ///
		name(insiders_epi, replace) nodraw

tw  (line pi_out_ec21 invest, lcolor(blue)  lpattern(solid)) ///
	(line pi_out_ec22 invest, lcolor(orange) lpattern(solid)) ///
	(line pi_out invest, lcolor(black) lpattern(solid)) ///
	(rarea pi_out_llbc21 pi_out_ulbc21 invest, color("blue%10")) ///
	(rarea pi_out_llbc22 pi_out_ulbc22 invest, color("orange%10")), ///
		title("Outsiders") xtitle("Poaching") ytitle("Expected payoff") ///
		legend(order(1 "Assigned" 2 "Earned" 3 "No punishment") cols(3)) ylabel(0(25)150) yline(50, lp(dash)) ///
		text(47 15 "{it:Reservation payoff}", size(small)) ///
		name(outsiders_epi, replace) nodraw

* combine plot		
grc1leg insiders_epi outsiders_epi, legendfrom(insiders_epi) cols(2) ycommon
*================================
*===============================================================================
