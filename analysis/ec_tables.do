*===============================================================================
* estimate extensive + intensive margins of punishment 
* produces Tables3 and A4 in manuscript
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
* EXTENSIVE+ + INTENSIVE MARGIN tables
levelsof target_type, local(types)
local controls i.treatment#c.self_invest i.treatment#c.target_invest i.treatment#c.lag_total_sanctions  
foreach t in `types' {
	preserve
	keep if target_type == `t'
	local T : label (target_type) `t'
	display "`T': extensive"
	qui eststo em`t': xtprobit sanctions `controls', re vce(cluster uniquegroup)
	qui eststo ame_em`t': margins, dydx(self_invest target_invest lag_total_sanctions) post
	display "`T': intensive"
	qui eststo im`t': xtpoisson sanctions `controls' if sanctions > 0, re vce(cluster uniquegroup) iter(2000)
	qui eststo ame_im`t': margins, dydx(self_invest target_invest lag_total_sanctions) post
	restore
}
* AMEs
esttab ame_em1 ame_im1 ame_em2 ame_im2 using punishment_ame.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("P(sanction)" "E[sanction]" "P(sanction)" "E[sanction]") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(1.treatment) ///
	varlabels(2.treatment "Earned" self_invest "Own harvest" target_invest "Target harvest/poaching" lag_total_sanctions "Sanctions in t-1")  	

* COEFFICIENTS
esttab em1 im1 em2 im2 using punishment_ate_hurdle.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("P(sanction)" "E[sanction]" "P(sanction)" "E[sanction]") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(1.treatment#c.self_invest 1.treatment#c.target_invest 1.treatment#c.lag_total_sanctions) ///
	varlabels(	2.treatment#c.self_invest "Earned X Own Harvest" ///
				2.treatment#c.target_invest "Earned X Target Harvest/Poaching" ///
				2.treatment#c.lag_total_sanctions "Earned X Sanctions in t-1" ///
				_cons "Constant") ///
	addnotes("Standard errors clustered at the group level.")
	
*===============================================================================
