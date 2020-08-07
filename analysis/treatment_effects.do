*===============================================================================
* average treatment effects
* produces inline chi-squared tests and Tables A1, A2 and A3
* @author: lrdegeest
*===============================================================================

*===============================================================================
use data/ar_er.dta, clear

encode type, gen(type_n)
drop type
rename type_n type
encode punishment, gen(punishment_n)
drop punishment
rename punishment_n punishment
gen uniquesubject = 1000*treatment + 100*group + 10*session + subject
gen uniquegroup = 100*group + 10*session 
sort treatment group uniquesubject period
order date treatment group uniquesubject period
encode treatment_full, gen(treatment_full_n)
drop treatment_full
rename treatment_full_n treatment_full
xtset uniquesubject period
*===============================================================================

*===============================================================================
* HARVEST/POACHING
* treatment effect: assigned vs earned
levelsof type, local(T)
levelsof punishment, local(P)
foreach t in `T' {
	foreach p in `P' {
		preserve
		keep if type == `t' & punishment == `p'
		qui eststo m`t'`p': mixed invest i.treatment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
		di "Type == `t' and Punishment == `p'"
		test 2.treatment
		di ""
		restore
	}
}
esttab m11 m12 m21 m22 using earned_assigned_ate.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("No Punishment" "Punishment" "No Punishment" "Punishment") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.treatment) ///
	varlabels(_cons Constant 2.treatment "Earned") ///
	addnotes("Standard errors clustered at the group level.") 	
	

* signrank test insider harvest == 25 (social optimum)
preserve
keep if type == 1 
collapse (mean) invest, by(treatment punishment uniquegroup)
levelsof treatment, local(T) // 1=Assigned, 2=Earned
levelsof punishment, local(P) // 1=NoPun, 2=Pun
foreach t in `T' {
	foreach p in `P' {
		di "Rights == `t' and Punishment == `p'"
		qui signrank invest = 25 if treatment == `t' & punishment == `p'
		di as text "Test statistic = " as result %9.2f r(z) 
		di as text "p-value = " as result %9.2f r(p)
		di ""
	}
}
restore

* overall punishment affect (within treatment)

** harvests/poaching
levelsof type, local(T)
levelsof treatment, local(P)
foreach t in `T' {
	foreach p in `P' {
		preserve
		keep if type == `t' & treatment == `p'
		qui eststo m`t'`p': mixed invest i.punishment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
		di "Type == `t' and treatment == `p'"
		test 2.punishment
		di ""
		restore
	}
}
esttab m11 m12 m21 m22 using earned_assigned_ate_punishment.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("Assigned" "Earned" "Assigned" "Earned") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.punishment) ///
	varlabels(_cons Constant 2.punishment "Punishment") ///
	addnotes("Standard errors clustered at the group level.") 
	

** payoffs	
levelsof type, local(T)
levelsof treatment, local(P)
foreach t in `T' {
	foreach p in `P' {
		preserve
		keep if type == `t' & treatment == `p'
		qui eststo m`t'`p': mixed profit i.punishment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
		di "Type == `t' and treatment == `p'"
		test 2.punishment
		di ""
		restore
	}
}
esttab m11 m12 m21 m22 using earned_assigned_profit_ate.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("No Punishment" "Punishment" "No Punishment" "Punishment") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.treatment) ///
	varlabels(_cons Constant 2.treatment "Earned") ///
	addnotes("Standard errors clustered at the group level.")
*===============================================================================


*===============================================================================
* PUNISHMENT
levelsof type, local(T)
foreach t in `T' {
	preserve
	keep if punishment == 2 & type == `t' 
	qui eststo m`t'_all: mixed points_received_loss i.treatment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
	di "All observations: Type == `t'"
	test 2.treatment
	di ""
	qui eststo m`t'_pos: mixed points_received_loss i.treatment || uniquegroup: || uniquesubject: if points_received_loss > 0, mle cluster(uniquegroup)
	di "Only positive observations: Type == `t'"
	test 2.treatment
	di ""
	restore
}

preserve
keep if punishment == 2 & type == 2 & surplus_loss > 0
egen total_punishment = sum(points_received_loss), by(treatment group period)
gen deterrence = 100*(total_punishment / surplus_loss)
qui eststo deter: mixed deterrence i.treatment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
restore
esttab m1_all m1_pos m2_all m2_pos deter using punishment_averages.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("All punishment" "Punishment > 0" "All punishment" "Punishment > 0""Deterrence") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.treatment) ///
	varlabels(_cons Constant 2.treatment "Earned") ///
	addnotes("Standard errors clustered at the group level.") 
*===============================================================================

*===============================================================================
* PAYOFFS
* treatment effect: assigned vs earned
levelsof type, local(T)
levelsof punishment, local(P)
foreach t in `T' {
	foreach p in `P' {
		preserve
		keep if type == `t' & punishment == `p'
		qui eststo m`t'`p': mixed profit i.treatment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
		restore
	}
}
esttab m11 m12 m21 m22 using earned_assigned_profit_ate.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("No Punishment" "Punishment" "No Punishment" "Punishment") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.treatment) ///
	varlabels(_cons Constant 2.treatment "Earned") ///
	addnotes("Standard errors clustered at the group level.")
 	
	
* overall punishment affect (within treatment)
levelsof type, local(T)
levelsof treatment, local(P)
foreach t in `T' {
	foreach p in `P' {
		preserve
		keep if type == `t' & treatment == `p'
		qui eststo m`t'`p': mixed profit i.punishment || uniquegroup: || uniquesubject:, mle cluster(uniquegroup)
		restore
	}
}
esttab m11 m12 m21 m22 using profit_ate_within.tex, replace ///
	cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
	numbers nodepvars booktabs ///
	mtitles("Assigned" "Earned" "Assigned" "Earned") ///
	mgroups("Insiders" "Outsiders", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	label legend  ///
	collabels(none) ///
	drop(lns1_1_1:_cons lns2_1_1:_cons lnsig_e:_cons 1.punishment) ///
	varlabels(_cons Constant 2.punishment "Punishment") ///
	addnotes("Standard errors clustered at the group level.") 	
*===============================================================================
