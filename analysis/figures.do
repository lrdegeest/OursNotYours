*===============================================================================
* harvest/poaching, punishment and effort figures
* produces Figures 2, 3 and A1 in manuscript
* @author: lrdegeest
*===============================================================================

*===============================================================================
use data/ar_er.dta, clear

set scheme lean1

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
* Figure 2

// average by time
preserve
collapse (mean) mean_invest=invest, by(period treatment punishment type)
gen axis_title = cond(type == 1, "Harvest", "Poaching")
levelsof type, local(types)
levelsof punishment, local(pun)
foreach i in `types' {
	foreach j in `pun' {
		local T : label (type) `i'
		local t : label (punishment) `j' 
		if `i' == 1 {
			local ax `" "Harvest" "'
		} 
		else {
			local ax `" "Poaching" "'
		}
		tw	(connected mean_invest period if treatment == 1 & type == `i' & punishment == `j', msymbol(o) lcolor(blue*0.75) mcolor(blue)) /// 
			(connected mean_invest period if treatment == 2 & type == `i' & punishment == `j', msymbol(o) lcolor(orange*0.75) mcolor(orange)), /// 
				ytitle(`ax') xtitle("Period") xlabel(1(2)15) ylabel(0(10)50) ///
				legend(order(1 "Assigned" 2 "Earned" ) cols(2)) ///
				scheme(lean1) ///
				subtitle("{bf:`T'-`t'}") ///
				legend(ring(0) position(7) size(small)) name(p`i'`j', replace) nodraw
	}
}
grc1leg p11 p12 p21 p22, title("Average harvest/poaching over time") name(avg_time, replace) 
restore	


// distributions
preserve
gen zero_base = 0
levelsof treatment, local(t)
levelsof punishment, local(pun)
levelsof type, local(types)
quietly {
	foreach i in `t' {
		foreach j in `pun' {
			foreach k in `types' {
				capture drop x`i'`j'`k' dens`i'`j'`k'
				kdensity invest if treatment == `i' & punishment == `j' & type == `k', generate(x`i'`j'`k' dens`i'`j'`k') nodraw
			}
		}
	}
}
// insiders
foreach k in `types' {
	if `k' == 1 {
		local ax `" "Harvest" "'
		local xlinecolor "black"
	} 
	else {
		local ax `" "Poaching" "'
		local xlinecolor "white%0"
	}
	local T : label (type) `k'
	twoway	(rarea dens11`k' zero_base x11`k', color("blue%50")) ///
			(rarea dens21`k' zero_base x21`k', color("orange%50")), ///
			ytitle("Smoothed density") xtitle(`ax') ///
			subtitle("{bf:`T'-No punishment}") ///
			xlabel(0(10)50) ylabel(0(0.02)0.1) ///
			xline(4.5, lc(`xlinecolor') lp(dot)) ///
			xline(25, lc(`xlinecolor') lp(dash)) ///
			xline(40, lc(`xlinecolor') lp(solid)) ///
			legend(ring(0) pos(11) col(2) order(1 "Assigned" 2 "Earned") region(color("white%0"))) ///
			name(no_pun`k', replace) nodraw			
	twoway	(rarea dens12`k' zero_base x12`k', color("blue%50")) ///
			(rarea dens22`k' zero_base x22`k', color("orange%50")), ///
			ytitle("Smoothed density") xtitle(`ax') ///
			legend(ring(0) pos(11) col(2) order(1 "Assigned" 2 "Earned") region(color("white%0"))) ///
			subtitle("{bf:`T'-Punishment}") ///
			xlabel(0(10)50) ylabel(0(0.02)0.1) ///
			xline(4.5, lc(`xlinecolor') lp(dot)) ///
			xline(25, lc(`xlinecolor') lp(dash)) ///
			xline(40, lc(`xlinecolor') lp(solid)) ///
			name(pun`k', replace) nodraw
	}
drop zero_base x* dens*
grc1leg no_pun1 pun1 no_pun2 pun2, title("Distributions of harvest/poaching") cols(2) name(dist, replace) 
restore

// combine plots
gr combine dist avg_time
*===============================================================================

*===============================================================================
* PUNISHMENT
* Figure 3
preserve
keep if punishment == 2
cibar points_received_loss, ///
	over1(treatment) over2(type) ///
	ciopts(lcolor(black) lwidth(0.5)) ///
	barcol(blue*0.75 orange*0.75) ///
	graphopts(legend(cols(2)) ylabel(0(10)30) ytitle("Average loss from sanctions (all observations)") name(all_p, replace) nodraw)
cibar points_received_loss if points_received_loss > 0, ///
	ciopts(lcolor(black) lwidth(0.5)) ///
	barcol(blue*0.75 orange*0.75) ///
	over1(treatment) over2(type) ///
	graphopts(ylabel(0(10)30) ytitle("Average loss from sanctions (sanctions > 0)") name(pos_p, replace) nodraw)
keep if type == 2 & surplus_loss > 0
egen total_punishment = sum(points_received_loss), by(treatment group period)
gen deterrence = 100*(total_punishment / surplus_loss)
cibar deterrence, over1(treatment) ///
	barcol(blue*0.75 orange*0.75) ///
	ciopts(lcolor(black) lwidth(0.5)) ///
	graphopts(ylabel(0(20)100) ytitle("Average deterrence of outsiders (%)") name(deter, replace) nodraw)
grc1leg all_p pos_p deter, cols(3) legendfrom(all_p) name(p, replace)
graph display p, xsize(5.0) ysize(2) 
restore

preserve
collapse (mean) points_received_loss=points_received_loss, by(period treatment type)
levelsof type, local(types)
foreach i in `types' {
	tw	(connected points_received_loss period if treatment == 1 & type == `i', msymbol(o) lcolor(blue*0.75) mcolor(blue)) /// 
		(connected points_received_loss period if treatment == 2 & type == `i', msymbol(o) lcolor(orange*0.75) mcolor(orange)), /// 
					ytitle("Average punishment received") xtitle("Period") xlabel(1(2)15)  ///
					legend(order(1 "Assigned" 2 "Earned" ) cols(2)) ///
					scheme(lean1) ///
					subtitle("{bf:`T'-`t'}") ///
					legend(ring(0) position(7) size(small)) name(p`i', replace) nodraw
}
grc1leg p1 p2, name(avg_pun_time, replace) 
restore	

*===============================================================================


*===============================================================================
* EFFORT
* Figure A1: average and distribution of effort

** average effort
cibar score, ///
	over1(treatment) over2(punishment) ///
	ciopts(lcolor(black) lwidth(0.5)) ///
	barcol(blue*0.75 orange*0.75) ///
	graphopts(	ylabel(0(25)100) ///
				ytitle("Average slider score") subtitle("{bf:A}", ring(0) pos(10) size(large)) /// 
				legend(pos(2) ring(0) cols(2)) ///
				name(avg_score, replace) nodraw) 
	
	
** distribution of effort
preserve
gen zero_base = 0
levelsof treatment, local(t)
levelsof punishment, local(pun)
quietly {
	foreach i in `t' {
		foreach j in `pun' {
			capture drop x`i'`j' dens`i'`j'
			kdensity score if treatment == `i' & punishment == `j', generate(x`i'`j' dens`i'`j') nodraw
		}
	}
}
twoway	(rarea dens11 zero_base x11, color("blue%50")) ///
		(rarea dens21 zero_base x21, color("orange%50")), ///
		ytitle("Smoothed density") xtitle("Slider score") ///
		subtitle("{bf:B}", ring(0) pos(10) size(large)) title("No punishment") ///
		legend(ring(0) pos(11) col(2) order(1 "Assigned" 2 "Earned") region(color("white%0"))) ///
		name(no_pun, replace) nodraw			
twoway	(rarea dens12 zero_base x12, color("blue%50")) ///
		(rarea dens22 zero_base x22, color("orange%50")), ///
		ytitle("Smoothed density") xtitle("Slider score") ///
		subtitle("{bf:C}", ring(0) pos(10) size(large)) title("Punishment") ///
		legend(ring(0) pos(11) col(2) order(1 "Assigned" 2 "Earned") region(color("white%0"))) ///
		name(pun, replace) nodraw
drop zero_base x* dens*
restore	  

** combine
grc1leg no_pun pun, name(dist_score, replace) cols(1)
grc1leg avg_score dist_score, legendfrom(avg_score)
*===============================================================================
