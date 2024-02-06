use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

// Most recent lonely wave

capture drop last_lonely_wave
capture drop last_lonely_wave_max

egen last_lonely_wave = max(wave) if ucla_binary == 1 & ucla_count >= 2, by(hhidpn)
egen last_lonely_wave_max = max(last_lonely_wave), by(hhidpn)

label variable last_lonely_wave_max "Most recent wave at which the participant was lonely"

label value last_lonely_wave_max wave_label

// Most recent lonely wave for incident analysis (based on 1st and 2nd LBQ for incident analysis)

capture drop last_lonely_wave_2lbq
capture drop last_lonely_wave_2lbq_max

egen last_lonely_wave_2lbq = max(wave) if ucla_binary == 1 & ucla_count >= 2 & wave <= second_lbq_wave_max, by(hhidpn)
egen last_lonely_wave_2lbq_max = max(last_lonely_wave_2lbq), by(hhidpn)

label variable last_lonely_wave_2lbq_max "Most recent wave at which the participant was lonely (1st and 2nd LBQ)"

label value last_lonely_wave_2lbq_max wave_label

// Most recent socially isolated wave

capture drop last_isolated_wave
capture drop last_isolated_ wave_max

egen last_isolated_wave = max(wave) if social6_binary == 1 & social6_count >= 2, by(hhidpn)
egen last_isolated_wave_max = max(last_isolated_wave), by(hhidpn)

label variable last_isolated_wave_max "Most recent wave at which the participant was socially isolated"

label value last_isolated_wave_max wave_label

// Most recent socially isolated wave for incident analysis (based on 1st and 2nd LBQ)

capture drop last_isolated_wave_2lbq
capture drop last_isolated_wave_2lbq_max

egen last_isolated_wave_2lbq = max(wave) if social6_binary == 1 & social6_count >= 2 & wave <= second_lbq_wave_max, by(hhidpn)
egen last_isolated_wave_2lbq_max = max(last_isolated_wave_2lbq), by(hhidpn)

label variable last_isolated_wave_2lbq_max "Most recent wave at which the participant was socially isolated (1st and 2nd LBQ)"

label value last_isolated_wave_2lbq_max wave_label

// Effect sizes for intermittent loneliness/isolation, stratified by most recent loneliness/isolation year

	// Among intermittently lonely/isolated, last lonely/isolated waves range from wave = 8 (2006) to wave = 12 (2014)

tab last_lonely_wave_max if analysis_sample == 1 & ucla_chronic == 1

tab last_isolated_wave_max if analysis_sample == 1 & social6_chronic == 1

	// Create new variables indicating those who were never lonely/isolated (0), those who were intermittently lonely/isolated and last lonely/isolated between 2006-2010 (1), 
	// and those who were intermittently lonely/isolated and last lonely/isolated at 2012/2014 (2)

	gen ucla_intermittent = 0 if ucla_chronic == 0
	replace ucla_intermittent = 1 if ucla_chronic == 1 & last_lonely_wave_max < 11
	replace ucla_intermittent = 2 if ucla_chronic == 1 & (last_lonely_wave_max == 11 | last_lonely_wave_max == 12)
	
	gen social6_intermittent = 0 if social6_chronic == 0
	replace social6_intermittent = 1 if social6_chronic == 1 & last_isolated_wave_max < 11
	replace social6_intermittent = 2 if social6_chronic == 1 & (last_isolated_wave_max == 11 | last_isolated_wave_max == 12)
	
	label define ucla_intermittent 0 "Never Lonely" 1 "Intermittently Lonely - Last Lonely 2006-2010" 2 "Intermittently Lonely - Last Lonely 2012-2014"
	label value ucla_intermittent ucla_intermittent

	label define social6_intermittent 0 "Never Isoalted" 1 "Intermittently Isolated - Last Isolated 2006-2010" 2 "Intermittently Isolated - Last Isolated 2012-2014"
	label value social6_intermittent social6_intermittent

	// Create for incident analysis (based on 1st and 2nd LBQ): 0 = never, 1 = intermittent, last lonely/isolated at first LBQ, 2 = intermittent, last lonely/isolated at second LBQ
	
tab last_lonely_wave_2lbq_max if analysis_sample == 1 & ucla_chronic_2lbq == 1

tab last_isolated_wave_2lbq_max if analysis_sample == 1 & social6_chronic_2lbq == 1

gen ucla_intermittent_2lbq = 0 if ucla_chronic_2lbq == 0
replace ucla_intermittent_2lbq = 1 if ucla_chronic_2lbq == 1 & last_lonely_wave_2lbq_max == first_lbq
replace ucla_intermittent_2lbq = 2 if ucla_chronic_2lbq == 1 & last_lonely_wave_2lbq_max != first_lbq
	
gen social6_intermittent_2lbq = 0 if social6_chronic_2lbq == 0
replace social6_intermittent_2lbq = 1 if social6_chronic_2lbq == 1 & last_isolated_wave_2lbq_max == first_lbq
replace social6_intermittent_2lbq = 2 if social6_chronic_2lbq == 1 & last_isolated_wave_2lbq_max != first_lbq
	
label define ucla_intermittent_2lbq 0 "Never Lonely" 1 "Intermittently Lonely - Last Lonely 1st LBQ" 2 "Intermittently Lonely - Last Lonely 2nd LBQ"
label value ucla_intermittent_2lbq ucla_intermittent_2lbq

label define social6_intermittent_2lbq 0 "Never Isolated" 1 "Intermittently Isolated - Last Isolated 1st LBQ" 2 "Intermittently Isoalted - Last Isolated 2nd LBQ"
label value social6_intermittent_2lbq social6_intermittent_2lbq

// Mortality Analysis

stset months, failure(death)
	
foreach x in ucla social6 {
    
	capture drop sch* sca*
	
    stcox i.`x'_intermittent c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust nolog strata(subsample)
	
	stphtest, log
}


// Prevalent Disability & Disease

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
			nbreg `y'_total i.`x'_intermittent c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
	
// Incident Disability & Disease

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
			nbreg inc2_`y' i.`x'_intermittent_2lbq c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
