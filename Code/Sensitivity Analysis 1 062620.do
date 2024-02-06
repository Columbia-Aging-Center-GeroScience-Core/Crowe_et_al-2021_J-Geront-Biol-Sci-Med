
use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

// Creating alternative measures of loneliness and social isolation

	// Cudjoe Social Isolation Measure

gen cudjoe_alone = 1 if social6_alone == 0
replace cudjoe_alone = 0 if social6_alone == 1

gen cudjoe_talk = 1 if spouse_worries == 1 | children_worries == 1 | family_worries == 1 | friends_worries == 1
replace cudjoe_talk = 0 if (spouse_worries == 4 | spouse_worries == 3 | spouse_worries == 2) & (children_worries == 4 | children_worries == 3 | children_worries == 2) & (family_worries == 4 | family_worries == 3 | family_worries == 2) & (friends_worries == 4 | friends_worries == 3 | friends_worries == 2)

gen cudjoe_religious = 1 if involved_religious <= 3
replace cudjoe_religious = 0 if involved_religious == 4 | involved_religious == 5

gen cudjoe_activities = 1 if involved_general <= 4 |involved_clubs <= 5 | involved_groups <= 5 | involved_arts <= 5
replace cudjoe_activities = 0 if (involved_general == 5 | involved_general == 6) & involved_clubs == 6 & involved_groups == 6 & involved_arts == 6

egen cudjoe_total = rowtotal(cudjoe_alone cudjoe_talk cudjoe_religious cudjoe_activities)
replace cudjoe_total = . if lbq > 2
egen cudjoe_n = rownonmiss(cudjoe_alone cudjoe_talk cudjoe_religious cudjoe_activities)
replace cudjoe_total = . if cudjoe_n < 2

	// Isolated at given wave?

gen social_cudjoe_binary = 1 if cudjoe_total <= 1
replace social_cudjoe_binary = 0 if cudjoe_total > 1

	// Ever isolated across follow-up?

egen social_cudjoe_ever = max(social_cudjoe_binary), by(hhidpn)

	// Isolation from first and second LBQ (for incident analysis)

egen social_cudjoe_chronic_2lbq_test = total(social_cudjoe_binary) if wave <= second_lbq_wave_max, by(hhidpn)
egen social_cudjoe_chronic_2lbq = max(social_cudjoe_chronic_2lbq_test), by(hhidpn)
egen social_cudjoe_count_test = count(social_cudjoe_binary) if wave <= second_lbq_wave, by(hhidpn)
replace social_cudjoe_chronic_2lbq = . if social_cudjoe_count_test < 2

recode social_cudjoe_chronic_2lbq (2=1), gen(social_cudjoe_ever_2lbq)

	// Restrict to those with measures across all social isolation measures (10-item measure is the only one with missing values)

replace social6_ever = . if social10_ever == .
replace social_cudjoe_ever = . if social10_ever == .
replace social6_ever2 = . if social10_ever == .

replace social6_ever_2lbq = . if social10_ever_2lbq == .
replace social_cudjoe_ever_2lbq = . if social10_ever_2lbq == .
replace social6_ever2_2lbq = . if social10_ever_2lbq == .

	// Lonely at given wave?

gen ucla_ong_binary = 1 if ucla_companion >= 2 | ucla_leftout >= 2 | ucla_isolated >= 2
replace ucla_ong_binary = . if ucla_companion == . & ucla_leftout == . & ucla_isolated == .
replace ucla_ong_binary = 0 if ucla_ong_binary != 1 & (ucla_companion != . | ucla_leftout != . | ucla_isolated != .)

	// Ever lonely across follow-up?

egen ucla_ong_ever = max(ucla_ong_binary), by(hhidpn)

	// Loneliness from first and second LBQ (for incident analysis)

egen ucla_ong_chronic_2lbq_test = total(ucla_ong_binary) if wave <= second_lbq_wave_max, by(hhidpn)
egen ucla_ong_chronic_2lbq = max(ucla_ong_chronic_2lbq_test), by(hhidpn)
egen ucla_ong_count_test = count(ucla_ong_binary) if wave <= second_lbq_wave, by(hhidpn)
replace ucla_ong_chronic_2lbq = . if ucla_ong_count_test < 2

recode ucla_ong_chronic_2lbq (2=1), gen(ucla_ong_ever_2lbq)

// Restrict to those with measures across all loneliness measures 

replace ucla_ong_chronic_2lbq = . if ucla_chronic_2lbq == .
replace ucla_ong_ever_2lbq = . if ucla_ever_2lbq == .

// Analysis

	// Percent ever lonely by end of follow-up

replace ucla_ong_ever = . if ucla_ever == .

tab ucla_ever if analysis_sample == 1
tab ucla_ever2 if analysis_sample == 1
tab ucla_ong_ever if analysis_sample == 1

	// Percent ever isolated by end of follow-up

tab social6_ever if analysis_sample == 1
tab social6_ever2 if analysis_sample == 1
tab social10_ever if analysis_sample == 1
tab social_cudjoe_ever if analysis_sample == 1

	// Mortality Analysis

stset months, failure(death)

	// HR
	
		// Test of Proportional Hazards Assumption -- subsample variable violates
		// PH assumption; models needs to be stratified by subsample
	
foreach x in ucla_ever ucla_ever2 ucla_ong_ever {
	
	capture drop sch* sca*

	stcox `x' c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1, robust nolog noshow schoenfeld(sch*) scaledsch(sca*)

	stphtest, log detail
	
	stcox `x' c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)
	
}

foreach x in social6_ever social6_ever2 social10_ever social_cudjoe_ever {
	
	capture drop sch* sca*

	stcox `x' c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1, robust nolog noshow schoenfeld(sch*) scaledsch(sca*)

	stphtest, log detail
	
	stcox `x' c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)
	
}	
		
	// Prevalent Disability Analysis

foreach x in ucla ucla_ong social6 social10 social_cudjoe {
	foreach y in adl iadl chronic{
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
			
foreach y in adl iadl chronic{
	nbreg `y'_total ucla_ever2 c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
}
		
foreach y in adl iadl chronic{
		nbreg `y'_total social6_ever2 c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
}
		
	// Incident Disability Analysis 
	
global C "c.cagey##c.cagey##gender i.ethnic subsample if wave==13 & analysis_sample_max == 1"
	
foreach x in ucla ucla_ong social6 social10 social_cudjoe {
	foreach y in adl iadl chronic{
		nbreg inc2_`y' i.`x'_ever_2lbq $C, robust nolog irr
	}
}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic{
		nbreg inc2_`y' i.`x'_ever2_2lbq $C, robust nolog irr
	}
}
	
	
