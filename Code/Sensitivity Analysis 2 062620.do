
use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

// Results stratified by older (65-95) vs. younger (50-64)

	// Mortality Analysis

stset months, failure(death)

	// HR

capture drop sch* sca*
	
stcox ucla_ever c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1 & base_age_years_max >= 50 & base_age_years_max < 65, robust strata(subsample)

stphtest, log 

capture drop sch* sca*
	
stcox ucla_ever c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1 & base_age_years_max >= 65 & base_age_years_max <= 95, robust strata(subsample)

stphtest, log

capture drop sch* sca*
	
stcox social6_ever c.cagey##c.cagey##gender i.ethnic  if analysis_sample_max == 1 & base_age_years_max >= 50 & base_age_years_max < 65, robust strata(subsample)

stphtest, log
	
capture drop sch* sca*

stcox social6_ever c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1 & base_age_years_max >= 65 & base_age_years_max <= 95, robust strata(subsample)

stphtest, log

	
	// Prevalent Disability Analysis

foreach x in ucla social6 {
	foreach y in adl iadl chronic{
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13 & base_age_years_max >= 50 & base_age_years_max < 65, robust nolog irr
	}
}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic{
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13 & base_age_years_max >= 65 & base_age_years_max <= 95, robust nolog irr
	}
}
		
	// Incident Disability Analysis 
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic{
		nbreg inc2_`y' i.`x'_ever_2lbq c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13 & base_age_years_max >= 50 & base_age_years_max < 65, robust nolog irr
	}
}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic{
		nbreg inc2_`y' i.`x'_ever_2lbq c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13 & base_age_years_max >= 65 & base_age_years_max <= 95, robust nolog irr
	}
}