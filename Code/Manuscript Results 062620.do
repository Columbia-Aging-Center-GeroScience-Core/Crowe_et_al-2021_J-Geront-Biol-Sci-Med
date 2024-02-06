

use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

// Methods

	// Full HRS Sample

tab first_interview if wave == minimum_wave

	// Missing Psychological Vulnerabilities
	
summ z_p5_neuro_baseline_max if analysis_sample == 1
summ z_cesd_nolone_94base_max if analysis_sample == 1

	// Supplemental Table 1 - Differences between samples
	
gen first_age_years = age_years if minimum_wave == wave
egen first_age_years_max = max(first_age_years), by(hhidpn)

summ first_age_years if compare_sample_max != .
summ first_age_years if compare_sample_max == 1 | compare_sample_max == 2
summ first_age_years if compare_sample_max == 2

tab gender if compare_sample != .
tab gender if compare_sample == 1 | compare_sample == 2
tab gender if compare_sample == 2

tab ethnic if compare_sample != .
tab ethnic if compare_sample == 1 | compare_sample == 2
tab ethnic if compare_sample == 2

summ ses_composite if compare_sample_max != .
summ ses_composite if compare_sample_max == 1 | compare_sample_max == 2
summ ses_composite if compare_sample_max == 2

summ z_cesd_nolone_94base if compare_sample_max != .
summ z_cesd_nolone_94base if compare_sample_max == 1 | compare_sample_max == 2
summ z_cesd_nolone_94base if compare_sample_max == 2

// Results 

	// Total sample size (included in analysis = 1)
	
tab analysis_sample

	// Supplemental Table 2 - Sample Characteristics
	
		// Mortality (Full Analysis) Sample
	
summ first_age_years if analysis_sample_max == 1

tab gender if analysis_sample == 1

tab ethnic if analysis_sample == 1

tab ucla_ever if analysis_sample == 1

tab social6_ever if analysis_sample == 1

summ ses_composite_max if analysis_sample == 1

summ z_cesd_nolone_94base_max if analysis_sample == 1

		// D&D Sample

summ first_age_years_max if analysis_sample_max == 1 & chronic_total != . & wave == 13

tab gender if analysis_sample == 1 & chronic_total != . & wave == 13

tab ethnic if analysis_sample == 1 & chronic_total != . & wave == 13

tab ucla_ever if analysis_sample == 1 & chronic_total != . & wave == 13

tab social6_ever if analysis_sample == 1 & chronic_total != . & wave == 13

tab ucla_chronic if analysis_sample == 1 & chronic_total != . & wave == 13

tab social6_chronic if analysis_sample == 1 & chronic_total != . & wave == 13

summ ses_composite_max if analysis_sample == 1 & chronic_total != . & wave == 13

summ z_cesd_nolone_94base_max if analysis_sample == 1 & chronic_total != . & wave == 13

		// BioAge Sample

summ first_age_years if analysis_sample_max == 1 & PAA != .

tab gender if analysis_sample == 1 & PAA != .

tab ethnic if analysis_sample == 1 & PAA != .

tab ucla_ever if analysis_sample == 1 & PAA != .

tab social6_ever if analysis_sample == 1 & PAA != .

tab ucla_chronic if analysis_sample == 1 & PAA != .

tab social6_chronic if analysis_sample == 1 & PAA != .

summ ses_composite_max if analysis_sample == 1 & PAA != .

summ z_cesd_nolone_94base_max if analysis_sample == 1 & PAA != .

// tab social10_ever if analysis_sample == 1

tab ucla_chronic if analysis_sample == 1

tab social6_chronic if analysis_sample == 1

// tab social10_chronic if analysis_sample == 1

tab death if analysis_sample == 1

summ months if analysis_sample_max == 1

tab adl_total_collapsed if analysis_sample_max == 1 & wave == 13

tab iadl_total_collapsed if analysis_sample_max == 1 & wave == 13

tab chronic_total if analysis_sample_max == 1 & wave == 13

summ PAA if analysis_sample == 1

	// N lonely at first wave
tab ucla_binary if (year == 2006 | year == 2008) & analysis_sample_max == 1 & wave == first_lbq

	// N socially isolated at first wave
tab social6_binary if (year == 2006 | year == 2008) & analysis_sample_max == 1 & wave == first_lbq

//tab social10_binary if (year == 2006 | year == 2008) & analysis_sample_max == 1 & wave == first_lbq

	// N ever lonely at last follow-up
tab ucla_ever if analysis_sample == 1

	// N ever socially isolated at last follow-up
tab social6_ever if analysis_sample == 1

//tab social10_ever if analysis_sample == 1

	// Supplemental Table 3 - 2x2 of ever lonely and isolated (of those who were ever lonely or isolated, what % was both lonely and isolated = 674/3290 = 20%)

tab social6_ever ucla_ever if wave == 13 & analysis_sample_max == 1, row col

	// RR for covariates
glm ucla_ever gender if analysis_sample == 1, fam(poisson) link(log) nolog eform robust

glm social6_ever gender if analysis_sample == 1, fam(poisson) link(log) nolog eform robust

//glm social10_ever gender if analysis_sample == 1, fam(poisson) link(log) nolog eform robust

glm ucla_ever cagey if analysis_sample_max == 1 & months != ., fam(poisson) link(log) nolog robust

glm social6_ever cagey if analysis_sample_max == 1 & months != ., fam(poisson) link(log) nolog robust

//glm social10_ever cagey if analysis_sample_max == 1 & months != ., fam(poisson) link(log) nolog robust

// 1 = White, 0 = Non-white

recode ethnic (2 3 4 = 0), gen(ethnic_nonwhite)

glm ucla_ever ethnic_nonwhite if analysis_sample == 1, fam(poisson) link(log) nolog robust eform

glm social6_ever ethnic_nonwhite if analysis_sample == 1, fam(poisson) link(log) nolog robust eform

// glm social10_ever ethnic_nonwhite if analysis_sample == 1, fam(poisson) link(log) nolog robust eform

	// Mortality Analysis

stset months, failure(death)

	// Number of deaths in analysis sample

tab death if _st == 1 & analysis_sample_max == 1

	// HR
	
		// Test of Proportional Hazards Assumption -- subsample variable violates
		// PH assumption; models needs to be stratified by subsample
	
capture drop sch* sca*

stcox ucla_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1, robust nolog noshow schoenfeld(sch*) scaledsch(sca*)

stphtest, log detail

capture drop sch* sca*

stcox social6_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1, robust nolog noshow schoenfeld(sch*) scaledsch(sca*)

stphtest, log detail

//stcox social10_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1, robust nolog noshow schoenfeld(sch*) scaledsch(sca*)
	
// capture drop sch* sca*

// stphtest, log detail

		// Stratified Models
		
stcox ucla_ever c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)

stcox social6_ever c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)

	// Prevalent Disability Analysis

tab1 adl_total iadl_total chronic_total if analysis_sample_max == 1 & wave == 13
	
foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
		
	// Covariate Adjustment (% change calculated manually as change in beta estimates)
	
		// SES composite
		
foreach x in ucla social6 {
	capture drop sch* sca*
	
	stcox `x'_ever c.cagey##c.cagey##gender i.ethnic ses_composite_max if analysis_sample_max == 1, robust strata(subsample)
	
	stphtest, log detail

}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic ses_composite_max subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
		
		// Baseline Neuroticism
		
foreach x in ucla social6 {
	capture drop sch* sca*
	
	stcox `x'_ever c.cagey##c.cagey##gender i.ethnic z_p5_neuro_baseline_max if analysis_sample_max == 1, robust strata(subsample)
	
	stphtest, log detail
}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic z_p5_neuro_baseline_max subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}
		
		// Baseline Depressive Symptoms
		
foreach x in ucla social6 {
	capture drop sch* sca*
	
	stcox `x'_ever c.cagey##c.cagey##gender i.ethnic z_cesd_nolone_94base_max if analysis_sample_max == 1, robust strata(subsample)
	
	stphtest, log detail
}
		
foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic z_cesd_nolone_94base_max subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
	}
}

		// Full Models + Marginal Effects for D&D
		
foreach x in ucla social6 {
	capture drop sch* sca*
	
	stcox `x'_ever c.cagey##c.cagey##gender i.ethnic ses_composite_max z_p5_neuro_baseline_max z_cesd_nolone_94base_max if analysis_sample_max == 1, robust strata(subsample)
	
	stphtest, log detail
}

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic ses_composite_max z_p5_neuro_baseline_max z_cesd_nolone_94base_max subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
		margins, dydx(`x'_ever) at(cagey=0 gender=1 ethnic=1 ses_composite_max=0 z_p5_neuro_baseline_max=0 z_cesd_nolone_94base_max=0 subsample=0)
		margins, dydx(`x'_ever) atmeans
		margins, dydx(`x'_ever)
	}
}
	
	// Supplemental Table 5 - Covariates & # of waves lonely/isolated
	
		// Association between # lonely/isolated waves and baseline sociodemographic risk factors/psych vulnerabilities
	
egen num_lonely = sum(ucla_binary), by(hhidpn)
egen num_social6 = sum(social6_binary), by(hhidpn)
egen num_social10 = sum(social10_binary), by(hhidpn)

		// Restrict to analysis_sample_max == 1 & months != . so we use cagey at end of exposure assessment (2012 or 2014)
		
foreach x in z_neighborhood_cohesion_max z_neighborhood_disorder_max ses_wealth z_p5_neuro_baseline_max z_cesd_nolone_94base_max {
	foreach y in num_lonely num_social6 {
		nbreg `y' `x' c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & months != ., robust irr
	}
}
		
foreach y in num_lonely num_social6 {
	nbreg `y' ib2.education_collapsed c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & months != ., robust irr
}
		
	// Incident Disability Analysis 
	
tab1 inc2_* if wave == 13 & analysis_sample_max == 1
	
global C "c.cagey##c.cagey##gender i.ethnic subsample if wave==13 & analysis_sample_max == 1"

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg inc2_`y' i.`x'_ever_2lbq $C, robust nolog irr
		}
	}
	
	// Persistent vs. Intermittent vs. Never

		// N persistent and intermittent
	
tab ucla_chronic if analysis_sample == 1 & ucla_chronic != 0
tab ucla_chronic if analysis_sample == 1

tab social6_chronic if analysis_sample == 1 & social6_chronic != 0
tab social6_chronic if analysis_sample == 1

		// Mortality Analysis
		
		
capture drop sch* sca*

stcox i.ucla_chronic c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)

stphtest, log 

capture drop sch* sca*

stcox i.social6_chronic c.cagey##c.cagey##gender i.ethnic if analysis_sample_max == 1, robust strata(subsample)

stphtest, log

	
		// Prevalent Disability Analysis 
	
foreach x in social6 ucla{
	foreach y in adl iadl chronic {
		nbreg `y'_total i.`x'_chronic c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
		}
	}

	// Biological Aging Analysis

summ PAA if analysis_sample_max == 1 & wave == 13

corr PhenoAge age_years if analysis_sample_max == 1 & wave == 13

egen Z = std(PAA) if base_age_years >= 50 & base_age_years <= 95
	
egen Z_max = max(Z), by(hhidpn)

reg Z_max i.ucla_chronic c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust 

reg Z_max i.social6_chronic c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust 

//reg Z_max social10_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust 	
	