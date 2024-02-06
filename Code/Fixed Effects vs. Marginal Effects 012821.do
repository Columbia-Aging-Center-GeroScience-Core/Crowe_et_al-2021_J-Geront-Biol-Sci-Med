

use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

keep hhidpn wave ucla_binary social6_binary adl_total iadl_total chronic_total analysis_sample_max analysis_sample ethnic cagey ucla_total_p social6_total_continuous ucla_count social6_count ucla_ever social6_ever gender ses_composite_max z_p5_neuro_baseline_max z_cesd_nolone_94base_max subsample

replace analysis_sample_max = 0 if ethnic == .

// 3-Item UCLA Loneliness

	// Create standardized loneliness variable based on participants' average loneliness scores from 2006-2014 
	
egen ucla_0614 = mean(ucla_total_p) if wave >= 8 & wave <= 13 & analysis_sample_max == 1 & ucla_count >= 2, by(hhidpn)

summ ucla_0614 if analysis_sample == 1

gen z_ucla_score = (ucla_total_p - r(mean)) / r(sd)

// 6-Item Social Isolation

	// Create standardized social isolation variable based on participants' average social isolation scores from 2006-2014 

egen social6_0614 = mean(social6_total_continuous) if wave >= 8 & wave <= 13 & analysis_sample_max == 1 & social6_count >= 2, by(hhidpn)

summ social6_0614 if analysis_sample == 1

gen z_social6_score = (social6_total_continuous - r(mean)) / r(sd)

// Add 1 to D&D outcomes to use in the fixed-effects Poisson model

gen adl_fe = adl_total + 1
gen iadl_fe = iadl_total + 1
gen chronic_fe = chronic_total + 1

tab adl_total adl_fe
tab iadl_total iadl_fe
tab chronic_total chronic_fe

// Fixed effects Poisson regression: Binary loneliness and social isolation
// Then calculate marginal effects for each

foreach y in adl iadl chronic {
	xtpoisson `y'_fe ucla_binary cagey if analysis_sample_max == 1, fe i(hhidpn) vce(bootstrap)
	margins, dydx(ucla_binary) atmeans
}

foreach y in adl iadl chronic {
	xtpoisson `y'_fe social6_binary cagey if analysis_sample_max == 1, fe i(hhidpn) vce(bootstrap)
	margins, dydx(social6_binary) atmeans
}

// Compare to negative binomial regression marginal effects from the full covariate adjusted models

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic ses_composite_max z_p5_neuro_baseline_max z_cesd_nolone_94base_max subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
		margins, dydx(`x'_ever) atmeans
	}
}

// Compare to negative binomial regression marginal effects from the base models

foreach x in ucla social6 {
	foreach y in adl iadl chronic {
		nbreg `y'_total `x'_ever c.cagey##c.cagey##gender i.ethnic subsample if analysis_sample_max == 1 & wave == 13, robust nolog irr
		margins, dydx(`x'_ever) atmeans
	}
}

// Fixed effects Poisson: Continuous loneliness and social isolation

foreach y in adl iadl chronic {
	xtpoisson `y'_total z_ucla_score cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

foreach y in adl iadl chronic {
	xtpoisson `y'_total z_social6_score cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

// Marginal effects:

