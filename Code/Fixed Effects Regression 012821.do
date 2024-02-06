

use "C:\Users\clc2229\cumc.columbia.edu\Belsky, Daniel - Crowe_SocIsol\Final Code and Data to Share\Creating Dataset\AnalysisSamples032920.dta", clear

keep hhidpn wave ucla_binary social6_binary adl_total iadl_total chronic_total analysis_sample_max analysis_sample ethnic cagey ucla_total_p social6_total_continuous ucla_count social6_count

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

// Binary loneliness and social isolation

foreach y in adl iadl chronic {
	xtreg `y'_total ucla_binary cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

foreach y in adl iadl chronic {
	xtreg `y'_total social6_binary cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

// Binary loneliness and social isolation - Poisson

foreach y in adl iadl chronic {
	xtpoisson `y'_total ucla_binary cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

foreach y in adl iadl chronic {
	xtpoisson `y'_total social6_binary cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

// Continuous loneliness and social isolation

foreach y in adl iadl chronic {
	xtreg `y'_total z_ucla_score cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

foreach y in adl iadl chronic {
	xtreg `y'_total z_social6_score cagey if analysis_sample_max == 1, fe i(hhidpn) robust
}

