*** Below is for Cameroon

import delimited "/Users/hubbard/UC Berkeley Biostat Dropbox/Alan Hubbard/hubbardlap/grantproposals/Data Science in Africa/Project 1/EcoClustGit/EcoClustering_Project/data/DataForAlan/Cameroon.csv", clear

encode de_cat, gen(N_dec_cat)
tab N_dec_cat

tab id N_dec_cat,  row col chi2

**  Re-order cluster variable from most wealth (1) to poorest (5)

capture drop SES_rank
gen SES_rank = id
recode SES_rank 1=1 2=4 3=3 4=2 5=5

capture drop dhs_rank 
gen dhs_rank = v190
recode dhs_rank 1=5 2=4 3=3 4=2 5=1

ologit N_dec_cat SES_rank, or

ologit N_dec_cat dhs_rank, or

tab SES_rank dhs_rank,  chi2

* Correspondence analyses
* Weighted Kappa with quadratic weights (equivalent to ICC)
*** Need to figure out how to add weights
kap SES_rank dhs_rank, wgt(w2)




** Look at Continuous index of SES in DHS (v191) in categories defined by 
** cluster

summarize v191

* Generate the normalized variable using stored return scalars
capture drop dhs_norm
gen dhs_norm = (v191 - r(min)) / (r(max) - r(min))

* Make table of cluster number summarizing dhs_norm in clusters

table SES_rank, stat(min dhs_norm) stat(p50 dhs_norm) stat(max dhs_norm)

table dhs_rank, stat(min dhs_norm) stat(p50 dhs_norm) stat(max dhs_norm)


graph box dhs_norm, over(SES_rank) ///
    ytitle("Normalized DHS wealth score") ///
    b1title("SES Ranked Clusters") ///
    title("") ///
    scheme(s2mono) ///
    box(1, color(black) fcolor(gs14)) ///
    marker(1, mcolor(gs6) msize(small)) ///
    graphregion(color(white))
	
	
*** Below is for South Africa

import delimited "/Users/hubbard/UC Berkeley Biostat Dropbox/Alan Hubbard/hubbardlap/grantproposals/Data Science in Africa/Project 1/EcoClustGit/EcoClustering_Project/data/DataForAlan/SouthAfrica.csv", clear

encode de_cat, gen(N_dec_cat)
tab N_dec_cat

tab id N_dec_cat,  row col chi2

**  Re-order cluster variable from most wealth (1) to poorest (5)

capture drop SES_rank
gen SES_rank = id
replace SES_rank = 1 if id==4
replace SES_rank = 2 if id==3
replace SES_rank = 3 if id==2
replace SES_rank = 4 if id==5
replace SES_rank = 5 if id==1



capture drop dhs_rank 
gen dhs_rank = 5-v190+1

ologit N_dec_cat SES_rank, or

ologit N_dec_cat dhs_rank, or

tab SES_rank dhs_rank,  chi2

* Correspondence analyses
* Weighted Kappa with quadratic weights (equivalent to ICC)
*** Need to figure out how to add weights
kap SES_rank dhs_rank, wgt(w2)



** Look at Continuous index of SES in DHS (v191) in categories defined by 
** cluster

capture drop dhs_norm
summarize v191
* Generate the normalized variable using stored return scalars
gen dhs_norm = (v191 - r(min)) / (r(max) - r(min))

* Make table of cluster number summarizing dhs_norm in clusters

table SES_rank, stat(min dhs_norm) stat(p50 dhs_norm) stat(max dhs_norm)

table dhs_rank, stat(min dhs_norm) stat(p50 dhs_norm) stat(max dhs_norm)


graph box dhs_norm, over(SES_rank) ///
    ytitle("Normalized DHS wealth score") ///
    b1title("SES Ranked Clusters") ///
    title("") ///
    scheme(s2mono) ///
    box(1, color(black) fcolor(gs14)) ///
    marker(1, mcolor(gs6) msize(small)) ///
    graphregion(color(white))
