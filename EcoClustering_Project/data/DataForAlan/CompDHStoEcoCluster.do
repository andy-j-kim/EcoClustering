import delimited "/Users/hubbard/UC Berkeley Biostat Dropbox/Alan Hubbard/hubbardlap/grantproposals/Data Science in Africa/Project 1/EcoClustGit/EcoClustering_Project/data/DataForAlan/Cameroon.csv"

encode de_cat, gen(N_dec_cat)
tab N_dec_cat

tab id N_dec_cat,  chi2
** Same as the paper


ologit N_dec_cat id, or

ologit N_de_cat v190

ologit N_de_cat id

