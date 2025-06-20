# Title: Does Maize Ear Husk Cover Inadequacy at Harvest Construe a Correlation with *Aspergillus* section *Flavi* Infection as Dry Spell Would?
-----------------------------

Supporting files to manuscript:

DOI: 10.1163/18750796-bja10023

# *1) "Infection.R"* # 
is run code, in base R, for statistical analysis of the csv files that contain _Aspergillus_ section _Flavi_ (hereafter called _Flavi_ ) and aflatoxin values for soil and/or maize. Code also provides visualisation of some of the data.

# *2) "Soil&Maize_Flav_seas1.csv" / "Soil&Maize_Flav_seas2.csv"* # 
contain relative abundance of Flavi in soil and on maize ears of different husk cover conditions, as wel as aflatoxin levels in maize kernels of different conditions (poor and clean seed). 
Values are scaled as follows per column:

a] Weather_var = S1 (low rain with dry spell, 2018/2019 season); N1 (high rain 2018/2019 season); S2 (low rain, 2020/2021 season); N2 (high rain 2020/2021 season).

b] Soil_CFU = CFU/g of soil _Flavi_,  10^-3 x sqrt. (To find original value, square the number then multiply by 1000).

c] Maize_CFU = CFU/g of (full-husk cover) maize _Flavi_,  10^-3 x sqrt.

d] huskGood = CFU/g of (full-husk cover) maize _Flavi_. 

e] huskPoor = CFU/g of (inadequate husk cover) maize _Flavi_. 

f] Flavi_Soil = CFU/g of soil Flavi,  sqrt x 10^-2. (To find original value, multiply by 100 then square the number).

g] Flavi_full-husk_cover = CFU/g of (full-husk cover) maize _Flavi_, sqrt x 10^-2.	

h) Flavi_inadequate_husk_cover = CFU/g of (full-husk cover) maize _Flavi_, sqrt x 10^-2.

i) AF_inadequate_husk_cover = ng/g total aflatoxin in maize, sqrt x 10^-2.

j) AF_good_Seed	= ng/g total aflatoxin in maize, sqrt x 10^-2.(To find original value, multiply by 100 then square the number).

k) AF_poor_Seed = ng/g total aflatoxin in maize, sqrt x 10^-2.

l) AF_full-husk_cover = ng/g total aflatoxin in maize, as weighted average of 'j' and 'k.'

m) AF_good = ng/g total aflatoxin in clean grain.

n) AF_poor = ng/g total aflatoxin in poor grain.

o) lnAF_good = ng/g total aflatoxin in clean grain, natural logarithm.

p) lnAF_poor = ng/g total aflatoxin in poor grain, natural logarithm.

# *3) "Soil&Maize_Flav.csv"* #
is derived from the two csv files in (2) and is used in the pairwise Wilcoxon test for maize as well as soil _Flavi_ densities (CFU/g) across the weather variables S1, N1, S2 and N2.

# *4) "Questionnaire_dataframe.xlsx"* #
contains the questionnaire matrix and dataframe for the agronomic factors that have potential influence on _Aspergillus_ section _Flavi_ relative abundance on maize ears.

# *5) "MDS_Flav.csv"* #
is dataframe of agronomic factors and weather variables with potential influence on _Aspergillus_ section _Flavi_ quantities (CFU/g) on maize and has been derived from step (4).

# *6) "Husk_fungal.csv"* #
is csv file for paired assessment of full-husk cover and inadequate husk cover ear effect on eye-visible fungal incidence, under season-2 (2020/2021 maize growth season) (supplemental Table S2).
