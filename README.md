# Title: Dry Preharvest Conditions are a Better Predictor for _Aspergillus_ section _Flavi_ Infection of Maize than Husk Cover Condition
-----------------------------

Supporting files to manuscript:

DOI: 

# *1) "Infection.R"* # 
is run code for statistical analysis in base R of the csv files that contain _Flavi_ and aflatoxin values for soil and/or maize. Code also provides visualisation of some of the data.

# *2) "Soil&Maize_Flav_seas1.csv" / "Soil&Maize_Flav_seas2.csv"* # 
contain relative abundance of Flavi in soil and on maize ears of different husk cover conditions, as wel as aflatoxin levels in maize kernels of different conditions (poor and clean seed). 
Values are scaled as follows per column:

a] Weather_var = S1 (low rain with dry spell, 2018/2019 season); N1 (high rain 2018/2019 season); S2 (low rain, 2020/2021 season); N2 (high rain 2020/2021 season).

b] Soil_CFU = CFU/g of soil _Flavi_,  10^-3 x sqrt. (To find original value, square the number then multiply by 1000).

c] Maize_CFU = CFU/g of (tight-husk) maize _Flavi_,  10^-3 x sqrt.

d] huskOK = CFU/g of (tight-husk) maize _Flavi_. 

e] huskPoor = CFU/g of (loose-husk) maize _Flavi_. 

f] Flavi_Soil = CFU/g of soil Flavi,  sqrt x 10^-2. (To find original value, multiply by 100 then square the number).

g] Flavi_Tight_Husk = CFU/g of (tight-husk) maize _Flavi_, sqrt x 10^-2.	

h) Flavi_Loose_Husk = CFU/g of (tight-husk) maize _Flavi_, sqrt x 10^-2.

i) Aflatoxin_Loose_Husk	= ng/g total aflatoxin in maize, sqrt x 10^-2.

j) Aflatoxin_Clean_Seed	= ng/g total aflatoxin in maize, sqrt x 10^-2.(To find original value, multiply by 100 then square the number).

k) Aflatoxin_Poor_Seed = ng/g total aflatoxin in maize, sqrt x 10^-2.

l) Aflatoxin_Tight_Husk = ng/g total aflatoxin in maize, as weighted average of 'j' and 'k.'

m) AF_clean = ng/g total aflatoxin in clean grain.

n) AF_poor = ng/g total aflatoxin in poor grain.

o) lnAF_clean = ng/g total aflatoxin in clean grain, natural logarithm.

p) lnAF_poor = ng/g total aflatoxin in poor grain, natural logarithm.

# *3) "Soil&Maize_Flav.csv"* #
is derived from the two csv files in (2) and is used in the pairwise Wilcoxon test over season1 (2018/2019) and season 2 (2020/2021).

# *4) "Questionnaire_dataframe.xlsx"* #
contains the questionnaire matrix and dataframe for the agronomic factors that have potential influence on _Aspergillus_ section _Flavi_ relative abundance on maize ears.

# *5) "MDS_Flav.csv"* #
is dataframe of agronomic factors and weather variables with potential influence on _Aspergillus_ section _Flavi_ quantities (CFU/g) on maize and has been derived from step (4).


