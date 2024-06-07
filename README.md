# frailty_score_calculation
Calculates frailty score from list of ICD10 codes based on Hospital Frailty Risk score Gilbert et al. Lancet 2018, 391: 1775-82

Basic function that takes dataframe that contains column with list of ICD 10 codes, then applies risk score based on above.

Does add about 100 columns to your data and so may add in later, a way to choose if you want all of the breakdown or just the score.  But that should be a straight forward tweak.

The main bit is saving you having to write the massive statement.
