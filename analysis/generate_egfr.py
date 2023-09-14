
def generate_egfr(creatinine, age, gender):
    
#convert umol/L to mg/dl
    creatining_mgdl = creatinine/88.4

#Min eGFR
    if (gender = 0): 
        egfr_min = (creatinine/0.7)**(-0.329)

    if (gender = 1): 
        egfr_min = (creatinine/0.9)**(-0.411)
    
    if (egfr_min<1):
        egfr_min = 1

#Max eGFR
    if (gender = 0): 
        egfr_max = (creatinine/0.7)**(-1.209)

    if (gender = 1): 
        egfr_max = (creatinine/0.9)**(-1.209)
    
    if (egfr_max>1):
        egfr_max = 1

#Final eGFR value
    egfr = egfr_min*egfr_max*141 * (0.993**age)

    if (gender = 0):
        egfr = egfr*1.018

    return egfr