* This is the Stata code used to perform the Difference-in-Difference estimation *

// CONNECTICUT REGRESSION // 
** SYNTHETIC REGRESSION ** 
*Data Setup*
clear all
import excel "C:\Users\Owner\OneDrive\Public Paper\CT_Synthetic(POCEDIT).xlsx", sheet("Sheet1") firstrow clear

*Generate Relevant Variables*
encode State, gen(state_num)
gen treat= (state_num==1)
gen postt = (Year >= 1999)

*Regression* 
reg Deaths_Est c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_Estimation.doc", replace title("Connecticut Regression and Robustness Checks") keep (Treat Post c.Treat##c.Post FOR popden CCR Percent_POC pOADR Income) 

reg Deaths_Est c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_Estimation.doc", replace title("Full Connecticut Regression and Robustness Checks")

* Robustness * 
reg Deaths_Min c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR   Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_Estimation.doc", append title("Connecticut Regression and Robustness Checks") keep (Treat Post c.Treat##c.Post FOR popden CCR Percent_POC  pOADR Income) 

reg Deaths_Min c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR   Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_Estimation.doc", append title("Full Connecticut Regression and Robustness Checks")

reg Deaths_Max c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR   Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_Estimation.doc", append title("Connecticut Regression and Robustness Checks") keep (Treat Post c.Treat##c.Post FOR popden CCR Percent_POC  pOADR Income) 

reg Deaths_Max c.Treat##c.Post i.Year FOR popden CCR Percent_POC pOADR   Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_Estimation.doc", append title("Full Connecticut Regression and Robustness Checks") 

** SECONDARY REGRESSION: CONNECTICUT **
clear all
import excel "C:\Users\Owner\OneDrive\Public Paper\CT_SecReg.xlsx", sheet("Sheet1") firstrow clear
*Generate Relevant Variables*
encode State, gen(state_num)
encode County, gen(county_num)
gen treat= (state_num==1)
gen postt = (Year >= 1999)

reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_SecEst.doc", replace title("Secondary Connecticut Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_SecEst.doc", replace title("Full Secondary Connecticut Regression and Robustness Checks")

reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_SecEst.doc", append title("Secondary Connecticut Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_SecEst.doc", append title("Full Secondary Connecticut Regression and Robustness Checks")

reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_inpaper_SecEst.doc", append title("Secondary Connecticut Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income if Year >= 1994 & Year <= 2004, r
outreg2 using "Connecticut_fulltable_SecEst.doc", append title("Full Secondary Connecticut Regression and Robustness Checks")


// INDIANA REGRESSION // 
** SYNTHETIC REGRESSION ** 
*Data Setup*
clear all
import excel "C:\Users\Owner\OneDrive\Public Paper\IN_Synthetic(POCEDIT).xlsx", sheet("Sheet1") firstrow clear

*Generate Relevant Variables*
encode State, gen(state_num)
gen treat= (state_num==1)
gen postt = (Year >= 2005)

*Regression* 
reg Deaths_Est c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_inpaper_Estimation.doc", replace title("Indiana Regression and Robustness Checks") keep (treat postt c.treat##c.postt FOR Pop_Den CCR Percent_POC  pOADR Income) 

reg Deaths_Est c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_fulltable_Estimation.doc", replace title("Full Indiana Regression and Robustness Checks") 

* Robustness * 
reg Deaths_Min c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >= 2000 &Year <= 2010, r
outreg2 using "Indiana_inpaper_Estimation.doc", append title("Indiana Regression and Robustness Checks") keep (treat postt c.treat##c.postt FOR Pop_Den CCR Percent_POC  pOADR Income) 


reg Deaths_Min c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >= 2000 & Year <= 2010, r
outreg2 using "Indiana_fulltable_Estimation.doc", append title("Full Indiana Regression and Robustness Checks") 


reg Deaths_Max c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >= 2000 & Year <= 2010, r
outreg2 using "Indiana_inpaper_Estimation.doc", append title("Indiana Regression and Robustness Checks") keep (treat postt c.treat##c.postt FOR Pop_Den CCR Percent_POC pOADR Income) 

reg Deaths_Max c.treat##c.postt i.Year FOR Pop_Den CCR Percent_POC pOADR   Income if Year >= 2000 & Year <= 2010, r
outreg2 using "Indiana_fulltable_Estimation.doc", append title("Full Indiana Regression and Robustness Checks") 


** SECONDARY REGRESSION: INDIANA **
clear all 
import excel "C:\Users\Owner\OneDrive\Public Paper\IN_SecReg.xlsx", sheet("Sheet1") firstrow clear

*Generate Relevant Variables*
encode State, gen(state_num)
encode County, gen(county_num)
gen treat= (state_num==1)
gen postt = (Year >= 2005)

* Regressions * 
reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_inpaper_SecEst.doc", replace title("Secondary Indiana Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_fulltable_SecEst.doc", replace title("Secondary Indiana Regression and Robustness Checks") 

** Robustness Checks ** 
reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_inpaper_SecEst.doc", append title("Secondary Indiana Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_fulltable_SecEst.doc", append title("Secondary Indiana Regression and Robustness Checks") 


reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_inpaper_SecEst.doc", append title("Secondary Indiana Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH Pop_Density CCR Percent_POC OADR Percapita_Personal_Income)

reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Den CCR Percent_POC OADR Percapita_Personal_Income if Year >=2000 & Year <=2010, r
outreg2 using "Indiana_fulltable_SecEst.doc", append title("Secondary Indiana Regression and Robustness Checks") 


// CALIFORNIA REGRESSION // 
** NON-SYNTH REG ** 
*Data Setup* 
clear all
import excel "C:\Users\Owner\OneDrive\Public Paper\CA_Synth(POCEDIT).xlsx", sheet("Sheet1") firstrow clear

*Generate Relevant Variables*
encode State, gen(state_num)
encode County, gen(county_num)
gen treat= (state_num==1)
gen postt = (Year >= 1991)

reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r
outreg2 using "California_inpaper_Estimation.doc", replace title ("California Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income)

reg Deaths_Est c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r
outreg2 using "California_fulltable_Estimation.doc", replace title ("Full California Regression and Robustness Checks")

** ROBUSTNESS: DEATHS_MIN **
reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r
outreg2 using "California_inpaper_Estimation.doc", append title ("California Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income)

reg Deaths_Min c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r
outreg2 using "California_fulltable_Estimation.doc", append title ("Full California Regression and Robustness Checks")


** ROBUSTNESS: DEATH_MAX ** 
reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r
outreg2 using "California_inpaper_Estimation.doc", append title ("California Regression and Robustness Checks") keep(treat postt c.treat##c.postt FRH FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income)

reg Deaths_Max c.treat##c.postt i.county_num i.Year FRH Pop_Density CCR Percent_POC pOADR Percapita_Personal_Income  if Year >= 1986 & Year <= 1996, r 
outreg2 using "California_fulltable_Estimation.doc", append title ("Full California Regression and Robustness Checks")
