import excel "/Users/averyatencio/Documents/Master's Thesis/Public Data/Combined_IN_Compressed_By_County_Mortality_1979-2016.xlsx", firstrow clear

destring Deaths_Min, replace force 

replace Deaths_Min = 0 if missing(Deaths_Min)

destring Deaths_Max, replace force
replace Deaths_Max = 9 if missing(Deaths_Max)

import excel "/Users/averyatencio/Documents/Master's Thesis/Public Data/Combined_NE_Compressed_By_County_Mortality_1979-2016.xlsx", firstrow clear

destring Deaths_Min, replace force 

replace Deaths_Min = 0 if missing(Deaths_Min)

destring Deaths_Max, replace force
replace Deaths_Max = 9 if missing(Deaths_Max)
