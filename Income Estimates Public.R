library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

################################# California ###################################

CA_Income <- read_excel("Documents/Master's Thesis/Public Data/CA_Income.xlsx", skip = 5)

CA_Income <- na.omit(CA_Income)
#Dropping rows with "california"
CA_Income <- CA_Income[!grepl("California", CA_Income$GeoName), ]
# Dropping column LineCode
CA_Income <- CA_Income[, -which(names(CA_Income) == "LineCode")]
CA_Income <- CA_Income[, -which(names(CA_Income) == "GeoFips")]
# Dropping rows that aren't per capita or personal income
CA_Income <- CA_Income[!grepl("Net earnings by place of residence", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Personal current transfer receipts", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Income maintenance benefits 1", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Unemployment insurance compensation", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Retirement and other", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Dividends, interest, and rent 2", CA_Income$Description), ]
CA_Income <- CA_Income[!grepl("Population (persons) 3", CA_Income$Description), ]
#Transforming the Data

# Change the name of rows containing "Personal income (thousands of dollars)" to "personal_income"
CA_Income <- CA_Income %>%
  mutate(Description = ifelse(Description == "Personal income (thousands of dollars)", "personal_income", Description))

CA_Income <- CA_Income %>%
  mutate(Description = ifelse(Description == "Per capita personal income 4", "percapita_personal_income", Description))

CA_Income <- CA_Income %>%
  mutate(Description = ifelse(Description == "Population (persons) 3", "population", Description))

# Pivot the year columns into rows
CA_long <- pivot_longer(CA_Income, cols = -c(GeoName, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
CA_wide <- pivot_wider(CA_long, names_from = Description, values_from = Value)

# Print the resulting data frame
print(CA_wide)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(CA_wide, file = "CA_Income(edited).xlsx")

############################# Connecticut ######################################

CT_Income <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/CT_Income.xlsx", 
                        skip = 5)
CT_Income <- na.omit(CT_Income)
#Dropping rows with "Connecticut"
CT_Income <- CT_Income[!grepl("Connecticut", CT_Income$GeoName), ]
# Dropping column LineCode
CT_Income <- CT_Income[, -which(names(CT_Income) == "LineCode")]
CT_Income <- CT_Income[, -which(names(CT_Income) == "GeoFips")]
# Dropping rows that aren't per capita or personal income
CT_Income <- CT_Income[!grepl("Net earnings by place of residence", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Personal current transfer receipts", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Income maintenance benefits 1", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Unemployment insurance compensation", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Retirement and other", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Dividends, interest, and rent 2", CT_Income$Description), ]
CT_Income <- CT_Income[!grepl("Population (persons) 3", CT_Income$Description), ]
#Transforming the Data

# Change the name of rows containing "Personal income (thousands of dollars)" to "personal_income"
CT_Income <- CT_Income %>%
  mutate(Description = ifelse(Description == "Personal income (thousands of dollars)", "personal_income", Description))

CT_Income <- CT_Income %>%
  mutate(Description = ifelse(Description == "Per capita personal income 4", "percapita_personal_income", Description))

CT_Income <- CT_Income %>%
  mutate(Description = ifelse(Description == "Population (persons) 3", "population", Description))

# Pivot the year columns into rows
CT_long <- pivot_longer(CT_Income, cols = -c(GeoName, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
CT_wide <- pivot_wider(CT_long, names_from = Description, values_from = Value)

# Print the resulting data frame
print(CT_wide)

write.xlsx(CT_wide, file = "CT_Income(edited).xlsx")

################################## Indiana #####################################

ID_Income <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/ID_Income.xlsx", 
                        skip = 5)
ID_Income <- na.omit(ID_Income)
#Dropping rows with "Indiana"
ID_Income <- ID_Income[!grepl("Indiana", ID_Income$GeoName), ]
# Dropping column LineCode
ID_Income <- ID_Income[, -which(names(ID_Income) == "LineCode")]
ID_Income <- ID_Income[, -which(names(ID_Income) == "GeoFips")]

#Transforming the Data

# Change the name of rows containing "Personal income (thousands of dollars)" to "personal_income"
ID_Income <- ID_Income %>%
  mutate(Description = ifelse(Description == "Personal income (thousands of dollars)", "personal_income", Description))

ID_Income <- ID_Income %>%
  mutate(Description = ifelse(Description == "Per capita personal income 4", "percapita_personal_income", Description))

# Pivot the year columns into rows
ID_long <- pivot_longer(ID_Income, cols = -c(GeoName, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
ID_wide <- pivot_wider(ID_long, names_from = Description, values_from = Value)

# Print the resulting data frame
print(ID_wide)
write.xlsx(ID_wide, file = "ID_Income(edited).xlsx")

################################## Nebraska ####################################

NE_Income <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/NE_Income.xlsx", 
                        skip = 5)
NE_Income <- na.omit(NE_Income)
#Dropping rows with "california"
NE_Income <- NE_Income[!grepl("Nebraska", NE_Income$GeoName), ]
# Dropping column LineCode
NE_Income <- NE_Income[, -which(names(NE_Income) == "LineCode")]
NE_Income <- NE_Income[, -which(names(NE_Income) == "GeoFips")]

#Transforming the Data

# Change the name of rows containing "Personal income (thousands of dollars)" to "personal_income"
NE_Income <- NE_Income %>%
  mutate(Description = ifelse(Description == "Personal income (thousands of dollars)", "personal_income", Description))

NE_Income <- NE_Income %>%
  mutate(Description = ifelse(Description == "Per capita personal income 4", "percapita_personal_income", Description))

# Pivot the year columns into rows
NE_long <- pivot_longer(NE_Income, cols = -c(GeoName, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
NE_wide <- pivot_wider(NE_long, names_from = Description, values_from = Value)

# Print the resulting data frame
print(NE_wide)
write.xlsx(NE_wide, file = "NE_Income(edited).xlsx")

################################## MA ##########################################
MA_Income <- read_excel("Documents/Master's Thesis/Public Data/MA_Income.xlsx")

# Pivot the year columns into rows
MA_long <- pivot_longer(MA_Income, cols = -c(County, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
MA_wide <- pivot_wider(MA_long, names_from = Description, values_from = Value)

write.xlsx(MA_wide, file = "Documents/Master's Thesis/Public Data/MA_Income(edited).xlsx")

#################################### OH ########################################

OH_Income <- read_excel("Documents/Master's Thesis/Public Data/OH_Income.xlsx")

# Pivot the year columns into rows
OH_long <- pivot_longer(OH_Income, cols = -c(County, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
OH_wide <- pivot_wider(OH_long, names_from = Description, values_from = Value)

write.xlsx(OH_wide, file = "Documents/Master's Thesis/Public Data/OH_Income(edited).xlsx")

################################## WA ##########################################

WA_Income <- read_excel("Documents/Master's Thesis/Public Data/WA_Income.xlsx")

# Pivot the year columns into rows
WA_long <- pivot_longer(WA_Income, cols = -c(County, Description), names_to = "Year", values_to = "Value")

# Pivot the "Description" column into separate columns
WA_wide <- pivot_wider(WA_long, names_from = Description, values_from = Value)

write.xlsx(WA_wide, file = "Documents/Master's Thesis/Public Data/WA_Income(edited).xlsx")




