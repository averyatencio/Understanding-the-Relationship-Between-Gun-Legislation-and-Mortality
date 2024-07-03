library(readxl)
library(dplyr)
library(openxlsx)

################################ California #####################################

CA_Age <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/CA_Age (1990-2020).xlsx")

CA_Age <- CA_Age[!grepl("Total", CA_Age$Notes), ]
CA_Age <- CA_Age[, -which(names(CA_Age) == "Notes")]
CA_Age <- CA_Age[, -which(names(CA_Age) == "State Code")]
CA_Age <- CA_Age[, -which(names(CA_Age) == "Yearly July 1st Estimates Code")]
CA_Age <- CA_Age[, -which(names(CA_Age) == "County Code")]
CA_Age <- CA_Age[, -which(names(CA_Age) == "Age Group Code")]

unique_age_groups <- unique(CA_Age$Age_Group)
print(unique_age_groups)

age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

CA_population <- CA_Age %>%
  mutate(Age_Category = age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

CA_OADR <- CA_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

print(CA_OADR)

CA_OADR <- CA_OADR %>%
  filter(Year <= 2016)

write.xlsx(CA_OADR, file = "CA_OADR.xlsx")

################################### Connecticut ################################

CT_Age <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/CT_Age.xlsx")

CT_Age <- CT_Age[, -which(names(CT_Age) == "Notes")]
CT_Age <- CT_Age[, -which(names(CT_Age) == "State Code")]
CT_Age <- CT_Age[, -which(names(CT_Age) == "Yearly July 1st Estimates Code")]
CT_Age <- CT_Age[, -which(names(CT_Age) == "County Code")]
CT_Age <- CT_Age[, -which(names(CT_Age) == "Age Group Code")]

unique_age_groups <- unique(CT_Age$Age_Group)
print(unique_age_groups)

CT_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

CT_population <- CT_Age %>%
  mutate(Age_Category = age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

CT_OADR <- CT_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

print(CT_OADR)

write.xlsx(CT_OADR, file = "CT_OADR.xlsx")

# CT 2017

CT_Age2017 <- read_excel("Documents/Master's Thesis/Labor Data/CT/Beginning Sets/CT_Age2017.xlsx")

unique_age_groups <- unique(CT_Age2017$Age_Group)
print(unique_age_groups)

CT_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

CT_population <- CT_Age2017 %>%
  mutate(Age_Category = CT_age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

CT_OADR <- CT_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

write.xlsx(CT_OADR, file = "CT_OADR2017.xlsx")


##################################### Indiana ##################################

IN_Age <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/IN_Age.xlsx")

IN_Age <- IN_Age[, -which(names(IN_Age) == "Notes")]
IN_Age <- IN_Age[, -which(names(IN_Age) == "State Code")]
IN_Age <- IN_Age[, -which(names(IN_Age) == "Yearly July 1st Estimates Code")]
IN_Age <- IN_Age[, -which(names(IN_Age) == "County Code")]
IN_Age <- IN_Age[, -which(names(IN_Age) == "Age Group Code")]

unique_age_groups <- unique(IN_Age$Age_Group)
print(unique_age_groups)

IN_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

IN_population <- IN_Age %>%
  mutate(Age_Category = age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

IN_OADR <- IN_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

print(IN_OADR)

write.xlsx(IN_OADR, file = "IN_OADR.xlsx")

##################################### Nebraska #################################

NE_Age <- read_excel("Documents/Master's Thesis/Public Data/Beginning Sets/NE_Age.xlsx")

NE_Age <- NE_Age[, -which(names(NE_Age) == "Notes")]
NE_Age <- NE_Age[, -which(names(NE_Age) == "State Code")]
NE_Age <- NE_Age[, -which(names(NE_Age) == "Yearly July 1st Estimates Code")]
NE_Age <- NE_Age[, -which(names(NE_Age) == "County Code")]
NE_Age <- NE_Age[, -which(names(NE_Age) == "Age Group Code")]

unique_age_groups <- unique(IN_Age$Age_Group)
print(unique_age_groups)

NE_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

NE_population <- NE_Age %>%
  mutate(Age_Category = age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Yearly, Age_Category) %>%
  summarise(Population = sum(Population))

NE_OADR <- NE_population %>%
  group_by(County, Yearly) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

print(NE_OADR)

write.xlsx(NE_OADR, file = "NE_OADR.xlsx")

#################################### MA #######################################

MA_Age <- read_excel("Documents/Master's Thesis/Public Data/MA_Age.xlsx")

unique_age_groups <- unique(MA_Age$Age_Group)
print(unique_age_groups)

MA_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

MA_population <- MA_Age %>%
  mutate(Age_Category = MA_age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

MA_OADR <- MA_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

write.xlsx(MA_OADR, file = "Documents/Master's Thesis/Public Data/MA_OADR(1990-2016).xlsx")



################################## OH ##########################################

OH_Age <- read_excel("Documents/Master's Thesis/Public Data/OH_Age.xlsx")

unique_age_groups <- unique(OH_Age$Age_Group)
print(unique_age_groups)

OH_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

OH_population <- OH_Age %>%
  mutate(Age_Category = OH_age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

OH_OADR <- OH_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

write.xlsx(OH_OADR, file = "Documents/Master's Thesis/Public Data/OH_OADR(1990-2016).xlsx")

################################### WA #########################################

WA_Age <- read_excel("Documents/Master's Thesis/Public Data/WA_Age.xlsx")

unique_age_groups <- unique(WA_Age$Age_Group)
print(unique_age_groups)

WA_age_group_mapping <- function(Age_Group) {
  case_when(
    Age_Group %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years") ~ "65-plus",
    Age_Group %in% c("16-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years") ~ "16-64",
    TRUE ~ NA_character_  # Return NA for age groups not in the specified ranges
  )
}

WA_population <- WA_Age %>%
  mutate(Age_Category = WA_age_group_mapping(Age_Group)) %>%
  filter(!is.na(Age_Category)) %>%
  group_by(County, Year, Age_Category) %>%
  summarise(Population = sum(Population))

WA_OADR <- WA_population %>%
  group_by(County, Year) %>%
  summarise(
    Population_65plus = sum(Population[Age_Category == "65-plus"]),
    Population_16to64 = sum(Population[Age_Category == "16-64"])
  ) %>%
  mutate(OADR = Population_65plus / Population_16to64)

write.xlsx(WA_OADR, file = "Documents/Master's Thesis/Public Data/WA_OADR(1990-2016).xlsx")


