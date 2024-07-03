library(tidyr)
library(readxl)
library(openxlsx)
library(dplyr)

#################################### MA ########################################

MA_Mortality <- read_excel("Documents/Master's Thesis/Public Data/MA_Mortality.xlsx")

MA_Arranged <- MA_Mortality%>%
  arrange(County, Year)

write.xlsx(MA_Arranged, file = "Documents/Master's Thesis/Public Data/MA_Mortality(edited).xlsx")


#################################### WA ########################################

WA_Mortality <- read_excel("Documents/Master's Thesis/Public Data/WA_Mortality.xlsx")

WA_Arranged <- WA_Mortality%>%
  arrange(County, Year)

write.xlsx(WA_Arranged, file = "Documents/Master's Thesis/Public Data/WA_Mortality(edited).xlsx")

##################################### OH #######################################

OH_Mortality <- read_excel("Documents/Master's Thesis/Public Data/OH_Mortality.xlsx")

OH_Arranged <- OH_Mortality%>%
  arrange(County, Year)

write.xlsx(OH_Arranged, file = "Documents/Master's Thesis/Public Data/OH_Mortality(edited).xlsx")


###################################### M/NM codes ###############################

Control_MNm <- read_excel("Documents/Master's Thesis/Public Data/Control_MNm.xlsx")

year_range <- 1979:2016
county_list <- unique(Control_MNm$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, Control_MNm, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Metro_Nonmetro_Code, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/MN_Code(edited).xlsx")

################################### Expanding Race ########################################

Control_Race <- read_excel("Documents/Master's Thesis/Public Data/Control_Race.xlsx")

year_range <- 1979:2016
county_list <- unique(Control_Race$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, Control_Race, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Percent_POC, .direction = "up")

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Percent_White, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/Control_Race(edited).xlsx")

################################### Expanding Age ########################################

#### MA

MA_OADR <- read_excel("Documents/Master's Thesis/Public Data/MA_OADR(1990-2016).xlsx")

year_range <- 1979:2016
county_list <- unique(MA_OADR$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, MA_OADR, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(OADR, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/MA_OADR(edited).xlsx")


#### OH

OH_OADR<- read_excel("Documents/Master's Thesis/Public Data/OH_OADR(1990-2016).xlsx")

year_range <- 1979:2016
county_list <- unique(OH_OADR$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, OH_OADR, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(OADR, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/OH_OADR(edited).xlsx")

### WA

WA_OADR<- read_excel("Documents/Master's Thesis/Public Data/WA_OADR(1990-2016).xlsx")

year_range <- 1979:2016
county_list <- unique(WA_OADR$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, WA_OADR, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(OADR, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/WA_OADR(edited).xlsx")

############################### Adding economic shock ##########################

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "MA")

economic_shock_years <- c(1990, 1991, 2001, 2007, 2008, 2009)

df <- Control_States %>%
  mutate(economic_shock = ifelse(Year %in% economic_shock_years, 1, 0))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/MA_econshock.xlsx")

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "OH")

economic_shock_years <- c(1990, 1991, 2001, 2007, 2008, 2009)

df <- Control_States %>%
  mutate(economic_shock = ifelse(Year %in% economic_shock_years, 1, 0))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/OH_econshock.xlsx")

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "WA")

economic_shock_years <- c(1990, 1991, 2001, 2007, 2008, 2009)

df <- Control_States %>%
  mutate(Economic_Shock = ifelse(Year %in% economic_shock_years, 1, 0))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/WA_econshock.xlsx")


################################ Min/Max Death Estimates #######################

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "MA")

df <- Control_States %>%
  mutate(deaths_min = ifelse(Deaths == "Suppressed", 0, Deaths))

df <- df %>%
  mutate(deaths_max = ifelse(Deaths == "Suppressed", 9, Deaths))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/MA_minmax.xlsx")


Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "OH")

df <- Control_States %>%
  mutate(deaths_min = ifelse(Deaths == "Suppressed", 0, Deaths))

df <- df %>%
  mutate(deaths_max = ifelse(Deaths == "Suppressed", 9, Deaths))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/OH_minmax.xlsx")

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "WA")

df <- Control_States %>%
  mutate(deaths_min = ifelse(Deaths == "Suppressed", 0, Deaths))

df <- df %>%
  mutate(deaths_max = ifelse(Deaths == "Suppressed", 9, Deaths))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/WA_minmax.xlsx")

################################# Race for Treatment States #####################

Treatment_Race <- read_excel("Documents/Master's Thesis/Public Data/Treatment_Race.xlsx")

year_range <- 1979:2016
county_list <- unique(Treatment_Race$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, Treatment_Race, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Percent_POC, .direction = "up")

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Percent_White, .direction = "up")

write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/Treatment_Race(edited).xlsx")

################################### Land Area ##################################

Land_Area <- read_excel("Documents/Master's Thesis/Public Data/Land_Area.xlsx")

year_range <- 1979:2016
county_list <- unique(Land_Area$County)

template <- expand.grid(Year = year_range, County = county_list)

extended_data <- merge(template, Land_Area, by = c("Year", "County"), all.x = TRUE)

extended_data <- extended_data %>% arrange(County, Year)

extended_data <- extended_data %>%
  group_by(County) %>%
  fill(Land_Area, .direction = "up")


write.xlsx(extended_data, file = "Documents/Master's Thesis/Public Data/Land_Area(edited).xlsx")

######################### Regional Mortality ###################################
Control_Regional_Mortality <- read_excel("Documents/Master's Thesis/Public Data/Control Regional Mortality.xlsx")

data_arranged <- Control_Regional_Mortality%>%
  arrange(Year, Census_Region)

write.xlsx(data_arranged, file = "Documents/Master's Thesis/Public Data/Regional Mortality(edited).xlsx")

##################################### Adding Post ###############################


Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "MA")

df <- Control_States %>%
  mutate(Post = ifelse(Year < "1999", 0, 1))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/MA_post.xlsx")

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "OH")

df <- Control_States %>%
  mutate(Post_IN = ifelse(Year < "2005", 0, 1))

df <- df %>%
  mutate(Post_NE = ifelse(Year < "1998", 0, 1))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/OH_post.xlsx")

Treat_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                           sheet = "IN")

df <- Treat_States %>%
  mutate(Post_IN = ifelse(Year < "2005", 0, 1))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/IN_post.xlsx")

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "WA")

df <- Control_States %>%
  mutate(Post = ifelse(Year < "1991", 0, 1))

write.xlsx(df, file = "Documents/Master's Thesis/Public Data/WA_post.xlsx")

