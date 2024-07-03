############################# Aggregating States #############################
########################### Aggregate Treatment States ##########################

######################################## CA #####################################

Treatment_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                               sheet = "CA")

collapsed_data <- Treatment_States %>%
  filter(State == "California") %>%  # Filter for California counties
  group_by(Year) %>%  # Group by year
  summarise(
    State = "California",  # Add state column with value "California"
    income = mean(Percapita_Personal_Income, na.rm = TRUE),  # Calculate mean income
    OADR = mean(OADR, na.rm = TRUE),  # Calculate mean OADR
    POC = mean(Percent_POC, na.rm = TRUE),  # Calculate mean POC
    popden = mean(Pop_Density, na.rm = TRUE),  # Calculate mean population density
    CCR = mean(CCR, na.rm = TRUE),  # Calculate mean CCR
    FOR = mean(FRH, na.rm = TRUE),  # Calculate mean FOR
    population = sum(Population),  # Sum population
    deaths_est = sum(Deaths_Est),  # Sum deaths_est
    deaths_min = sum(Deaths_Min),  # Sum deaths_min
    deaths_max = sum(Deaths_Max)  # Sum deaths_max
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_CA.xlsx")

######################################## IN ####################################

Treatment_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                               sheet = "IN")

collapsed_data <- Treatment_States %>%
  filter(State == "Indiana") %>%  
  group_by(Year) %>%  
  summarise(
    State = "Indiana",  
    income = mean(Percapita_Personal_Income, na.rm = TRUE), 
    OADR = mean(OADR, na.rm = TRUE),  
    POC = mean(Percent_POC, na.rm = TRUE),  
    popden = mean(Pop_Density, na.rm = TRUE),  
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FRH, na.rm = TRUE),  
    population = sum(Population),  
    deaths_est = sum(Deaths_Est), 
    deaths_min = sum(Deaths_Min),  
    deaths_max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_IN.xlsx")


######################################### NE ###################################

Treatment_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                               sheet = "NE")
collapsed_data <- Treatment_States %>%
  filter(State == "Nebraska") %>%  
  group_by(Year) %>%  
  summarise(
    State = "Nebraska", 
    Percapita_Personal_Income = mean(Percapita_Personal_Income, na.rm = TRUE),  
    OADR = mean(OADR, na.rm = TRUE), 
    POC = mean(Percent_POC, na.rm = TRUE),  
    popden = mean(Pop_Density, na.rm = TRUE),  
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FRH, na.rm = TRUE),  
    Population = sum(Population),  
    Deaths_Est = sum(Deaths_Est),  
    Deaths_Min = sum(Deaths_Min),  
    Deaths_Max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_NE.xlsx")

###################################### CT ######################################

Treatment_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                               sheet = "CT")
collapsed_data <- Treatment_States %>%
  filter(State == "Connecticut") %>%  
  group_by(Year) %>%  
  summarise(
    State = "Connecticut",  
    Percapita_Personal_Income = mean(Percapita_Personal_Income, na.rm = TRUE),  
    OADR = mean(OADR, na.rm = TRUE),  
    POC = mean(Percent_POC, na.rm = TRUE),  
    popden = mean(Pop_Density, na.rm = TRUE),  
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FRH, na.rm = TRUE),  
    Population = sum(Population),  
    Deaths_Est = sum(Deaths_Est),  
    Deaths_Min = sum(Deaths_Min),  
    Deaths_Max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_CT.xlsx")


############################# Aggregate control states ######################### 


##################################### MA ######################################

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "MA")

collapsed_data <- Control_States %>%
  filter(State == "Massachusetts") %>%  
  group_by(Year) %>% 
  summarise(
    State = "Massachusetts",  
    Percapita_Personal_Income = mean(Percapita_Personal_Income, na.rm = TRUE),  
    OADR = mean(OADR, na.rm = TRUE),  
    POC = mean(Percent_POC, na.rm = TRUE), 
    popden = mean(Pop_Density, na.rm = TRUE),  
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FRH, na.rm = TRUE),  
    Population = sum(Population), 
    Deaths_Est = sum(Deaths_Est), 
    Deaths_Min = sum(Deaths_Min), 
    Deaths_Max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_MA.xlsx")


################################### OH #########################################

Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "OH")

collapsed_data <- Control_States %>%
  filter(State == "Ohio") %>%  
  group_by(Year) %>% 
  summarise(
    State = "Ohio",  
    Percapita_Personal_Income = mean(Percapita_Personal_Income, na.rm = TRUE), 
    OADR = mean(OADR, na.rm = TRUE),  
    POC = mean(Percent_POC, na.rm = TRUE),  
    popden = mean(Pop_Density, na.rm = TRUE),  
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FHR, na.rm = TRUE),  
    Population = sum(Population),  
    Deaths_Est = sum(Deaths_Est),  
    Deaths_Min = sum(Deaths_Min), 
    Deaths_Max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_OH.xlsx")

################################# WA ##########################################


Control_States <- read_excel("Documents/Master's Thesis/Public Data/Control_States.xlsx", 
                             sheet = "WA")

collapsed_data <- Control_States %>%
  filter(State == "Washington") %>%  
  group_by(Year) %>%  
  summarise(
    State = "Washington", 
    Percapita_Personal_Income = mean(Percapita_Personal_Income, na.rm = TRUE),  
    OADR = mean(OADR, na.rm = TRUE), 
    POC = mean(Percent_POC, na.rm = TRUE),  
    popden = mean(Pop_Density, na.rm = TRUE), 
    CCR = mean(CCR, na.rm = TRUE),  
    FOR = mean(FHR, na.rm = TRUE),  
    Population = sum(Population),  
    Deaths_Est = sum(Deaths_Est),  
    Deaths_Min = sum(Deaths_Min),  
    Deaths_Max = sum(Deaths_Max)  
  ) %>%
  ungroup() 
write.xlsx(collapsed_data , file = "Documents/Master's Thesis/Public Data/aggregated_WA.xlsx")
