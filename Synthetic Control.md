# Understanding-the-Relationship-Between-Gun-Legislation-and-Mortality
# Below is the code used to create the synthetic control, the entire replication package is linked in the comments and includes all R codes used in the analysis as well as Stata code and the excels needed to complete te repliation 
######################################## Template loop #########################

CT_Synth <- CT_Synth %>%
  filter(State %in% c("Connecticut", "Washington"))

# Function to perform synthetic control analysis
perform_synthetic_control <- function(CT_Synth, treated_county){
  synthetic_control <- 
    CT_Synth %>%
    synthetic_control(outcome = Deaths_Est, 
                      unit = County, 
                      time = Year, 
                      i_unit = treated_county, 
                      i_time = 1999, 
                      generate_placebos = TRUE) %>%
    generate_predictor(time_window = 1990:1999,
                       income = mean(Percapita_Personal_Income, na.rm = TRUE),
                       OADR = mean(OADR, na.rm = TRUE),
                       POC = mean(Percent_POC, na.rm = TRUE),
                       White = mean(Percent_White, na.rm = TRUE),
                       Popden = mean(Pop_Density, na.rm = TRUE),
                       CCR = mean(CCR, na.rm = TRUE),
                       FOR = mean(FRH, na.rm = TRUE)) %>%
    generate_weights(optimization_window = 1990:1999,
                     margin.ipop = 0.02, sigf.ipop = 7, bound.ipop = 6) %>%
    generate_control()
  
  return(synthetic_control)
  }


treated_counties_CT <- CT_Synth %>%
  filter(grepl("CT$", County)) %>%
  pull(County) %>%
  unique()

# Perform synthetic control analysis for each treated county
results_list <- map(treated_counties_CT, ~ perform_synthetic_control(CT_Synth, .x))

# Plotting and analysis for each result
map(results_list, ~ {
  plot_trends(.x, time_window = 1979:2016)
  plot_differences(.x, time_window = 1979:2016)
  plot_weights(.x)
  plot_placebos(.x)
  plot_mspe_ratio(.x)
})
map(results_list, ~ {
  plot_trends(.x, time_window = 1979:2016)})

print(results_list[[1]])

grab_predictor_weights(results_list[[1]])
grab_synthetic_control(results_list[[1]])

plot_trends(results_list[[1]])

map(results_list, ~ {
  plot_weights(.x)})

map(results_list, ~ {
  plot_differences(.x, time_window = 1979:2016)})


############# Running the synthetic controls with Aggregated States ############

#################################### CT ########################################
aggregated_CT <- read_excel("Documents/Master's Thesis/Public Data/aggregated_CT.xlsx")

synthetic_control_CT <- 
  aggregated_CT %>% 
  synthetic_control(outcome = Deaths_Est, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Connecticut", # unit where the intervention occurred
                    i_time = 1999, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 1990:1999,
                     income = mean(Percapita_Personal_Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(popden, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 1990:1999, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

################################ Deaths Min CT ################################

synthetic_control_CT_min <- 
  aggregated_CT %>% 
  synthetic_control(outcome = Deaths_Min, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Connecticut", # unit where the intervention occurred
                    i_time = 1999, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 1990:1999,
                     income = mean(Percapita_Personal_Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(popden, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 1990:1999, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

################################ Deaths Max CT ###################################

synthetic_control_CT_max <- 
  aggregated_CT %>% 
  synthetic_control(outcome = Deaths_Max, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Connecticut", # unit where the intervention occurred
                    i_time = 1999, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 1990:1999,
                     income = mean(Percapita_Personal_Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(popden, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 1990:1999, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

#plot trends

synthetic_control_CT %>% plot_trends(time_window = 1994:2004)

plot_differences(synthetic_control_CT, time_window = 1994:2004)

synthetic_control_CT %>% plot_weights()

synthetic_control_CT %>% plot_placebos()

synthetic_control_CT %>% plot_mspe_ratio()

################################# CA ###########################################
aggregated_CA <- read_excel("Documents/Master's Thesis/Public Data/aggregated_CA.xlsx")

synthetic_control_CA <- 
  aggregated_CA %>% 
  synthetic_control(outcome = Deaths_Est, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "California", # unit where the intervention occurred
                    i_time = 1991, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 1986:1996,
                     income = mean(Percapita_Personal_Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(Pop_Den, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 1986:1996, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 



#plot trends

synthetic_control_CA %>% plot_trends(time_window = 1986:1996)

plot_differences(synthetic_control_CA, time_window = 1986:1996)

synthetic_control_CA %>% plot_weights()

synthetic_control_CA %>% plot_placebos()

synthetic_control_CA %>% plot_mspe_ratio()

######################################## IN ####################################
aggregated_IN <- read_excel("Documents/Master's Thesis/Public Data/aggregated_IN.xlsx")

synthetic_control_IN <- 
  aggregated_IN %>% 
  synthetic_control(outcome = Deaths_Est, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Indiana", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2000:2010,
                     income = mean(Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(Pop_Den, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 2000:2010, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

############################## Deaths Min IN ###################################

synthetic_control_IN_min <- 
  aggregated_IN %>% 
  synthetic_control(outcome = Deaths_Min, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Indiana", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2000:2010,
                     income = mean(Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(Pop_Den, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 2000:2010, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 


################################ Deaths Max IN ###################################

synthetic_control_IN_max <- 
  aggregated_IN %>% 
  synthetic_control(outcome = Deaths_Max, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Indiana", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2000:2010,
                     income = mean(Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(Pop_Den, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 2000:2010, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

#plot trends

synthetic_control_IN %>% plot_trends(time_window = 2000:2010)

plot_differences(synthetic_control_IN, time_window = 2000:2010)

synthetic_control_IN %>% plot_weights()

synthetic_control_IN %>% plot_placebos()

synthetic_control_IN %>% plot_mspe_ratio()

####################################### NE ####################################

aggregated_NE <- read_excel("Documents/Master's Thesis/Public Data/aggregated_NE.xlsx")

synthetic_control_NE <- 
  aggregated_NE %>% 
  synthetic_control(outcome = Deaths_Est, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Nebraska", # unit where the intervention occurred
                    i_time = 1998, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 1993:2003,
                     income = mean(Percapita_Personal_Income, na.rm = T),
                     OADR = mean(OADR, na.rm = T),
                     POC = mean(POC, na.rm = T),
                     Popden = mean(popden, na.rm = T),
                     CCR = mean(CCR, na.rm = T),
                     FOR = mean(FOR, na.rm = T))%>%
  generate_weights(optimization_window = 1993:2003, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

synthetic_control_NE %>% plot_trends(time_window = 1993:2003)

plot_differences(synthetic_control_NE, time_window = 1993:2003)

synthetic_control_NE %>% plot_weights()

synthetic_control_NE %>% plot_placebos()

synthetic_control_NE %>% plot_mspe_ratio()

######################## Outputting Synthetic Controls ########################

################################### IN #########################################

#CCR
# Initialize an empty dataframe to store the results
results_df_CCR <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  CCR_var_name <- paste0("CCR_", year)
  WA_CCR_1979_var_name <- paste0("WA_CCR_", year)
  OH_CCR_1979_var_name <- paste0("OH_CCR_", year)
  MA_CCR_1979_var_name <- paste0("MA_CCR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_CCR_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(CCR)
  
  OH_CCR_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(CCR)
  
  MA_CCR_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(CCR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_CCR <- synthetic_control_IN %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_CCR <- synthetic_control_IN %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_CCR <- synthetic_control_IN %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_CCR_weight <- WA_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  OH_CCR_weight <- OH_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  MA_CCR_weight <- MA_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  CCR_1979 <- ((WA_CCR_1979 * WA_CCR_weight) + (OH_CCR_1979 * OH_CCR_weight) + (MA_CCR_1979 * MA_CCR_weight))
  
  # Combine the results for the current year into a data frame
  result_df_CCR <- data.frame(Year = rep(year, nrow(CCR_1979)), CCR = as.vector(CCR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_CCR <- rbind(results_df_CCR, result_df_CCR)
}

write.xlsx(results_df_CCR, "Documents/Master's Thesis/Public Data/CCR_results.xlsx")

################################# FOR ###########################################
results_df_FOR <- data.frame(Year = numeric(), FOR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  FOR_var_name <- paste0("FOR_", year)
  WA_FOR_1979_var_name <- paste0("WA_FOR_", year)
  OH_FOR_1979_var_name <- paste0("OH_FOR_", year)
  MA_FOR_1979_var_name <- paste0("MA_FOR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_FOR_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(FOR)
  
  OH_FOR_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(FOR)
  
  MA_FOR_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(FOR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_FOR <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_FOR <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_FOR <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_FOR_weight <- WA_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  OH_FOR_weight <- OH_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  MA_FOR_weight <- MA_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  # Calculate CCR for the current year
  FOR_1979 <- (WA_FOR_1979 * WA_FOR_weight) + (OH_FOR_1979 * OH_FOR_weight) + (MA_FOR_1979 * MA_FOR_weight)
  
  # Combine the results for the current year into a data frame
  result_df_FOR <- data.frame(Year = rep(year, nrow(FOR_1979)), FOR = as.vector(FOR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_FOR <- rbind(results_df_FOR, result_df_FOR)
}

write.xlsx(results_df_FOR, "Documents/Master's Thesis/Public Data/FOR_results.xlsx")


#################################### OADR #####################################
results_df_OADR <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  OADR_var_name <- paste0("OADR_", year)
  WA_OADR_1979_var_name <- paste0("WA_OADR_", year)
  OH_OADR_1979_var_name <- paste0("OH_OADR_", year)
  MA_OADR_1979_var_name <- paste0("MA_OADR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_OADR_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(OADR)
  
  OH_OADR_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(OADR)
  
  MA_OADR_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(OADR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_OADR <- synthetic_control_IN %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_OADR <- synthetic_control_IN %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_OADR <- synthetic_control_IN %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_OADR_weight <- WA_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)
  
  OH_OADR_weight <- OH_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)
  
  MA_OADR_weight <- MA_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)

  
  # Calculate CCR for the current year
  OADR_1979 <- ((WA_OADR_1979 * WA_OADR_weight) + (OH_OADR_1979 * OH_OADR_weight) + (MA_OADR_1979 * MA_OADR_weight))
  
  # Combine the results for the current year into a data frame
  result_df_OADR <- data.frame(Year = rep(year, nrow(OADR_1979)), OADR = as.vector(OADR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_OADR <- rbind(results_df_OADR, result_df_OADR)
}

write.xlsx(results_df_OADR, "Documents/Master's Thesis/Public Data/OADR_results.xlsx")

################################### Income #####################################

results_df_income <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  income_var_name <- paste0("income_", year)
  WA_income_1979_var_name <- paste0("WA_income_", year)
  OH_income_1979_var_name <- paste0("OH_income_", year)
  MA_income_1979_var_name <- paste0("MA_income_", year)
  
  # Filter aggregated_IN for each state and year
  WA_income_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(Income)
  
  OH_income_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(Income)
  
  MA_income_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(Income)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_income <- synthetic_control_IN %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_income <- synthetic_control_IN %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_income <- synthetic_control_IN %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_income_weight <- WA_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  OH_income_weight <- OH_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  MA_income_weight <- MA_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  income_1979 <- ((WA_income_1979 * WA_income_weight) + (OH_income_1979 * OH_income_weight) + (MA_income_1979 * MA_income_weight))
  
  # Combine the results for the current year into a data frame
  result_df_income <- data.frame(Year = rep(year, nrow(income_1979)), OADR = as.vector(income_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_income <- rbind(results_df_income, result_df_income)
}

write.xlsx(results_df_income, "Documents/Master's Thesis/Public Data/income_results.xlsx")

##################################### POC ########################################
results_df_POC <- data.frame(Year = numeric(), FOR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  POC_var_name <- paste0("POC_", year)
  WA_POC_1979_var_name <- paste0("WA_POC_", year)
  OH_POC_1979_var_name <- paste0("OH_POC_", year)
  MA_POC_1979_var_name <- paste0("MA_POC_", year)
  
  # Filter aggregated_IN for each state and year
  WA_POC_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(POC)
  
  OH_POC_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(POC)
  
  MA_POC_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(POC)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_POC <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_POC <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictors_weights)
  
  MA_POC <- synthetic_control_IN %>%
    filter(.id == "Indiana" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_POC_weight <- WA_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  OH_POC_weight <- OH_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  MA_POC_weight <- MA_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  # Calculate CCR for the current year
  POC_1979 <- (WA_POC_1979 * WA_POC_weight) + (OH_POC_1979 * OH_POC_weight) + (MA_POC_1979 * MA_POC_weight)
  
  # Combine the results for the current year into a data frame
  result_df_POC <- data.frame(Year = rep(year, nrow(POC_1979)), FOR = as.vector(POC_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_POC <- rbind(results_df_POC, result_df_POC)
}

write.xlsx(results_df_POC, "Documents/Master's Thesis/Public Data/POC_results.xlsx")

###################################### Pop Den ################################
results_df_popden <- data.frame(Year = numeric(), popden = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  popden_var_name <- paste0("popden_", year)
  WA_popden_1979_var_name <- paste0("WA_popden_", year)
  OH_popden_1979_var_name <- paste0("OH_popden_", year)
  MA_popden_1979_var_name <- paste0("MA_popden_", year)
  
  # Filter aggregated_IN for each state and year
  WA_popden_1979 <- aggregated_IN %>%
    filter(State == "Washington" & Year == year) %>%
    select(Pop_Den)
  
  OH_popden_1979 <- aggregated_IN %>%
    filter(State == "Ohio" & Year == year) %>%
    select(Pop_Den)
  
  MA_popden_1979 <- aggregated_IN %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(Pop_Den)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_popden <- synthetic_control_IN %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_popden <- synthetic_control_IN %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_popden <- synthetic_control_IN %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_popden_weight <- WA_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  OH_popden_weight <- OH_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  MA_popden_weight <- MA_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  popden_1979 <- ((WA_popden_1979 * WA_popden_weight) + (OH_popden_1979 * OH_popden_weight) + (MA_popden_1979 * MA_popden_weight))
  
  # Combine the results for the current year into a data frame
  result_df_popden <- data.frame(Year = rep(year, nrow(popden_1979)), OADR = as.vector(popden_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_popden <- rbind(results_df_popden, result_df_popden)
}

write.xlsx(results_df_popden, "Documents/Master's Thesis/Public Data/popden_results.xlsx")

####################################### CT #####################################
#CCR

# Initialize an empty dataframe to store the results
results_df_CCR <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  CCR_var_name <- paste0("CCR_", year)
  WA_CCR_1979_var_name <- paste0("WA_CCR_", year)
  OH_CCR_1979_var_name <- paste0("OH_CCR_", year)
  MA_CCR_1979_var_name <- paste0("MA_CCR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_CCR_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(CCR)
  
  OH_CCR_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(CCR)
  
  MA_CCR_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(CCR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_CCR <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_CCR <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_CCR <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_CCR_weight <- WA_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  OH_CCR_weight <- OH_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  MA_CCR_weight <- MA_CCR[[1]][[1]] %>%
    filter(variable == "CCR") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  CCR_1979 <- ((WA_CCR_1979 * WA_CCR_weight) + (OH_CCR_1979 * OH_CCR_weight) + (MA_CCR_1979 * MA_CCR_weight))
  
  # Combine the results for the current year into a data frame
  result_df_CCR <- data.frame(Year = rep(year, nrow(CCR_1979)), CCR = as.vector(CCR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_CCR <- rbind(results_df_CCR, result_df_CCR)
}

write.xlsx(results_df_CCR, "Documents/Master's Thesis/Public Data/CCR_results.xlsx")

################################# FOR ###########################################
results_df_FOR <- data.frame(Year = numeric(), FOR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  FOR_var_name <- paste0("FOR_", year)
  WA_FOR_1979_var_name <- paste0("WA_FOR_", year)
  OH_FOR_1979_var_name <- paste0("OH_FOR_", year)
  MA_FOR_1979_var_name <- paste0("MA_FOR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_FOR_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(FOR)
  
  OH_FOR_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(FOR)
  
  MA_FOR_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(FOR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_FOR <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_FOR <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_FOR <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_FOR_weight <- WA_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  OH_FOR_weight <- OH_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  MA_FOR_weight <- MA_FOR[[1]][[1]] %>%
    filter(variable == "FOR") %>%
    pull(weight)
  
  # Calculate CCR for the current year
  FOR_1979 <- (WA_FOR_1979 * WA_FOR_weight) + (OH_FOR_1979 * OH_FOR_weight) + (MA_FOR_1979 * MA_FOR_weight)
  
  # Combine the results for the current year into a data frame
  result_df_FOR <- data.frame(Year = rep(year, nrow(FOR_1979)), FOR = as.vector(FOR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_FOR <- rbind(results_df_FOR, result_df_FOR)
}

write.xlsx(results_df_FOR, "Documents/Master's Thesis/Public Data/FOR_results.xlsx")


#################################### OADR #####################################
results_df_OADR <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  OADR_var_name <- paste0("OADR_", year)
  WA_OADR_1979_var_name <- paste0("WA_OADR_", year)
  OH_OADR_1979_var_name <- paste0("OH_OADR_", year)
  MA_OADR_1979_var_name <- paste0("MA_OADR_", year)
  
  # Filter aggregated_IN for each state and year
  WA_OADR_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(OADR)
  
  OH_OADR_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(OADR)
  
  MA_OADR_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(OADR)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_OADR <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_OADR <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_OADR <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_OADR_weight <- WA_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)
  
  OH_OADR_weight <- OH_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)
  
  MA_OADR_weight <- MA_OADR[[1]][[1]] %>%
    filter(variable == "OADR") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  OADR_1979 <- ((WA_OADR_1979 * WA_OADR_weight) + (OH_OADR_1979 * OH_OADR_weight) + (MA_OADR_1979 * MA_OADR_weight))
  
  # Combine the results for the current year into a data frame
  result_df_OADR <- data.frame(Year = rep(year, nrow(OADR_1979)), OADR = as.vector(OADR_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_OADR <- rbind(results_df_OADR, result_df_OADR)
}

write.xlsx(results_df_OADR, "Documents/Master's Thesis/Public Data/OADR_results.xlsx")

################################### Income #####################################

results_df_income <- data.frame(Year = numeric(), CCR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  income_var_name <- paste0("income_", year)
  WA_income_1979_var_name <- paste0("WA_income_", year)
  OH_income_1979_var_name <- paste0("OH_income_", year)
  MA_income_1979_var_name <- paste0("MA_income_", year)
  
  # Filter aggregated_IN for each state and year
  WA_income_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(Percapita_Personal_Income)
  
  OH_income_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(Percapita_Personal_Income)
  
  MA_income_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(Percapita_Personal_Income)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_income <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_income <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_income <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_income_weight <- WA_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  OH_income_weight <- OH_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  MA_income_weight <- MA_income[[1]][[1]] %>%
    filter(variable == "income") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  income_1979 <- ((WA_income_1979 * WA_income_weight) + (OH_income_1979 * OH_income_weight) + (MA_income_1979 * MA_income_weight))
  
  # Combine the results for the current year into a data frame
  result_df_income <- data.frame(Year = rep(year, nrow(income_1979)), OADR = as.vector(income_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_income <- rbind(results_df_income, result_df_income)
}

write.xlsx(results_df_income, "Documents/Master's Thesis/Public Data/income_results.xlsx")

##################################### POC ########################################
results_df_POC <- data.frame(Year = numeric(), FOR = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  POC_var_name <- paste0("POC_", year)
  WA_POC_1979_var_name <- paste0("WA_POC_", year)
  OH_POC_1979_var_name <- paste0("OH_POC_", year)
  MA_POC_1979_var_name <- paste0("MA_POC_", year)
  
  # Filter aggregated_IN for each state and year
  WA_POC_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(POC)
  
  OH_POC_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(POC)
  
  MA_POC_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(POC)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_POC <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_POC <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_POC <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_POC_weight <- WA_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  OH_POC_weight <- OH_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  MA_POC_weight <- MA_POC[[1]][[1]] %>%
    filter(variable == "POC") %>%
    pull(weight)
  
  # Calculate CCR for the current year
  POC_1979 <- (WA_POC_1979 * WA_POC_weight) + (OH_POC_1979 * OH_POC_weight) + (MA_POC_1979 * MA_POC_weight)
  
  # Combine the results for the current year into a data frame
  result_df_POC <- data.frame(Year = rep(year, nrow(POC_1979)), FOR = as.vector(POC_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_POC <- rbind(results_df_POC, result_df_POC)
}

write.xlsx(results_df_POC, "Documents/Master's Thesis/Public Data/POC_results.xlsx")

###################################### Pop Den ################################
results_df_popden <- data.frame(Year = numeric(), popden = numeric())

# Loop through each year from 1979 to 2016
for (year in 1979:2016) {
  
  # Construct variable names dynamically
  popden_var_name <- paste0("popden_", year)
  WA_popden_1979_var_name <- paste0("WA_popden_", year)
  OH_popden_1979_var_name <- paste0("OH_popden_", year)
  MA_popden_1979_var_name <- paste0("MA_popden_", year)
  
  # Filter aggregated_IN for each state and year
  WA_popden_1979 <- aggregated_CT %>%
    filter(State == "Washington" & Year == year) %>%
    select(popden)
  
  OH_popden_1979 <- aggregated_CT %>%
    filter(State == "Ohio" & Year == year) %>%
    select(popden)
  
  MA_popden_1979 <- aggregated_CT %>%
    filter(State == "Massachusetts" & Year == year) %>%
    select(popden)
  
  # Filter synthetic_control_IN for Indiana and controls
  WA_popden <- synthetic_control_CT %>%
    filter(.id == "Washington" & .type == "controls") %>%
    select(.predictor_weights)
  
  OH_popden <- synthetic_control_CT %>%
    filter(.id == "Ohio" & .type == "controls") %>%
    select(.predictor_weights)
  
  MA_popden <- synthetic_control_CT %>%
    filter(.id == "Massachusetts" & .type == "controls") %>%
    select(.predictor_weights)
  
  # Extract predictor weights for each state
  WA_popden_weight <- WA_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  OH_popden_weight <- OH_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  MA_popden_weight <- MA_popden[[1]][[1]] %>%
    filter(variable == "Popden") %>%
    pull(weight)
  
  
  # Calculate CCR for the current year
  popden_1979 <- ((WA_popden_1979 * WA_popden_weight) + (OH_popden_1979 * OH_popden_weight) + (MA_popden_1979 * MA_popden_weight))
  
  # Combine the results for the current year into a data frame
  result_df_popden <- data.frame(Year = rep(year, nrow(popden_1979)), OADR = as.vector(popden_1979))
  
  # Append the results for the current year to the overall results dataframe
  results_df_popden <- rbind(results_df_popden, result_df_popden)
}

write.xlsx(results_df_popden, "Documents/Master's Thesis/Public Data/popden_results.xlsx")


