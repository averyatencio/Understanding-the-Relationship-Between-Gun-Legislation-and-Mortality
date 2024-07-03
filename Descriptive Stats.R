library(readxl)
library(ggplot2)
library(dplyr)

################################## California ###################################

CA_Mortality <- read_excel("Documents/Master's Thesis/Public Data/FourState_Mortality_1979-2016.xlsx", sheet = "California")

#making suppressed estimates less than 1 = to 1 and other values whole numbers
CA_Mortality <- CA_Mortality %>%
  mutate(Deaths_Est = ifelse(Deaths_Est < 1 & Deaths_Est != 0, 1, Deaths_Est))

CA_Mortality$Deaths_Est <- round(CA_Mortality$Deaths_Est)

#creating a graph of death_est over time

# Filter data for metro areas
CA_metro_data <- subset(CA_Mortality, Metro_Nonmetro_Code == "metro")

# Create a graph for metro areas
ggplot(CA_metro_data, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time in Metro Areas")   # Add title to the plot

# Filter data for non-metro areas
CA_nonmetro_data <- subset(CA_Mortality, Metro_Nonmetro_Code == "nonmetro")

# Create a graph for non-metro areas
ggplot(CA_nonmetro_data, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time in Non-Metro Areas")   # Add title to the plot

#Graph of firearm over time

ggplot(CA_metro_data, aes(x = Year, y = FRH, color = County)) +
  geom_point() +   
  labs(x = "Time", y = "FRH") +  
  ggtitle("Firearm per County Over Time Metro")

ggplot(CA_nonmetro_data, aes(x = Year, y = FRH, color = County)) +
  geom_point() +   
  labs(x = "Time", y = "FRH") +  
  ggtitle("Firearm per County Over Time NonMetro")


#Graph of Crime rate and income

ggplot(CA_Mortality, aes(x = PersonalIncome_perCap, y = CCR, color = County )) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "Crime Rate") +   # Label axes
  ggtitle("Correlation Between Crime Rate and Income") 

#Graph of Income and FRH

ggplot(CA_Mortality, aes(x =  PersonalIncome_perCap, y = FRH, color = County)) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "FRH") +   # Label axes
  ggtitle("Correlation Between Firearm per County and Income") 

###################################### CT #######################################

CT_Mortality <- read_excel("Documents/Master's Thesis/Public Data/FourState_Mortality_1979-2016.xlsx", 
                                            sheet = "Connecticut")

#making suppressed estimates less than 1 = to 1 and other values whole numbers
CT_Mortality <- CT_Mortality %>%
  mutate(Deaths_Est = ifelse(Deaths_Est < 1 & Deaths_Est != 0, 1, Deaths_Est))

CT_Mortality$Deaths_Est <- round(CT_Mortality$Deaths_Est)

#creating a graph of death_est over time

ggplot(CT_Mortality, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot


#Graph of firearm over time

ggplot(CT_Mortality, aes(x = Year, y = FRH, color = County)) +
  geom_point() +   
  labs(x = "Time", y = "FRH") +  
  ggtitle("Firearm per County")


#Graph of Crime rate and income

ggplot(CT_Mortality, aes(x = PersonalIncome_perCap, y = CCR, color = County )) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "Crime Rate") +   # Label axes
  ggtitle("Correlation Between Crime Rate and Income") 

#Graph of Income and FRH

ggplot(CT_Mortality, aes(x =  PersonalIncome_perCap, y = FRH, color = County)) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "FRH") +   # Label axes
  ggtitle("Correlation Between Firearm per County and Income") 

##### CT Synth

CT_Synthetic <- read_excel("Documents/Master's Thesis/Public Data/CT_Synthetic.xlsx")

ggplot(CT_Synthetic, aes(x = Year, y = Deaths_Est, color = State)) +
  geom_line() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot


###################################### IN #######################################

IN_Mortality <- read_excel("Documents/Master's Thesis/Public Data/FourState_Mortality_1979-2016.xlsx", sheet = "Indiana")

IN_Mortality <- IN_Mortality %>%
  mutate(Deaths_Est = ifelse(Deaths_Est < 1 & Deaths_Est != 0, 1, Deaths_Est))

IN_Mortality$Deaths_Est <- round(IN_Mortality$Deaths_Est)

#creating a graph of death_est over time

ggplot(IN_Mortality, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot


#Graph of firearm over time

ggplot(IN_Mortality, aes(x = Year, y = FRH, color = County)) +
  geom_point() +   
  labs(x = "Time", y = "FRH") +  
  ggtitle("Firearm per County")

#Graph of Crime rate and income

ggplot(IN_Mortality, aes(x = PersonalIncome_perCap, y = CCR, color = County )) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "Crime Rate") +   # Label axes
  ggtitle("Correlation Between Crime Rate and Income") 

#Graph of Income and FRH

ggplot(IN_Mortality, aes(x =  PersonalIncome_perCap, y = FRH, color = County)) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "FRH") +   # Label axes
  ggtitle("Correlation Between Firearm per County and Income") 

##### IN Synth

IN_Synthetic <- read_excel("Documents/Master's Thesis/Public Data/IN_Synthetic.xlsx")

ggplot(IN_Synthetic, aes(x = Year, y = Deaths_Est, color = State)) +
  geom_line() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot

######################################## NE ####################################

NE_Mortality<- read_excel("Documents/Master's Thesis/Public Data/FourState_Mortality_1979-2016.xlsx", 
                                            sheet = "Nebraska")

NE_Mortality <- NE_Mortality %>%
  mutate(Deaths_Est = ifelse(Deaths_Est < 1 & Deaths_Est != 0, 1, Deaths_Est))

NE_Mortality$Deaths_Est <- round(NE_Mortality$Deaths_Est)

#creating a graph of death_est over time

ggplot(NE_Mortality, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot


#Graph of firearm over time

ggplot(NE_Mortality, aes(x = Year, y = FRH, color = County)) +
  geom_point() +   
  labs(x = "Time", y = "FRH") +  
  ggtitle("Firearm per County")


#Graph of Crime rate and income

ggplot(NE_Mortality, aes(x = PersonalIncome_perCap, y = CCR, color = County )) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "Crime Rate") +   # Label axes
  ggtitle("Correlation Between Crime Rate and Income") 

#Graph of Income and FRH

ggplot(NE_Mortality, aes(x =  PersonalIncome_perCap, y = FRH, color = County)) +
  geom_point() +   # Use scatter plot
  labs(x = "Income", y = "FRH") +   # Label axes
  ggtitle("Correlation Between Firearm per County and Income") 

################################### CA and NE pretrend ###############################
aggregated_CA <- read_excel("Documents/Master's Thesis/Public Data/aggregated_CA.xlsx")

ggplot(aggregated_CA, aes(x = Year, y = Deaths_Est, color = State)) +
  geom_line() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot

aggregated_NE <- read_excel("Documents/Master's Thesis/Public Data/aggregated_NE.xlsx")

ggplot(aggregated_NE, aes(x = Year, y = Deaths_Est, color = State)) +
  geom_line() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot

######################### Descriptive stats for covariates #####################

####################################### CA #####################################
Treatment_States <- read_excel("Documents/Master's Thesis/Public Data/Treatment_States.xlsx", 
                               sheet = "CA")
ggplot(Treatment_States, aes(x = Year, y = Deaths_Est, color = County)) +
  geom_line() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot


CA_Synth <- read_excel("Documents/Master's Thesis/Public Data/Combined Excels/CA_Synth.xlsx")

ggplot(CA_Synth, aes(x = Year, y = Deaths_Est, color = State)) +
  geom_point() +   # Use line plot
  labs(x = "Time", y = "Deaths") +   # Label axes
  ggtitle("Deaths Over Time")   # Add title to the plot

CA_Synth <- CA_Synth %>% filter(State == "California")

ggplot(CA_Synth, aes(x = Percapita_Personal_Income, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Income", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Income CA")   # Add title to the plot

ggplot(CA_Synth, aes(x = OADR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "OADR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs OADR CA")   # Add title to the plot

ggplot(CA_Synth, aes(x = Percent_POC, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "POC", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs POC CA")   # Add title to the plot

ggplot(CA_Synth, aes(x = CCR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "CCR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs CCR CA")   # Add title to the plot

ggplot(CA_Synth, aes(x = FRH, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "FOR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs FOR CA")   # Add title to the plot

ggplot(CA_Synth, aes(x = Pop_Density, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Popden", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Popden CA")   # Add title to the plot

####################################### CT ######################################
CT_Synth <- read_excel("Documents/Master's Thesis/Public Data/Combined Excels/CT_Synth.xlsx")

CT_Synth <- CT_Synth %>% filter(State == "Connecticut")

ggplot(CT_Synth, aes(x = Percapita_Personal_Income, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Income", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Income CT")   # Add title to the plot

ggplot(CT_Synth, aes(x = OADR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "OADR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs OADR CT")   # Add title to the plot

ggplot(CT_Synth, aes(x = Percent_POC, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "POC", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs POC CT")   # Add title to the plot

ggplot(CT_Synth, aes(x = CCR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "CCR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs CCR CT")   # Add title to the plot

ggplot(CT_Synth, aes(x = FRH, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "FOR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs FOR CT")   # Add title to the plot

ggplot(CT_Synth, aes(x = Pop_Density, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Popden", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Popden CT")   # Add title to the plot


####################################### IN ######################################
IN_Synth <- read_excel("Documents/Master's Thesis/Public Data/Combined Excels/IN_Synth.xlsx")

IN_Synth <- IN_Synth %>% filter(State == "Indiana")

ggplot(IN_Synth, aes(x = Percapita_Personal_Income, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Income", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Income IN")   # Add title to the plot

ggplot(IN_Synth, aes(x = OADR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "OADR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs OADR IN")   # Add title to the plot

ggplot(IN_Synth, aes(x = Percent_POC, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "POC", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs POC IN")   # Add title to the plot

ggplot(IN_Synth, aes(x = CCR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "CCR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs CCR IN")   # Add title to the plot

ggplot(IN_Synth, aes(x = FRH, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "FOR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs FOR IN")   # Add title to the plot

ggplot(IN_Synth, aes(x = Pop_Density, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Popden", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Popden IN")   # Add title to the plot

######################################## NE #####################################

NE_Synth <- read_excel("Documents/Master's Thesis/Public Data/Combined Excels/NE_Synth.xlsx")

NE_Synth <- NE_Synth %>% filter(State == "Nebraska")

ggplot(NE_Synth, aes(x = Percapita_Personal_Income, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Income", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Income NE")   # Add title to the plot

ggplot(NE_Synth, aes(x = OADR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "OADR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs OADR NE")   # Add title to the plot

ggplot(NE_Synth, aes(x = Percent_POC, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "POC", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs POC NE")   # Add title to the plot

ggplot(NE_Synth, aes(x = CCR, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "CCR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs CCR NE")   # Add title to the plot

ggplot(NE_Synth, aes(x = FRH, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "FOR", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs FOR NE")   # Add title to the plot

ggplot(NE_Synth, aes(x = Pop_Density, y = Deaths_Est, color = Year)) +
  geom_point() +   # Use line plot
  labs(x = "Popden", y = "Deaths") +   # Label axes
  ggtitle("Deaths vs Popden NE")   # Add title to the plot



