library(dplyr)
library(readr)
library(tidyr)


Vacc <-read.csv("MD_COVID19_VaccinationPercentAgeGroupPopulation.csv")
View(Vacc)

CasesxCounty <- read.csv("MDCOVID19_CasesByCounty.csv")
View(CasesxCounty)

Casesx100<- read.csv("MDCOVID19_CasesPer100KpopulationStatewide.csv")
View(Casesx100)

Deaths <- read.csv("MDCOVID19_ConfirmedDeathsByCounty.csv")
View(Deaths)

DeathsDropped <- Deaths %>% select(-Unknown)
View(DeathsDropped)

CasesxCountyDropped <- CasesxCounty %>% select(-Unknown)
View(CasesxCountyDropped)


# Standardize date column names
Vacc <- Vacc %>% rename(Date = VACCINATION_DATE)
Casesx100 <- Casesx100 %>% rename(Date = ReportDate)

# Convert dates
Vacc$Date <- as.Date(Vacc$Date)
Casesx100$Date <- as.Date(Casesx100$Date)

# Join statewide datasets
Statewide <- Vacc %>%
  left_join(Casesx100, by = "Date")

# Rename the "Statewide" column to "Cases_per_100"
Statewide <- Statewide %>%
  rename(Cases_per_100 = Statewide)

# --- Drop OBJECTID columns in Statewide ---
Statewide <- Statewide %>%
  select(-starts_with("OBJECTID"))

# Replace NAs in numeric columns only for Statewide
Statewide <- Statewide %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Check result
names(Statewide)

# Look at result
View(Statewide)


# -------------------------------
# 2. CREATE COUNTY-LEVEL DATASET
# -------------------------------

# Standardize dates
CasesxCountyDropped <- CasesxCountyDropped %>% rename(Date = DATE)
DeathsDropped <- DeathsDropped %>% rename(Date = DATE)

# Convert dates
CasesxCountyDropped$Date <- as.Date(CasesxCountyDropped$Date)
DeathsDropped$Date <- as.Date(DeathsDropped$Date)

# Join county datasets by Date
County <- CasesxCountyDropped %>%
  left_join(DeathsDropped, by = "Date", suffix = c("_Cases", "_Deaths"))

# --- Drop OBJECTID columns in County ---
County <- County %>%
  select(-starts_with("OBJECTID"))

# Replace NAs in numeric columns only for County
County <- County %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Look at result
View(County)


# -------------------------------
# Create County Dataset into Tidy Long Format
# -------------------------------

# Pivot county cases to long format
Cases_long <- County %>%
  select(Date, ends_with("_Cases")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "County",
    values_to = "Cases"
  ) %>%
  mutate(County = gsub("_Cases", "", County))  # remove suffix

# Pivot county deaths to long format
Deaths_long <- County %>%
  select(Date, ends_with("_Deaths")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "County",
    values_to = "Deaths"
  ) %>%
  mutate(County = gsub("_Deaths", "", County))  # remove suffix

# Join cases and deaths by Date + County
County_long <- Cases_long %>%
  left_join(Deaths_long, by = c("Date", "County"))

# View the final tidy dataset
View(County_long)




