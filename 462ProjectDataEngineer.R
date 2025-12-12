library(dplyr)
library(readr)
library(tidyr)


###Load Datasets###
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


###Create Statewide Dataset###

#Standardize date column names#
Vacc <- Vacc %>% rename(Date = VACCINATION_DATE)
Casesx100 <- Casesx100 %>% rename(Date = ReportDate)

#Convert dates#
Vacc$Date <- as.Date(Vacc$Date)
Casesx100$Date <- as.Date(Casesx100$Date)

#Join Vacc Dataset w Casesx100 datasets#
Statewide <- Vacc %>%
  left_join(Casesx100, by = "Date")

#Rename the Statewide column to Cases_per_100#
Statewide <- Statewide %>%
  rename(Cases_per_100 = Statewide)

#Drop OBJECTID columns in Statewide Dataset#
Statewide <- Statewide %>%
  select(-starts_with("OBJECTID"))

#Replace NAs 
Statewide <- Statewide %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

#Dataset
View(Statewide)


###County Datasets###

#Standardize dates
CasesxCountyDropped <- CasesxCountyDropped %>% rename(Date = DATE)
DeathsDropped <- DeathsDropped %>% rename(Date = DATE)

#Convert dates
CasesxCountyDropped$Date <- as.Date(CasesxCountyDropped$Date)
DeathsDropped$Date <- as.Date(DeathsDropped$Date)

# Join CasesxCounty and DeathsDropped datasets
County <- CasesxCountyDropped %>%
  left_join(DeathsDropped, by = "Date", suffix = c("_Cases", "_Deaths"))

#Drop OBJECTID columns 
County <- County %>%
  select(-starts_with("OBJECTID"))

#Replace NAs 
County <- County %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

#DataSet
View(County)


###Convert County Dataset into Tidy Long Format##

#Convert County Cases
Cases_long <- County %>%
  select(Date, ends_with("_Cases")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "County",
    values_to = "Cases"
  ) %>%
  mutate(County = gsub("_Cases", "", County))  

#Convert County Deaths
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

#Dataset
View(County_long)



###Main Two Datasets to Use###
View(Statewide)
View(County_long)

#Downloaded the datasets into a csv file
readr::write_csv(Statewide, "Statewide.csv")
readr::write_csv(County_long, "County.csv")




#####DATA ENGINEER CHECKLIST######

  ###Data Provided###
#The original datasets can be found in our Github but if not, here are links:
#Cases by County: https://data.imap.maryland.gov/datasets/maryland::mdcovid19-casesbycounty/explore
#Deaths by County: https://data.imap.maryland.gov/datasets/3dbd3e633b344c7c9a0d166b1d6a2b03_0/explore?filters=eyJEQVRFIjpbMTYzOTg3MDc3OTg1Mi4xNiwxNzU5ODMxMjAwMDAwXX0%3D 
#Cases Per 100k: https://data.imap.maryland.gov/datasets/05f47db60aa442488b0defc3421307f4_0/explore 
#Vaccination: https://data.imap.maryland.gov/datasets/13e385378f604db8b6725d636656540c_0/explore 
  #With these links, you can download the dataset, in this Project we used CSV files. To download, click the
  #download button, it should be a cloud icon with an arrow.
  #As for two datasets we created and used, the code to create the dataset and download it can be found above 
  #this section.

  ####Data Source Described###
#The links to the original dataset can be found above. The two dataset we created and used can also be found 
#above. The data was collected by Maryland's State Government and the dataset was found on Maryland.gov site.
#The data was collected and reported through the partner and use of multiple agencies such as the Vital Statistics 
#Administration, local health departments, Maryland Department of Planning, and ImmuNet. 
  #For Cases by County, local health department reported the number of positive test results. As for how they
#they got the test results, it unclear but it likely from testing sites held by health departments.
  #For Deaths by County, Viral Statistics collected the data from hospitals and other facilities. Deaths were 
#confirmed from death certificates indicating if deaths were from COVID. Hospitals and other facilities conducted 
#the testing to determine if death was due to COVID.
  #For per 100k, it is calculated by the sum of the CasesByCounty layer and the 2019 estimated county populations
#(Maryland Department of Planning).
  #For the vaccination dataset, the method of collection of data was not given but with some research, it can be 
#assumed collection were from reports from multiple vaccination sites, then added up to get the totals.

  ###Data appropriate and valid###
#The data is real as it comes from an official state government site and datasets. The data is useful and good
#to create insights and graphs regarding COVID in Maryland.


  ###Data Variables Described + Stats###

  ##County Data Set##

#Date
  #This variable describes the date of when the date is inputted into the dataset. In this case, date is a
#ordinal data. This variable is important as we can use it to track the number of cases and deaths over time.
County_long %>% count(Date)


#County
  #This variable describes the location(county) of the reported data. County is a categorical variable. The 
#possible values are county names such as Prince Georges, Montgomery, Calvert, etc...
County_long %>% count(County)


#Cases
  #This variable describes the number of reported COVID-19 cases. Cases is a numeric variable and the unit is 
#cases. The number aren't in hundreds or thousands, it is the number reported. So like 4 would mean 4 cases 
#of COVID-19.
mean(County_long$Cases)
median(County_long$Cases)
range(County_long$Cases)

#Deaths
  #This variable describes the number of deaths from COVID-19. Deaths is a numeric variable and unit is death.
#The number isn't in hundreds, thousands, or anything, it is the number reported. So a report of 10 would mean
#10 deaths.
mean(County_long$Deaths)
median(County_long$Deaths)
range(County_long$Deaths)


  ##Statewide Data Set##  

#The first and second daily dose refers to COVID vaccinations that should be two doses.

#Date
  #This variable describes the date of when the date is inputted into the dataset. In this case, date is a
#ordinal data. This variable is important as we can use it to track other variables over time. For example 
#date can be used to track doses over time.
Statewide %>% count(Date)


#AgeRange
  #This variable describes the age ranges in years of the people reported in the data. Age Range is a ordinal 
#categorical data. This variable's possible values is broken into age ranges, 11 and under, 12-17, 18-49, etc...
Statewide %>% count(AgeRange)



#FirstDailyDose
  #This variable describes the number of people who got their first dose of the COVID-19 vaccination for 
#that date. This variable is numeric and the name of the unit is people. #The number isn't in hundreds, 
#thousands, or anything, it is the number reported. So, a input of 10 means in that day, 10 people got their
#first vaccination dose.
mean(Statewide$FirstDailyDose)
median(Statewide$FirstDailyDose)
range(Statewide$FirstDailyDose)


#FirstDailyDoseCumulative
  #This variable describes the total number of people who got their first dose of COVID-19 vaccination. This 
#variable tallies the total from the beginning to the date reported. This variable is number and name of the unit
#is people. The number isn't in hundreds, thousands, or anything, it is the number reported. So, an input of 
#120 would mean that up to to that date, 120 people have gotten their first dose of the vaccination.
mean(Statewide$FirstDailyDoseCumulative)
median(Statewide$FirstDailyDoseCumulative)
range(Statewide$FirstDailyDoseCumulative)


#SecondDailyDose
  #This variable describes the number of people who got their second dose of the COVID-19 vaccination for 
#that date. This variable is numeric and the name of the unit is people. #The number isn't in hundreds, 
#thousands, or anything, it is the number reported. So, a input of 12 means in that day, 12 people got their
#second vaccination dose.
mean(Statewide$SecondDailyDose)
median(Statewide$SecondDailyDose)
range(Statewide$SecondDailyDose)


#SecondDailyDoseCumulative
  #This variable describes the total number of people who got their second dose of COVID-19 vaccination. This 
#variable tallies the total from the beginning to the date reported. This variable is number and name of the unit
#is people. The number isn't in hundreds, thousands, or anything, it is the number reported. So, an input of 
#210 would mean that up to to that date, 210 people have gotten their second dose of the vaccination.
mean(Statewide$SecondDailyDoseCumulative)
median(Statewide$SecondDailyDoseCumulative)
range(Statewide$SecondDailyDoseCumulative)

#SingleDailyDose
  #With this variable it refers to a COVID vaccination that only requires one dose. This variable describes the 
#total number of people who got their single dose for that date. This variable is numeric and the name of the 
#unit is people. #The number isn't in hundreds, thousands, or anything, it is the number reported. So, a input 
#of 67 means in that day, 67 people got their single daily vaccination dose.
mean(Statewide$SingleDailyDose)
median(Statewide$SingleDailyDose)
range(Statewide$SingleDailyDose)


#SingleDailyDoseCumulative
  #With this variable it refers to a COVID vaccination that only requires one dose. This variable describes the 
#total number of people who got their single daily dose of COVID-19 vaccination. This variable tallies the total 
#from the beginning to the date reported. This variable is number and name of the unit is people. The number 
#isn't in hundreds, thousands, or anything, it is the number reported. So, an input of 117 would mean that up to
#to that date, 210 people have gotten their single daily dose of the vaccination.
mean(Statewide$SingleDailyDoseCumulative)
median(Statewide$SingleDailyDoseCumulative)
range(Statewide$SingleDailyDoseCumulative)


#CombinedAllDoses
  #This variable incorporates all different doses that were taken in that day. That includes first daily, second
#daily, and single daily. This variable is numeric and the name of the unit is doses. The number isn't in 
#hundreds, thousands, or anything, it is the number reported. So, a input of 82 means in that day, 82 doses 
#were administered that day.
mean(Statewide$CombinedAllDoses)
median(Statewide$CombinedAllDoses)
range(Statewide$CombinedAllDoses)



#CombinedAllDosesCumulative
  #This variables is the count of All Doses administered from the beginning to that day. This variable is 
#numeric and the name of the unit is doses. The number isn't in hundreds, thousands, or anything, it is the 
#number reported. So, a input of 212 means that up to that date, a total of 212 of doses were administered.
mean(Statewide$CombinedAllDosesCumulative)
median(Statewide$CombinedAllDosesCumulative)
range(Statewide$CombinedAllDosesCumulative)


#FullyVaccinated
  #This variable is the count of people that were considered fully vaccinated for that day. This variable is 
#numeric and the name of the unit is people. The number isn't in hundreds, thousands, or anything, it is the 
#number reported. So, a input of 5 means that 5 people were reported to be fully vaccinated for that day.
mean(Statewide$FullyVaccinated)
median(Statewide$FullyVaccinated)
range(Statewide$FullyVaccinated)


#FullyVaccinatedCumulative
  #This variable is the total number of fully vaccinated people from the beginning up to the date reported.
#This variable is numeric and the unit is people. The number isn't in hundreds, thousands, or anything, it is the 
#number reported. So, a input of 301 means that up to that date, a total of 301 people were fully vaccinated.
mean(Statewide$FullyVaccinatedCumulative)
median(Statewide$FullyVaccinatedCumulative)
range(Statewide$FullyVaccinatedCumulative)


#Cases_per_100
  #This variable is for the whole entire state of Maryland. With this variable, it is the number of cases per 100 thousand
#people. This variable is numeric and the unit is cases. An input of 10.12 would mean that there is a report 
#10.12 cases per 100 thousand people.
mean(Statewide$Cases_per_100)
median(Statewide$Cases_per_100)
range(Statewide$Cases_per_100)

