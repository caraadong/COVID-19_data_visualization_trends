# README

# Maryland COVID-19 Data Visualization Project

This project analyzes COVID-19 trends in Maryland using four cleaned datasets that include county-level cases, county-level deaths, statewide case rates, and vaccination progress by age group. The goal is to explore how COVID-19 spread across the state, how different populations were affected, and how vaccination rollout shaped later trends.

The final output includes three visualizations that each highlight a different part of the pandemic timeline.

# 1. Daily COVID-19 Cases by County

**Files used:** `County.csv`  

This graph shows how COVID-19 cases changed over time for every Maryland county. More populated counties like Montgomery, Prince George’s, and Baltimore consistently had the highest case counts, while smaller rural counties showed much lower levels. The visualization helps illustrate how local population density and community spread influenced the impact of COVID-19 across the state.

# 2. Cumulative Fully Vaccinated Residents by Age Group

**Files used:** `Statewide.csv`  

This plot highlights Maryland’s vaccination rollout by age group. Older adults were vaccinated earliest and at the fastest rate, reflecting the state’s risk-based prioritization strategy. Younger age groups saw increases later on as eligibility expanded. The chart makes it easy to compare how quickly each group reached higher vaccination levels.

# 3. Cases per 100K Compared to Vaccination Rollout

**Files used:** `Statewide.csv`  

This graph overlays statewide case rates with cumulative vaccination progress. Vaccination numbers are scaled so both lines can be viewed together. As vaccinations increased sharply in early 2021, statewide case growth began to flatten. This visualization helps show the relationship between rising immunity and slowing case rates.

# 📁 Project Files

- `County.csv` — cleaned county-level cases and deaths  
- `Statewide.csv` — cleaned statewide vaccination and case-rate data  
- `README.md` — project explanation and documentation  

# 🛠️ Technologies Used

- **R** (tidyverse, ggplot2)  
- **CSV formatted datasets**  
- **RStudio** or any R environment  

# Purpose of the Project

This project was completed for a data visualization assignment to explore real public-health data using clear and effective graphics. The visualizations aim to communicate major COVID-19 patterns in Maryland, including:

- Differences in how counties were affected  
- Age based vaccination rollout  
- The relationship between vaccinations and case trends  

# 👩‍💻 How to Run the Code

1. Place all CSV files in the same working directory.  
2. Open any of the `*.rmd` script files.  
3. Run the script in an R environment (RStudio recommended).  
4. Each script will produce one of the final visualizations.  

# ✔️ Summary

Together, the three visualizations provide a clear look at how COVID-19 evolved across Maryland, how vaccination unfolded across age groups, and how these two trends interacted over time. The project demonstrates how data visualization can make complex public-health data more understandable and meaningful.
