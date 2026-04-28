# Statistical Data Modeling & Analysis in R

This repository contains multiple data analysis projects focused on exploratory data analysis (EDA), hypothesis testing, and predictive modeling using R. The projects demonstrate the ability to clean complex datasets, perform rigorous statistical tests, and create data visualizations to uncover actionable insights.

## Project 1: Air Quality Predictive Modeling & Hypothesis Testing
**Objective:** To analyze how meteorological factors (solar radiation, wind speed, and temperature) influence ozone levels in New York and to build a predictive model for daily air quality.

* **Exploratory Data Analysis:** Handled missing values, normalized variables, and generated comprehensive scatterplot matrices and boxplots to identify correlations and seasonal outliers.
* **Hypothesis Testing:** Conducted ANOVA, Kruskal-Wallis, and independent sample t-tests to validate statistically significant differences in air quality across different months. Evaluated data distribution using the Shapiro-Wilk normality test.
* **Predictive Modeling:** Developed a Multiple Linear Regression model explaining ~60.6% of the variance in ozone levels. Evaluated the model's validity using diagnostic plots (Residuals vs Fitted, Normal Q-Q, Scale-Location).

## Project 2: Stochastic Simulation of Football Scores
**Objective:** To model and simulate the outcomes of the 2011/2012 English Premier League season based on historical goal-scoring data.

* **Statistical Modeling:** Applied the Poisson Distribution to transform continuous team attack/defense strengths into discrete probabilities for specific scorelines.
* **Algorithm Calibration:** Developed a custom calibration factor using geometrical means to correct discrepancies between expected and actual goals. Included a randomized "Form" variable to account for real-world unpredictability.
* **Monte Carlo Simulation:** Implemented a Monte Carlo method running 5,000 parallel iterations to identify the optimal random seed. The best simulation successfully predicted the total season goals with a very low error margin (only a 27-goal difference out of 1066 real goals) and projected a highly accurate final league table.

## Technical Stack
* **Language:** R
* **Core Concepts:** Statistical Modeling, Predictive Analytics, Stochastic Simulation, Hypothesis Testing, Data Cleansing, Data Visualization.

## Repository Structure
* `Code/Air_Quality_Analysis` - R scripts, datasets, and visualizations for the predictive modeling project.
* `Code/Football_Simulation` - R scripts for the Poisson and Monte Carlo simulations.
* `/Docs` - Comprehensive technical documentation and mathematical methodologies for both projects.
