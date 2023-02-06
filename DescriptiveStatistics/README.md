# Descriptive Statistics

Codes for descriptive statistics on the cohort. 

## Structure

In the file `code_figure_1.Rmd` the code used to produce Figure 1 is contained.
The code uses three different datasets:
- Dataset 1.	The number of new SARS-CoV-2 cases reported from the city municipality of Munich (results_nowcast_2022-10-20),
- Dataset 2.	The cleaned and locked dataset for this analysis (R5_CompleteData_NC.RDS), and
- Dataset 3.	The dataset containing all rounds of KoCo19, including participants that were recruited for KoCo19 but were excluded (Database_KoCo_all_studies.RDS). 
Dataset 3 is needed to catch the variables containing the time when the sample was either taken or arrived our institute. In chunk 5 we select those variables and only the samples until Follow-up 5 of the KoCo19 participants.
In chunk 7 we select the new cases inly until March 1st, 2022.
In chuck 8 we create a table indicating how many samples came back on each day.
In chuck 9 we join the dataset containing the new cases and the dataset conaining the number of samples that came back every day.
In chunks 11-12 we transform the date from daily format to a weekly format.
In chunk 13 we plot and in chunk 14 we save the plot.


In the file `code_figure_2.Rmd` the code used to produce Figure 2 is contained.
The code uses only one dataset:
- Dataset 1.	The cleaned and locked dataset for this analysis (R5_CompleteData_NC.RDS).
From Dataset 1 only the variables containing the qualitative (positive/negative) results of anti-N und anti-S and the questionnaire-data on vaccination are needed (chunk 3).
In chunk 4 we define the status of each sample: only infected (positive in only anti-N), naïve (negative in all), vaccinated (positive in only anti-S), missing. The definition of ever positive is taken, meaning that if one person turned anti-N positive, it is considered positive for all following rounds independently of the round-result.
In chunk 5-7 we produce and save the Sankey-plot.
In chunks 8-9 we produce and save the missing value analysis.
In chunk 10 we put the two plots together in one unique plot, save in chunk 11


In the file `code_figure_S2.Rmd` the code used to produce Figure S2 is contained.
The code uses only one dataset:
- Dataset 1.	The cleaned and locked dataset for this analysis (R5_CompleteData_NC.RDS).
From Dataset 1 only the variables containing the qualitative (positive/negative) results of anti-N und anti-S and the questionnaire-data on vaccination are needed (chunk 3).
In chunk 4 we define the status of each sample: only infected (positive in only anti-N), naïve (negative in all), vaccinated (positive in only anti-S), missing.
In chunk 5-7 we produce and save the Sankey-plot.


## Contact

Noemi Castelletti

