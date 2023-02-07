# Risk factors analyses

Codes for the risk factors analyses. 

## Structure

In the file `code_figure_4.Rmd` the code used to produce Figure 4 is contained.
The code uses only one dataset:
- Dataset 1.	The data resulted from the multiple imputation analysis (Results_mi.RDS).

In chunk 3 we calculate the Cox regression analysis for the non-reference categories.
In chunks 4-5 there reference category is changed for the sum to zero analysis.
In chunk 6 we calculate the Cox regression analysis for the reference categories.
In chunks 7-9 the estimates with confidence intervals are put together and prepared for plotting (chunk 10).
In chunk 11 the figure is saved.


## Contact

Noemi Castelletti
