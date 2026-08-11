# README.md

`Code` directory contains all of the codes used and an explainer document:

- code_explainer.qmd : A quarto (markdown) file that explains the code. 
  - Calls the three R scripts below
  - Run this to generate all analyses, tables, figures and `code_explainer.html` (read this in a web browser).

### Reproducing the analysis

in Quarto (from the command line in the Code folder):
1. run `code_explainer.qmd` to run all three R scripts below and generate the explainer .html

`> quarto render "code_explainer.qmd"`

Or one script at a time (in R):
1. Run `maps.R` from the Code folder (Figure 3)
2. Run `oreocode.R` from the Code folder (Figure 2)
3. Run `tables.R` from the Code folder (Tables 1-4, Supplementary Tables 1-2)

- `clean_functions.R` = custom cleaning functions
- `plotting_functions.R` = custom plotting functions 

