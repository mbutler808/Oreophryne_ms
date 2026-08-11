# README.md

Products Directory 

- Manuscript
  - oreo-taxonomy.qmd: the main manuscript file
  - oreophryne.bib: the bibliography file
  - oreo-taxonomy{.html} rendered manuscript files (in .html, .docx and .pdf formats)  
  - Figures: All manusript figures
  - Tables: includes tables to be embedded in the final manuscript in .csv format. 
  - etc: style files and docx formatting
- Validation_Figures
    - figures to check output and steps in analysis

### Reproducing the manuscript

in Quarto (from the command line in the Products folder):
1. render `oreo-taxonomy.qmd` 

`> quarto render "oreo-taxonomy.qmd"`

The manuscript will output here, with figures and tables in their respective subdirectories