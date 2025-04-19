# Oreophryne_manuscript

As of April 18th, 2025, this repo is the primary location for all materials pertaining to the Oreophryne taxonomic revision manuscript. It was split from the conflict between the codes manuscript. 

This repo contains folders:
- Code: all code to produce the analyses and figures

- Data: 
  - IQTree: all of the phylogenetic inputs and outputs
    - mesquite_alignment: sequence data as alignments
    - input: all other inputs, model specifications, etc.
    - model_search: all outputs
  - Processed_data: 
    - data for manuscript tables
    - processed data for input to analyses
  - metadata files for the manuscript (called from oreo-taxonomy.qmd) are at the top level of Data.

- Products: manuscript files (in .qmd, .html, .docx and .pdf formats) and references (in .bib) 
  - oreo-taxonomy.qmd: the main manuscript file
  - oreo-taxa-cut.qmd: cut from the joint manuscript
  - Figures: All manusript figures
  - Tables: includes tables to be embedded in the final manuscript in .csv format. Will need to be updated to match changes on current working copies in Google sheets/Google doc draft
