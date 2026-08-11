# Oreophryne_Auparoparo_manuscript
[![DOI](https://zenodo.org/badge/646599963.svg)](https://zenodo.org/badge/latestdoi/646599963)

## This repository contains the data and code to produce the analyses and manuscript for "Hiding in the trees: Unmasking a cryptic genus of arboreal New Guinea frogs with phylogenetic taxonomy"

## History

2026-08-10 First release with Data, Code, and Products folders.  

## Overview

This repository supports _Fisher et al. (2026 in review)_. For over 130 years, _Oreophryne_ was thought to be a single clade, but we recently found that they are two separate clades that were independently evolved (i.e., not sister taxa). We propose a new name _Auparoparo_ to distinguish between the clades which are seemingly identical in morphology and arboreal lifestyle. We find no morphological synapomorphies that can distinguish the clades, and therefore define the clades using a phylogenetic nomenclature. We map the known distribution of _Oreophryne_ as previously recognized overlaid _Oreophryne_ and _Auparoparo_ assigned via molecular phylogenetic analysis using Maximum Likelihood phylogenetics implemented in `IQTREE2`, and produce graphics using the R packages `ggtree` and ``. See the code and data explainer provided in `Code/oreo-taxonomy.html` and the paper _Fisher et al. (2026 in review)_ at _Biological Journal of the Linnean Society_.

## Software requirements

This repository requires use of R, Quarto, Github and a reference manager for bibtex. A plain text editor is also necessary. 

## Repository structure

The description of the directory structure is as follows (Please see the `README.md` files in each folder for more details):

* All data is in the `Data` folder or is referenced as a link to a publicly available file.
* All code is in the `Code` folder. An explanation of all of the code is in the quarto file `code_explainer.qmd`, and rendered for easy reading in `code_explaine.html` (open the .html in a browser).
* All results (Figures, Tables) are saved in subdirectories of the `Products/Manuscript` folder.
* The `quarto` file for the manuscript and its component parts are in the `Manuscript` folder.
* See the various `README.md` files in those folders for some more information.

  
### Reproducing the analysis

1. Run `maps.R` from the code folder (Figure 3)
2. Run `oreocode.R` from the code folder (Figure 2)
3. Run `tables.R` from the code folder (Tables 1-4, Supplementary Tables 1-2)
4. Or run `code_explainer.qmd` to run all three and generate the explainer .html

### Citations

(This study) __Fisher, A.R., Cao, K., Allison A., Iova B., and  Butler M.A. (in review)__ Hiding in the trees: Unmasking a cryptic genus of arboreal New Guinea frogs with phylogenetic taxonomy  _Biological Journal of the Linnean Society_.  

This repo contains folders:
- Code: all code to produce the analyses, figures, and tables
  - code_explainer.qmd - a quarto file runs all the code and produces the .hmml explainer file that explains the three code scripts
  - maps.R - generates map
  - oreophylogeny.R - generates the phylogeny
  - tables.R - generates all tables 

- Data: 
  - IQTree: all of the phylogenetic inputs and outputs generated using IQTREE
    - alignment: sequence data as alignments
    - input: all other inputs, model specifications, etc.
    - model_search: all outputs
      - iqtreerun_bestmodel.sh - shell script for generating phylogeny
      - subdirectories for testing alternate partition models
    - README explains how to produce the results
  - Raw_data:
    - input data for the codes, prior to any processing.
    - metadata for the phylogeny and map and input data for the tables. 
  - Processed_data: 
    - data for manuscript tables
    - processed data for input to analyses
  - metadata files for broader subfamiliy are at the top level of Data.

- Products: 
  - Manuscript
    - oreo-taxonomy.qmd: the main manuscript file
    - oreophryne.bib: the bibliography file
    - rendered manuscript files (in .html, .docx and .pdf formats) 
    - Figures: manuscript figures
    - Tables: mansucript tables in .docx format. 
    - etc: style files and docx formatting
  - Validation_Figures
    - figures to check output and steps in analysis