# README.md

`Data` directory contains three subdiretories for the three data types:

- IQTree: all of the phylogenetic inputs (and outputs) generated using IQTREE
  - alignment: sequence data as alignments
    - input: all other inputs, model specifications, etc.
  - model_search: all outputs
    - iqtreerun_bestmodel.sh - shell script for generating phylogeny
    - subdirectories for testing alternate partition models
  - README explains how to produce the results
- Raw_data:
    - input data for the codes for creating maps, plotting phylogenies, and making tables, prior to any processing.
    - metadata for the phylogeny and map and input data for the tables. 
- Processed_data: 
    - data for manuscript tables
    - processed data for input to analyses
  - metadata files for broader subfamiliy are at the top level of Data.

Files:
- gencolorABC.csv = color codes for genera
- genus_references.csv = type specimens and proxies used in the phylogeny
- genus_type.csv = list of type species/proxies and citations