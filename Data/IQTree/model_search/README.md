# README.md

To reconstruct the phylogeny using IQTREE, run the shell script from the command line from within the `model_search` directory (adjust according the your shell - you must have IQTREE installed. See <https://iqtree.github.io/doc/Quickstart> for help):

`> sh ./iqtreerun_bestmodel.sh`

The following is an explanation of the command specified in the shell script `iqtreerun_bestmodel.sh` 

Four inputs (in the `../input/` folder): 

-  senkenbergiana_alignment.nex = sequence alignment  
-  Partition file (choose one):
  +  asterophryinae_15partitions.nex = 15 partitions (5 loci x 3 codons) 
  +  asterophryinae_5partitions.nex = 5 partitions (5 loci) 
  +  asterophryinae_codonpartitions.nex = codon paritions (3) 
-  asterophryinae_dates.txt = dates file (timings) 
-  "UMMZ219489_Scaphiophryne_marmorata" =  outgroup specified on the command line with -o flag

IQTREE2 Flags:
-s  alignment.phy
-p  partitions.nex
-m  MFP+MERGE to run tree and find best model
    MF+MERGE  to find best model w/o running tree
-B  1000  to run tree with 1000 bootstrap replicates    
-pre  a prefix to name the outputs. can also redirect output to a different folder by prepending the path   
-o  "outgroup" 
--date  dates.txt
--date-tip  the tip dates, 0 for a time-calibrated tree
-mset  limit models to a smaller subset
-redo add -redo flag if redoing the tree, or delete all output files

Use IQTREE2 and run the following command (IQTREE is run from the command line):

Template:

  `iqtree2 -s <alignment> -p <partitions> -m MFP+MERGE -B 1000 --date <dates.txt> -o "outgroup" --date-tip 0`

Run PartitionFinder plus tree with the filenames specified (specify path to input folder):

  `iqtree2 -s ../input/senkenbergiana_alignment.nex  -spp ../input/asterophryinae_15partitions.nex -pre senkenbergiana_merge -m MFP+MERGE -B 1000 -o "UMMZ219489_Scaphiophryne_marmorata" --date ../input/asterophryinae_dates.txt --date-tip 0 -mset JC,F81,K80,HKY,TN,TNe,TPM2,TPM2u,TPM3,TPM3u,TIM,TIMe,TIM2,TIM2e,TIM3,TIM3e,TVMe,TVM,SYM,GTR;` 

To run PartitionFinder only: 

  `iqtree2 -s ../input/senkenbergiana_alignment.nex  -spp ../input/asterophryinae_15partitions.nex -m MF+MERGE -o "UMMZ219489_Scaphiophryne_marmorata" --date ../input/asterophryinae_dates.txt --date-tip 0 -mset JC,F81,K80,HKY,TN,TNe,TPM2,TPM2u,TPM3,TPM3u,TIM,TIMe,TIM2,TIM2e,TIM3,TIM3e,TVMe,TVM,SYM,GTR;` 
