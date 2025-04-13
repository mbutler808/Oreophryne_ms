# README.md

To reconstruct the phylogeny using IQTREE, use these four inputs: 

-  alignment_asterophryinae_20230720.nex = the sequence alignment 
-  asterophryinae_partitions.nex = the sequence partition file
-  asterophryinae_dates.txt  = the dates file with timings
-  "UMMZ219489_Scaphiophryne_marmorata" outgroup specified on the command line with -o flag

Include the path to the file(s) above to run them from original location (everyting is in the ../input/ folder).

Flags:
-s    alignent.phy
-spp  partitions.nex
-m    MFP+MERGE find best model w/merging, run tree
      MF+MERGE  find best model w/o running tree
      MFP to find best model for each partition, run tree
      MF to find best model for each partition, no tree
-B    1000  to run tree with 1000 bootstrap replicates 
-pre  a prefix to name the outputs. can also redirect output to a different folder by prepending the path   
-o    "outgroup" 
--date  dates.txt
--date-tip  the tip dates, 0
-mset  limit models to a smaller subset, BEAST2 has JC,F81,K80,HKY,TN,TNe,TPM2,TPM2u,TPM3,TPM3u,TIM,TIMe,TIM2,TIM2e,TIM3,TIM3e,TVMe,TVM,SYM,GTR 

Use IQTREE2 and run the following command (IQTREE is run from the command line):

Template:

  iqtree2 -s alignment.phy -spp partitions.nex -m MFP+MERGE -B 1000 --date dates.txt -o "outgroup" --date-tip 0

Filled in - run PartitionFinder plus tree:

  iqtree2 -s ../input/alignment_asterophryinae_20230720.nex -spp ../input/asterophryinae_15partitions.nex -m MFP+MERGE -B 1000 --date ../input/asterophryinae_dates.txt -o "UMMZ219489_Scaphiophryne_marmorata" --date-tip 0 -mset JC,F81,K80,HKY,TN,TNe,TPM2,TPM2u,TPM3,TPM3u,TIM,TIMe,TIM2,TIM2e,TIM3,TIM3e,TVMe,TVM,SYM,GTR ;

Filled in - run PartitionFinder only 

  iqtree2 -s asterophryinae_12232022.phy -spp asterophryinae_partitions.nex -m MF+MERGE -o "UMMZ219489_Scaphiophryne_marmorata" --date-tip 0 -mset JC,F81,K80,HKY,TN,TNe,TPM2,TPM2u,TPM3,TPM3u,TIM,TIMe,TIM2,TIM2e,TIM3,TIM3e,TVMe,TVM,SYM,GTR ;

