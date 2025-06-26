library(ape)
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

## input alignment comes from Mesquite. This one has all oreophryne, auparoparo including aphantophryne and references for cophixalus, paedophryne and outgroups

dat <- read.nexus.data("../Data/IQTree/mesquite_alignment/alignment_20240508_oreophryne_aphantophryne_references_allspecies_trimmed.nex") # 240 taxa

######### output directory for alignments
outpath <- "../Data/IQTree/mesquite_alignment/"

######### drop multiples of same species
tips <- names(dat)
outgroups <- grep("Dyscophis|Scaphiophryne|Platypelis", tips, value=T)			 
references <- nospaces(read.csv("genus_references.csv")$type)
refid <- sub("_.*$", "", references)
refgensp <- sub("^[A-Z0-9]*_", "", references)
keep <- grep(paste(refgensp, collapse="|"), tips, value=T, invert=T) # tips without reference species  # 198 tips

dups <- keep[duplicated(sub("^[A-Z0-9]*_", "", keep))]  # 19 duplicates

keep <- keep %w/o% dups

keep <- c(keep, references)  #198 tips  
keepgensp <- sub("^[A-Z0-9]*_", "", keep)  # for checking
sum(duplicated(keepgensp))

write.csv(tips %w/o% keep, file="duplicate_taxa_todrop.csv")

dat <- dat[keep]  # now 198 tips, only 1 sample per taxon


######### Write fasta files containing the desired tips

write.dna(dat, file=paste(outpath, "alignment_20240508_allspecies_198tips", ".fasta", sep=""), format="fasta") 				
