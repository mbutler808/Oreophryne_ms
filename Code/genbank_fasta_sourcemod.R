###SEQUENCE SUBMISSION SCRIPT###
#Generates Source Mod Tables and updated FASTA files

#BankIt info
#In Sequence Modifiers table
#-Sequence_ID
#-Altitude, need to add units
#-Ecotype, leave out until verified
#-Lat_Lon (in decimal degrees)
#-Specimen_voucher (museum number)
#-Country="Papua New Guinea: site"
#In FASTA
# 	for example:
#	-[organism="genus species"] Genus species Museum number Seven in Absentia (SIA) gene, partial sequence; nuclear.


########################################################################### 
############################# FUNCTIONS ################################### 
########################################################################### 
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
nospaces <- function( var ) { return(gsub(" ", "", var)) }  # replace spaces with ""
sub_2space <- function(var) {return(gsub("_"," ", var))}  # replace _ to " "

# clean genus, species names:
strip_ABC <- function(var) { return (sub( "_[ABC]", "",var)) }  # remove _A,B,C
sp.2space <- function(var) {
	sp <- sub("(sp.)([0-9]+)", "\\1 \\2",dat$species)  # sp.1 -> sp. 1
	sp <- sub(".([0-9]+)", " \\1", sp)       # spname.1 -> spname 1
	sp[str_which(sp, "\\d$")] <- paste(sp[str_which(sp, "\\d$")], "CJF-2021")
	return(sp)
} 

collname <- function( var ) {
	nn <- sub("([A-Z]*)([0-9]*)", "\\1", nospaces(var))
	nn[nn=="AA"] <- "Allen Allison"
	nn[nn=="FK"] <- "Fred Kraus"
	nn[nn=="MB"] <- "Marguerite A. Butler"
	nn[nn=="JAM"] <- "Jimmy A. McGuire"
	nn[nn=="RG"] <- "Rainer Guenther"
	nn[nn=="RN"] <- "Ron Nussbaum"
	nn[nn=="BJE"] <- "Ben J. Evans and Iqbal Setiadi"
	return(nn)
}

# Prints fasta to file. 
# Input a list of sequences, 
# 	names = fasta first line
prlist <- function(x, out="out.fasta"){ 
					if (file.exists(out)) file.remove(out)
 					nams=names(x) 
                   for (i in seq_along(x)) cat(nams[i], "\n", x[[i]], "\n", file=out, append=T, sep="")
                   }


## Makes final fasta files and source modifier table
## Inputs: sequences from FASTA files, 
##				smod, 
## matches identifiers (ID) 
## Outputs: write out .fasta, .tsv with smod info

make.fasta <- function( dat, smod ) {
	# locus = "sia",
	# inp,
	# ls,
	# gt,
	# smod = smod, 
	# out.fasta.string= "sia.fasta", 
	# out.tsv.string="sourcemodifiers.tsv") {

	for (i in nrow(dat)) {
		llist <- as.list(dat[i,]) 
	}
  input <- inp[locus]
  locusstring <- ls[locus]
  genetype <- gt[locus]
  
  seq <- read.dna(input, format = "fasta", as.character=TRUE,as.matrix=F)
  seq <- lapply(seq, function(x) gsub("[-?]", "n", x))
  seq <- lapply(seq, paste,sep="",collapse="")

  ## Dropped sequences have all nʻs - we want to remove these
  # for example: remove dropped taxon- JR73   is 171
  #which(str_count(seq, pattern="n") == nchar(seq))  # finds entry that is all nʻs
#  seq <- seq[str_count(seq, pattern="n") != nchar(seq)]

  #extract IDs from fasta
  nam <- names(seq)
  ids <- sub(" (.*)$", "", nam)

  # Make fasta file into a dataframe seqdat
  seqdat <- data.frame( Sequence_ID=ids, 
  						nam, 
  						seq=unlist(seq), 
  						row.names=ids
  					  )  #dataframe with sequences

  # Merge sequence data and smod -> bigseq
  bigseq <- merge(seqdat, smod, all.x=T, sort=F)
  fasta <- bigseq$seq
  names(fasta) <- with(bigseq,
  						paste( ">", 
  						Sequence_ID, 
  						" [organism=", 
  						genus, 
  						" ", 
  						species, 
  						"] ", 
  						genus,
  						" ",  
  						species, 
  						" ", 
  						Specimen_voucher, 
  						" ",  
  						locusstring, 
  						genetype, 
  						sep=""
  						)
  					)

  out.fasta <- paste0(outpath, "/", locus, out.fasta.string)
  out.tsv <- paste0(outpath, "/", "sourcemodifiers_",locus, out.tsv.string)
					
  prlist( fasta, out.fasta )  # Print to .fasta file 
  write.table( bigseq[ c( "Sequence_ID", 
  						  "Specimen_voucher",
  						  "Collected_by",
  						  "Country", 
  						  "Altitude", 
  					 	  "Lat_Lon", 
  				 		  "Note", 
  						)
  					 ], 
  			  file=out.tsv, 
  			  sep="\t", 
  		 	  row.names=F
  			)  # write to source modifier table in .tsv format
  
}

########################################################################### 

###############################
## Start of Execution
###############################
#require(tidyr)
#require(stringr)
require(ape)
#require(seqinr)
require(dplyr)
#require( googlesheets4 )
#taxa2020master <- "https://docs.google.com/spreadsheets/d/1rKLExg3Bve-vJntTHmz-S8WXp3rR2DKlM-VQ3xZKKUM/edit#gid=1482811660"
#dat <- as.data.frame(read_sheet(taxa2020master))  ### INPUT DATA Taxa2020_master googlesheet 

if(dir.exists("../Data/Processed_data/genbank")!=TRUE) dir.create("../Data/Processed_data/genbank") # check if output directory out exists, if false create
outdir <- "../Data/Processed_data/genbank"

dat <- read.csv("../Data/Raw_data/metadata.csv")
dat <- dat[grep("BJE", dat$id),] # all samples donated by BJE

## Write Master Source Modifiers
## Tidying the data elements that we will use 

Altitude <- dat %>%
			mutate (
				Altitude = case_when(
					!is.na(elevation) ~ paste(elevation, "m"), 
					.default = ""
				)
			) %>% 
			select(Altitude) %>%
			as.vector()
ID <- dat$id
Lat_Lon <- dat %>% 
			mutate( 
				lat = case_when( 
					is.na(latitude) ~ "",
					latitude >= 0 ~ paste(latitude, "N"),
					.default = paste(latitude, "S")
				), 
				lon = case_when(
					is.na(longitude) ~ "",
					longitude >= 0 ~ paste(longitude, "E"), 
					.default = paste(longitude, "W")
				), 
				Lat_Lon = ifelse( lat!="" & lon!= "", paste( lat, lon ), "")
			) %>% 
			select (Lat_Lon) %>%
			as.vector()
genus <- dat$genus
species <- dat$species
site <- dat$site
locality <- dat$locality
province <- dat$province
country <- dat$country
lifestyle <- dat$lifestyle
speciesref <- dat$species_reference
typelocality <- dat$type_locality
Specimen_voucher <- paste("personal collection:Ben Evans", ID)
Collected_by <- "Iqbal Setiadi"
Collection_date <- dat$Collection_date
geo_loc_name <- with(dat, paste(country, ": ", site, ", ", locality, sep="" ))
Note <- 'in Fisher et al. 2025'    # fill Note field

## Source Modifier Dataframe + Genus + Species names:
## Make smod dataframe with column headers to match BankIt source modifier names
## genus and species will be used later for FASTA files, donʻt include genus species in BankIt

smod <- data.frame( Sequence_ID=ID, Specimen_voucher, Collected_by, Collection_date, geo_loc_name, Altitude, Lat_Lon, Note,  genus, species, country, province, locality, site )


###############
## Remake FASTA FILES and write SOURCE MODIFIER TABLES for submission to GenBank via BankIt

inpath <- "../Data/Raw_data/genbank"
outpath <- "../Data/Processed_data/genbank"
inputfile <- list.files(inpath)
locus <- sub("_(.*)$", "", inputfile) # get locus from filename
LOCUS <- toupper(locus)
dat <- data.frame(locus, LOCUS, inputfile)
row.names(dat) <- locus

loc.dat <- dat %>% 
	mutate( locusstring = 
		case_when( 
			locus == "bdnf" ~ "Brain Derived Neurotrophic Factor (BDNF) gene, partial sequence.",
			locus == "cytb" ~ "Cytochrome oxidase b (CYTB) gene, partial sequence.",
			locus == "nd4" ~ "NADH dehydrogenase subunit 4 (ND4) gene, partial sequence.", 
			locus == "nxc" ~ "Sodium Calcium Exchange subunit-1 (NXC-1) gene, partial sequence.", 
			locus == "sia" ~ "Seventh in Absentia (SIA) gene, partial sequence.")
	) %>% 
	mutate ( genetype = 
		case_when(
			locus %in% c("bdnf","nxc","sia") ~ " nuclear.",
			locus %in% c("cytb","nd4") ~ " mitochondrial."
			)
	) %>% 
	mutate ( input = paste(inpath, inputfile, sep="/") ) %>%
	mutate ( out.fasta = paste0(outpath, "/", locus, "_O_senkenbergiana_genbank.fasta")) %>% 
	mutate ( out.tsv=paste0(outpath, "/", "sourcemodifiers_",locus, "_O_senkenbergiana.tsv")
	) 
	

make.fasta <- function( X = loc.dat["sia",], smod ) {

  input <- X$input
  locusstring <- X$locusstring
  genetype <- X$genetype
  out.fasta <- X$out.fasta
  out.tsv <- X$out.tsv
  
  seq <- read.dna( input, 
  				   format = "fasta", 
  				   as.character=TRUE,
  				   as.matrix=F) # read in fasta files
  seq <- lapply(seq, 
  			function(x) {
  				gsub("[-?]", "n", x) # convert - and ? to n
  				paste(x, sep="", collapse="") # collapse to character string
  			}
  		)

  ids <- sub(" (.*)$", "", names(seq))   #extract IDs from fasta

  seqdat <- data.frame( Sequence_ID=ids,  # fasta -> dataframe seqdat
  						nam=names(seq), 
  						seq=unlist(seq), 
  						row.names=ids
  					  )  #dataframe with sequences

  # Merge sequence data and smod -> bigseq
  bigseq <- merge(seqdat, smod, all.x=T, sort=F)
  fasta <- bigseq$seq
  names(fasta) <- with(bigseq,
  					     paste( paste0(">", Sequence_ID), 
  						 		"[organism=", genus, species, "]", 
  						 		nam, locusstring
   						)
   					  )

					
  prlist( fasta, out.fasta )  # Print to .fasta file 
  write.table( bigseq[ c( "Sequence_ID", 
  						  "Specimen_voucher",
  						  "Collected_by",
  						  "Collection_date",
  						  "geo_loc_name", 
  						  "Altitude", 
  					 	  "Lat_Lon", 
  				 		  "Note" 
  						)
  					 ], 
  			  file=out.tsv, 
  			  sep="\t", 
  		 	  row.names=F
  			)  # write to source modifier table in .tsv format
  
}

make.fasta(	loc.dat["bdnf",], smod )
make.fasta(	loc.dat["cytb",], smod )
make.fasta(	loc.dat["nd4",], smod )
make.fasta(	loc.dat["nxc",], smod )
make.fasta(	loc.dat["sia",], smod )
