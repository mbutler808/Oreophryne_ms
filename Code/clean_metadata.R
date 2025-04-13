## Functions
newgenus <- function(name) {										# rename Oreo A/B
	name <- sub("Oreophryne[ _]B", "Auparoparo", name)
	name <- sub("Oreophryne[ _]A", "Oreophryne", name)
	name <- sub("Aphantophryne[ _]B", "Aphantophryne", name)
	return(name)
}

######### Start code 
library(ggtree)
library(treeio)

## ---- inputs --------
references <- read.csv("../Data/genus_references.csv")$type
iqtree <- read.iqtree("../Data/IQTree/model_search/senkenbergiana_merge.treefile")
#iqtree <- read.iqtree("../Data/IQTree/oreophryne_aphantophryne_references_trimmed_allspecies/oreophryne_aphantophryne_references_trimmed.treefile")
taxa <- read.csv("../Data/Table1_oreotype.csv")          # tree metadata spreadsheet
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  

taxa$label <- newgenus(taxa$label)
taxa$label_clean <- gsub("[-_]", " ", taxa$label) 
taxa$label <- gsub("[-.]", "_", taxa$label) 
taxa$label <- sub("sp_$", "sp", taxa$label) # tree labels donʻt have trailing sp_ on sp.
taxa$oldgenus <- taxa$genus
taxa$genus <- newgenus(taxa$genus)  # renamed genera
taxa$gensp <- paste(taxa$genus, taxa$species, sep=" ")
taxa$reference <- taxa$label %in% references  # references for type species
taxa$fontface <- "italic"
taxa$fontface[taxa$reference] <- "bold.italic"  # bold for references
taxa$outgroup <- taxa$id %in% c("UMMZ211174", "UMMZ211181", "UMMZ219489") # outgroup flag
taxa$gcol <- gcol[taxa$genus]

# The senkengerbiana dataset has old taxa labels
tips <- get_taxa_name(ggtree(iqtree))
tips <- newgenus(tips)
keep <- taxa$label %in%  tips # keep taxa in tree

# metadata matches tree now
taxa <- taxa[keep,]   # 235 taxa

######### Output 
# metadata for taxa that match tips on tree
write.csv(taxa, "../Data/Processed_data/metadata.csv", row.names=F)