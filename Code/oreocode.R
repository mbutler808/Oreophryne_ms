# 1. Read in trees
# 2. Clean tree labels 
# 3. Trim tree down to 1 species per site  (210 sp tree plus new samples)
# 4. Plot tree with Type Oreophryne highlighted
# 5. Drop non-Oreophryne tips (or reduce to one representative per genus?)
# 6. Plot reduced trees
################################################################
## ---- packages --------
# If you need ggtree, treeio use BiocManager
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("ggtree")
# BiocManager::install("treeio")

library(tidytree)
library(treeio)
library(ggtree)
library(dplyr)
library(ggplot2)

#library(ape)
#library(ggpmisc)
#library(gridExtra)

## ---- functions --------
source("clean_functions.R") # contains no_()
source("plotting_functions.R") 

## ---- loaddata --------
iqtree <- read.iqtree("../Data/IQTree/model_search/senkenbergiana_merge.treefile")
# if you change the input treefile, run clean_metadata.R with the new treefile
d <- read.csv("../Data/Raw_data/metadata.csv")      # tree metadata spreadsheet
references <- read.csv("../Data/genus_references.csv")    # genus reference taxa 
references$id <- gsub("_(.*)","", references)
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  

#tree <- read.beast("../Data/Processed_data/tree_with_data.nex")

## ---- plotfulltree --------
p <- ggtree(iqtree, size=.1)  %<+% d +  # add annotation dataframe
     geom_tiplab(aes(label=no_(label)), size=1.3) + 
#     geom_tiplab(aes(label=label), size=1.3) + 
 		geom_text2(aes(subset = (!isTip), label=label), 
 					size=1.5, 
   					nudge_x=-.007, 
  					nudge_y=1) +
  		ggplot2::xlim(0, 0.7)

print_output("../Products/Figures/FullIQTree", p) 

## ---- trimtree --------
### 3. Trim tree down to 1 sample per species (204 sp tree)

tree <- full_join(iqtree, d, by="label") # full tree with metadata 235 taxa
## full_join(iqtree, d, by="label") %>% as_tibble %>% as.data.frame # check merge
tibb <- as_tibble(tree) %>% cutnoderows  # a tree tibble, tip rows only 235

ss <- tibb %>%    # 207 taxa
   arrange (desc(id %in% references$id)) %>%  # move references to top
   distinct(across(gensp), .keep_all=TRUE) %>% as.data.frame  # drop duplicates
   
todrop <- tibb$label %w/o% ss$label # drop 32 taxa at multiple sites, outgroups
todropid <- sub("[_-].*$", "", todrop)
write.csv(data.frame("drops"=c(todrop, d$label[d$outgroup])), file="dropped_taxa_tips_fulliqtree.csv", row.names=F)

dtree207og <- treeio::drop.tip(tree, todrop)  # tree with only one sample per species - 203 tips. 
dtree204 <- treeio::drop.tip(tree, c(todrop, d$label[d$outgroup]))  # ingroup tree with only one sample per species - 200 tips. 

keepids <- c("moluccensis") # leave in all  senkenbergiana
todrop <- grep(paste(keepids, collapse="|"), todrop, invert=T, value=T )  # drop 30 taxa

dtree215og <- treeio::drop.tip(tree, todrop)  # outgroup tree with all new samples, only one of others - 205 tips. 
dtree212 <- treeio::drop.tip(tree, c(todrop, d$label[d$outgroup]))  # ingroup tree as above - 202 tips. 

# Print trimmed iqtree 204 spp
p <- ggtree(dtree204, size=.1) + 
      geom_tiplab(aes(label=label_clean), size=1.3) + 
      geom_text2(aes(subset = (!isTip), label=label), 
               size=1.5, 
                  nudge_x=-.005, 
               nudge_y=1) +
      ggplot2::xlim(0, 0.38)
 
print_output("../Products/Figures/TrimmedIQTree", p) 

## ---- fulltree --------
## Figure 2 - support with clades highlighted

dtree <- dtree212  # all new samples, no outgroups, one of other taxa

mrcas <- sapply( c("Oreophryne", "Auparoparo"), function(x) get_MRCA( ggtree(dtree), x))
MRCA_labels <- data.frame(node=mrcas, MRCA_label=names(mrcas), letter=c("A", "B"))

p_support <- ggtree(dtree, size=.2) %<+% MRCA_labels  +
   geom_tiplab(aes(label=gensp, 
#                    color=tdat$tipcol, 
                     fontface=fontface), 
                  size=1.3, 
                  offset=.005 ) + 
   geom_point2(aes(subset=(!isTip & as.numeric(label)>=95 )), size=1, color="black") +
   geom_point2(aes(subset=(!isTip & as.numeric(label)>=70 & as.numeric(label)<95 )), color="grey60", size=1) +
   geom_text2(aes(subset = (!isTip & as.numeric(label)<70), label=label), 
                     size=1.75, 
                     color="black",
                     nudge_x=-.004, 
                     nudge_y=1) +
   geom_text2(aes(subset = !is.na(MRCA_label), label=letter), 
                     size=4, 
                     color="black",
                     nudge_x=-.005, 
                     nudge_y=2,
                     fontface='bold') +
   theme_tree(legend.position="none") +
   ggplot2::xlim(0, 0.4) 
   
print_output("../Products/Figures/FullTree_support", p_support) 
print_output("../Results/fig2.FullTree_support_highlight", gradient_tree(p_support)) 


