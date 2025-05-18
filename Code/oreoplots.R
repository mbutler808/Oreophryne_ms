# 1. Read in trees
# 2. Clean tree labels 
# 3. Trim tree down to 1 species per site  (210 sp tree plus new samples)
# 4. Plot tree with Type Oreophryne highlighted
# 5. Drop non-Oreophryne tips (or reduce to one representative per genus?)
# 6. Plot reduced trees
################################################################
#BiocManager::install("ggtree", force=T, type = 'source')

## ---- packages --------
library(dplyr)     # general
library(ggplot2)
library(tidytree)  # phylogenies
library(treeio)
library(ggtree)
library(phytools)  
library(ape)
library(ggpmisc)   # one taxa tests
library(gridExtra)
library(ggrepel)   # genetree speciestree
library(viridis)
library(GGally)
library(mapdata) ## must install from CRAN

## ---- functions --------
source("clean_functions.R") # contains no_()
source("plotting_functions.R") # contains no_()

## ---- loaddata --------
# the full tree
iqtree <- read.iqtree("../Data/IQTree/model_search/senkenbergiana_merge.treefile") 
d <- read.csv("../Data/Processed_data/metadata.csv")      # tree metadata spreadsheet
references <- read.csv("../Data/genus_references.csv")$type    # genus reference taxa 
outgroups <- d$label[d$outgroup]
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus

gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  


## ---- plotfulltree --------
p <- ggtree(iqtree, size=.1)  %<+% d +  # add annotation dataframe
 		geom_tiplab(aes(label=label_clean), size=1.3) + 
 		geom_text2(aes(subset = (!isTip), label=label), 
 					size=1.5, 
   					nudge_x=-.007, 
  					nudge_y=1) +
  		ggplot2::xlim(0, 0.7)

print_output("../Products/Figures/FullIQTree", p) 

## ---- trimtree --------
### 3. Trim tree down to 1 sample per species (204 sp tree)

tree <- full_join(iqtree, d, by="label") # full tree with metadata 240 taxa
## full_join(iqtree, d, by="label") %>% as_tibble %>% as.data.frame # check merge
tibb <- as_tibble(tree) %>% cutnoderows  # a tree tibble, tip rows only 240

ss <- tibb %>%    # 207 taxa
   arrange (desc(reference)) %>%  # move references to top
   distinct(across(gensp), .keep_all=TRUE) %>% as.data.frame  # drop duplicates
   
todrop <- tibb$label %w/o% ss$label # drop 33 taxa at multiple sites, outgroups
todropid <- sub("[_-].*$", "", todrop)
write.csv(data.frame("drops"=c(todrop, d$label[d$outgroup])), file="dropped_taxa_tips_fulliqtree.csv", row.names=F)

## ---- noduptrees --------
dtree207og <- treeio::drop.tip(tree, todrop)  # ingroup tree with only one sample per species - 204 tips. 
dtree204 <- treeio::drop.tip(tree, c(todrop, d$label[d$outgroup]))  # ingroup tree with only one sample per species - 204 tips. 

keepids <- c("44363", "44369", "44373", "43952", "43954", "cf._pansa.3", "sencken") # leave in all the new aphantophrynes, senkenbergiana
todrop <- grep(paste(keepids, collapse="|"), todrop, invert=T, value=T )  # drop 25 taxa

## ---- fulltrees --------
dtree215og <- treeio::drop.tip(tree, todrop)  # outgroup tree with all new samples, only one of others - 215 tips. 
dtree212 <- treeio::drop.tip(tree, c(todrop, d$label[d$outgroup]))  # ingroup tree as above - 212 tips. 

# Print trimmed iqtree 204 spp
p <- ggtree(dtree204, size=.1) + 
      geom_tiplab(aes(label=label_clean), size=1.3) + 
      geom_text2(aes(subset = (!isTip), label=label), 
               size=1.5, 
                  nudge_x=-.005, 
               nudge_y=1) +
      ggplot2::xlim(0, 0.38)
 
print_output("../Products/Figures/TrimmedIQTree", p) 

## ---- printfulltree --------
## Figure 1 - support with clades highlighted

dtree <- dtree212  # all new samples, no outgroups, one of other taxa

mrcas <- sapply( c("Oreophryne", "Auparoparo", "Aphantophryne"), function(x) get_MRCA( ggtree(dtree), x))
MRCA_labels <- data.frame(node=mrcas, MRCA_label=names(mrcas), letter=c("A", "B", "C"))

p_support <- ggtree(dtree, size=.2) %<+% MRCA_labels  +
   geom_tiplab(aes(label=gensp, 
#                    color=tdat$tipcol, 
                     fontface=fontface), 
                  size=1.5, 
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
print_output("../Products/Figures/FullTree_support_highlight", gradient_tree(p_support)) 

## ---- definitions --------
####  Phy:ogenetic definition trees

# Oreophryne sensu lato
tree_osl <- read.iqtree("../Data/IQTree/oreophryne_auparoparo_outgroup_trimmed/oreophryne_outgroup_trimmed.treefile")

# phylogenetic definition tree 
tree_osla <- treeio::read.iqtree("../Data/IQTree/oreophryne_auparoparo_aphantophryne_outgroup_trimmed/oreophryne_auparoparo_aphantophryne_outgroup_trimmed.treefile")

# phylogenetic def Oreoprhyne, Auparoparo, Cophixalus ref, Paedophryne ref
tree_oacp <- treeio::read.iqtree("../Data/IQTree/oreophryne_aphantophryne_cophix_paedo_outgroup_trimmed/oreophryne_aphantophryne_cophix_paedo_outgroup_trimmed.treefile")

# phylogenetic def Oreoprhyne, Auparoparo, all reference taxa
tree_oaref <- treeio::read.iqtree("../Data/IQTree/oreophryne_aphantophryne_references_trimmed/oreo_aupa_apha_references_trimmed.treefile")

## plot adjustment options
ls=1.25
nls=2.5
as=3
anx=.01
any=6
nx = -0.02
ny = 1.25

p1 <- printggtree_cladelabels(  # sensulato 
                        tree_osl, 
                        d, 
                        labsize=ls, 
                        sensulatotree=T, 
                        nx=nx,
                        ny=ny,
                        annotsize=as,
                        anx=anx,
                        any=any 
                        )
p2 <- printggtree_cladelabels( # auparoparo + oreophryne
                        tree_osla, 
                        d, 
                        labsize=ls, 
                        nx=nx,
                        ny=ny,
                        annotsize=as, 
                        nodelettersize=nls,
                        anx=anx,
                        any=any  
                        )
p3 <- printggtree_cladelabels( # aupa + oreo + cophixref,paedoref
                        tree_oacp, 
                        d, 
                        labsize=ls, 
                        nx=nx,
                        ny=ny,
                        annotsize=as, 
                        nodelettersize=nls,
                        anx=anx,
                        any=any 
                        )
p4 <- printggtree_cladelabels(  # aupa + oreo + all refs
                        tree_oaref, 
                        d, 
                        off=.3, 
                        xxlim=0.9, 
                        labsize=ls-0.15, # a little more crowded
                        nx=nx,
                        ny=ny,
                        annotsize=as, 
                        nodelettersize=nls,
                        anx=anx,
                        any=any,
                        reftree=T 
                        )

omtg <- cowplot::plot_grid(
                     p1, 
                     gradient_tree(p2),
                     gradient_tree(p3),
                     gradient_tree(p4),
                     labels = c('A', 'B', "C", "D"),
                     ncol=2
                     )

print_output("../Products/Figures/Oreo_multi_tree_gradient", omtg, height=7) 

## ---- onetest --------
## One reference taxon tests  

# drop Aphantophryne pansa from references
references1 <- grep("pansa", references, value=T, invert=T)
prefix <- "../Data/IQTree/onetaxa_tests_threeoutgroups/contrees/"
files <- paste(prefix, list.files(path=prefix), sep="")
trees1 <- lapply(files, read.tree)

## Two reference taxa (ingroup) tests 

#ref2 <- "A_variabilis"
ref2 <- "A_sp4"
prefix <- paste0("../Data/IQTree/twotaxa_tests_", ref2, "/contrees/")
files <- paste(prefix, list.files(path=prefix), sep="")
trees2 <- lapply(files, read.tree)
references2 <- c(references1, grep("Auparoparo_sp_4", trees2[[1]]$tip.label, value=T), grep("Oreophryne_cf__geislerorum_1", trees2[[1]]$tip.label, value=T))

## Two reference taxa, 2 cophixalus tests tests 

#ref2 <- "A_variabilis"
ref3 <- "twocoph"
prefix <- paste0("../Data/IQTree/twotaxa_", ref3, "_tests/contrees/")
files <- paste(prefix, list.files(path=prefix), sep="")
trees3 <- lapply(files, read.tree)
references3 <- c(references2, grep("Cophixalus_variabilis", trees3[[1]]$tip.label, value=T))


## Output for data analysis 

printanalysis = FALSE   # already plotted, if you want to run again, change to TRUE

if (printanalysis) {
   pdf(file="../Data/Processed_data/minimum_definition_tests/trees_threeoutgrouptaxa_contrees.pdf")  # one taxa
       p <- lapply(trees, print_trees, references)
      print(p)
   dev.off()
   pdf(file=paste0("../Data/Processed_data/minimum_definition_tests/trees_twotaxa_", ref2, "_threeoutgrouptaxa.pdf"))   # two Aupa, two Oreo
       p2 <- lapply(trees2, print_trees, references2)
       print(p2)
   dev.off()
   pdf(file=paste0("../Data/Processed_data/minimum_definition_tests/trees_twotaxa_", ref3, "_threeoutgrouptaxa.pdf"))   # two Aupa, Oreo, Cophixalus
       p2c2 <- lapply(trees3, print_trees, references3)
       print(p2c2)
   dev.off()
}

## Figure for paper

# data collected from figures above
p_dat <- data.frame(
            "Auparoparo"=c(13, 1, 3), 
            "Oreophryne"=c(23, 4, 0)
            )
p2_dat <- data.frame( 
            "Auparoparo"=c(13, 3, 0), 
            "Oreophryne"=c(24, 1, 1)
            )
p2c2_dat <- data.frame( 
            "Auparoparo"=c(13, 3, 0), 
            "Oreophryne"=c(25, 0, 1)
            )
row.names(p2_dat) <- row.names(p2c2_dat) <- row.names(p_dat) <- c(">70% BS", "<70% BS", "Not Sisters")

xxlim=4
hjust=-.25
vjust= .75

md <- cowplot::plot_grid(
      print_bubble_trees(
         trees1[[1]], 
         references1, 
         outgroups, # defined in loaddata
         label.size=2, 
         hjust=hjust, 
         vjust=vjust, 
         xxlim=xxlim, 
         noid=T
      ),
      print_bubble_trees(
         trees2[[2]], 
         references2, 
         outgroups, 
         label.size=2, 
         hjust=hjust, 
         vjust=vjust, 
         xxlim=xxlim, 
         noid=T
      ),
      print_bubble_trees(
         trees3[[23]], 
         references3, 
         outgroups, 
         label.size=2, 
         hjust=hjust, 
         vjust=vjust, 
         xxlim=xxlim, 
         noid=T
      ),
      tableGrob(
         p_dat, 
         theme=ttheme_default(base_size = 8) 
      ),
      tableGrob(
         p2_dat, 
         theme=ttheme_default(base_size = 8) 
      ),
      tableGrob(
         p2c2_dat, 
         theme=ttheme_default(base_size = 8) 
      ),    
      ncol=3,
      rel_heights= c(2,1),
      labels = c('A', 'B', "C", "", "", "")
   )

print_output("../Products/Figures/MinDef", md, width=7.5, height=3.5) 

## ---- genespeciestree --------
path <- "../Data/IQTree/genetree_speciestree_5loci/5partitions/"
outpath <- "../Products/Figures/"
dataset <- "all198"
label_size <- ifelse (dataset=="all198", 2, 2.5)

# read the tree & data 
tree <- read.iqtree(paste(path, dataset, "_sitel_concord.cf.tree",sep=""))  
tdat <- full_join(tree, d, by = "label")


dg = read.delim(paste(path, dataset, "_5loci_gene_concord.cf.stat",sep=""), header = T, comment.char='#')[]
dg <- dg[names(dg)!="Label"&names(dg)!="Length"] # drop the bootstrap and branch length info from the gene concordance dataset, keep the site concordance
ds = read.delim(paste(path, dataset, "_sitel_concord.cf.stat",sep=""), header = T, comment.char='#')

dgs <- merge(dg, ds)
names(dgs)[names(dgs)=="Label"] <- "bootstrap"
names(dgs)[names(dgs)=="Length"] <- "branchlength"
dgs$node <- dgs$ID + 1 
dgs$label2 <- paste(dgs$bootstrap, round(dgs$sCF), sep="/")

# merge tree + scf data
tdgs <- full_join(tdat, dgs, by = "node")  # combine the tree (tib) and data (d)

## explore low BS support
bs <- ggplot(dgs, aes(x = bootstrap, y = branchlength)) + 
    geom_point() + 
    ylim(0, 0.15) 

scf <- ggplot(dgs, aes(x = sCF, y = branchlength)) + 
    geom_point() + 
    ylim(0, 0.15) +
    geom_vline(xintercept = 33, linetype = "dashed") 

# plot subtrees
ta <- tidytree::drop.tip(tdgs, grep("Auparoparo|Aphantophryne", get_taxa_name(p), value=T, invert=T))
to <- tidytree::drop.tip(tdgs, grep("Oreophryne", get_taxa_name(p), value=T, invert=T))

o <- .02
pa <- print_scfggtree(ta, tip_size=2.5, label_size=2, offset=o, xxlim = c(-0.02, 0.35), nudge_x=0.025, alpha=.7, legend.position=c(.07,0.78))  # Auparoparo
po <- print_scfggtree(to, tip_size=2.5, label_size=2, offset=o, xxlim = c(-0.02, 0.4), nudge_x=0.025, alpha=.7, noLegend=T)  # Oreophryne

scfplot <- cowplot::plot_grid(
      bs,
      scf,
      pa,
      po,    
      ncol=2,
      rel_heights= c(1,2),
      labels = c('A', 'B', "C", "D")
   )
   
print_output("../Products/Figures/SiteCF", scfplot, width=7.5, height=7.5) 

## ---- phylomap --------
tree <- dtree204   # one sample per species tree
dat <- tree %>% as_tibble %>% as.data.frame %>% filter(!is.na(id))  # just the tip data rows
dat$gensp2 <- sub("cf. ", "cf.", dat$gensp)
dat$gensp2 <- sub("aff. ", "aff.", dat$gensp2)
dat$gensp2 <- sub("Oreophryne", "O.", dat$gensp2)
dat$gensp2 <- sub("Auparoparo", "A.", dat$gensp2)
dat$gensp2 <- sub("Aphantophryne", "A.", dat$gensp2)
dat$gensp2 <- gsub(" ", "_", dat$gensp2)

rownames(dat) <- dat$gensp2
tree <- rename_taxa(tree, dat, label, gensp2)  # labels are genus species

# subset Oreophryne and Auparoparo trees
atree <- tidytree::drop.tip(tree, 
            grep(
               "A._", 
               get_taxa_name(ggtree(tree)), 
               value=T, 
               invert=T)
            )
otree <- treeio::drop.tip(tree, 
            grep(
               "O._", 
               get_taxa_name(ggtree(tree)), 
               value=T, 
               invert=T)
            )

phylomapa <- phylo.to.map(
               as.phylo(atree), 
               dat[(dat$genus=="Auparoparo"|dat$genus=="Aphantophryne"), 
                  c("latitude", "longitude")],
               database="worldHires", 
               plot=FALSE           
               )
phylomapo <- phylo.to.map(
               as.phylo(otree), 
               dat[dat$genus=="Oreophryne", 
                  c("latitude", "longitude")],
                database="worldHires", 
                plot=FALSE                 
               )

# named color vector for phylomap
cola <- dat[dat$genus=="Auparoparo"|dat$genus=="Aphantophryne", "gcol"]
names(cola) <- row.names(dat[dat$genus=="Auparoparo"|dat$genus=="Aphantophryne",])
colo <- dat[(dat$genus=="Oreophryne"), "gcol"]
names(colo) <- row.names(dat[dat$genus=="Oreophryne",])

pdf(file="../Products/Figures/Phylomap.pdf", height=10, width=7.5, bg="transparent")
   par(bg="transparent", mfcol=c(2,1))
   plot(phylomapa,
      type= "phylogram",
      asp=1,
      mar=c(0.1,0.5,2.1,0.1), 
      tree.mar=c(0.1,0.1,.5,1.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=.75, 
      lty="dashed",
      lwd=1,
      map.bg="lightgreen",
      colors=cola, 
      split = c(.25,.75),
      pts=TRUE
   )
   plot(phylomapo,
      type= "phylogram",
      asp=1,
      mar=c(0,0.1,0,0.1), 
      tree.mar=c(0,0.1,0,.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=.75, 
      lty="dashed",
      lwd=1,
      map.bg="lightgreen",
      colors=colo, 
      split = c(.25,.75),
      pts=TRUE
   )
dev.off()

png(file="../Products/Figures/Phylomap.png", height=10, width=7.5, bg="transparent", units="in", res=300)
   par(bg="transparent", mfcol=c(2,1))
   plot(phylomapa,
      type= "phylogram",
      asp=1,
      mar=c(0,0.1,0,0.1), 
      tree.mar=c(0,0.1,0,.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=.75, 
      lty="dashed",
      lwd=1,
      map.bg="lightgreen",
      colors=cola, 
      split = c(.25,.75),
      pts=TRUE
   )
   plot(phylomapo,
      type= "phylogram",
      asp=1,
      mar=c(0,0.1,0,0.1), 
      tree.mar=c(0,0.1,0,.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=.75, 
      lty="dashed",
      lwd=1,
      map.bg="lightgreen",
      colors=colo, 
      split = c(.25,.75),
      pts=TRUE
   )
dev.off()

pdf(file="../Products/Figures/Phylomap_direct.pdf", height=10, width=7.5, bg="transparent")
   par(bg="transparent", mfcol=c(2,1))
   plot(phylomapa,
      type= "direct",
      asp=1,
      mar=c(0.1,0.5,2.1,0.1), 
      tree.mar=c(0.1,0.1,.5,1.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=1, 
      lty="dotted",
      map.bg="lightgreen",
      colors=cola, 
      split = c(.25,.75),
      pts=FALSE
   )
   plot(phylomapo,
      type= "direct",
      asp=1,
      mar=c(0,0.1,0,0.1), 
      tree.mar=c(0,0.1,0,.1), 
      xlim=c(118,157), 
      ylim=c(-13,11), 
      fsize=.75, 
      ftype="i", 
      psize=1, 
      lty="dotted",
      map.bg="lightgreen",
      colors=colo, 
      split = c(.25,.75),
      pts=FALSE
   )
dev.off()

## ---- cophylo --------
tree <- read.tree("../Data/IQTree/genetree_speciestree_5loci/15partitions/all198_15part_concat.contree")
tree_astral <- read.tree("../Data/Astral/astralresults/astral-BS10.tree")
tree_astral$edge.length[is.na(tree_astral$edge.length)] <- 0

# root tree
tree_astralr <- root(tree_astral, outgroup="UMMZ219489_Scaphiophryne_marmorata", resolve.root=T) # 

# association matrix - tips in each tree
association <- cbind(tree$tip.label, tree$tip.label)

tips <- tree$tip.label
x <- sub("Austrochaperina_","Austrochaperina ", tips)
x <- sub("^[^_]*_","", x)
genus <- sub("_[^.]*$","", x)

linecol <- gcol[genus]
linecol[is.na(linecol)] <- "#d3d3d3"
linecoltr <- phytools::make.transparent(linecol,0.5)

# remove IDs
tips_astral <- tree_astral$tip.label

tree_cophylo <- cophylo(tree, tree_astralr, association, pts=FALSE)

pdf("../Products/Figures/GeneTreeSpeciesTree_cophylo_withIDs.pdf", width=7.5, height=10)
  plot(tree_cophylo, fsize=c(.25,.25), link.type="curved", link.col=linecoltr, link.lwd=2, link.lty="solid", cex=.5, pts=FALSE)
dev.off()
png("../Products/Figures/GeneTreeSpeciesTree_cophylo_withIDs.png", width=7.5, height=10, units="in", res=300)
  plot(tree_cophylo, fsize=c(.25,.25), link.type="curved", link.col=linecoltr, link.lwd=2, link.lty="solid", cex=.5, pts=FALSE)
dev.off()

