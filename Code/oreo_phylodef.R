## ---- definitions --------
####  Phy:ogenetic definition trees

tree_osl <- read.iqtree("../Data/IQTree/oreophryne_auparoparo_outgroup_trimmed/oreophryne_outgroup_trimmed.treefile")

tree_osla <- treeio::read.iqtree("../Data/IQTree/oreophryne_auparoparo_aphantophryne_outgroup_trimmed/oreophryne_auparoparo_aphantophryne_outgroup_trimmed.treefile")

tree_oacp <- treeio::read.iqtree("../Data/IQTree/oreophryne_aphantophryne_cophix_paedo_outgroup_trimmed/oreophryne_aphantophryne_cophix_paedo_outgroup_trimmed.treefile")

tree_oaref <- treeio::read.iqtree("../Data/IQTree/oreophryne_aphantophryne_references_trimmed/oreo_aupa_apha_references_trimmed.treefile")

## plot adjustment options
ls=1.5
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
                        labsize=ls, 
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
p_dat <- data.frame("Auparoparo"=c(13, 1, 3), "Oreophryne"=c(23, 4, 0), row.names=c(">70% BS", "<70% BS", "Not Sisters"))
p2_dat <- data.frame( "Auparoparo"=c(13, 3, 0), "Oreophryne"=c(24, 1, 1))
p2c2_dat <- data.frame( "Auparoparo"=c(13, 3, 0), "Oreophryne"=c(25, 0, 1))
row.names(p2_dat) <- row.names(p2c2_dat) <- row.names(p_dat)

xxlim=4
hjust=-.25
vjust= .75

md <- cowplot::plot_grid(
      print_bubble_trees(
         trees1[[1]], 
         references1, 
         label.size=2, 
         hjust=hjust, 
         vjust=vjust, 
         xxlim=xxlim, 
         noid=T
      ),
      print_bubble_trees(
         trees2[[2]], 
         references2, 
         label.size=2, 
         hjust=hjust, 
         vjust=vjust, 
         xxlim=xxlim, 
         noid=T
      ),
      print_bubble_trees(
         trees3[[23]], 
         references3, 
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
