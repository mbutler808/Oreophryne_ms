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

library(ape)
library(ggpmisc)
library(gridExtra)
library(maps) # for the map plotting
library(phytools)

## ---- functions --------
source("clean_functions.R") # contains no_()
source("plotting_functions.R") 

## ---- loaddata --------
iqtree <- read.iqtree("../Data/IQTree/model_search/senkenbergiana_merge.treefile")
# if you change the input treefile, run clean_metadata.R with the new treefile
d <- read.csv("../Data/Processed_data/metadata.csv")      # tree metadata spreadsheet
references <- read.csv("../Data/genus_references.csv")    # genus reference taxa 
references$id <- gsub("_(.*)","", references)
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  

tree <- read.beast("../Data/Processed_data/tree_with_data.nex")

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
   
todrop <- tibb$label %w/o% ss$label # drop 33 taxa at multiple sites, outgroups
todropid <- sub("[_-].*$", "", todrop)
write.csv(data.frame("drops"=c(todrop, d$label[d$outgroup])), file="dropped_taxa_tips_fulliqtree.csv", row.names=F)

dtree207og <- treeio::drop.tip(tree, todrop)  # ingroup tree with only one sample per species - 204 tips. 
dtree204 <- treeio::drop.tip(tree, c(todrop, d$label[d$outgroup]))  # ingroup tree with only one sample per species - 204 tips. 

keepids <- c("44363", "44369", "44373", "43952", "43954", "cf._pansa.3", "sencken") # leave in all the new aphantophrynes, senkenbergiana
todrop <- grep(paste(keepids, collapse="|"), todrop, invert=T, value=T )  # drop 25 taxa

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

## ---- fulltree --------
## Figure 1 - support with clades highlighted

dtree <- dtree212  # all new samples, no outgroups, one of other taxa

mrcas <- sapply( c("Oreophryne", "Auparoparo", "Aphantophryne"), function(x) get_MRCA( ggtree(dtree), x))
MRCA_labels <- data.frame(node=mrcas, MRCA_label=names(mrcas), letter=c("A", "B", "C"))

p_support <- ggtree(dtree, size=.2) %<+% MRCA_labels  +
   geom_tiplab(aes(label=gensp, 
#                    color=tdat$tipcol, 
                     fontface=fontface), 
                  size=1.75, 
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


## ---- phylomap --------

dat <- tree %>% as_tibble %>% as.data.frame %>% filter(!is.na(label_clean)) # tips only

library(sf)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(grid)


odat <- d %>% filter(genus=="Oreophryne")
adat <- d %>% filter(genus=="Auparoparo"|genus=="Aphantophryne")

world_map <- map_data("world")
southpacific <- subset(world_map, 
	world_map$region=="Papua New Guinea"|
	world_map$region=="Indonesia"|
	world_map$region=="Philippines" 
	)
	
#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())


#Create a map base plot with gpplot2
pmap <- ggplot() + 
		xlab("") + 
		ylab("") +
		geom_polygon(data=southpacific, 
			aes(x=long, y=lat, group=group), 
            colour="grey", 
            fill="light green",
            alpha=.7) +
		coord_map(xlim = c(120, 154), ylim = c(-12, 10)) 
	
#Add simple data points to map
map_data <- 
  pmap +
  geom_point(data=adat, 
  	aes(x=longitude, y=latitude), 
  	colour="red", 
    pch=19, 
    size=1.5) +
  geom_point(data=odat, 
    aes(x=longitude, y=latitude), 
    colour=gcol["Oreophryne"], 
    pch=19, 
    size=1.5) 
map_data 


osf <- st_as_sf(odat, coords=c("longitude", "latitude"))
asf <- st_as_sf(adat, coords=c("longitude", "latitude"))
vp <- viewport(x=0.8, y=0.8,
                   width=unit(3, "inches"), height=unit(2,"inches"))
grid.show.viewport(vp)

world <- ne_countries()

print(map_sf)
nrow(map_sf)
plot(map_sf$geometry)

#https://r-graph-gallery.com/168-load-a-shape-file-into-r.html

par(mar = c(0, 0, 0, 0))
plot(st_geometry(map_sf), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

library(ggplot2)
ggplot(map_sf) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()
  
scales::show_col(c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5", "#4A6C6F", "#FF5E5B"))

#### begin map code

map_sf <- ne_countries(country = c("Papua New Guinea", "Indonesia", "Philippines"), scale=50, returnclass="sf")

mp <- ggplot(map_sf) +
	geom_sf(color = "grey90", fill = "#f2f2f2", linewidth = 0.25) +
	coord_sf(xlim = c(120, 154), ylim = c(-12, 10)) +
#	  theme_void() + 
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA)
#        panel.background = element_rect(fill = "skyblue", colour = NA)
        )

mp +
  geom_point(data=adat, 
  	aes(x=longitude, y=latitude), 
  	colour="grey50", 
  	fill="red", 
    pch=21, 
    size=2, 
    alpha=.7) +
  geom_point(data=odat, 
    aes(x=longitude, y=latitude), 
    colour="grey50", 
    fill=gcol["Oreophryne"], 
    pch=24, 
    size=2, 
    alpha=.7) +
  geom_hline(yintercept = -8.75, lty = 2, colour = "red") +
  geom_hline(yintercept = -12, lty = 2, colour = "red") +
  geom_vline(xintercept = 147, lty = 2, colour = "red") +
  geom_vline(xintercept = 155, lty = 2, colour = "red") 

#xlim = c(120, 154), ylim = c(-12, 10)
        
mpdat <- mp +
  geom_point(data=adat, 
  	aes(x=longitude, y=latitude), 
  	colour="red", 
    pch="+", 
    size=8, 
    alpha=.75) +
  geom_point(data=odat, 
    aes(x=longitude, y=latitude), 
    colour="grey50", 
    fill=gcol["Oreophryne"], 
    pch=24, 
    size=2, 
    alpha=.75) +
  geom_rect(aes(xmin = 149, xmax = 154.5, ymin = -12, ymax = -8.75), color = "grey30", fill = NA) +
  theme_void() +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA)
        )

inset <-  
  ggplot(map_sf) + 
	geom_sf(color = "grey90", fill = "#f2f2f2", linewidth = 0.25) +
	coord_sf(xlim = c(149, 154.5), ylim = c(-12, -9)) +
  geom_point(data=adat, 
  	aes(x=longitude, y=latitude), 
  	colour="red", 
    pch="+", 
    size=8, 
    alpha=.75) +
  geom_point(data=odat, 
    aes(x=longitude, y=latitude), 
    colour="grey50", 
    fill=gcol["Oreophryne"], 
    pch=24, 
    size=3, 
    alpha=.75) +
  geom_rect(aes(xmin = 149, xmax = 154.5, ymin = -12, ymax = -8.75), color = "grey30", fill = NA) +
  labs(x = NULL, y = NULL) +  
  xlim(149, 154.5) +
  ylim(-12, -8.75) +
  theme_void() +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA)) +
  guides(size = "none") 


mpdat
print(inset, vp = viewport(0.75, 0.75, width = 0.5, height = 0.5))  
  
mpdat  +
  ggsn::north(location = "topleft", scale = 0.8, symbol = 12,
               x.min = 145, x.max = 155, y.min = 8, y.max = 10) +
  ggsn::scalebar(location = "bottomleft", dist = 100,
           dist_unit = "km", transform = TRUE, 
           x.min=150.5, x.max=152, y.min=-38, y.max=-30,
           st.bottom = FALSE, height = 0.025,
           st.dist = 0.05, st.size = 3)



	
pmap <- ggplot() + 
		xlab("") + 
		ylab("") +
		geom_polygon(data=southpacific, 
			aes(x=long, y=lat, group=group), 
            colour="grey", 
            fill="light green",
            alpha=.7) +
		coord_map(xlim = c(120, 154), ylim = c(-12, 10)) 

adat %>% ggplot() +
	geom_sf(data=map_sf, fill = "#69b3a2", color = "white") +
	theme_void() +
	coord_map(xlim = c(120, 154), ylim = c(-12, 10)) 

map_data <- 
  pmap +
  geom_point(data=adat, 
  	aes(x=longitude, y=latitude), 
  	colour="red", 
    pch=19, 
    size=1.5) +
  geom_point(data=odat, 
    aes(x=longitude, y=latitude), 
    colour=gcol["Oreophryne"], 
    pch=19, 
    size=1.5) 
map_data 


  
print_output(filepath="../Products/Figures/map", map_data+cleanup)
pdf(file="../Products/Figures/map.pdf", height=6, width=10)
  print(map_data + cleanup)
dev.off()
png(file="../Products/Figures/map.png", height=6, width=10)
  print(map_data + cleanup)
  print(map_data + cleanup, vp=vp)
dev.off()

tree <- dtree204   # one sample per species tree
tree <- rename_taxa(tree, label, gensp)  # labels are genus species
dat <- tree %>% as_tibble %>% as.data.frame %>% filter(!is.na(label_clean))
rownames(dat) <- dat$gensp


# subset Oreophryne and Auparoparo trees
tips <- get_taxa_name(tree)
otree <- treeio::drop.tip(tree, grep("Oreophryne", tips, value=T, invert=T))
atree <- treeio::drop.tip(tree, grep("Auparoparo|Aphantophryne", tips, value=T, invert=T))

phylomapo <- phylo.to.map(
               otree, 
               dat[dat$genus=="Oreophryne", c("latitude", "longitude")]
               )
phylomapa <- phylo.to.map(
               atree, 
               dat[(dat$genus=="Auparoparo"|dat$genus=="Aphantophryne"), c("latitude", "longitude")]
               )

mapo <- plot(phylomapo,
   type="phylogram",
   asp=1,
   mar=c(0.1,0.5,2.1,0.1), 
   tree.mar=c(0.1,0.1,0.5,1.1), 
   xlim=c(120,155), 
   ylim=c(-15,12), 
   fsize=.65, 
   ftype="i", 
   psize=.7, 
   colors=gcol["Oreophryne"], 
   split = c(.3,.7)
   )
mapa <- plot(phylomapa,
   type="phylogram",
   asp=1,
   mar=c(0.1,0.5,2.1,0.1), 
   tree.mar=c(0.1,0.1,.5,1.1), 
   xlim=c(118,155), 
   ylim=c(-15,3), 
   fsize=.65, 
   ftype="i", 
   psize=.7, 
   colors=gcol["Auparoparop"], 
   split = c(.3,.7)
   )

pdf(file="../Products/Figures/Phylomap.pdf", height=6, width=10)


