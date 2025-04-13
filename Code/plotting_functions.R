## Plotting and tree manipulation functions
## ---- cutnoderows --------
cutnoderows <- function( tibb ){   ### keeps only the tip rows of the dataframe
   tibb <- tibb[grep('^[0-9]{2,3}$', tibb$label, invert=T),]  # remove rows with support values as labels (internal nodes)
    tibb <- tibb[tibb$label != "",]  # removes rows with no label (root node)
    tibb <- tibb[!is.na(tibb$label),]  # removes rows with NA no support and not tip
    return(tibb)
} 

## ---- getmrca --------
get_MRCA <- function( ggtree, genus ) {
   MRCA(ggtree, grep(genus, get_taxa_name(ggtree), value=T))
}

## ---- getnoderange --------
get_node_range <- function( treedata, x="Auparoparo" ){
   lo <- get_MRCA(ggtree(treedata), x)
   range <- sort(offspring(treedata, lo))
   range <- c(lo, range[!isTip(treedata, range)])
   return(range)  # all node numbers in subtree
}

## ---- gradienttree --------
gradient_tree <- function (ggtree) {  # print tree with gradient highights
   grad_ggtree <- 
      ggtree + 
      geom_hilight( 
         node=get_MRCA(ggtree, "Oreophryne"),
         fill=gcol["Oreophryne"],
         type = "gradient", 
         gradient.direction = 'rt',
         alpha=0.25) +
      geom_hilight( 
         node=get_MRCA(ggtree, "Auparoparo"), 
         fill=gcol["Auparoparo"],
         type = "gradient", 
         gradient.direction = 'rt',
         alpha=0.25) 
   return(grad_ggtree)
}

## ---- rotatenodes --------
## When we want to rotate clades around nodes
flip_ggtree <- function( ggtree, mrca="Oreophryne|Auparoparo") {   
   p <- ggtree 
   c1 <- child(as.treedata(p), get_MRCA(p, mrca))[1]
   c2 <- child(as.treedata(p), get_MRCA(p, mrca))[2]
   return( flip(p, c1, c2) )
}  

## ---- supporttree --------
# print tree with support values
print_supggtree <- function(treedata, metadata, xxlim=0.75, printlo=F, labsize=2.75) {
   treed <- full_join(treedata, metadata, by="label") # join tree and data

# ggtree with tiplabels and support values
   tree <- ggtree(treed, size=.25) +   
      geom_tiplab(aes(label=gensp, fontface=fontface), 
                  size=labsize, 
                  offset=.005 
                   ) + 
      ggplot2::xlim(0, xxlim) +
      geom_point2(aes(subset=(!isTip & as.numeric(label)>=95 )), size=labsize*.25, color="black") +
      geom_point2(aes(subset=(!isTip & as.numeric(label)>=70 & as.numeric(label)<95 )), color="grey60", size=labsize*.25) +
      if (printlo) geom_text2(aes(subset = (!isTip & as.numeric(label)<70), label=label), 
                     size=labsize*.75, 
                     color="black",
                     nudge_x=-.01, 
                     nudge_y=.5)                      
   return(tree)
}

## ---- bstree --------
print_bsggtree <- function(treedata, label_size=2, xxlim = c(0, 0.47)){
   p <- ggtree(treedata, size=.25) +          
         theme(legend.position="right") +
         geom_tiplab(size=label_size) + 
          geom_text2(  # bs values
            aes(label=label, subset=!isTip), 
            color="black", 
            size=label_size, 
            nudge_x=-.01, 
            nudge_y=.7) +  
         ggplot2::xlim(xxlim)
   return(p)
}

## ---- nodenumbertree --------
print_nodenumggtree <- function(treedata, label_size=2, xxlim = c(0, 0.47)){
   p <- ggtree(treedata, size=.25) +          
         theme(legend.position="right") +
         geom_tiplab(size=label_size) +
         geom_text2(   # node numbers
            aes(label=node, subset=!isTip), 
            color="black", 
            size=label_size, 
            nudge_x=-.01, 
            nudge_y=.7) +   
         ggplot2::xlim(xxlim)
   return(p)
}

## ---- treecladelabels --------
# print support tree with clade labels and node letter labels
printggtree_cladelabels <- function( treedata, metadata, off=.22, nx = -0.0125, ny = 1.25, any=6, anx=0.008, xxlim=0.75, printlo=F, labsize=2.75, nodelettersize=4, annotsize=2, sensulatotree=F, reftree=F) {
   tree <- print_supggtree( treedata, metadata, xxlim=xxlim, printlo=printlo, labsize=labsize)
   if (!reftree)  tree <-  flip_ggtree ( tree, "Oreophryne|Auparoparo")                         
   # If sensulato tree (no node label)
   if (sensulatotree) {    # sensulato tree
      labeled_tree <- tree +  
         geom_cladelab(node=get_MRCA(tree, "Oreophryne|Auparoparo"), 
            label="Oreophryne sensu lato", 
            fontsize=annotsize,
            align=TRUE,  
            offset = off, 
            angle=-90, 
            nudge_y=any*2,
            nudge_x=anx) 
   } else {   # all phylogenetic definition trees
            # ggtree with tiplabels and support values + clade annotations
         labeled_tree <- tree + 
         geom_cladelab(node=get_MRCA(tree, "Oreophryne"), 
            label="Oreophryne",
            fontsize=annotsize, 
            align=TRUE,  
            fontface="italic",
            offset = off, 
            angle=-90, 
            nudge_y=any,
            nudge_x=anx) +
         geom_cladelab(node=get_MRCA(tree, "Auparoparo"), 
            label="Auparoparo", 
            fontsize=annotsize,
            align=TRUE,  
            fontface="italic",
            offset = off, 
            angle=-90, 
            nudge_y=any,
            nudge_x=anx) 
   }      
   # ggtree with tiplabels and support values + clade labels + node labels
   if (!sensulatotree & (nodelettersize>0))    labeled_tree <- labeled_tree + 
         geom_text2(aes(subset=(node == get_MRCA(tree, "Oreophryne"))), 
            label="Or",
            size= nodelettersize, 
            color="black",
            nudge_x=nx, 
            nudge_y=ny,
            fontface='bold') +
         geom_text2(aes(subset =(node == get_MRCA(tree, "Auparoparo"))), 
            label="Au",
            size= nodelettersize, 
            color="black",
            nudge_x=nx, 
            nudge_y=ny,
            fontface='bold') 
   return(labeled_tree) 
}

## ---- bubbletree --------
# one taxa test trees with clade reference taxa in color bubbles
print_bubble_trees <- function(tt=tree, references, outgroups, label.size=3, hjust=-1, vjust=0, xxlim=3, noid=F){
   # create vector of node support values
   bs <- c(rep(NA, times=length(tt$tip.label)), tt$node.label)

   # identify which is the test taxon (not in references), make bold
   label <- get_taxa_name(ggtree(tt))
   genus <- gsub("^(.*?)\\_(.*?)\\_(.*)","\\2",  label)
   fontface <- rep("italic", times=length(label))
   isRef <- label %in% references
   isOutgroup <- label %in% outgroups
   fontface[!isRef & !isOutgroup] <- "bold.italic"
   label2 <- gsub("_", " ", label)
 
   if (noid) label2 <- sub("^[^ ]* ", "", label2)
   tt2 <- full_join(tt, data.frame(label, label2, genus, isRef, fontface), by="label")
    
   p <- ggtree(tt2) + 
         geom_tiplab(aes(subset=isRef, 
               label=label2, 
               fontface=fontface,
               fill=genus
               ), 
            geom="label",
            alpha=0.3,
            size=label.size) + 
         geom_tiplab(aes(subset=!isRef, 
               label=label2, 
               fontface=fontface
               ), 
            size=label.size) + 
         xlim(0, xxlim) + 
         geom_text2(aes(label=bs), 
            hjust=hjust, 
            vjust=vjust, 
            size=label.size) +
         scale_fill_manual(values=gcol[genus]) +
         theme(legend.position="none")
   return(p)
}

## ---- pdfpng --------
# print pdf and png versions of output
print_output <- function(filepath, p, height=10, width=7, units="in", res=300){
   pdf(file=paste0(filepath, ".pdf"), height=height, width=width)
      print(p)
   dev.off()
   png(file=paste0(filepath, ".png"), height=height, width=width, units=units, res=res)
      print(p)
   dev.off()
}

## ---- scftree --------
print_scfggtree <- function(treedata, tip_size=4, label_size=3, offset=0, xxlim = c(-0.05, 0.3), nudge_x=0.025, alpha=1, legend.position=c(0.1,0.8), noLegend=F){
     p <- ggtree(treedata, aes(color=bootstrap)) +          # plot tree
      theme(legend.position=legend.position,
         legend.background = element_rect(color = NA, fill = NA)) +
      geom_tiplab(aes(label=gensp), 
            size=tip_size,
            fontface="italic",
            offset=offset, 
            color="black") + # tip labels       
      geom_text2(aes(
               label=label2, 
               subset=!isTip & (bootstrap>95)
               ), 
               color="black", 
               nudge_x=nudge_x,
               size=label_size) + # node labels 
      geom_label2(aes(
               label=label2, 
               subset= !isTip & (bootstrap<95 & sCF<40),
               fill=bootstrap,
               ),
            alpha=alpha,
            color="black",
            size=label_size
            ) +  # highlight low bootstrap support 
      scale_color_continuous(guide="none", low="yellow", high="purple") +
      ggplot2::xlim(xxlim)

  if (noLegend) { 
   p <- p + scale_fill_continuous(guide="none", low="yellow", high="purple", limits=c(70,100)) 
   } else { 
   p <- p + 
      scale_fill_continuous(name="BS\nSupport", low="yellow", high="purple", limits=c(70,100))  
   }
 return(p)
}

