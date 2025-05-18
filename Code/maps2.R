## ---- packages --------
library(dplyr)
library(ggplot2)
library(sf)
library(grid)
library(rnaturalearth)
library(ggspatial)
library(extrafont)

## ---- loaddata --------
d <- read.csv("../Data/Processed_data/metadata.csv")      # tree metadata spreadsheet

gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  

## ---- shapedata ----

# shapedat <- 
# read_sf("../Data/Raw_data/redlist_species_data/data_0.shp")  #file >100MB
# sd <- shapedat %>% as_tibble %>% as.data.frame
# write.csv(sd[1:8], "IUCNdat.csv")

# ii <- grep("Oreophryne", sd$SCI_NAME)
# sf_oreo <- shapedat[ii,] # all Oreophryne sensu lato shape data
# write_rds(sf_oreo, "../Data/Processed_data/oreophryne_sensu_lato.rds")
sf_oreo <- readRDS("../Data/Processed_data/oreophryne_sensu_lato.rds") # contains sf_oreo
types <- read.csv("../Data/Raw_data/Oreophryne_Type_Localities_v20250513.csv", skip=1)
names(types) <- c("sp", "latitude", "longitude")
## ---- metadata --------

dat <- d %>% mutate(genus = replace(genus, 
							genus=="Aphantophryne", "Auparoparo")) %>%
			 filter (genus=="Oreophryne"|
					 genus=="Auparoparo")

datsf <- st_as_sf(dat, coords=c("longitude", "latitude"))

#osf <- st_as_sf(odat, coords=c("longitude", "latitude"))
#asf <- st_as_sf(adat, coords=c("longitude", "latitude"))

vp <- viewport(	x=0.75, 
				y=0.75,
                width=unit(3, "inches"),
                height=unit(2,"inches"))
grid.show.viewport(vp)
scales::show_col(c( "#9DBF9E", 
					"#A84268", 
					"#FCB97D", 
					"#C0BCB5", 
					"#4A6C6F", 
					"#FF5E5B"))

## ---- map --------
map_sf <- ne_countries(country = 
						c(
						  "Papua New Guinea", 
						  "Indonesia", 
						  "Philippines"
						), 
						scale=50, 
						returnclass="sf")

watercolor <- "lightskyblue1"

basemap <- ggplot() +
	geom_sf( data=map_sf,  # base map
			color = "grey80", 
			fill = "#f2f2f2", 
			linewidth = 0.25
			) + 
	geom_sf(data=sf_oreo,  # IUCN redlist data Oreophryne sensu lato
		color = "grey20", 
		fill = "#4A6C6F", 
		linewidth = .1, 
		alpha=.4) +
	coord_sf(xlim = c(113, 155), ylim = c(-15, 15)) +
  	annotate( "rect", 
  			  xmin = 149, 
  			  xmax = 154.5, 
  			  ymin = -12, 
  			  ymax = -8.75, 
  			  color = "white", 
  			  fill = NA
  			) 

## ---- pointmap --------			
pointmap <- basemap + 
	geom_point(data=dat, 
  		aes(x=longitude, 
  			y=latitude, 
  			color=genus, 
  			fill=genus, 
  			shape=genus, 
  			size=genus), 
	    	alpha=.75) +
	scale_color_manual(values = c("Auparoparo" = "red", "Oreophryne" = "white")) +
	scale_fill_manual(values = c("Auparoparo" = NA,"Oreophryne" = "#0059FF")) +
	scale_shape_manual(values = c("Auparoparo" = 3, "Oreophryne" = 24)) +
	scale_size_manual(values = c("Auparoparo" = 3, "Oreophryne" = 2.5)) +
	labs(color = "", fill="", shape="", size="") +
  	theme( axis.title = element_blank(),
  		   panel.grid.major = element_blank(),
           panel.background = element_rect(
         							fill = watercolor, 
         							colour = NA), 
           legend.background = element_rect(fill = NA),
           legend.position = "inside",
           legend.position.inside = c(0.165, 0.04),
           legend.direction = "horizontal"
       ) 

## ---- annotatedmap --------
annotated <- pointmap +  
	geom_sf_text(data=map_sf,
		aes(label=name_en),
		size = 4,
		fontface = "bold",
		family = "serif",
		color = "grey50",
		nudge_x = c(-4.75,8,13),
		nudge_y = c(1,-.75, -5.75)) +
	#Add scale bar to bottom left from ggspatial
  	annotation_scale(location = "bl", 
  		height = unit(.25, "cm"), 
   		pad_x = unit(0.3, "in"), 
  		pad_y = unit(0.4, "in")) +
  	#Add north arrow to bottom left from ggspatial
  	annotation_north_arrow(height = unit(1, "cm"), 
  		width = unit(1, "cm"),
  		which_north = "true", 
  		location = "bl", 
  		pad_x = unit(0.3, "in"), 
  		pad_y = unit(0.6, "in") 
	)


## ---- insetedges --------
# basemap +
  # geom_hline(yintercept = -8.75, lty = 2, colour = "red") +
  # geom_hline(yintercept = -12, lty = 2, colour = "red") +
  # geom_vline(xintercept = 147, lty = 2, colour = "red") +
  # geom_vline(xintercept = 155, lty = 2, colour = "red") 

## ---- insetmap --------
xlim = c(149, 154.5)
ylim = c(-12, -8.75) 

ybreaks <- c(-12, -11, -10, -9)
xbreaks <- pretty(seq(xlim[1], xlim[2], length.out=3), n=3)

inset <-  
  ggplot() +
	geom_sf( data=map_sf, 
			color = "grey80", 
			fill = "#f2f2f2", 
			linewidth = 0.25
			) +
	geom_sf(data=sf_oreo, 
		color = "grey20", 
		fill = "#4A6C6F", 
		linewidth = .1, 
		alpha=.4) +
	coord_sf( xlim = xlim, 
			  ylim = ylim, 
			  expand=FALSE
			 ) + #expand=F prevents ggplot from expanding slightly beyond the limits
  	labs(x = NULL, y = NULL) + 
  	scale_y_continuous(breaks = ybreaks) +
	filter(dat,  latitude > ylim[1] & latitude < ylim[2] &
  				 longitude > xlim[1] & longitude < xlim[2] 
  			 ) %>%
	geom_point( 
  		mapping=aes(x=longitude, 
  			y=latitude, 
  			color=genus, 
  			fill=genus, 
  			shape=genus, 
  			size=genus), 
	    	alpha=.75) +
	scale_color_manual(values = c("Auparoparo" = "red", "Oreophryne" = "white")) +
	scale_fill_manual(values = c("Auparoparo" = NA,"Oreophryne" = "#0059FF")) +
	scale_shape_manual(values = c("Auparoparo" = 3, "Oreophryne" = 24)) +
	scale_size_manual(values = c("Auparoparo" = 3, "Oreophryne" = 2.5)) +
	labs(color = "", fill="", shape="", size="") +
    annotate("rect", 
    		 xmin = 149, 
    		 xmax = 154.5,
    		 ymin = -12,
    		 ymax = -8.75,
    		 color = "white",
    		 fill=NA,
    		 linewidth=1.5
    		) +
	theme( axis.title = element_blank(),
  		   panel.grid.major = element_blank(),
           panel.background = element_rect(
         							fill = watercolor, 
         							colour = NA), 
           plot.background = element_rect(
           							fill = watercolor,
           							color = NA),
           legend.position = ""
         ) 


## ---- plotmap --------
# print(annotated)
# print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  

## ---- writemap --------

filepath="../Products/Figures/map"
height=6
width=9
pdf(file=paste0(filepath, ".pdf"), height=height, width=width)
      print(annotated)
      print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  
dev.off()
png(file=paste0(filepath, ".png"), height=height, width=width, units="in", res=300)
      print(annotated)
      print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  
dev.off()

## ---- typemap --------			
typemap <- ggplot() +
	geom_sf( data=map_sf,  # base map
			color = "grey80", 
			fill = "#f2f2f2", 
			linewidth = 0.25
			) + 
	geom_sf(data=sf_oreo,  # IUCN redlist data Oreophryne sensu lato
		color = "grey20", 
		fill = "#4A6C6F", 
		linewidth = .1, 
		alpha=.4) +
	coord_sf(xlim = c(113, 155), ylim = c(-15, 15)) + 
	geom_point(data=types, 
  		aes(x=longitude, 
  			y=latitude), 
  		size=3,
  		shape=4,
  		color="blue") +
  	theme( axis.title = element_blank(),
  		   panel.grid.major = element_blank(),
           panel.background = element_rect(
         							fill = watercolor, 
         							colour = NA), 
           legend.background = element_rect(fill = NA)
       ) +  
	geom_sf_text(data=map_sf,
		aes(label=name_en),
		size = 4,
		fontface = "bold",
		family = "serif",
		color = "grey50",
		nudge_x = c(-4.75,8,13),
		nudge_y = c(1,-.75, -5.75)) +
	#Add scale bar to bottom left from ggspatial
  	annotation_scale(location = "bl", 
  		height = unit(.25, "cm"), 
   		pad_x = unit(0.3, "in"), 
  		pad_y = unit(0.4, "in")) +
  	#Add north arrow to bottom left from ggspatial
  	annotation_north_arrow(height = unit(1, "cm"), 
  		width = unit(1, "cm"),
  		which_north = "true", 
  		location = "bl", 
  		pad_x = unit(0.3, "in"), 
  		pad_y = unit(0.6, "in") 
	)

## ---- writetypemap --------

filepath="../Products/Figures/map_Oreophyrne_sensu_lato_types"
height=6
width=9
pdf(file=paste0(filepath, ".pdf"), height=height, width=width)
      print(typemap)
dev.off()
png(file=paste0(filepath, ".png"), height=height, width=width, units="in", res=300)
      print(typemap)
dev.off()
