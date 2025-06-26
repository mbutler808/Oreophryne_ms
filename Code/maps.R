## ---- packages --------
library(dplyr)
library(ggplot2)
library(sf)
library(grid)
library(rnaturalearth)
library(ggspatial)

## ---- shapedata ----

# code shown but not run
# shapedat <- 
# read_sf("../Data/Raw_data/redlist_species_data/data_0.shp")  #file >100MB
# sd <- shapedat %>% as_tibble %>% as.data.frame
# write.csv(sd[1:8], "IUCNdat.csv")

# ii <- grep("Oreophryne", sd$SCI_NAME)
# oreo_sf <- shapedat[ii,] # all Oreophryne sensu lato shape data
# write_rds(oreo_sf, "../Data/Processed_data/oreophryne_sensu_lato.rds")

oreo_sf <- readRDS("../Data/Processed_data/oreophryne_sensu_lato.rds") # contains sf_oreo

## ---- metadata --------

d <- read.csv("../Data/Raw_data/metadata.csv")      # tree metadata spreadsheet
metadata <- d %>% mutate(genus = replace(genus, 
							genus=="Aphantophryne", "Auparoparo")) %>%
			 filter (genus=="Oreophryne"|
					 genus=="Auparoparo")

metadata_sf <- st_as_sf(metadata, coords=c("longitude", "latitude"))

## ---- pointspec --------			

# specifications for point plots used later
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gfill <- gc$col
names(gfill) <- gc$gen  # point-fill colors by genus (value, key)
gfill <- gfill[c("Auparoparo", "Oreophryne")] # keep only these two
gfill["Auparoparo"] = "#FF0000"  # change value to red

scales::show_col(c( "#FF0000", # Auparoparo
					"#0059FF", # Oreophryne
					"#9DBF9E", 
					"#A84268", 
					"#FCB97D", 
					"#C0BCB5", 
					"#4A6C6F", 
					"#FF5E5B"
					))

gshape <- c("Auparoparo" = 21,  # point shapes, filled circle 
			"Oreophryne" = 24)  # filled triange
gsize <-  c("Auparoparo" = 1.75, # point sizes
			"Oreophryne" = 2.5)

# show points
par(bg= "grey80")
plot(x=0:(length(gshape)+1), 
	 y=rep(1, length(gshape)+2), 
	 type = "n",  
	 axes = F, 
	 xlab = "", 
	 ylab = "")

points( x=1:length(gshape), 
		y=rep(1, length(gshape)), 
		pch = gshape, 
		col ="white", 
		bg = gfill, 
		cex = gsize)  

## ---- maplimitsinset --------

mapxlim = c(113, 155) # map limits, x and y
mapylim= c(-15,15) 
insetxlim = c(149, 154.5) # inset area
insetylim = c(-12, -8.75) 

vp <- viewport( x=0.75, 
			    y=0.75,
                width=unit(3, "inches"),
                height=unit(2,"inches"))
grid.show.viewport(vp)

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

## make_basemap() is a function to plot the basemap (countries and map limits) 
## and the distribution data from IUCN redlist as grey polygons

make_basemap <- function( .map = map_sf, # basemap sf data
						  redlist_sf = oreo_sf, # iucn redlist range sf
						  redlist_alpha=.2, 	# grey transparency
						  xlim = c(113, 155), # map limits, x and y
						  ylim = c(-15,15) 
						  ) {
	ggplot() +
		geom_sf( data=.map,  # base map
			color = "grey80", 
			fill = "#f2f2f2", 
			linewidth = 0.25
			) + 
		geom_sf(data=redlist_sf,  # IUCN redlist data Oreophryne sensu lato
			color = "grey20", 
			fill = "#4A6C6F", 
			linewidth = .1, 
			alpha=redlist_alpha) +
		coord_sf(xlim = xlim, ylim = ylim, expand=F) #expand=F prevents ggplot from expanding slightly beyond the limits
		  			
}

map_sf %>% make_basemap()

		
## ---- pointmap --------			

## map_points() adds points for specimens on the basemap 

map_points <- function( .map, 
						dat = metadata, # expects columns: longitude, latitude, genus
						legend_pos = "inside",
						legend_coord = c(0.165, 0.04), 
						legend_dir = "horizontal",
						pgfill = gfill, # point fill, shape, size
						pgshape = gshape, 
						pgsize = gsize
						) {
	.map + 
		geom_point(data=dat, 
  			aes(x=longitude, 
  				y=latitude, 
  				fill=genus, 
  				shape=genus, 
  				size=genus), 
  			color="white", 
	    		alpha=.7) +
		scale_fill_manual(values = pgfill ) +
		scale_shape_manual(values = pgshape ) +
		scale_size_manual(values = pgsize ) +
		labs(color = "", fill="", shape="", size="") +
	  	theme( axis.title = element_blank(),
	  		   panel.grid.major = element_blank(),
	           panel.background = element_rect(
	         							fill = watercolor, 
	         							colour = NA), 
	           legend.background = element_rect(fill = NA),
	           legend.position = legend_pos,
	           legend.position.inside = legend_coord,
	           legend.direction = legend_dir
	     )        
}

map_sf %>% 
	make_basemap() %>%
	map_points()

## ---- annotatedmap --------

## annotate_map() adds the scale bar, north arrow, and zoom rectangle

annotate_map <- function( .map, 
						  mapsf = map_sf # contains names of countries
						) {
	.map +  
		geom_sf_text(data=mapsf,
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
}

## zoom_box() adds the rectangle around the zoom-in area

zoom_box <- function( .map, 
					  box_xlim=c(149,154.5), # box around zoomed in area
					  box_ylim=c(-12,-8.75), 
					  ... 
					) {
		# Add white rectangle around zoom-in area
	  	.map +  
	  	annotate( geom="rect", 
			xmin = box_xlim[1], 
	  		xmax = box_xlim[2], 
	  		ymin = box_ylim[1], 
			ymax = box_ylim[2], 
			color = "white", 
			fill = NA, 
			...
		) 
}

## ---- savebasemap --------
basemap <- map_sf %>% 
	make_basemap() %>%
	map_points() %>%
	annotate_map() %>%
	zoom_box()

print(basemap)

## ---- insetmap --------
insetxlim = c(149, 154.5)
insetylim = c(-12, -8.75) 
ybreaks <- -12:-9

inset_dat <- filter(metadata,  
				latitude > insetylim[1] & 
				latitude < insetylim[2] &
				longitude > insetxlim[1] & 
				longitude < insetxlim[2] 
  			 ) 

insetmap <- map_sf %>%
	make_basemap( xlim=insetxlim, ylim=insetylim) %>% 
	map_points( dat=inset_dat, legend_pos="") %>%
	zoom_box(linewidth=1.5) +	
	scale_y_continuous(breaks = ybreaks) +  	# pretty y-breaks for inset	
	theme( plot.background = element_rect( # set margin area to watercolor
										fill = watercolor,
           								color = NA)
         	) 
print(insetmap)

## ---- printmap --------
print(basemap)
print(insetmap, vp = viewport(0.72, 0.78, 
                          width = 0.55, height = 0.4))  

## ---- writemap --------
## print the basemap with the inset map in viewport
filepath="../Results/fig3.map-dot"
height=6
width=9
pdf(file=paste0(filepath, ".pdf"), height=height, width=width)
      print(basemap)
      print(insetmap, vp = viewport(0.72, 0.78, width = 0.55, height = 0.4))  
dev.off()
png(file=paste0(filepath, ".png"), height=height, width=width, units="in", res=300)
      print(basemap)
      print(insetmap, vp = viewport(0.72, 0.78, width = 0.55, height = 0.4))  
dev.off()

## ---- typemap --------			

types <- read.csv("../Data/Raw_data/Oreophryne_Type_Localities_v20250513.csv", skip=1)
names(types) <- c("sp", "latitude", "longitude")

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
		alpha=.3) +
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


## ---- insetedges --------
# basemap +
  # geom_hline(yintercept = -8.75, lty = 2, colour = "red") +
  # geom_hline(yintercept = -12, lty = 2, colour = "red") +
  # geom_vline(xintercept = 147, lty = 2, colour = "red") +
  # geom_vline(xintercept = 155, lty = 2, colour = "red") 
