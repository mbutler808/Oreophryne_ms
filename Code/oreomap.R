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

## ---- functions --------
source("plotting_functions.R") # contains print_pdfpng()

## ---- loaddata --------
d <- read.csv("../Data/Processed_data/metadata.csv")      # tree metadata spreadsheet
gc <- read.csv("../Data/gencolorABC.csv")    # color codes by genus
gcol <- gc$col
names(gcol) <- gc$gen      # gcol holds the colors for the genera (value, key)  

## ---- map --------

odat <- d %>% filter(genus=="Oreophryne")
adat <- d %>% filter(genus=="Auparoparo"|genus=="Aphantophryne")

osf <- st_as_sf(odat, coords=c("longitude", "latitude"))
asf <- st_as_sf(adat, coords=c("longitude", "latitude"))
vp <- viewport(	x=0.75, 
				y=0.75,
                width=unit(3, "inches"),
                height=unit(2,"inches"))
grid.show.viewport(vp)
scales::show_col(c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5", "#4A6C6F", "#FF5E5B"))

map_sf <- ne_countries(country = 
						c(
						  "Papua New Guinea", 
						  "Indonesia", 
						  "Philippines"
						), 
						scale=50, 
						returnclass="sf")
print(map_sf)
nrow(map_sf)
plot(map_sf$geometry)

mp <- ggplot(map_sf) +
	geom_sf(color = "grey80", fill = "#f2f2f2", linewidth = 0.25) +
	coord_sf(xlim = c(120, 154), ylim = c(-12, 10)) +
#	  theme_void() + 
  theme( axis.title = element_blank(),
         panel.background = element_rect(fill = "lightblue", colour = NA)
       )

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
    alpha=.75) 

mpdat +
  geom_hline(yintercept = -8.75, lty = 2, colour = "red") +
  geom_hline(yintercept = -12, lty = 2, colour = "red") +
  geom_vline(xintercept = 147, lty = 2, colour = "red") +
  geom_vline(xintercept = 155, lty = 2, colour = "red") 

mpdat <- mpdat +
  annotate("rect", xmin = 149, xmax = 154.5, ymin = -12, ymax = -8.75, color = "grey50", fill = NA) +
  theme_void() +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA)
        )

inset <-  
  ggplot(map_sf) + 
	geom_sf(color = "grey70", fill = "#f2f2f2", linewidth = 0.25) +
	coord_sf(xlim = c(149, 154.5), ylim = c(-12, -8.75), expand=FALSE) + #expand=F prevents ggplot from expanding slightly beyond the limits
  filter(d, genus=="Auparoparo" & (latitude > -12 &
  	latitude < -8.75 &
  	longitude > 149 & 
  	longitude < 154.5) 
  	) %>% geom_point( 
  		mapping = aes(x=longitude, y=latitude), 
	  	color="red", 
	    pch="+", 
	    size=8, 
	    alpha=.75) +
  filter(d, genus=="Oreophryne" & (latitude > -12 &
  	latitude < -8.75 &
  	longitude > 149 & 
  	longitude < 154.5) 
  	) %>%geom_point(
    		mapping = aes(x=longitude, y=latitude), 
    		colour="grey50", 
	    fill=gcol["Oreophryne"], 
	    pch=24, 
	    size=3, 
	    alpha=.75) +
  annotate("rect", xmin = 149, xmax = 154.5, ymin = -12, ymax = -8.75, color = "grey50", fill=NA, linewidth=1.5) +
  labs(x = NULL, y = NULL) + 
  xlim(149, 154.5) +
  ylim(-12, -8.75) +
  theme_void() +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA)) +
  guides(size = "none") 

mpannot <- mpdat + 
	geom_sf_text(aes(label=name_en),
		size = 3,
		fontface = "bold",
		family = "serif",
		color = "grey30",
		nudge_x = c(0,0,25),
		nudge_y = c(0,0,-4)) +
	#Add scale bar to bottom left from ggspatial
  	annotation_scale(location = "bl", 
  		height = unit(.25, "cm"), 
   		pad_x = unit(0.3, "in"), 
  		pad_y = unit(0.3, "in")) +
  	#Add north arrow to bottom left from ggspatial
  	annotation_north_arrow(height = unit(1, "cm"), 
  		width = unit(1, "cm"),
  		which_north = "true", 
  		location = "bl", 
  		pad_x = unit(2, "in"), 
  		pad_y = unit(0.5, "in")) 
  		
print(mpannot)
print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  

filepath="../Products/Figures/map"
height=6
width=9
pdf(file=paste0(filepath, ".pdf"), height=height, width=width)
      print(mpannot)
      print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  
dev.off()
png(file=paste0(filepath, ".png"), height=height, width=width, units="in", res=300)
      print(mpannot)
      print(inset, vp = viewport(0.75, 0.78, width = 0.4, height = 0.4))  
dev.off()

print_pdfpng(, print(mpannot+inset, vp=vp))

